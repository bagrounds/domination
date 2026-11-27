// Every control on every Settings tab must be reachable by scrolling, on a
// small phone held either way and on a desktop, with the tab bar staying put.
//
// Each device starts with a dozen saved rooms, so the Storage tab is taller
// than any screen, and every <details> on a tab is opened before scrolling.
// Phones scroll with touch swipes, the desktop with the mouse wheel, both from
// mid-screen and near the bottom, until nothing on the page scrolls any
// further. Then the tab's last control must be on screen, and a tap at its
// centre must land on it.
//
// usage: node settings-scroll.js   (after `npm run bundle` at the root)
import { chromium } from 'playwright-core'
import { startServers } from './servers.js'

const chrome = process.env.CHROME ?? '/usr/bin/google-chrome'

const screens = [
  { name: 'small phone', width: 360, height: 640, touch: true },
  { name: 'landscape phone', width: 844, height: 390, touch: true },
  { name: 'desktop', width: 1280, height: 800, touch: false },
]

const savedRooms = Object.fromEntries(Array.from({ length: 12 }, (_, i) =>
  [`room/room${i}/chat`, JSON.stringify('x'.repeat(100 * (i + 1)))]))

const seedStorage = entries =>
  Object.entries(entries).forEach(([key, value]) => localStorage.setItem(key, value))

const totalScroll = page => page.evaluate(() =>
  [...document.querySelectorAll('*')].reduce((sum, el) => sum + el.scrollTop, 0))

const anchors = [0.55, 0.9]

const swipeUp = async (cdp, { width, height }, anchor) => {
  const x = Math.round(width / 2)
  const from = Math.round(height * anchor)
  const to = Math.round(height * (anchor - 0.5))
  await cdp.send('Input.dispatchTouchEvent', { type: 'touchStart', touchPoints: [{ x, y: from }] })
  for (const step of Array.from({ length: 10 }, (_, i) => i + 1)) {
    await cdp.send('Input.dispatchTouchEvent', {
      type: 'touchMove', touchPoints: [{ x, y: from + (to - from) * step / 10 }],
    })
  }
  await cdp.send('Input.dispatchTouchEvent', { type: 'touchEnd', touchPoints: [] })
}

const wheelDown = async (page, { width, height }, anchor) => {
  await page.mouse.move(width / 2, height * anchor)
  await page.mouse.wheel(0, height)
}

const scrollToEnd = async (page, scroll, previous = -1, tries = 80) => {
  const current = await totalScroll(page)
  if (current === previous || tries === 0) return
  for (const anchor of anchors) {
    await scroll(anchor)
    await page.waitForTimeout(150)
  }
  return scrollToEnd(page, scroll, current, tries - 1)
}

const lastControlReach = page => page.evaluate(() => {
  const panel = document.querySelector('[role=tabpanel]')
  const controls = panel.querySelectorAll('button, input, textarea, select, a, summary')
  const control = controls[controls.length - 1]
  const box = control.getBoundingClientRect()
  const hit = document.elementFromPoint(box.left + box.width / 2, box.top + box.height / 2)
  const tabs = document.querySelector('[role=tablist]').getBoundingClientRect()
  return {
    control: control.outerHTML.slice(0, 60),
    onScreen: box.top >= 0 && box.bottom <= innerHeight,
    tappable: hit !== null && (hit === control || control.contains(hit)),
    tabsOnScreen: tabs.top >= 0 && tabs.bottom <= innerHeight,
  }
})

const checkScreen = async (browser, siteUrl, entries, screen) => {
  const context = await browser.newContext({
    viewport: { width: screen.width, height: screen.height },
    hasTouch: screen.touch,
    isMobile: screen.touch,
  })
  await context.addInitScript(seedStorage, entries)
  const page = await context.newPage()
  await page.goto(`${siteUrl}/assets/index.html`)
  await page.click('.settings-button')
  const cdp = await context.newCDPSession(page)
  const scroll = screen.touch ? anchor => swipeUp(cdp, screen, anchor) : anchor => wheelDown(page, screen, anchor)
  const tabs = await page.$$eval('[role=tab]', buttons => buttons.map(button => button.textContent))
  const results = []
  for (const tab of tabs) {
    await page.click(`[role=tab]:text-is("${tab}")`)
    await page.waitForSelector('[role=tabpanel] :is(button, input, textarea, select, a, summary)')
    await page.waitForTimeout(500)
    await page.$$eval('[role=tabpanel] details', all => all.forEach(details => { details.open = true }))
    await scrollToEnd(page, scroll)
    const reach = await lastControlReach(page)
    const passed = reach.onScreen && reach.tappable && reach.tabsOnScreen
    console.log(`${passed ? 'PASS' : 'FAIL'} ${screen.name} ${screen.width}x${screen.height} ${tab}:`
      + ` last control on screen ${reach.onScreen}, tappable ${reach.tappable}, tabs on screen ${reach.tabsOnScreen}`
      + ` (${reach.control})`)
    results.push(passed)
  }
  await context.close()
  return results
}

const main = async () => {
  const servers = await startServers({ staticPort: 8095, relayPort: 8096, mailboxPort: 8097, journal: () => {} })
  const browser = await chromium.launch({ executablePath: chrome, args: ['--no-sandbox', '--headless=new'] })
  const entries = {
    'server-url': JSON.stringify(servers.relay.url('settings')),
    'mailbox-url': JSON.stringify(servers.mailbox.url),
    ...savedRooms,
  }
  try {
    const results = []
    for (const screen of screens) results.push(...await checkScreen(browser, servers.site.url, entries, screen))
    const failed = results.filter(passed => !passed).length
    console.log(failed === 0 ? `All ${results.length} tab checks passed` : `${failed} of ${results.length} tab checks failed`)
    process.exitCode = failed === 0 ? 0 : 1
  } finally {
    await browser.close()
    servers.close()
  }
}

await main()
