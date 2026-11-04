// A chaos test of room sync in real browsers.
//
// Two players (A on seat 0, B on seat 1) play one game in separate browser
// profiles. A seeded random schedule makes random valid plays (clicking only
// what the game offers: playable cards, buyable cards, the next-phase button,
// and choices), and randomly does what phones do to a page: hide it, freeze it
// (Chrome freezes hidden pages, as on Android), take the network away, leave a
// socket half-open, close the tab or discard it, and take the mailbox down.
// Then everything comes back, nobody plays, and both devices must show the
// same game, the latest one, within the window the resync policy promises.
//
// usage: node chaos.js [--room mailbox|global] [--seed n] [--runs n] [--steps n]
import { chromium } from 'playwright-core'
import { mkdir, writeFile } from 'node:fs/promises'
import { startServers } from './servers.js'

const args = Object.fromEntries(process.argv.slice(2).reduce((pairs, arg, i, all) =>
  arg.startsWith('--') ? [...pairs, [arg.slice(2), all[i + 1]]] : pairs, []))
const roomKind = args.room ?? 'mailbox'
const firstSeed = Number(args.seed ?? Date.now() % 100000)
const runs = Number(args.runs ?? 1)
const stepCount = Number(args.steps ?? 120)
const chrome = process.env.CHROME ?? '/usr/bin/google-chrome'

// Resync policy (src/Data/Resync.purs): a visible page polls every 30 s and
// replaces a socket silent for 15 s, backing off to 2 minutes.
const scenario = args.scenario ?? 'chaos'
const windowMs = scenario === 'away' ? 45_000 : roomKind === 'mailbox' ? 70_000 : 130_000

const mulberry32 = seed => () => {
  seed = (seed + 0x6D2B79F5) | 0
  let t = Math.imul(seed ^ (seed >>> 15), 1 | seed)
  t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t
  return ((t ^ (t >>> 14)) >>> 0) / 4294967296
}

const sleep = ms => new Promise(resolve => setTimeout(resolve, ms))

const seedStorage = ({ entries }) => {
  if (location.protocol !== 'http:') return
  Object.entries(entries).forEach(([key, value]) => {
    if (localStorage.getItem(key) === null) localStorage.setItem(key, value)
  })
  // Pages are never really hidden in a test browser; this stands in for the
  // phone's screen going off or another app coming to the front.
  let hidden = false
  Object.defineProperty(Document.prototype, 'visibilityState', { get: () => hidden ? 'hidden' : 'visible' })
  Object.defineProperty(Document.prototype, 'hidden', { get: () => hidden })
  window.__setHidden = value => {
    hidden = value
    document.dispatchEvent(new Event('visibilitychange'))
  }
}

// Runs in the page: one random valid choice, if the game offers any.
const randomClick = ({ r1, r2, key }) => {
  const saved = JSON.parse(localStorage.getItem(key) ?? 'null')
  const shown = element => element.offsetParent !== null && !element.disabled
  const all = selector => [...document.querySelectorAll(selector)].filter(shown)
  const dialogue = all('.dialogue .card, .dialogue button, .dialogue .stack-card, .dialogue .resolve-choice')
  const resolve = all('.dialogue .resolve-choice')
  const myTurn = saved !== null && saved.state.turn === saved.playerIndex
  const candidates = dialogue.length > 0
    ? (r2 < 0.4 && resolve.length > 0 ? resolve : dialogue)
    : myTurn ? all('.can-play, .can-buy, button.next-phase') : []
  if (candidates.length === 0) return null
  const element = candidates[Math.floor(r1 * candidates.length)]
  const label = (element.className || element.tagName) + ' ' + (element.textContent || '').trim().slice(0, 30)
  element.click()
  return label
}

const gameKey = room => room === null ? 'game_state' : `room/${room}/game_state`
const syncKey = room => room === null ? 'mailbox_sync' : `room/${room}/mailbox_sync`

const run = async seed => {
  const random = mulberry32(seed)
  const chance = p => random() < p
  const pick = xs => xs[Math.floor(random() * xs.length)]
  const started = Date.now()
  const lines = []
  const journal = line => lines.push(`${((Date.now() - started) / 1000).toFixed(1).padStart(7)} ${line}`)
  const port = 8100 + (seed % 800)
  const servers = await startServers({
    staticPort: port, relayPort: port + 1000, mailboxPort: port + 2000, journal,
  })
  const room = roomKind === 'mailbox' ? `chaos${seed}` : null
  const url = `${servers.site.url}/assets/index.html${room === null ? '' : `#room=${room}`}`
  const browser = await chromium.launch({ executablePath: chrome, args: ['--no-sandbox'] })

  const makeDevice = async (name, seat) => {
    const context = await browser.newContext()
    await context.addInitScript(seedStorage, {
      entries: {
        'server-url': JSON.stringify(servers.relay.url(name)),
        'mailbox-url': JSON.stringify(servers.mailbox.url),
        username: JSON.stringify(name),
        player_index: JSON.stringify(seat),
        player_count: JSON.stringify(2),
      },
    })
    const device = { name, seat, context, page: null, cdp: null, hidden: false, frozen: false, offline: false }
    device.open = async () => {
      device.page = await context.newPage()
      device.page.on('console', message => {
        const text = message.text()
        if (!/Received message|sent message|queued message|Heartbeat from|username incoming|maybeBroadcaster|Expected i/.test(text)) {
          journal(`${name} console: ${text.slice(0, 300)}`)
        }
      })
      device.page.on('pageerror', error => journal(`${name} PAGE ERROR: ${error.message}`))
      device.cdp = await context.newCDPSession(device.page)
      await device.page.goto('about:blank')
      await device.page.goto(url)
      await device.page.waitForSelector('.domination', { timeout: 20_000 })
      device.hidden = false
      device.frozen = false
    }
    device.close = async () => {
      await device.page.close({ runBeforeUnload: false })
      device.page = null
      device.cdp = null
    }
    device.awake = () => device.page !== null && !device.frozen
    device.setHidden = async value => {
      device.hidden = value
      await device.page.evaluate(v => window.__setHidden(v), value)
    }
    device.lifecycle = async state => {
      await device.cdp.send('Page.setWebLifecycleState', { state })
      device.frozen = state === 'frozen'
    }
    device.setOffline = async value => {
      device.offline = value
      await context.setOffline(value)
    }
    device.read = async () => device.page.evaluate(([gk, sk]) => ({
      game: localStorage.getItem(gk),
      sync: localStorage.getItem(sk),
    }), [gameKey(room), syncKey(room)])
    return device
  }

  const summary = async device => {
    const { game, sync } = await device.read()
    const parsed = game === null ? null : JSON.parse(game)
    return {
      i: parsed?.i ?? null,
      turn: parsed?.state?.turn ?? null,
      state: parsed === null ? null : JSON.stringify(parsed.state),
      sync,
    }
  }

  const startGame = async d => {
    if (!await d.page.isVisible('button.new-game-button')) await d.page.click('.settings-button')
    await d.page.click('button.new-game-button')
  }

  const a = await makeDevice('A', 0)
  const b = await makeDevice('B', 1)
  const devices = [a, b]
  const failure = []
  let plays = 0
  try {
    await a.open()
    if (room === null) {
      // In the global room a device takes the seat of the last game it started.
      await b.open()
      await startGame(b)
      await sleep(1000)
      await startGame(a)
    } else {
      await startGame(a)
      await sleep(1000)
      await b.open()
    }
    journal('setup: A started the game')
    await sleep(3000)

    const actions = [
      [30, 'play', d => d.awake() && !d.hidden],
      [4, 'hide', d => d.awake() && !d.hidden],
      [2, 'pagehide', d => d.awake() && !d.hidden],
      [5, 'show', d => d.awake() && d.hidden],
      [3, 'freeze', d => d.awake() && d.hidden],
      [4, 'resume', d => d.page !== null && d.frozen],
      [2, 'offline', d => !d.offline],
      [3, 'online', d => d.offline],
      [2, 'half-open', d => d.page !== null],
      [1, 'cut', d => d.page !== null],
      [1, 'close', d => d.page !== null && !d.frozen],
      [3, 'open', d => d.page === null && !d.offline],
      [1, 'mailbox-down', () => room !== null && servers.mailbox.isUp()],
      [2, 'mailbox-up', () => room !== null && !servers.mailbox.isUp()],
      [6, 'wait', () => true],
    ]
    const perform = async (action, d) => {
      switch (action) {
        case 'play': {
          const label = await d.page.evaluate(randomClick, { r1: random(), r2: random(), key: gameKey(room) })
          if (label !== null) plays += 1
          if (label !== null) journal(`${d.name} play: ${label}`)
          await sleep(label === null ? 20 : 120)
          break
        }
        case 'hide': await d.setHidden(true); break
        // Mobile browsers may fire pagehide on a page they keep alive and
        // later show again without the back-forward cache.
        case 'pagehide':
          await d.setHidden(true)
          await d.page.evaluate(() => window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: false })))
          break
        case 'show': await d.setHidden(false); break
        case 'freeze': await d.lifecycle('frozen'); break
        case 'resume': await d.lifecycle('active'); break
        case 'offline': await d.setOffline(true); if (chance(0.5)) servers.relay.cut(d.name); else servers.relay.halfOpen(d.name); break
        case 'online': await d.setOffline(false); break
        case 'half-open': servers.relay.halfOpen(d.name); break
        case 'cut': servers.relay.cut(d.name); break
        case 'close': await d.close(); break
        case 'open': await d.open(); break
        case 'mailbox-down': servers.mailbox.setUp(false); break
        case 'mailbox-up': servers.mailbox.setUp(true); break
        case 'wait': await sleep(Math.floor(random() * 8000)); break
      }
    }
    // The reported stuck game: A and B play a while, A leaves the app, B
    // keeps playing, and A comes back to B's newest game.
    const playUntil = async (d, target, limit) => {
      for (let tries = 0; plays < target && tries < limit; tries++) await perform('play', d)
    }
    if (scenario === 'away') {
      const leave = args.leave ?? pick(['hide', 'freeze', 'close', 'offline', 'half-open', 'pagehide'])
      for (let n = 0; n < stepCount; n++) await perform('play', pick(devices))
      for (let tries = 0; tries < 600 && (await summary(a)).turn === a.seat; tries++) await perform('play', a)
      await sleep(1500)
      journal(`away: A leaves (${leave}) after ${plays} plays`)
      await perform('hide', a)
      if (leave === 'freeze') await perform('freeze', a)
      if (leave === 'close') await perform('close', a)
      if (leave === 'pagehide') await perform('pagehide', a)
      if (leave === 'offline') await perform('offline', a)
      if (leave === 'half-open') { await perform('freeze', a); servers.relay.halfOpen('A') }
      await sleep(Math.floor(random() * 20000))
      await playUntil(b, plays + 40, 400)
      await sleep(Math.floor(random() * 20000))
      journal(`away: A comes back after B made it ${plays} plays`)
    }
    for (let n = 0; scenario === 'chaos' && n < stepCount; n++) {
      const options = devices.flatMap(d => actions.filter(([, , ok]) => ok(d)).map(([w, action]) => ({ w, action, d })))
      const total = options.reduce((sum, { w }) => sum + w, 0)
      let x = random() * total
      const { action, d } = options.find(({ w }) => (x -= w) < 0) ?? options[0]
      if (action !== 'play') journal(`step ${n}: ${d.name} ${action}`)
      await perform(action, d)
    }

    journal(`calm: everyone comes back (${plays} plays made)`)
    servers.mailbox.setUp(true)
    for (const d of devices) {
      if (d.offline) await d.setOffline(false)
      if (d.page === null) await d.open()
      if (d.frozen) await d.lifecycle('active')
      if (d.hidden) await d.setHidden(false)
    }
    const deadline = Date.now() + windowMs
    let last = null
    for (;;) {
      const [sa, sb] = await Promise.all(devices.map(summary))
      last = { A: sa, B: sb }
      const agree = sa.state !== null && sa.state === sb.state && sa.i === sb.i
      const mailboxOk = room === null || sa.sync === sb.sync
      if (agree && mailboxOk) {
        journal(`converged at i=${sa.i} after ${((windowMs - (deadline - Date.now())) / 1000).toFixed(0)} s`)
        break
      }
      if (Date.now() > deadline) {
        failure.push(`no convergence within ${windowMs / 1000} s: A i=${sa.i} turn=${sa.turn} sync=${sa.sync}; B i=${sb.i} turn=${sb.turn} sync=${sb.sync}`
          + (room === null ? '' : `; mailbox revision ${servers.mailbox.revision(room)}`))
        break
      }
      await sleep(2000)
    }
    journal(`final: ${JSON.stringify({ A: { i: last.A.i, sync: last.A.sync }, B: { i: last.B.i, sync: last.B.sync } })}`)
  } catch (error) {
    failure.push(`harness error: ${error.stack}`)
  } finally {
    for (const d of devices) {
      try {
        if (d.page !== null) {
          if (d.frozen) await d.lifecycle('active')
          const report = await d.page.evaluate(() => localStorage.getItem('debug_log'))
          await mkdir(new URL(`./runs/${scenario}-${roomKind}-${seed}/`, import.meta.url), { recursive: true })
          await writeFile(new URL(`./runs/${scenario}-${roomKind}-${seed}/${d.name}-debug-log.json`, import.meta.url), report ?? '')
        }
      } catch (_) { /* the report is a bonus */ }
    }
    await browser.close()
    servers.close()
  }
  const dir = new URL(`./runs/${scenario}-${roomKind}-${seed}/`, import.meta.url)
  await mkdir(dir, { recursive: true })
  await writeFile(new URL('journal.txt', dir), lines.join('\n') + '\n')
  const ok = failure.length === 0
  console.log(`${ok ? 'PASS' : 'FAIL'} ${scenario} ${roomKind} seed ${seed}: ${plays} plays${ok ? '' : '\n  ' + failure.join('\n  ')}`)
  return ok
}

let failed = 0
for (let k = 0; k < runs; k++) {
  if (!await run(firstSeed + k)) failed += 1
}
console.log(`${runs - failed}/${runs} runs converged`)
process.exit(failed === 0 ? 0 : 1)
