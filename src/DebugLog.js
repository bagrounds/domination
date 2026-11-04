// Two ring buffers, persisted in localStorage so a report survives a reload
// or a discarded tab:
// - `debug_log`: everything written to the console, with the chatty
//   per-message lines shortened so they cannot push out the rest;
// - `sync_journal`: only what explains whether a device kept up with its
//   room (connection, mailbox, clock and page lifecycle), kept much longer.
const consoleLog = { key: 'debug_log', max: 400 }
const syncJournal = { key: 'sync_journal', max: 1500 }
const maxEntryLength = 1000
const maxChattyLength = 160
const persistDelayMs = 1000

// Console lines that belong in the sync journal.
const syncPrefixes = [
  'Mailbox:',
  'Clock:',
  'Resync:',
  'Relay',
  'Sync:',
  'Initialize completed',
  'WebSocket FFI:  WebSocket',
  'WebSocket FFI:  reconnecting',
  'WebSocket FFI:  reopening',
  'WebSocket FFI:  restarting',
  'WebSocket FFI:  stopped',
  'WebSocket FFI:  flushing',
  'WebSocket FFI:  queue full',
  'WebSocket FFI:  could not reach',
  'Client joined',
  'Client left',
]

// Console lines that carry whole payloads or repeat every few seconds.
const chattyPrefixes = [
  'WebSocket FFI:  Received message',
  'WebSocket FFI:  sent message',
  'WebSocket FFI:  not connected, queued message',
  'Heartbeat from',
  'maybeBroadcaster',
  'username incoming',
]

const readPersisted = ({ key, max }) => {
  try {
    const parsed = JSON.parse(window.localStorage.getItem(key))
    return Array.isArray(parsed)
      ? parsed.filter(entry => typeof entry === 'string').slice(-max)
      : []
  } catch (_) {
    return []
  }
}

const buffers = new Map()
let persistTimer = null
let installed = false

const entries = channel => {
  if (!buffers.has(channel)) buffers.set(channel, readPersisted(channel))
  return buffers.get(channel)
}

const persist = () => {
  clearTimeout(persistTimer)
  persistTimer = null
  ;[consoleLog, syncJournal].forEach(channel => {
    try {
      window.localStorage.setItem(channel.key, JSON.stringify(entries(channel)))
    } catch (_) {
      // A full or unavailable localStorage must never break the game.
    }
  })
}

const schedulePersist = () => {
  if (persistTimer === null) {
    persistTimer = setTimeout(persist, persistDelayMs)
  }
}

const describe = value => {
  if (typeof value === 'string') return value
  if (value instanceof Error) return `${value.name}: ${value.message}\n${value.stack || ''}`
  try {
    return JSON.stringify(value)
  } catch (_) {
    return String(value)
  }
}

const truncate = (text, max) => text.length > max
  ? `${text.slice(0, max)}… (${text.length} chars)`
  : text

const startsWithAny = prefixes => text => prefixes.some(prefix => text.startsWith(prefix))
const isSync = startsWithAny(syncPrefixes)
const isChatty = startsWithAny(chattyPrefixes)

const append = (channel, line) => {
  const log = entries(channel)
  log.push(line)
  log.splice(0, Math.max(0, log.length - channel.max))
  schedulePersist()
}

const record = (level, values) => {
  const text = values.map(describe).join(' ')
  const stamp = new Date().toISOString()
  append(consoleLog, `${stamp} ${level} ${truncate(text, isChatty(text) ? maxChattyLength : maxEntryLength)}`)
  if (isSync(text) || level === 'ERROR' || level === 'UNCAUGHT') {
    append(syncJournal, `${stamp} ${level} ${truncate(text, maxEntryLength)}`)
  }
}

const lifecycle = what => record('LIFECYCLE', [what])

const captureConsole = level => {
  const original = console[level].bind(console)
  console[level] = (...values) => {
    record(level.toUpperCase(), values)
    original(...values)
  }
}

const visibility = () => document.visibilityState

// The page lifecycle as mobile browsers drive it: hidden, frozen, resumed,
// discarded and reloaded, or hidden by `pagehide`.
const watchLifecycle = () => {
  document.addEventListener('visibilitychange', () => {
    lifecycle(`visibility ${visibility()}`)
    if (visibility() === 'hidden') persist()
  })
  document.addEventListener('freeze', () => {
    lifecycle('frozen')
    persist()
  })
  document.addEventListener('resume', () => lifecycle(`resumed (visibility ${visibility()})`))
  window.addEventListener('pagehide', ({ persisted }) => {
    lifecycle(`pagehide (persisted: ${persisted})`)
    persist()
  })
  window.addEventListener('pageshow', ({ persisted }) => lifecycle(`pageshow (persisted: ${persisted})`))
  window.addEventListener('online', () => lifecycle('browser online'))
  window.addEventListener('offline', () => lifecycle('browser offline'))
}

export const install = () => {
  if (installed) return
  installed = true
  ;['log', 'info', 'warn', 'error'].forEach(captureConsole)
  window.addEventListener('error', ({ message, filename, lineno, colno, error }) =>
    record('UNCAUGHT', [message, `${filename}:${lineno}:${colno}`, error || '']))
  window.addEventListener('unhandledrejection', ({ reason }) =>
    record('UNHANDLED_REJECTION', [reason]))
  watchLifecycle()
  lifecycle(`session started (visibility ${visibility()}, discarded before: ${Boolean(document.wasDiscarded)}, online: ${navigator.onLine})`)
}

export const snapshot = () => entries(consoleLog).slice()

export const journal = () => entries(syncJournal).slice()

export const clear = () => {
  buffers.set(consoleLog, [])
  buffers.set(syncJournal, [])
  persist()
}

export const userAgent = () => navigator.userAgent

export const nowIso = () => new Date().toISOString()

export const pageFacts = () => [
  { label: 'Page', value: `${location.pathname}${location.hash}` },
  { label: 'Visibility', value: visibility() },
  { label: 'Browser online', value: String(navigator.onLine) },
  { label: 'Discarded before this load', value: String(Boolean(document.wasDiscarded)) },
  { label: 'Standalone app', value: String(window.matchMedia('(display-mode: standalone)').matches) },
]
