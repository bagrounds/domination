const storageKey = 'debug_log'
const maxEntries = 400
const maxEntryLength = 1000
const persistDelayMs = 1000

const readPersisted = () => {
  try {
    const parsed = JSON.parse(window.localStorage.getItem(storageKey))
    return Array.isArray(parsed)
      ? parsed.filter(entry => typeof entry === 'string').slice(-maxEntries)
      : []
  } catch (_) {
    return []
  }
}

let buffer = null
let persistTimer = null
let installed = false

const entries = () => buffer || (buffer = readPersisted())

const persist = () => {
  clearTimeout(persistTimer)
  persistTimer = null
  try {
    window.localStorage.setItem(storageKey, JSON.stringify(entries()))
  } catch (_) {
    // A full or unavailable localStorage must never break the game.
  }
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

const truncate = text => text.length > maxEntryLength
  ? text.slice(0, maxEntryLength) + '…'
  : text

const record = (level, values) => {
  const log = entries()
  log.push(`${new Date().toISOString()} ${level} ${truncate(values.map(describe).join(' '))}`)
  log.splice(0, Math.max(0, log.length - maxEntries))
  schedulePersist()
}

const captureConsole = level => {
  const original = console[level].bind(console)
  console[level] = (...values) => {
    record(level.toUpperCase(), values)
    original(...values)
  }
}

export const install = () => {
  if (installed) return
  installed = true
  ;['log', 'info', 'warn', 'error'].forEach(captureConsole)
  window.addEventListener('error', ({ message, filename, lineno, colno, error }) =>
    record('UNCAUGHT', [message, `${filename}:${lineno}:${colno}`, error || '']))
  window.addEventListener('unhandledrejection', ({ reason }) =>
    record('UNHANDLED_REJECTION', [reason]))
  window.addEventListener('pagehide', persist)
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'hidden') persist()
  })
  record('SESSION', ['started'])
}

export const snapshot = () => entries().slice()

export const clear = () => {
  buffer = []
  persist()
}

export const userAgent = () => navigator.userAgent

export const nowIso = () => new Date().toISOString()
