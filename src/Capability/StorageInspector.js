// Discovers and removes what this origin stores, area by area, with only
// standard APIs. Every area is feature-detected and every call is guarded:
// an area that is missing or blocked reports why, and never breaks the rest.
const describeFailure = e => e && e.name
  ? `${e.name}: ${e.message}`
  : String(e)

const webStorage = which => which === 'session'
  ? window.sessionStorage
  : window.localStorage

const webStorageEntries = which => left => right => () => {
  try {
    const storage = webStorage(which)
    const entries = Array.from({ length: storage.length }, (_, i) => storage.key(i))
      .filter(name => name !== null)
      .map(name => ({ name, size: name.length + (storage.getItem(name) || '').length }))
    return right(entries)
  } catch (e) {
    return left(describeFailure(e))
  }
}

export const localEntries = webStorageEntries('local')

export const sessionEntries = webStorageEntries('session')

export const readWebStorageImpl = which => just => nothing => name => () => {
  try {
    const value = webStorage(which).getItem(name)
    return value === null ? nothing : just(value)
  } catch (_) {
    return nothing
  }
}

export const cookieEntries = left => right => () => {
  try {
    if (typeof document.cookie !== 'string') return left('not available')
    const entries = document.cookie.split(';')
      .map(cookie => cookie.trim())
      .filter(cookie => cookie !== '')
      .map(cookie => ({ name: cookie.split('=')[0], size: cookie.length }))
    return right(entries)
  } catch (e) {
    return left(describeFailure(e))
  }
}

// A promise as an EffectFnAff that always succeeds, with `left` for a
// failure, so one area's failure is data rather than an exception.
const settle = (left, right, start) => (onError, onSuccess) => {
  Promise.resolve()
    .then(start)
    .then(value => onSuccess(right(value)), e => onSuccess(left(describeFailure(e))))
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess()
}

const unavailable = what => () => Promise.reject(new Error(`${what} is not available in this browser`))

export const databaseEntriesImpl = left => right => settle(left, right,
  typeof indexedDB !== 'undefined' && typeof indexedDB.databases === 'function'
    ? () => indexedDB.databases().then(databases => databases
      .filter(({ name }) => typeof name === 'string')
      .map(({ name, version }) => ({ name, version: Number(version) || 0 })))
    : unavailable('Listing IndexedDB databases'))

const bodySize = response => response && response.type !== 'opaque'
  ? response.blob().then(blob => blob.size)
  : Promise.resolve(null)

const cacheSize = name => caches.open(name)
  .then(cache => cache.keys().then(requests => Promise.all(requests.map(request => cache.match(request).then(bodySize)))))
  .then(sizes => sizes.some(size => size === null) ? null : sizes.reduce((sum, size) => sum + size, 0))

export const cacheEntriesImpl = just => nothing => left => right => settle(left, right,
  typeof caches !== 'undefined'
    ? () => caches.keys().then(names => Promise.all(names.map(name =>
      cacheSize(name)
        .catch(() => null)
        .then(size => ({ name, size: size === null ? nothing : just(size) })))))
    : unavailable('Cache Storage'))

export const registrationEntriesImpl = left => right => settle(left, right,
  typeof navigator !== 'undefined' && navigator.serviceWorker && navigator.serviceWorker.getRegistrations
    ? () => navigator.serviceWorker.getRegistrations().then(registrations => registrations.map(registration => {
      const worker = registration.active || registration.waiting || registration.installing
      return { scope: registration.scope, script: worker ? worker.scriptURL : '' }
    }))
    : unavailable('Service workers'))

const fileSize = handle => handle.kind === 'file'
  ? handle.getFile().then(file => file.size)
  : Promise.resolve(null)

const directoryEntries = async directory => {
  const entries = []
  for await (const handle of directory.values()) {
    const size = await fileSize(handle).catch(() => null)
    entries.push({ name: handle.name, size })
  }
  return entries
}

export const fileEntriesImpl = just => nothing => left => right => settle(left, right,
  typeof navigator !== 'undefined' && navigator.storage && navigator.storage.getDirectory
    ? () => navigator.storage.getDirectory()
      .then(directoryEntries)
      .then(entries => entries.map(({ name, size }) => ({ name, size: size === null ? nothing : just(size) })))
    : unavailable('Origin private files'))

export const estimateImpl = just => nothing => (onError, onSuccess) => {
  const storage = typeof navigator !== 'undefined' ? navigator.storage : undefined
  if (!storage || !storage.estimate) {
    onSuccess(nothing)
  } else {
    Promise.all([
      storage.estimate(),
      storage.persisted ? storage.persisted() : Promise.resolve(false),
    ]).then(
      ([{ usage, quota }, persisted]) => onSuccess(just({ usage: Number(usage) || 0, quota: Number(quota) || 0, persisted: Boolean(persisted) })),
      () => onSuccess(nothing))
  }
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess()
}

// Removals report one of three outcomes; a database that other connections
// hold open is deleted once they close, so it is waiting, not failed.
const outcome = (removed, waiting, failed, start) => (onError, onSuccess) => {
  Promise.resolve()
    .then(start)
    .then(result => onSuccess(result === 'waiting' ? waiting('close other tabs that use it') : removed),
      e => onSuccess(failed(describeFailure(e))))
  return (cancelError, onCancelerError, onCancelerSuccess) => onCancelerSuccess()
}

export const removeWebStorageImpl = which => name => removed => waiting => failed =>
  outcome(removed, waiting, failed, () => webStorage(which).removeItem(name))

export const deleteDatabaseImpl = name => removed => waiting => failed =>
  outcome(removed, waiting, failed, () => new Promise((resolve, reject) => {
    const request = indexedDB.deleteDatabase(name)
    request.onsuccess = () => resolve('removed')
    request.onerror = () => reject(request.error)
    request.onblocked = () => resolve('waiting')
  }))

export const deleteCacheImpl = name => removed => waiting => failed =>
  outcome(removed, waiting, failed, () => caches.delete(name).then(found => {
    if (!found) throw new Error('no cache by that name')
  }))

export const unregisterImpl = scope => removed => waiting => failed =>
  outcome(removed, waiting, failed, () => navigator.serviceWorker.getRegistration(scope)
    .then(registration => {
      if (!registration || registration.scope !== scope) throw new Error('no service worker for that scope')
      return registration.unregister()
    }))

export const removeFileImpl = name => removed => waiting => failed =>
  outcome(removed, waiting, failed, () => navigator.storage.getDirectory()
    .then(directory => directory.removeEntry(name, { recursive: true })))

export const origin = () => location.origin

export const isOnline = () => navigator.onLine !== false

// Where `sw.js`, registered relative to the page, gets its default scope.
export const serviceWorkerScope = () => new URL('./', document.baseURI).href
