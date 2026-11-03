'use strict'

// One HTTP exchange with the room mailbox. Every outcome, including a network
// failure or a timeout, resolves to { status, etag, body }; status 0 means no
// response arrived. A timed-out request is aborted, so its reply can never
// arrive after a newer one.
export const requestFFI = nothing => just => request => (onError, onSuccess) => {
  const controller = new AbortController()
  const timer = setTimeout(() => controller.abort(), request.timeoutMs)
  const headers = Object.fromEntries(request.headers.map(({ name, value }) => [name, value]))
  const failed = error => ({ status: 0, etag: nothing, body: String(error && error.message || error) })

  fetch(request.url, {
    method: request.method,
    headers,
    body: request.body === '' ? undefined : request.body,
    cache: 'no-store',
    signal: controller.signal,
  })
    .then(response => response.text().then(body => ({
      status: response.status,
      etag: response.headers.get('ETag') === null ? nothing : just(response.headers.get('ETag')),
      body,
    })))
    .catch(failed)
    .then(result => {
      clearTimeout(timer)
      onSuccess(result)
    })

  return (cancelError, onCancelerError, onCancelerSuccess) => {
    controller.abort()
    clearTimeout(timer)
    onCancelerSuccess()
  }
}

export const documentVisible = () =>
  typeof document === 'undefined' || document.visibilityState !== 'hidden'
