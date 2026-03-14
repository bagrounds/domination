// Add singleton management at the top
const getWebSocket = () => window._singletonWebSocket
const setWebSocket = (ws) => { window._singletonWebSocket = ws }
const clearWebSocket = () => {
  const ws = getWebSocket()
  if (ws) {
    try {
      ws.close()
      logInfo('Closed existing WebSocket connection')
    } catch (e) {
      logError('Error closing existing WebSocket:', e)
    }
  }
  window._singletonWebSocket = null
}

const log = level => (...args) => console[level]('WebSocket FFI: ', ...args)
const logInfo = (...args) => log('log')(...args)
const logError = (...args) => {
  log('error')(...args)
  const errorMessage = args.map(arg =>
    typeof arg === 'object' ? JSON.stringify(arg) : arg
  ).join(' ')
  alert('Error: ' + errorMessage)
}

const broadcastEvent = messageTarget => event => {
  const eventTarget = document.querySelector('#' + messageTarget)
  if (eventTarget) {
    eventTarget.dispatchEvent(customEvent(event))
  }
  else {
    logError(`${messageTarget} undefined, cannot dispatch event: `, event)
  }
}

const customEvent = detail =>
  new CustomEvent('purescript', { detail })

export const detail = ({ detail }) => detail

export const showWebSocket = ws => `WebSocket(${ws.url})`

export const makeWebSocketFFI = left =>
  right =>
  roomCode =>
  remoteMessageTarget =>
  serverUrl =>
  callback =>
  () => {
    try {
      // Check for existing connection
      const existing = getWebSocket()
      if (existing) {
        logInfo('Reusing existing WebSocket connection')
        callback(right(existing))()
        return () => {} // No-op cleanup for reused connection
      }

      const ws = new WebSocket(serverUrl)
      ws.address = crypto.randomUUID()
      setWebSocket(ws)

      const shutdown = (event) => {
        try {
          ws.close()
          clearWebSocket()
          logInfo(`closed websocket on '${event.type}'`)
        } catch (e) {
          logInfo(`failed to close websocket on '${event.type}'`)
        }
      }

      window.addEventListener("unload", shutdown)
      window.addEventListener("beforeunload", shutdown)

      ws.onopen = () => {
        logInfo('WebSocket connected')
        callback(right(ws))()
      }

      ws.onclose = (event) => {
        logInfo('WebSocket disconnected', event.code, event.reason)

        // Attempt reconnection if not intentionally closed
        if (event.code !== 1000 && event.code !== 1001) {
          logInfo('Attempting to reconnect in 5 seconds...')
          setTimeout(() => {
            clearWebSocket()
            makeWebSocketFFI(left)(right)(roomCode)
              (remoteMessageTarget)(serverUrl)(callback)()
          }, 5000)
        }
      }

      ws.onmessage = event => {
        logInfo('Received message:', event.data)
        if (event.data instanceof Blob) {
          event.data.text().then(text => {
            broadcastEvent(remoteMessageTarget)(text)
          })
        } else {
          broadcastEvent(remoteMessageTarget)(event.data)
        }
      }

      ws.onerror = error => {
        logError('WebSocket error:', error)
        callback(left(error))()
      }

      return () => shutdown({ type: 'cancel' })
    } catch (error) {
      logError('Connection setup error:', error)
      callback(left(error))()
      return () => {}
    }
}

export const send = ws => message => () => {
  logInfo("sent message: ", message)
  ws.send(message)
}

export const address = ws => () => ws.address
