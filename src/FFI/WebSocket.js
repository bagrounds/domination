'use strict'

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

// Add message buffer constants and functions at the top
const MESSAGE_BUFFER_LENGTH = 4
const messageBuffer = []

const newMessage = (message) => {
  if (messageBuffer.includes(message)) {
    logInfo('Duplicate message detected, ignoring')
    return false
  }
  messageBuffer.push(message)
  while (messageBuffer.length > MESSAGE_BUFFER_LENGTH) {
    messageBuffer.shift()
  }
  return true
}

const log = level => (...args) => console[level]('WebSocket FFI: ', ...args)
const logInfo = (...args) => log('log')(...args)
const logError = (...args) => log('error')(...args)

const broadcastEvent = messageTarget => event => {
  const eventTarget = document.querySelector('#' + messageTarget)
  if (eventTarget) {
    eventTarget.dispatchEvent(customEvent(event))
  }
  else {
    logError(`${domQuery} undefined, cannot dispatch event: `, event)
  }
}

const customEvent = detail =>
  new CustomEvent('purescript', { detail })

exports.detail = ({ detail }) => detail

exports.showWebSocket = ws => `WebSocket(${ws.url})`

exports.makeWebSocketFFI = left =>
  right =>
  connections =>
  seen =>
  roomCode =>
  remoteMessageTarget =>
  localMessageTarget =>
  announce =>
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

      const ws = new WebSocket(`wss://purescript-wip.onrender.com:10000`)
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
        broadcastEvent(localMessageTarget)(connections(1))
        callback(right(ws))()
      }

      ws.onclose = () => {
        logInfo('WebSocket disconnected')
        broadcastEvent(localMessageTarget)(connections(0))
      }

      ws.onmessage = event => {
        logInfo('Received message:', event.data)
        const processMessage = data => {
          if (data instanceof Blob) {
            data.text().then(text => {
              if (newMessage(text)) {
                broadcastEvent(remoteMessageTarget)(text)
              }
            })
          } else {
            if (newMessage(data)) {
              broadcastEvent(remoteMessageTarget)(data)
            }
          }
        }
        processMessage(event.data)
      }

      ws.onerror = error => {
        logError('WebSocket error:', error)
        callback(left(error))()
      }

      return () => shutdown({ type: 'cancel' })
    } catch (error) {
      callback(left(error))()
      return () => {}
    }
}

exports.send = ws => message => () => {
  logInfo("sent message: ", message)
  ws.send(message)
}

exports.address = ws => () => ws.address
