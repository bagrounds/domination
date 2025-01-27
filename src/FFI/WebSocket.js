'use strict'

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
      const ws = new WebSocket(`ws://localhost:8081`)
      ws.address = crypto.randomUUID()

      const shutdown = (event) => {
        try {
          ws.close()
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
        broadcastEvent(remoteMessageTarget)(event.data)
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
