// One connection per page. It outlives the sockets it opens: when a socket
// closes unexpectedly, the connection opens a new one after a backoff delay,
// and `send` always uses the live socket. Messages sent while no socket is
// open are queued and flushed when the next socket opens. A socket can also
// die without closing (half-open, after a phone sleeps or the network
// switches); `reopen` replaces it at once (Domination.Data.Resync).
const maxQueuedMessages = 100

const getConnection = () => window._dominationConnection
const setConnection = connection => { window._dominationConnection = connection }

const log = level => (...args) => console[level]('WebSocket FFI: ', ...args)
const logInfo = log('log')
const logError = log('error')

const dispatch = (targetId, event) => {
  const target = document.querySelector('#' + targetId)
  if (target) {
    target.dispatchEvent(event)
  } else {
    logError(`#${targetId} not found, cannot dispatch ${event.type}`)
  }
}

const purescriptEvent = detail => new CustomEvent('purescript', { detail })

export const detail = ({ detail }) => detail

export const showConnection = connection => `WebSocket(${connection.url})`

const isOpen = socket => socket !== null && socket.readyState === WebSocket.OPEN

const enqueue = (connection, message) => {
  connection.queue.push(message)
  if (connection.queue.length > maxQueuedMessages) {
    connection.queue.shift()
    logInfo(`queue full, dropped the oldest of ${maxQueuedMessages} messages`)
  }
}

const flush = connection => {
  logInfo(`flushing ${connection.queue.length} queued messages`)
  while (connection.queue.length > 0 && isOpen(connection.socket)) {
    connection.socket.send(connection.queue.shift())
  }
}

export const makeConnectionFFI = left =>
  right =>
  reconnectDelay =>
  remoteMessageTarget =>
  serverUrl =>
  callback =>
  () => {
    const existing = getConnection()
    if (existing) {
      logInfo('Reusing existing connection')
      callback(right(existing))()
      return
    }

    const connection = {
      url: serverUrl,
      address: crypto.randomUUID(),
      socket: null,
      queue: [],
      attempt: 0,
      lostOnce: false,
      stopped: false,
      retryTimer: null,
      reopen: () => {},
    }
    setConnection(connection)

    let settled = false
    const settle = result => {
      if (!settled) {
        settled = true
        callback(result)()
      }
    }

    const scheduleReconnect = () => {
      const delay = reconnectDelay(connection.attempt)(Math.random())
      connection.attempt += 1
      logInfo(`reconnecting in ${Math.round(delay)} ms (attempt ${connection.attempt})`)
      connection.retryTimer = setTimeout(open, delay)
    }

    // The old socket's handlers are detached first, so its late close
    // neither reports a disconnection nor schedules a second socket.
    connection.reopen = () => {
      if (connection.stopped) return
      logInfo('reopening the socket')
      clearTimeout(connection.retryTimer)
      const old = connection.socket
      if (old) {
        old.onopen = old.onclose = old.onmessage = old.onerror = null
        try {
          old.close(4000, 'reopen')
        } catch (e) {
          logInfo('failed to close the old socket')
        }
      }
      connection.socket = null
      connection.attempt = 0
      connection.lostOnce = true
      open()
    }

    const open = () => {
      connection.retryTimer = null
      let socket
      try {
        socket = new WebSocket(serverUrl)
      } catch (error) {
        logError('Connection setup error:', error)
        settle(left(error))
        return
      }
      connection.socket = socket

      socket.onopen = () => {
        logInfo('WebSocket connected')
        connection.attempt = 0
        flush(connection)
        settle(right(connection))
        if (connection.lostOnce) {
          dispatch(remoteMessageTarget, new CustomEvent('reconnected'))
        }
      }

      socket.onclose = event => {
        logInfo('WebSocket disconnected', event.code, event.reason)
        if (connection.stopped) return
        connection.lostOnce = true
        dispatch(remoteMessageTarget, new CustomEvent('disconnected'))
        scheduleReconnect()
      }

      socket.onmessage = event => {
        logInfo('Received message:', event.data)
        if (event.data instanceof Blob) {
          event.data.text().then(text => {
            dispatch(remoteMessageTarget, purescriptEvent(text))
          })
        } else {
          dispatch(remoteMessageTarget, purescriptEvent(event.data))
        }
      }

      socket.onerror = () => {
        logError(`could not reach ${serverUrl}`)
        settle(left(new Error(`could not reach ${serverUrl}`)))
      }
    }

    const shutdown = event => {
      connection.stopped = true
      clearTimeout(connection.retryTimer)
      try {
        if (connection.socket) connection.socket.close(1000)
        logInfo(`closed websocket on '${event.type}'`)
      } catch (e) {
        logInfo(`failed to close websocket on '${event.type}'`)
      }
    }

    const resume = event => {
      if (event.persisted && connection.stopped) {
        logInfo('page restored from the back-forward cache, reconnecting')
        connection.stopped = false
        connection.lostOnce = true
        open()
      }
    }

    window.addEventListener('pagehide', shutdown)
    window.addEventListener('pageshow', resume)

    open()
  }

export const send = connection => message => () => {
  if (isOpen(connection.socket)) {
    logInfo('sent message: ', message)
    connection.socket.send(message)
  } else {
    logInfo('not connected, queued message: ', message)
    enqueue(connection, message)
  }
}

export const address = connection => () => connection.address

export const reopen = connection => () => connection.reopen()
