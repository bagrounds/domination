'use strict'

// helpers

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

const log = level => (...args) => console[level]('FFI: ', ...args)

const logInfo = (...args) => log('log')(...args)

const logError = (...args) => log('error')(...args)

// exports

exports.registerServiceWorker = () => {
  if ('serviceWorker' in navigator) {
    const onLoad = () =>
      navigator.serviceWorker
        .register('sw.js')
        .then(({ scope }) =>
          logInfo(
            'ServiceWorker registration successful with scope: ',
            scope
          )
        )
        .catch(error =>
          logInfo('ServiceWorker registration failed: ', error)
        )
    window.addEventListener('load', onLoad)
  } else {
    logInfo('ServiceWorker unsupported')
  }
}

exports.copyToClipboard = id => () => {
  const element = document.getElementById(id)

  element.select()

  element.setSelectionRange(0, 99999)

  document.execCommand('copy')
}

exports.detail = ({ detail }) => detail

const MESSAGE_BUFFER_LENGTH = 4
const messageBuffer = []

const newMessage = (message) => {
  if (messageBuffer.includes(message)) {
    return false
  }
  messageBuffer.push(message)
  while (messageBuffer.length > MESSAGE_BUFFER_LENGTH) {
    messageBuffer.shift()
  }
  return true
}

exports.showBugout = bugout => bugout.ek

const getBugout = () => {
  // TODO: check if bugout is in a good state, or if it needs to be destroyed
  if (window.bugout instanceof Bugout) {
    return window.bugout
  }
}

exports.makeBugoutFFI = left =>
  right =>
  connections =>
  seen =>
  roomCode =>
  remoteMessageTarget =>
  localMessageTarget =>
  announce =>
  callback =>
  () => {

  const existingBugout = getBugout()

  if (existingBugout) {
    logInfo('using existing bugout: ', existingBugout)
    callback(right(existingBugout))()
    return
  }

  const options = {
    heartbeat: 10000,
    timeout: 25000,
    seed: (localStorage || {}).seed,
    announce: [announce]
  }

  var fresh = true
  const installBugoutHandlers = bugout => {
    const shutdown = (event) => {
      try {
        bugout.destroy()
        logInfo(`destroyed bugout on '${event.type}'`)
      } catch (e) {
        logInfo(`failed to destroy bugout on '${event.type}'`)
      }
    }

    window.addEventListener("unload", shutdown)
    window.addEventListener("beforeunload", shutdown)

    bugout.on("connections", count => {
      if (count == 0 && fresh) {
        fresh = false
        callback(right(bugout))()
      }
    })

    bugout.on("message", (address, message) => {
      logInfo(`Incoming message from address '${address}'. Message length: (${message.length}).`)
      if (!newMessage(message)) {
        logInfo('Ignoring duplicate message.')
      } else {
        if (address === bugout.address()) {
          logInfo('Ignoring message from our own address.')
        } else {
          logInfo('Broadcasting new remote message')
          broadcastEvent(remoteMessageTarget)(message)
        }
      }
    })

    bugout.on("seen", address => {
      broadcastEvent(localMessageTarget)(seen(address))
      const peers = Object.keys(bugout.peers || {}).length
      broadcastEvent(localMessageTarget)(connections(peers))
    })

    bugout.on("ping", address => {
      if (address !== bugout.address()) {
        logInfo("ping from: ", address)
      } else {
        logInfo("self ping? ", address)
      }
    })

    bugout.on("timeout", address => {
      const peers = Object.keys(bugout.peers || {}).length
      broadcastEvent(localMessageTarget)(connections(peers))
      if (address !== bugout.address()) {
        logInfo("timeout: ", address)
      } else {
        logInfo("self timeout? ", address)
      }
    })
  }

  try {
    const bugout = new Bugout(roomCode, options)
    window.bugout = bugout
    logInfo(`new Bugout(roomCode='${roomCode}', options=${JSON.stringify(options)})`)
    reloadOnNetworkChange()
    try {
      localStorage.seed = bugout.seed
    } catch (error) {
      logError(`failed to save seed. oh well`)
    }
    installBugoutHandlers(bugout)
  } catch (error) {
    callback(left(error))()
  }
}

const reloadOnNetworkChange = () => {
  const connection = navigator.connection || navigator.mozConnection || navigator.webkitConnection
  let type = connection.effectiveType

  function updateConnectionStatus() {
    logInfo("Connection type changed from " + type + " to " + connection.effectiveType)
    location.reload()
  }

  connection.addEventListener('change', updateConnectionStatus);
}

exports.address = bugout => () => bugout.address()

exports.send = bugout => message => () => {
  logInfo("sent message length: ", message.length)
  bugout.send(message)
}

// https://stackoverflow.com/a/8809472
exports.genUuid = () => { // Public Domain/MIT
  var d = new Date().getTime()

  // Time in microseconds since page-load or 0 if unsupported
  var d2 = (performance && performance.now && (performance.now() * 1000)) || 0
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'
    .replace(/[xy]/g, c => {
      // random number between 0 and 16
      var r = Math.random() * 16

      if(d > 0){
        // Use timestamp until depleted
        r = (d + r) % 16 | 0
        d = Math.floor(d / 16)
      } else {
        // Use microseconds since page-load if supported
        r = (d2 + r) % 16 | 0
        d2 = Math.floor(d2 / 16)
      }
      return (c === 'x' ? r : (r & 0x3 | 0x8))
        .toString(16)
    })
}

exports.arrayBufferAsString = buffer =>
  String.fromCharCode.apply(null, new Uint8Array(buffer))

exports.stringAsArrayBuffer = string => {
  const stringLength = string.length
  const buffer = new ArrayBuffer(stringLength * 2)
  const bufferView = new Uint8Array(buffer)
  for (let i = 0; i < stringLength; i++) {
    bufferView[i] = string.charCodeAt(i)
  }
  return buffer
}

// cannot eta-reduce this function because LZString does not
// exist in our test suite - we currently import the library
// in an HTML script tag
exports.compressString = s => LZString.compress(s)

exports.decompressStringFFI = just => nothing => s => {
  const result = LZString.decompress(s)
  return result == null
    ? nothing
    : just(result)
}

exports.setItem = left => right => unit => key => value => storage => () => {
  try {
    storage.setItem(key, value)
    return right(unit)
  } catch (e) {
    return left(`Error: setItem: key='${key}', value='${value}', error message: '${e.message || e}'`)
  }
}
