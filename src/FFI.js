'use strict'

// polyfill

const RTCPeerConnection
  = window.RTCPeerConnection
  || webkitRTCPeerConnection
  || mozRTCPeerConnection

// constants

const MESSAGE_EVENT_TARGET = '#msg'

// TODO: do not use global mutable state

const peerConnections = []
const dataChannels = []

// exports

exports.copyToClipboard = id => () => {
  const element = document.getElementById(id)

  element.select()

  element.setSelectionRange(0, 99999)

  document.execCommand('copy')
}

exports.create = i => right => callback => {
  newPeerConnection(i).then(peerConnection => {
    peerConnections[i] = peerConnection

    const dataChannel = peerConnection.createDataChannel(`dc ${i}`)
    dataChannels[i] = dataChannel

    dataChannel.onmessage = event => {
      logInfo(`(create)dataChannels[${i}].onmessage: `, event)
      broadcastEvent(event)

      dataChannels
        .filter((x, j) => i != j)
        .forEach((channel, j) => {
          logInfo(`(create)dataChannels[${j}].send: `, event)
          channel.send(event.data)
        })
    }

    dataChannel.onopen = onopen

    peerConnection.createOffer({})
      .then(description => peerConnection.setLocalDescription(description))
      .catch(error => logError(`(create)createOffer >>> peerConnections[${i}].setLocalDescription failed with error: `, error))

    peerConnection.onicecandidate = event => {
      logInfo(`(create)peerConnections[${i}].onicecandidate: `, event)
      if (event.candidate == null) {
        callback(right(JSON.stringify(peerConnection.localDescription)))()
      }
    }

    window.gotAnswer = i => answer => () => {
      logInfo(`gotAnswer(${i})(${answer})`)
      peerConnection.setRemoteDescription(remoteDescription(answer))
    }
  })

  return canceller('create')
}

exports.detail = customEvent => customEvent.detail

exports.gotAnswer = answer => window.gotAnswer(answer)

exports.join = offer => right => callback => {
  const i = 0
  newPeerConnection(i).then(peerConnection => {
    peerConnections[i] = peerConnection

    peerConnection.ondatachannel = event => {
      const dataChannel = event.channel
      dataChannels[i] = dataChannel

      dataChannel.onopen = onopen

      dataChannel.onmessage = event => {
        logInfo(`(join)dataChannels[${i}].onmessage: `, event)
        broadcastEvent(event)
      }
    }

    peerConnection.onicecandidate = event => {
      logInfo(`(join)peerConnections[${i}].onicecandidate: `, event)
      if (event.candidate == null) {
        callback(right(JSON.stringify(peerConnection.localDescription)))()
      }
    }

    logInfo(`gotOffer(${i})(${offer})`)
    peerConnection.setRemoteDescription(remoteDescription(offer))

    peerConnection.createAnswer({})
      .then(description => peerConnection.setLocalDescription(description))
      .catch(error => logError(`(join)createAnswer >>> peerConnections[${i}].setLocalDescription failed with error: `, error))
  })

  return canceller('join')
}

// exports.say = message => () => window.say && window.say(message)

// helpers

const broadcastEvent = event => {
  const eventTarget = document.querySelector(MESSAGE_EVENT_TARGET)
  if (eventTarget) {
    eventTarget.dispatchEvent(customEvent(event))
  }
  else {
    logError(`${MESSAGE_EVENT_TARGET} undefined, cannot dispatch event: `, event)
  }
}

const canceller = name => () => logInfo(`cancel(${name})`)

const customEvent = detail => new CustomEvent('msg', { detail })

const log = level => (...args) => console[level]('FFI: ', ...args)

const logInfo = (...args) => log('log')(...args)

const logError = (...args) => log('error')(...args)

const onopen = event => {
  logInfo('onopen: ', event)
  window.say = message => dataChannels.forEach(channel => channel.send(message))
}

const remoteDescription = description => new RTCSessionDescription(JSON.parse(description))

const newPeerConnection = label => {
  const getCert = localStorage.cert
    ? () => loadCert()
    : () => RTCPeerConnection.generateCertificate({
        name: 'RSASSA-PKCS1-v1_5',
        hash: 'SHA-256',
        modulusLength: 2048,
        publicExponent: new Uint8Array([1, 0, 1])
      }).then(cert => saveCert(cert).then(() => cert))

  return getCert().then(cert => {
    localStorage.cert = JSON.stringify(cert)
    logInfo('returned cert:', cert)
    logInfo('returned cert (stringified):', JSON.stringify(cert))
    const peerConnection = new RTCPeerConnection(
      { bundlePolicy: "balanced"
      , certificates: [cert]
      // , iceCandidatePoolSize:
      , iceServers: [{ urls: ['stun:stun.l.google.com:19302']}]
      // , iceTransportPolicy:
      // , peerIdentity:
      // , rtcpMuxPolicy:
      }
    )

    peerConnection.addEventListener("signalingstatechange", event => {
      const state = peerConnection.signalingState
      logInfo(`(${label})signalingstatechange(state: ${state}): `, event)
    })

    peerConnection.addEventListener("iceconnectionstatechange", event => {
      const state = peerConnection.iceConnectionState
      logInfo(`(${label})iceconnectionstatechange(state: ${state}): `, event)
      if (state === "failed") {
        logInfo("meager attempt to fix failed connection...")
        /* possibly reconfigure the connection in some way here */
        /* then request ICE restart */
        peerConnection.restartIce()
      }
    })

    peerConnection.addEventListener('icecandidate', event => {
      logInfo(`(${label})icecandidate: `, event)
    })

    peerConnection.addEventListener('connectionstatechange', event => {
      const state = peerConnection.connectionState
      logInfo(`(${label})connectionstatechange(state: ${state}): `, event)
    })

    return peerConnection
  })
}

const OBJECT_STORE = "MyObjectStore"
const DB_NAME = "db"
const indexedDB = window.indexedDB || window.mozIndexedDB || window.webkitIndexedDB || window.msIndexedDB || window.shimIndexedDB

const saveCert = cert => new Promise((resolve, reject) => {
  const open = indexedDB.open(DB_NAME, 1)

  // Create the schema
  open.onupgradeneeded = () => {
    const db = open.result
    const store = db.createObjectStore(OBJECT_STORE, { keyPath: "id" })
  }

  open.onsuccess = () => {
    // Start a new transaction
    const db = open.result
    const tx = db.transaction(OBJECT_STORE, "readwrite")
    const store = tx.objectStore(OBJECT_STORE)

    store.put({ id: 1, cert })

    tx.oncomplete = () => {
      db.close()
      localStorage.cert = true
      resolve()
    }
  }
})

const loadCert = () => new Promise((resolve, reject) => {
  const open = indexedDB.open(DB_NAME, 1)

  // Create the schema
  open.onupgradeneeded = () => {
    const db = open.result
    const store = db.createObjectStore(OBJECT_STORE, {keyPath: "id"})
  }

  open.onsuccess = () => {
    // Start a new transaction
    const db = open.result
    const tx = db.transaction(OBJECT_STORE, "readwrite")
    const store = tx.objectStore(OBJECT_STORE)

    const query = store.get(1)

    query.onsuccess = () => {
      tx.oncomplete = () => {
        logInfo("transaction complete")
        db.close()
        resolve(query.result.cert)
      }
    }
  }
})

const prepareMessage = (tag, values) =>
  JSON.stringify({ id: "LOCAL", message: { tag, values } })

exports.makeBugout = roomCode => left => right => callback => () => {
  const bugout = new Bugout(roomCode)
  var fresh = true

  bugout.on("connections", count => {
    if (count == 0 && fresh) {
      fresh = false
      callback(right(bugout))()
    }
    const message = prepareMessage("ConnectionsMessage", [ count ])
    broadcastEvent(message)
  })

  bugout.on("message", (address, message) => {
    address === bugout.address() || broadcastEvent(message)
  })

  bugout.on("seen", address => {
    const message = prepareMessage("SeenMessage", [ address ])
    broadcastEvent(message)
  })
}

exports.address = bugout => () => bugout.address()

exports.send = bugout => message => () => {
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
