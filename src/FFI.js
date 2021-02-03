'use strict'

// constants

const MESSAGE_EVENT_TARGET = '#msg'

// exports

exports.copyToClipboard = id => () => {
  const element = document.getElementById(id)

  element.select()

  element.setSelectionRange(0, 99999)

  document.execCommand('copy')
}

exports.detail = customEvent => customEvent.detail

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

const customEvent = detail => new CustomEvent('msg', { detail })

const log = level => (...args) => console[level]('FFI: ', ...args)

const logInfo = (...args) => log('log')(...args)

const logError = (...args) => log('error')(...args)

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

exports.makeBugout = roomCode => tuple => connections => seen =>  left => right => callback => () => {
  const options = {
    //heartbeat: 5000,
    //timeout: 10000,
    announce: [
      "wss://hub.bugout.link",
      "wss://tracker.btorrent.xyz",
      "wss://tracker.openwebtorrent.com",
      "wss://tracker.fastcast.nz"
    ]
  }
  const bugout = new Bugout(roomCode, options)
  var fresh = true

  bugout.on("connections", count => {
    if (count == 0 && fresh) {
      fresh = false
      callback(right(bugout))()
    }
    const message = tuple("LOCAL")(connections(count))
    //broadcastEvent(message)
  })

  bugout.on("message", (address, message) => {
    logInfo("incoming message length: ", message.length)
    const decompressed = LZString.decompress(message)
    logInfo("incoming decompressed message length: ", decompressed.length)
    address === bugout.address() || broadcastEvent(decompressed)
  })

  bugout.on("seen", address => {
    const message = tuple("LOCAL")(seen(address))
    //broadcastEvent(message)
  })

  bugout.on("ping", address => {
    if (address !== bugout.address()) {
      logInfo("ping from: ", address)
    } else {
      logInfo("self ping? ", address)
    }
  })

  bugout.on("timeout", address => {
    if (address !== bugout.address()) {
      logInfo("timeout: ", address)
    } else {
      logInfo("self timeout? ", address)
    }
  })
}

exports.address = bugout => () => bugout.address()

exports.send = bugout => message => () => {
  logInfo("sent message length: ", message.length)
  const compressed = LZString.compress(message)
  logInfo("sent compressed message length: ", compressed.length)
  bugout.send(compressed)
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

exports.arrayBufferAsString = buf =>
  String.fromCharCode.apply(null, new Uint8Array(buf))

exports.stringAsArrayBuffer = str => {
  var buf = new ArrayBuffer(str.length*2) // 2 bytes for each char
  var bufView = new Uint8Array(buf)
  for (var i=0, strLen=str.length; i < strLen; i++) {
    bufView[i] = str.charCodeAt(i)
  }
  return buf
}
