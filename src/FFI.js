'use strict'

// polyfill

const RTCPeerConnection
  = window.RTCPeerConnection
  || webkitRTCPeerConnection
  || mozRTCPeerConnection

// constants

const RTC_PEER_CONNECTION_CONFIG = {
  iceServers: [{ urls: ['stun:stun.l.google.com:19302']}]
}
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
  const peerConnection = new RTCPeerConnection(RTC_PEER_CONNECTION_CONFIG)
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

  return canceller('create')
}

exports.detail = customEvent => customEvent.detail

exports.gotAnswer = answer => window.gotAnswer(answer)

exports.join = offer => right => callback => {
  const peerConnection = new RTCPeerConnection(RTC_PEER_CONNECTION_CONFIG)
  peerConnections[0] = peerConnection

  peerConnection.ondatachannel = event => {
    const dataChannel = event.channel
    dataChannels[0] = dataChannel

    dataChannel.onopen = onopen

    dataChannel.onmessage = event => {
      logInfo('(join)dataChannels[0].onmessage: ', event)
      broadcastEvent(event)
    }
  }

  peerConnection.onicecandidate = event => {
    logInfo('(join)peerConnections[0].onicecandidate: ', event)
    if (event.candidate == null) {
      callback(right(JSON.stringify(peerConnection.localDescription)))()
    }
  }

  peerConnection.setRemoteDescription(remoteDescription(offer))

  peerConnection.createAnswer({})
    .then(description => peerConnection.setLocalDescription(description))
    .catch(error => logError('(join)createAnswer >>> peerConnections[0].setLocalDescription failed with error: ', error))

  return canceller('join')
}

exports.say = message => () => window.say && window.say(message)

// helpers

const broadcastEvent = event => document
  .querySelector(MESSAGE_EVENT_TARGET)
  .dispatchEvent(customEvent(event.data))

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

