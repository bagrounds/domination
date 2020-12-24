"use strict";

var RTCPeerConnection = window.RTCPeerConnection || webkitRTCPeerConnection || mozRTCPeerConnection
const peerConn = []
const dataChannel = []

const canceller = name => () => console.log(`attempting to cancel ${name}`)

exports.log = x => () => console.log(x);
exports.detail = customEvent => customEvent.detail

const onmessage = e => {
  console.log('onmessage:', e.data)
  document.querySelector('#msg').dispatchEvent(new CustomEvent('msg', { detail: e.data }))
}
const creatorOnmessage = i => e => {
  console.log(`creatorOnmessage(${i}):`, e.data)
  document.querySelector('#msg').dispatchEvent(new CustomEvent('msg', { detail: e.data }))
  dataChannel.filter((x, j) => i != j).forEach((dc, j) => {
    console.log(`creatorOnMessage - sending to dc${j}`)
    dc.send(e.data)
  })
}
const onopen = e => {
  console.log('onopen(', e, ')')
  window.say = msg => dataChannel.forEach(dc => dc.send(msg))
}
exports.create = i => right => callback => {
  peerConn[i] = new RTCPeerConnection({'iceServers': [{'urls': ['stun:stun.l.google.com:19302']}]});
  dataChannel[i] = peerConn[i].createDataChannel(`dc ${i}`);
  dataChannel[i].onmessage = creatorOnmessage(i)
  dataChannel[i].onopen = onopen
  peerConn[i].createOffer({})
    .then((desc) => peerConn[i].setLocalDescription(desc))
    .then(() => {})
    .catch((err) => console.error(err))

  peerConn[i].onicecandidate = (e) => {
    console.log('onicecandidate(', e, ')');
    if (e.candidate == null) {
      const localDescription = JSON.stringify(peerConn[i].localDescription)
      callback(right(localDescription))()
    }
  }
  window.gotAnswer = i => answer => () => {
    console.log("Initializing ...");
    peerConn[i].setRemoteDescription(new RTCSessionDescription(JSON.parse(answer)));
  }
  return canceller("create")
}
exports.gotAnswer = answer => window.gotAnswer(answer)
exports.say = message => () => window.say && window.say(message)

function join(offer, right, callback) {
  peerConn[0] = new RTCPeerConnection({'iceServers': [{'urls': ['stun:stun.l.google.com:19302']}]});
  peerConn[0].ondatachannel = e => {
    dataChannel[0] = e.channel;
    dataChannel[0].onopen = onopen
    dataChannel[0].onmessage = onmessage
  };

  peerConn[0].onicecandidate = (e) => {
    console.log('onicecandidate(', e, ')');
    if (e.candidate == null) {
      callback(right(JSON.stringify(peerConn[0].localDescription)))()
    }
  };

  var offerDesc = new RTCSessionDescription(JSON.parse(offer));
  peerConn[0].setRemoteDescription(offerDesc);
  peerConn[0].createAnswer({})
    .then((answerDesc) => peerConn[0].setLocalDescription(answerDesc))
    .catch((err) => console.warn("Couldn't create answer"));
  return canceller('join')
}
exports.join = offer => right => callback => join(offer, right, callback)
window.join = join
exports.windowLocalDescription = () => window.localDescription

exports.copyToClipboard = id => () => {
  const element = document.getElementById(id)
  element.select()
  element.setSelectionRange(0, 99999)
  document.execCommand("copy")
}

