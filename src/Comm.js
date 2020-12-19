"use strict";

var RTCPeerConnection = window.RTCPeerConnection || webkitRTCPeerConnection || mozRTCPeerConnection
const peerConn = new RTCPeerConnection({'iceServers': [{'urls': ['stun:stun.l.google.com:19302']}]});

const canceller = name => () => console.log(`attempting to cancel ${name}`)

exports.log = x => () => console.log(x);
exports.detail = customEvent => customEvent.detail

const onmessage = e => {
  console.log('onmessage:', e.data)
  document.querySelector('#msg').dispatchEvent(new CustomEvent('msg', { detail: e.data }))
}
const onopen = dataChannel => e => {
  console.log('onopen(', e, ')')
  window.say = msg => dataChannel.send(msg)
}
const create = right => callback => {
  var dataChannel = peerConn.createDataChannel('test');
  dataChannel.onmessage = onmessage
  dataChannel.onopen = onopen(dataChannel)
  peerConn.createOffer({})
    .then((desc) => peerConn.setLocalDescription(desc))
    .then(() => {})
    .catch((err) => console.error(err))

  peerConn.onicecandidate = (e) => {
    console.log('onicecandidate(', e, ')');
    if (e.candidate == null) {
      const localDescription = JSON.stringify(peerConn.localDescription)
      callback(right(localDescription))()
    }
  }
  window.gotAnswer = (answer) => () => {
    console.log("Initializing ...");
    peerConn.setRemoteDescription(new RTCSessionDescription(JSON.parse(answer)));
  }
  return canceller("create")
}
exports.create = right => callback => create(right)(callback)
exports.gotAnswer = answer => window.gotAnswer(answer)
exports.say = message => () => window.say(message)

function join(offer, right, callback) {
  peerConn.ondatachannel = (e) => {
    var dataChannel = e.channel;
    dataChannel.onopen = onopen(dataChannel)
    dataChannel.onmessage = onmessage
  };

  peerConn.onicecandidate = (e) => {
    console.log('onicecandidate(', e, ')');
    if (e.candidate == null) {
      callback(right(JSON.stringify(peerConn.localDescription)))()
    }
  };

  var offerDesc = new RTCSessionDescription(JSON.parse(offer));
  peerConn.setRemoteDescription(offerDesc);
  peerConn.createAnswer({})
    .then((answerDesc) => peerConn.setLocalDescription(answerDesc))
    .catch((err) => console.warn("Couldn't create answer"));
  return canceller('join')
}
exports.join = offer => right => callback => join(offer, right, callback)
window.join = join
exports.windowLocalDescription = () => window.localDescription
