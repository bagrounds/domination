"use strict";

var RTCPeerConnection = window.RTCPeerConnection || webkitRTCPeerConnection || mozRTCPeerConnection

exports.setRemoteDescription = rd => pc => () => {
  console.log(`setRemoteDescription: ${rd}`)

  peerConn.onicecandidate = (e) => {
    if (e.candidate == null) {
      console.log("Get the creator to call: gotAnswer(", JSON.stringify(peerConn.localDescription), ")");
    }
  };
  pc.setRemoteDescription(new RTCSessionDescription(JSON.parse(rd)))
  pc.createAnswer({})
    .then((answerDesc) => pc.setLocalDescription(answerDesc))
    .catch((err) => console.warn("Couldn't create answer"));
}
exports.mkPeerConnection = () => new RTCPeerConnection({'iceServers': [{'urls': ['stun:stun.l.google.com:19302']}]})
exports.mkDataChannel = s => peerConnection => mkResult => callback => {
  const dataChannel = peerConnection.createDataChannel(s)

  dataChannel.onopen = e => console.log("data channel is open!");
  callback(mkResult(dataChannel))()

  return () => { console.log("attempting to cancel mkDataChannel") }
}
exports.getLocalDescription = peerConnection => mkResult => callback /* (Either Error a -> Effect Unit) */ => {
  peerConnection
    .createOffer({})
    .then((desc) => {
      peerConnection.setLocalDescription(desc)
    })
    .catch((err) => console.error(err));

  peerConnection.onicecandidate = e => {
    console.log(`onicecandidate(${e})`);
    if (e.candidate == null) {
      callback(mkResult(JSON.stringify(peerConnection.localDescription)))()
    }
  };

  return canceller('getLocalDescription')
}

exports.log = x => () => console.log(x);

exports.joinDataChannel = offer => peerConnection => mkResult => callback => {
  console.log("joinDataChannel")
  peerConnection.ondatachannel = e => {
    console.log(`ondatachannel(${e})`)
    const dataChannel = e.channel;
    dataChannel.onopen = e => callback(mkResult(dataChannel))()
  };
  return canceller('joinDataChannel')
}

const canceller = name => () => console.log(`attempting to cancel ${name}`)

// example code begins here
var peerConn = new RTCPeerConnection({'iceServers': [{'urls': ['stun:stun.l.google.com:19302']}]});

function create(right, callback) {
  console.log("Creating ...");
  var dataChannel = peerConn.createDataChannel('test');
  dataChannel.onopen = (e) => {
    window.say = (msg) => { dataChannel.send(msg); };
    console.log('Say things with say("hi")');
  };
  dataChannel.onmessage = (e) => { console.log('Got message:', e.data); };
  peerConn.createOffer({})
    .then((desc) => peerConn.setLocalDescription(desc))
    .then(() => {})
    .catch((err) => console.error(err));
  peerConn.onicecandidate = (e) => {
    if (e.candidate == null) {
      console.log("Get joiners to call: ", "join(", JSON.stringify(peerConn.localDescription), ")");
      callback(right(JSON.stringify(peerConn.localDescription)))()
    }
  };
  window.gotAnswer = (answer) => () => {
    console.log("Initializing ...");
    peerConn.setRemoteDescription(new RTCSessionDescription(JSON.parse(answer)));
  };
  return canceller("create")
}
window.create = create
exports.create = right => callback => create(right, callback)
exports.gotAnswer = answer => window.gotAnswer(answer)
exports.say = message => () => window.say(message)

function join(offer, right, callback) {
  console.log(`join(${offer})`);

  peerConn.ondatachannel = (e) => {
    var dataChannel = e.channel;
    dataChannel.onopen = (e) => {
      window.say = (msg) => { dataChannel.send(msg); };
      console.log('Say things with say("hi")');
    };
    dataChannel.onmessage = (e) => { console.log('Got message:', e.data); }
  };

  peerConn.onicecandidate = (e) => {
    if (e.candidate == null) {
      console.log("Get the creator to call: gotAnswer(", JSON.stringify(peerConn.localDescription), ")");
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
