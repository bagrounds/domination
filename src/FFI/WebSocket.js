'use strict'

exports.createImpl = url => () => {
  const ws = new WebSocket(url)
  return ws
}

exports.onMessageImpl = ws => callback => () => {
  ws.onmessage = event => {
    callback(event.data)()
  }
}

exports.sendImpl = ws => data => () => {
  ws.send(data)
}

exports.closeImpl = ws => () => {
  ws.close()
}
