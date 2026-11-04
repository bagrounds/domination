// The three servers a room needs, run locally so a test can break them:
// - a static server for the app (the repository root, as `assets/index.html`
//   loads `../dist/app.js`),
// - the relay of `server/index.js`, plus switches that close a device's sockets
//   or make them half-open (still open at both ends, but nothing gets through),
// - the room mailbox, using the Worker's own pure rules (`mailbox/src/decide.js`),
//   plus a switch that takes it down.
import { createServer } from 'node:http'
import { readFile } from 'node:fs/promises'
import { extname, join, normalize } from 'node:path'
import { fileURLToPath } from 'node:url'
import { WebSocketServer, WebSocket } from 'ws'
import { EMPTY_MAILBOX, decide, printRevisionTag } from '../mailbox/src/decide.js'
import { corsHeaders, preflightHeaders } from '../mailbox/src/cors.js'
import { roomFromPath } from '../mailbox/src/room.js'

const root = fileURLToPath(new URL('..', import.meta.url))

const contentTypes = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.ico': 'image/x-icon',
  '.webmanifest': 'application/manifest+json',
}

const listen = (server, port) => new Promise(resolve => server.listen(port, '127.0.0.1', resolve))

const startStatic = async port => {
  const server = createServer(async (request, response) => {
    const path = normalize(decodeURIComponent(new URL(request.url, 'http://x').pathname))
    const file = join(root, path)
    if (!file.startsWith(root)) {
      response.writeHead(403).end()
      return
    }
    try {
      const body = await readFile(file)
      response.writeHead(200, {
        'Content-Type': contentTypes[extname(file)] ?? 'application/octet-stream',
        'Cache-Control': 'no-store',
      }).end(body)
    } catch (_) {
      response.writeHead(404).end()
    }
  })
  await listen(server, port)
  return { url: `http://localhost:${port}`, close: () => server.close() }
}

const startRelay = async (port, journal) => {
  const clients = new Set()
  const server = createServer()
  const wss = new WebSocketServer({ server })
  wss.on('connection', (socket, request) => {
    socket.device = new URL(request.url, 'http://x').searchParams.get('device') ?? '?'
    socket.halfOpen = false
    clients.add(socket)
    journal(`relay: ${socket.device} connected (${clients.size} sockets)`)
    socket.on('message', (message, isBinary) => {
      if (socket.halfOpen) return
      clients.forEach(client => {
        if (client !== socket && !client.halfOpen && client.readyState === WebSocket.OPEN) {
          client.send(message, { binary: isBinary })
        }
      })
    })
    socket.on('close', () => {
      clients.delete(socket)
      journal(`relay: ${socket.device} disconnected (${clients.size} sockets)`)
    })
  })
  await listen(server, port)
  const socketsOf = device => [...clients].filter(socket => socket.device === device)
  return {
    url: device => `ws://localhost:${port}/?device=${device}`,
    halfOpen: device => socketsOf(device).forEach(socket => { socket.halfOpen = true }),
    cut: device => socketsOf(device).forEach(socket => socket.terminate()),
    live: device => socketsOf(device).filter(socket => !socket.halfOpen).length,
    close: () => {
      clients.forEach(socket => socket.terminate())
      wss.close()
      server.close()
    },
  }
}

const startMailbox = async (port, journal) => {
  const rooms = new Map()
  let up = true
  const server = createServer(async (request, response) => {
    const origin = request.headers.origin
    const room = roomFromPath(new URL(request.url, 'http://x').pathname)
    const chunks = []
    for await (const chunk of request) chunks.push(chunk)
    if (request.method === 'OPTIONS') {
      response.writeHead(204, preflightHeaders(origin)).end()
      return
    }
    if (!up) {
      journal(`mailbox: down, dropped ${request.method}`)
      request.socket.destroy()
      return
    }
    if (room === null) {
      response.writeHead(404, corsHeaders(origin)).end()
      return
    }
    const current = rooms.get(room) ?? EMPTY_MAILBOX
    const decision = decide({
      method: request.method,
      ifMatch: request.headers['if-match'] ?? null,
      ifNoneMatch: request.headers['if-none-match'] ?? null,
      body: request.method === 'PUT' ? Buffer.concat(chunks).toString('utf8') : null,
    }, current)
    if (decision.store !== null) rooms.set(room, decision.store)
    journal(`mailbox: ${request.method} if-match=${request.headers['if-match'] ?? '-'}`
      + ` if-none-match=${request.headers['if-none-match'] ?? '-'} -> ${decision.status} ${decision.revision ?? ''}`)
    response.writeHead(decision.status, {
      ...corsHeaders(origin),
      'Cache-Control': 'no-store, no-transform',
      ...(decision.revision === null ? {} : { ETag: printRevisionTag(decision.revision) }),
      ...(decision.body === null ? {} : { 'Content-Type': 'text/plain; charset=utf-8' }),
    }).end(decision.body ?? undefined)
  })
  await listen(server, port)
  return {
    url: `http://localhost:${port}`,
    revision: room => (rooms.get(room) ?? EMPTY_MAILBOX).revision,
    envelope: room => (rooms.get(room) ?? EMPTY_MAILBOX).envelope,
    setUp: value => { up = value },
    isUp: () => up,
    close: () => server.close(),
  }
}

export const startServers = async ({ staticPort, relayPort, mailboxPort, journal }) => {
  const site = await startStatic(staticPort)
  const relay = await startRelay(relayPort, journal)
  const mailbox = await startMailbox(mailboxPort, journal)
  return {
    site,
    relay,
    mailbox,
    close: () => [site, relay, mailbox].forEach(server => server.close()),
  }
}
