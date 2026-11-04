import { DurableObject } from 'cloudflare:workers'
import { corsHeaders, preflightHeaders } from './cors.js'
import {
  EMPTY_MAILBOX,
  MAX_ENVELOPE_BYTES,
  decide,
  expire,
  printRevisionTag,
} from './decide.js'
import { roomFromPath } from './room.js'

const EXPIRES_AFTER_MS = 90 * 24 * 60 * 60 * 1000

const LATEST = 'latest'

const toResponse = (decision, origin) =>
  new Response(decision.body, {
    status: decision.status,
    headers: {
      ...corsHeaders(origin),
      // no-transform: compressing would weaken the ETag to W/"n" (RFC 9110).
      'Cache-Control': 'no-store, no-transform',
      ...(decision.revision === null ? {} : { ETag: printRevisionTag(decision.revision) }),
      ...(decision.body === null ? {} : { 'Content-Type': 'text/plain; charset=utf-8' }),
    },
  })

const plain = (status, origin) =>
  toResponse({ status, revision: null, body: null }, origin)

const declaredTooLarge = request =>
  Number(request.headers.get('Content-Length') ?? 0) > MAX_ENVELOPE_BYTES

const readRequest = async request => ({
  method: request.method,
  ifMatch: request.headers.get('If-Match'),
  ifNoneMatch: request.headers.get('If-None-Match'),
  body: request.method === 'PUT' ? await request.text() : null,
})

export default {
  async fetch(request, env) {
    const origin = request.headers.get('Origin')
    const room = roomFromPath(new URL(request.url).pathname)
    return request.method === 'OPTIONS'
      ? new Response(null, { status: 204, headers: preflightHeaders(origin) })
      : room === null
        ? plain(404, origin)
        : declaredTooLarge(request)
          ? plain(413, origin)
          : toResponse(
            await env.MAILBOX.get(env.MAILBOX.idFromName(room)).handle(await readRequest(request)),
            origin)
  },
}

// One object per room. A Durable Object runs one request at a time and holds
// other requests back while it waits on its own storage, so the get, decide
// and put below are an atomic compare-and-set.
export class Mailbox extends DurableObject {
  async handle(request) {
    const current = (await this.ctx.storage.get(LATEST)) ?? EMPTY_MAILBOX
    const decision = decide(request, current)
    if (decision.store !== null) {
      await this.ctx.storage.put(LATEST, decision.store)
      await this.ctx.storage.setAlarm(Date.now() + EXPIRES_AFTER_MS)
    }
    return { status: decision.status, revision: decision.revision, body: decision.body }
  }

  async alarm() {
    const current = await this.ctx.storage.get(LATEST)
    if (current !== undefined) await this.ctx.storage.put(LATEST, expire(current))
  }
}
