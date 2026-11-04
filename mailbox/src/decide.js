// The mailbox's rules as a pure function: a request and the stored value go in,
// a response and maybe a new stored value come out. The Durable Object in
// index.js is only the imperative shell around it.

export const MAX_ENVELOPE_BYTES = 16 * 1024

export const EMPTY_MAILBOX = Object.freeze({ revision: 0, envelope: null })

const revisionTagPattern = /^"(0|[1-9][0-9]{0,14})"$/

export const printRevisionTag = revision => `"${revision}"`

export const parseRevisionTag = tag => {
  const match = typeof tag === 'string' ? revisionTagPattern.exec(tag.trim()) : null
  return match === null ? null : Number(match[1])
}

// If-None-Match uses weak comparison (RFC 9110 §13.1.2): W/"5" matches "5".
export const parseWeakRevisionTag = tag =>
  parseRevisionTag(typeof tag === 'string' ? tag.trim().replace(/^W\//, '') : tag)

export const utf8ByteLength = text => new TextEncoder().encode(text).length

const reply = (status, revision, body = null, store = null) =>
  ({ status, revision, body, store })

const bare = status => reply(status, null)

const decideGet = ({ ifNoneMatch }, { revision, envelope }) =>
  parseWeakRevisionTag(ifNoneMatch) === revision
    ? reply(304, revision)
    : envelope === null
      ? reply(204, revision)
      : reply(200, revision, envelope)

const decidePut = ({ ifMatch, body }, current) =>
  ifMatch == null
    ? bare(428)
    : utf8ByteLength(body ?? '') > MAX_ENVELOPE_BYTES
      ? bare(413)
      : (body ?? '') === ''
        ? bare(400)
        : parseRevisionTag(ifMatch) !== current.revision
          ? reply(412, current.revision, current.envelope)
          : reply(200, current.revision + 1, null,
            { revision: current.revision + 1, envelope: body })

const deciders = { GET: decideGet, PUT: decidePut }

export const decide = (request, current) =>
  (deciders[request.method] ?? (() => bare(405)))(request, current)

// After a long silence the game is dropped, but the revision still advances so
// no client can ever mistake a later game for one it already has.
export const expire = ({ revision }) => ({ revision: revision + 1, envelope: null })
