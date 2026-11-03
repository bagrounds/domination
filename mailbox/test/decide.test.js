import { test } from 'node:test'
import assert from 'node:assert/strict'
import fc from 'fast-check'
import {
  EMPTY_MAILBOX,
  MAX_ENVELOPE_BYTES,
  decide,
  expire,
  parseRevisionTag,
  printRevisionTag,
} from '../src/decide.js'

const revision = fc.nat({ max: 1_000_000 })
const envelope = fc.string({ minLength: 1, maxLength: 200, unit: 'binary' })
const mailbox = fc.record({ revision, envelope: fc.option(envelope, { nil: null }) })

const get = ifNoneMatch => ({ method: 'GET', ifMatch: null, ifNoneMatch, body: null })
const put = (ifMatch, body) => ({ method: 'PUT', ifMatch, ifNoneMatch: null, body })

test('revision tags round-trip', () => fc.assert(fc.property(revision, r =>
  parseRevisionTag(printRevisionTag(r)) === r)))

test('malformed revision tags are rejected', () => {
  ['', '5', '"-1"', '"01"', '"1.5"', 'W/"3"', '"abc"', '*', null, undefined]
    .forEach(tag => assert.equal(parseRevisionTag(tag), null, `tag ${tag}`))
})

test('every room starts empty at revision 0', () => {
  assert.deepEqual(decide(get(null), EMPTY_MAILBOX),
    { status: 204, revision: 0, body: null, store: null })
})

test('GET returns the stored envelope and its revision', () => fc.assert(fc.property(
  revision, envelope, (r, e) => {
    const d = decide(get(null), { revision: r, envelope: e })
    return d.status === 200 && d.revision === r && d.body === e && d.store === null
  })))

test('GET answers 304 when the client already has the revision', () => fc.assert(fc.property(
  mailbox, m => {
    const d = decide(get(printRevisionTag(m.revision)), m)
    return d.status === 304 && d.body === null && d.store === null
  })))

test('GET with another revision answers with the current state', () => fc.assert(fc.property(
  mailbox, revision, (m, other) => {
    fc.pre(other !== m.revision)
    const d = decide(get(printRevisionTag(other)), m)
    return d.status === (m.envelope === null ? 204 : 200) && d.revision === m.revision
  })))

test('PUT with the current revision commits the next one', () => fc.assert(fc.property(
  mailbox, envelope, (m, e) => {
    const d = decide(put(printRevisionTag(m.revision), e), m)
    return d.status === 200
      && d.revision === m.revision + 1
      && d.store.revision === m.revision + 1
      && d.store.envelope === e
  })))

test('PUT with any other revision is refused with the current envelope', () => fc.assert(fc.property(
  mailbox, revision, envelope, (m, other, e) => {
    fc.pre(other !== m.revision)
    const d = decide(put(printRevisionTag(other), e), m)
    return d.status === 412 && d.revision === m.revision && d.body === m.envelope && d.store === null
  })))

test('PUT without If-Match is refused', () => fc.assert(fc.property(
  mailbox, envelope, (m, e) => decide(put(null, e), m).status === 428)))

test('PUT with an empty body is refused', () => fc.assert(fc.property(
  mailbox, m => decide(put(printRevisionTag(m.revision), ''), m).status === 400)))

test('PUT larger than 16 KiB of UTF-8 is refused', () => {
  const tooLarge = 'é'.repeat(MAX_ENVELOPE_BYTES / 2 + 1)
  const atLimit = 'x'.repeat(MAX_ENVELOPE_BYTES)
  assert.equal(decide(put('"0"', tooLarge), EMPTY_MAILBOX).status, 413)
  assert.equal(decide(put('"0"', atLimit), EMPTY_MAILBOX).status, 200)
})

test('other methods are not allowed', () => {
  ['POST', 'DELETE', 'PATCH'].forEach(method =>
    assert.equal(decide({ method, ifMatch: '"0"', ifNoneMatch: null, body: 'x' }, EMPTY_MAILBOX).status, 405))
})

test('expiry drops the envelope and advances the revision', () => fc.assert(fc.property(
  mailbox, m => {
    const e = expire(m)
    return e.envelope === null && e.revision === m.revision + 1
  })))

// Model test: many writers racing with whatever revision they last saw.
const operation = fc.oneof(
  fc.record({ kind: fc.constant('put'), seen: fc.nat({ max: 12 }), body: envelope }),
  fc.record({ kind: fc.constant('get') }),
  fc.record({ kind: fc.constant('expire') }),
)

const step = ({ stored, accepted, ok }, op) => {
  const next = {
    put: () => {
      const d = decide(put(printRevisionTag(op.seen), op.body), stored)
      return d.store === null
        ? { stored, accepted, ok: ok && d.status === 412 && op.seen !== stored.revision }
        : {
          stored: d.store,
          accepted: [...accepted, d.store.revision],
          ok: ok && op.seen === stored.revision && d.store.revision > stored.revision,
        }
    },
    get: () => {
      const d = decide(get(null), stored)
      return { stored, accepted, ok: ok && d.revision === stored.revision && d.body === stored.envelope }
    },
    expire: () => ({ stored: expire(stored), accepted, ok: ok && expire(stored).revision > stored.revision }),
  }[op.kind]
  return next()
}

test('compare-and-set: writes only succeed on the latest revision, and revisions are never reused',
  () => fc.assert(fc.property(fc.array(operation, { maxLength: 60 }), ops => {
    const { accepted, ok } = ops.reduce(step, { stored: EMPTY_MAILBOX, accepted: [], ok: true })
    return ok && new Set(accepted).size === accepted.length
      && accepted.every((r, i) => i === 0 || r > accepted[i - 1])
  }), { numRuns: 500 }))
