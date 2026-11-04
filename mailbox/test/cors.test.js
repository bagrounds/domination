import { test } from 'node:test'
import assert from 'node:assert/strict'
import { corsHeaders, isAllowedOrigin, preflightHeaders } from '../src/cors.js'
import { roomFromPath } from '../src/room.js'

test('the app origins and localhost are allowed', () => {
  [
    'https://domination.fun',
    'https://bagrounds.github.io',
    'https://bagrounds.gitlab.io',
    'http://localhost:8080',
    'http://127.0.0.1',
  ].forEach(origin => assert.ok(isAllowedOrigin(origin), origin))
})

test('other origins are not allowed', () => {
  [
    'https://evil.example',
    'https://domination.fun.evil.example',
    'http://domination.fun',
    'https://localhost.evil.example',
    'null',
    null,
  ].forEach(origin => assert.ok(!isAllowedOrigin(origin), String(origin)))
})

test('allowed origins can read the ETag', () => {
  const headers = corsHeaders('https://domination.fun')
  assert.equal(headers['Access-Control-Allow-Origin'], 'https://domination.fun')
  assert.equal(headers['Access-Control-Expose-Headers'], 'ETag')
})

test('preflight allows conditional PUTs from allowed origins only', () => {
  assert.match(preflightHeaders('https://domination.fun')['Access-Control-Allow-Headers'], /If-Match/)
  assert.equal(preflightHeaders('https://evil.example')['Access-Control-Allow-Origin'], undefined)
})

test('room paths use the client grammar', () => {
  assert.equal(roomFromPath('/v1/rooms/abc123'), 'abc123')
  ;['/v1/rooms/', '/v1/rooms/ABC', '/v1/rooms/a-b', `/v1/rooms/${'a'.repeat(33)}`,
    '/v1/rooms/abc/extra', '/rooms/abc', '/v1/rooms/abc%20']
    .forEach(path => assert.equal(roomFromPath(path), null, path))
})
