const appOrigins = new Set([
  'https://domination.fun',
  'https://www.domination.fun',
  'https://bagrounds.github.io',
  'https://bagrounds.gitlab.io',
])

const localOrigin = /^http:\/\/(localhost|127\.0\.0\.1|\[::1\])(:[0-9]{1,5})?$/

export const isAllowedOrigin = origin =>
  typeof origin === 'string' && (appOrigins.has(origin) || localOrigin.test(origin))

export const corsHeaders = origin =>
  isAllowedOrigin(origin)
    ? {
      'Access-Control-Allow-Origin': origin,
      'Access-Control-Expose-Headers': 'ETag',
      Vary: 'Origin',
    }
    : { Vary: 'Origin' }

export const preflightHeaders = origin =>
  isAllowedOrigin(origin)
    ? {
      ...corsHeaders(origin),
      'Access-Control-Allow-Methods': 'GET, PUT, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type, If-Match, If-None-Match',
      'Access-Control-Max-Age': '86400',
    }
    : corsHeaders(origin)
