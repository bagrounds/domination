'use strict'

const CACHE_NAME = 'domination-cache-v1'
const cacheAllowlist = [ CACHE_NAME ]
const urlsToCache = [
  '/',
  '/stylesheet.css',
  '/app.js',
  '/manifest.webmanifest'
]

self.addEventListener('install', event =>
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => {
        console.log('Opened cache')
        return cache.addAll(urlsToCache)
      })
  )
)

self.addEventListener('fetch', event =>
  event.respondWith(
    caches.match(event.request)
      .then(response => response
        || fetch(event.request)
          .then(response => {
            const badRespones = !response
              || response.status !== 200
              || response.type !== 'basic'

            if (badRespones) {
              return response
            }

            const responseToCache = response.clone()
            caches
              .open(CACHE_NAME)
              .then(cache => cache.put(event.request, responseToCache))

            return response
          }
        )
      )
    )
)

self.addEventListener('activate', event =>
  event.waitUntil(
    caches
      .keys()
      .then(cacheNames =>
        Promise.all(
          cacheNames.map(cacheName => {
            if (cacheAllowlist.indexOf(cacheName) === -1) {
              return caches.delete(cacheName)
            }
          })
        )
      )
  )
)

