// Same grammar as Domination.Data.Room: lower-case letters and digits, 1 to 32.
const roomPath = /^\/v1\/rooms\/([a-z0-9]{1,32})$/

export const roomFromPath = pathname => roomPath.exec(pathname)?.[1] ?? null
