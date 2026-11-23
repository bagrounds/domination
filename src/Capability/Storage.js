// Every step can throw: reading `window.localStorage` when storage is blocked,
// and `getItem` / `setItem` when it is unavailable or full.
const failureOf = e => ({
  name: e && e.name ? String(e.name) : 'Error',
  message: e && e.message !== undefined ? String(e.message) : String(e),
})

export const setItemFFI = left => right => key => value => () => {
  try {
    window.localStorage.setItem(key, value)
    return right
  } catch (e) {
    return left(failureOf(e))
  }
}

export const getItemFFI = left => right => just => nothing => key => () => {
  try {
    const item = window.localStorage.getItem(key)
    return right(item === null ? nothing : just(item))
  } catch (e) {
    return left(failureOf(e))
  }
}
