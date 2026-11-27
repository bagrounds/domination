// A counter in localStorage that goes up whenever the Storage panel deletes
// data a page may hold in memory. Every page reads it once, when this module
// is first evaluated; a page whose counter is no longer current must not
// write, or it would put deleted data back.
export const generationKey = 'storage_generation'

const read = () => {
  try {
    return window.localStorage.getItem(generationKey)
  } catch (_) {
    return null
  }
}

let startedWith = read()

const next = current => String((Number.parseInt(current, 10) || 0) + 1)

export const isCurrent = () => read() === startedWith

export const advance = () => {
  try {
    window.localStorage.setItem(generationKey, next(read()))
    return true
  } catch (_) {
    return false
  }
}

export const adopt = () => {
  startedWith = read()
}
