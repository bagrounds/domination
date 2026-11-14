import { readFileSync } from 'fs'

export const queryParamFFI = just => nothing => name => url => {
  const value = new URL(url).searchParams.get(name)
  return value === null ? nothing : just(value)
}

export const readTextFile = path => () => readFileSync(path, 'utf8')
