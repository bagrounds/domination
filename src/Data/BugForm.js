// URLSearchParams writes valid UTF-8 even for a lone surrogate, which
// encodeURIComponent would reject with a URIError.
export const formUrlEncode = text =>
  new URLSearchParams([['', text]]).toString().slice(1)

export const encodeAsciiUriComponent = text => encodeURIComponent(text)
