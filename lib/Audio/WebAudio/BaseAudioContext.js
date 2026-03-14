export const newAudioContext = () =>
  new (window.AudioContext || window.webkitAudioContext)();

export const destination = ctx => () => ctx.destination;

export const currentTime = ctx => () => ctx.currentTime;

export const resume = ctx => () => { ctx.resume(); };

export const createGain = ctx => () => ctx.createGain();

export const createOscillator = ctx => () => ctx.createOscillator();
