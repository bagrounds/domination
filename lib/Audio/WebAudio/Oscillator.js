export const frequency = node => () => node.frequency;

export const startOscillator = when => n => () => {
  n[n.start ? 'start' : 'noteOn'](when);
};

export const stopOscillator = when => n => () => {
  n[n.stop ? 'stop' : 'noteOff'](when);
};
