export const connectImpl = source => sink => () => {
  source.connect(sink);
};
