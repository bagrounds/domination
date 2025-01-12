# Domination

A peer-to-peer deck building game.
Try it at [domination.fun](https://domination.fun/)

# Technology
* peer-to-peer encrypted networking via [Bugout](https://github.com/chr15m/bugout), [WebTorrent](https://webtorrent.io/), [WebRTC](https://webrtc.org/), and [NaCl](http://nacl.cr.yp.to/)
* message compression via [lz-string](https://github.com/pieroxy/lz-string)
* Application written in [PureScript](https://www.purescript.org/)
* Using the [Halogen](https://github.com/purescript-halogen/purescript-halogen) UI framework
* JSON codecs via [Argonaut](https://github.com/purescript-contrib/purescript-argonaut)
* Binary codecs via [arraybuffer-class](https://github.com/athanclark/purescript-arraybuffer-class)
* Sound effects via [webaudio](https://github.com/adkelley/purescript-webaudio) and the [Web Audio API](https://webaudio.github.io/web-audio-api/)
* Effectful capability pattern inspired by [Push Effects To The Edges](https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/)
* Light weight, installable, mobile-first, responsive, single page [Progressive Web App](https://web.dev/progressive-web-apps/)
* Light weight [SVG](https://www.w3.org/Graphics/SVG/) icons
* Pure CSS animations
* Fast GitLab CI/CD pipeline deploys to GitLab pages on commits to master

# Game features
* Custom deck building game logic
* Games are configurable with a custom choice of cards
* Game state and settings persist in local storage
* Undo up to 10 actions
* Play alone, or with as many friends as you can find
* In game chat and game log

# TODO
* Matchmaking
* Explicit game save and load controls
* Card specific icons
* Reliable message passing
* Custom card editor
* Pseudo-random numbers (for deterministic shuffling and undo, and light weight game state diff based messages)
* More expressive card effect Domain Specific Language (DSL)
* Game AI players
* More thorough tests

# Install
```sh
npm install
```

# Build
```sh
npm run build
```

# Test
```sh
npm run test
```

# Deploy
```sh
# compiles, bundles, minifies, and gzips all assets in public directory
npm run deploy
```

# Start an HTTP server to host the public directory
```sh
# prompts to install node-static if it isn't already installed
# then starts a web server to host the static files in the public directory
# on port 8080
# open localhost:8080 in your browser to view the web app after this
npm run serve
```
