# Domination

A peer-to-peer deck building game.
Try it at [domination.fun](https://domination.fun/)

## Technology
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

## Game features
* Custom deck building game logic
* Games are configurable with a custom choice of cards
* Game state and settings persist in local storage
* Undo up to 10 actions
* Play alone, or with as many friends as you can find
* In game chat and game log

## TODO
* Matchmaking
* Explicit game save and load controls
* Card specific icons
* Reliable message passing
* Custom card editor
* Pseudo-random numbers (for deterministic shuffling and undo, and light weight game state diff based messages)
* More expressive card effect Domain Specific Language (DSL)
* Game AI players
* More thorough tests

## Principles
1. Catch and prevent bugs as early as possible
    1. Write tickets, think, and discuss before doing
    2. Use strong static types to get instant editor squiggles and type-checking errors that prevent writing bugs
    3. Use automated tests to catch bugs we can't catch with static types
    4. Run all quality tests in controlled environments on every build and before allowing merges
    5. Use well-designed, principled abstractions when possible
    6. Constraints liberate; liberties constrain - the less power we have in code, the more guarantees tooling can provide.
2. Automate aggressively
3. Always strive for simplicity
5. Aim for broad platform support by using open, standard technologies
6. Zero cost infrastructure
    1. By running the game code on clients (browsers) we don't need to pay for expensive servers

## Design Decisions & Rationale
1. Progressive Web App
    1. Implement once, support every device, minimizing development and maintenance costs
2. All style, graphics, animations, etc are handled in pure CSS
    1. Complete separation of concerns
    2. All you need to know is CSS to make style, graphic, and animation changes
    3. Avoid all the bugs and complications that come with JavaScript in styling
3. Data is code
    1. The game engine is written such that logic can be programmed with JSON
    2. This allows us to make the logic customizable and put that power in the hands of non-programmer users

## Miscelaneous Notes
- Gitlab doesn't support PureScript syntax highlighting...
  - so we pretend that we're writing Haskell code in markdown code blocks, etc.
  - The languages are similar enough that it works pretty well.

## Development
### Install
```sh
npm install
```

### Build
```sh
npm run build
```

### Test
```sh
npm run test
```

### Deploy
```sh
# compiles, bundles, minifies, and gzips all assets in public directory
npm run deploy
```

### Start an HTTP server to host the public directory
```sh
# prompts to install node-static if it isn't already installed
# then starts a web server to host the static files in the public directory
# on port 8080
# open localhost:8080 in your browser to view the web app after this
npm run serve
```

### Write header comments for PureScript files with Ollama
Assumes Ollama is installed locally. See the script for more info.
2025-01-12: This script ran for about an hour and a half after I went to bed (😎) to generate comments for all 100 PureScript files in the codebase.
```sh
./scripts/ai-header-comments
```

## AI Summaries
### Project Overview
Domination is a modern web-based deck building game that emphasizes peer-to-peer gameplay. The core architecture features:

- A state-driven game engine written in PureScript using the Halogen framework
- Real-time multiplayer support through WebRTC-based peer-to-peer networking
- Persistent game state using browser local storage with 10-action undo capability
- Built-in chat system that integrates with the game state
- Configurable game settings including:
  - Custom kingdom card selection
  - Flexible player counts
  - Game rule variations (e.g. long game option)
- Audio feedback system using the Web Audio API
- Robust message passing system for game state synchronization between peers

The application follows modern web architecture patterns with clear separation of capabilities (audio, broadcasting, storage, etc.) and a strong focus on type safety through PureScript's type system.

The game also features progressive enhancement with PWA support, making it installable and mobile-friendly while maintaining a lightweight footprint.

### Project Architecture
Domination follows a modern, capability-based architecture pattern:

**Core Architecture Layers:**
- UI Components (Halogen-based)
  - Game HUD for displaying player state
  - Card chooser for deck building
  - Chat interface for player communication
  - Settings menu for game configuration
- Game Engine
  - Card and supply management system
  - Player state tracking
  - Turn-based game loop
  - Action resolution system
- Network Layer
  - P2P message broadcasting
  - Game state synchronization
  - Player presence management

**Key Technical Components:**
- State Management
  - Uses PureScript's lens system for immutable state updates
  - Maintains game history for undo functionality
  - Persists state to local storage
- Type System
  - Leverages PureScript's strong typing for game rules
  - Uses algebraic data types for card actions and effects
  - Type-safe message passing between peers
- Data Serialization
  - JSON encoding for storage (via Argonaut)
  - Binary encoding for network messages
  - Compressed wire format for efficient transmission

**Framework Integration:**
- Halogen Components
  - Hierarchical component structure
  - Type-safe component communication
  - Pure render functions with effectful edges
- Web Audio
  - Managed audio context
  - Effect-based sound triggering
  - Dynamic sound loading
- WebRTC
  - Peer discovery and connection management
  - Encrypted data channels
  - Connection state monitoring

This architecture enables a fully distributed multiplayer game without requiring a central server, while maintaining type safety and pure functional programming principles throughout the codebase.

