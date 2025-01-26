# WebSocket Migration Plan

## 1. Server Setup ✅
- Create `/server` directory
- Add `package.json` with ws dependency
- Create basic WebSocket server
- Add render.yaml deployment config
- Test server locally

## 2. Configuration ✅
- Add WebSocket URL to Env module
- Keep URL hardcoded for initial implementation
- Can add environment-based config later if needed

## 3. Client Implementation
- ✅ Create new WebSocket FFI module
- Next: Refactor Broadcast Implementation
  1. Split Broadcast.purs into:
     - Broadcast.purs (typeclass + generic functions)
     - Capability/Broadcast/Bugout.purs (existing implementation)
  2. Move types:
     - Move `Broadcaster` to Bugout module
     - Rename to `BugoutBroadcaster`
  3. Move functions:
     - Move implementation-specific functions to Bugout module
     - Keep generic helpers in Broadcast module
  4. Update imports in dependent modules
- Then: Implement WebSocket Broadcaster
  1. Create WebSocket.Broadcaster type
  2. Implement Broadcast typeclass
  3. Add connection handling
  4. Wire up in Main.purs

## 4. Testing
- Test locally with new WebSocket server
- Verify chat functionality
- Verify game state sync
- Test reconnection behavior

## 5. Deployment
- Deploy WebSocket server to Render
- Update hardcoded WebSocket URL for production
- Test deployed version

## 6. Cleanup
- Remove Bugout dependencies
- Remove old WebRTC code
- Update documentation

## Implementation Order
1. ✅ Basic server with message broadcasting
2. ✅ Simple configuration in Env module
3. ✅ WebSocket FFI module
4. Create WebSocketBroadcaster
5. Implement Broadcast typeclass
6. Add connection handling
7. Testing & deployment
8. Cleanup

## Files to Change
- `/server/*` (new) ✅
- `src/Domination/Env.purs` ✅
- `src/FFI/WebSocket.purs` ✅
- `src/FFI/WebSocket.js` ✅
- Current: Split broadcast implementation
  - `src/Domination/Capability/Broadcast.purs` (simplify)
  - `src/Domination/Capability/Broadcast/Bugout.purs` (new, existing implementation)
- Then: Add WebSocket implementation
  - `src/Domination/Capability/Broadcast/WebSocket.purs` (new)
  - `src/Main.purs`

## Implementation Notes
- Pure refactoring step first
- No functionality changes during split
- Better separation of concerns
- Easier to maintain multiple implementations