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
- Current: Create WebSocketBroadcaster
  1. Create WebSocket.Broadcaster type with Ref for connection state
  2. Add `create` function to initialize broadcaster
  3. Add basic message handling (send/receive)
  4. Add connection status tracking
- Next: Broadcast typeclass implementation
  1. Update Broadcast typeclass for WebSocket
  2. Add error handling
  3. Add reconnection logic
- Final: Wire up in Main.purs
  1. Replace Bugout with WebSocket broadcaster
  2. Update app initialization

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
- Next: `src/Domination/Capability/Broadcast/WebSocket.purs` (new)
- Next: `src/Domination/Capability/Broadcast.purs`
- `src/Main.purs`