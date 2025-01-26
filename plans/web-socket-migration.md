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
- Next: Create WebSocketBroadcaster type
- Next: Implement Broadcast typeclass for WebSocketBroadcaster
- Add connection status handling
- Add reconnection logic

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