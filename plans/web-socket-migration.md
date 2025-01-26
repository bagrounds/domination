# WebSocket Migration Plan

## 1. Server Setup
- Create `/server` directory
- Add `package.json` with ws dependency
- Create basic WebSocket server
- Add render.yaml deployment config
- Test server locally

## 2. Environment Configuration  
- Create `.env` file for development
- Add WebSocket URL configs
- Update .gitignore

## 3. Client Implementation
- Create new WebSocket FFI module
- Implement new Broadcaster type
- Update existing Broadcast typeclass implementation
- Add connection status handling
- Add reconnection logic

## 4. Testing
- Test locally with new WebSocket server
- Verify chat functionality
- Verify game state sync
- Test reconnection behavior

## 5. Deployment
- Deploy WebSocket server to Render
- Update client WebSocket URL for production
- Test deployed version

## 6. Cleanup
- Remove Bugout dependencies
- Remove old WebRTC code
- Update documentation

## Implementation Order
1. Basic server with message broadcasting
2. Client WebSocket connection
3. Message handling
4. Error handling & reconnection
5. Deployment
6. Cleanup

## Files to Change
- `/server/*` (new)
- `src/Domination/Capability/Broadcast.purs`
- `src/FFI.purs`
- `src/FFI.js`
- `src/Main.purs`