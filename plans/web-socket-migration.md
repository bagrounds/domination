# WebSocket Migration Plan

## 1. Server Setup ✅
- Create `/server` directory ✅
- Add `package.json` with ws dependency ✅
- Create basic WebSocket server ✅
- Add render.yaml deployment config ✅
- Test server locally ✅

## 2. Configuration ✅
- Add WebSocket URL to Env module ✅
- Keep URL hardcoded for initial implementation ✅
- Can add environment-based config later if needed ✅

## 3. Client Implementation
- ✅ Create new WebSocket FFI module
- ✅ Refactor Broadcast Implementation
  1. ✅ Split Broadcast.purs into:
     - Broadcast.purs (typeclass + generic functions)
     - Capability/Broadcast/Bugout.purs (existing implementation)
  2. ✅ Move types:
     - Move `Broadcaster` to Bugout module
     - Rename to `BugoutBroadcaster`
  3. ✅ Move functions:
     - Move implementation-specific functions to Bugout module
     - Keep generic helpers in Broadcast module
  4. ✅ Update imports in dependent modules
- ✅ Implement WebSocket Broadcaster
  1. ✅ Create WebSocket.Broadcaster type
  2. ✅ Implement Broadcast typeclass
  3. ✅ Add connection handling
  4. ✅ Wire up in Main.purs

## 4. Testing (Current)
- Create test infrastructure:
  1. Add server lifecycle helpers:
     ```purescript
     startTestServer :: Effect Server
     stopTestServer :: Server -> Effect Unit
     ```
  2. Add integration test runner with server management
  3. Configure test-specific port/settings

- Implement test cases:
  ```purescript
  describe "WebSocket Broadcast" do
    beforeAll $ startTestServer
    afterAll $ stopTestServer
    
    it "connects multiple clients" do
      -- Test with real WebSocket connections
    it "syncs messages between clients" do
      -- Test with real broadcast
    it "handles reconnection" do
      -- Test actual server restart
  ```

- Test coverage:
  1. Connection Management
     - Multiple client connections
     - Reconnection handling
     - Connection error states
  2. Message Broadcasting
     - Chat message sync
     - Game state updates
     - Username changes
  3. State Synchronization
     - Initial state sync
     - Move propagation
     - Undo/redo operations
  4. Error Handling
     - Server disconnection
     - Invalid messages
     - Network timeouts

- CI/CD Integration:
  1. Configure test server port/settings
  2. Run integration tests in CI
  3. Add test coverage reporting

## 5. Deployment
- Deploy WebSocket server to Render
- Update production WebSocket URL in Env.purs
- Add environment-based URL selection:
  ```purescript
  wsUrl = case process.env.NODE_ENV of
    "production" -> "wss://purescript-wip.onrender.com"
    _ -> "ws://localhost:8081"
  ```
- Run full test suite on staging
- Test deployed version in production

## 6. Cleanup
- Remove Bugout dependencies from package.json
- Remove old WebRTC code and FFI bindings
- Update documentation:
  1. Add WebSocket server setup instructions
  2. Document deployment process
  3. Update architecture diagrams
- Archive Bugout implementation for reference

## Implementation Order
1. ✅ Basic server with message broadcasting
2. ✅ Simple configuration in Env module
3. ✅ WebSocket FFI module
4. ✅ Split Broadcast implementation
5. ✅ Create WebSocketBroadcaster
6. ✅ Implement Broadcast typeclass
7. ✅ Add connection handling
8. Current: Testing & deployment verification
9. Next: Final cleanup and documentation

## Files to Change
- `/server/*` (new) ✅
- `src/Domination/Env.purs` ✅
- `src/FFI/WebSocket.purs` ✅
- `src/FFI/WebSocket.js`