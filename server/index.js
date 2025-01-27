const WebSocket = require('ws');

const port = process.env.PORT || 8081;
const wss = new WebSocket.Server({ port });

const clients = new Set();

wss.on('connection', (ws) => {
  clients.add(ws);
  console.log('Client connected. Total clients:', clients.size);

  ws.on('message', (message) => {
    // Broadcast to all other clients
    for (const client of clients) {
      if (client !== ws && client.readyState === WebSocket.OPEN) {
        client.send(message.toString());
      }
    }
  });

  ws.on('close', () => {
    clients.delete(ws);
    console.log('Client disconnected. Total clients:', clients.size);
  });
});

console.log(`WebSocket server started on port ${port}`);
