msgs = show <~ count Mouse.clicks

main = asText <~ WebSocket.open "ws://localhost:8000/ws" msgs
