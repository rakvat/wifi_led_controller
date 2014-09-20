require 'socket'               # Get sockets from stdlib

server = TCPServer.open("127.0.0.1",5577)  # Socket to listen on port 2000
client = server.accept       # Wait for a client to connect
loop {                         # Servers run forever
  line = client.read(5)
  array = line.unpack("C*")
  puts "got #{array}"
}
client.close                 # Disconnect from the client
