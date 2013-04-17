#!/usr/bin/env ruby

require 'socket'
require 'json'

host = 'localhost'
port = 8001

s = TCPSocket.open(host, port)

s.puts({:e => "u/socket"}.to_json)

p1 = {:e => "o/page", :a => "set", :p => {:url => "index.html"}}.to_json
p2 = {:e => "o/page", :a => "set", :p => {:url => "3d.html"}}.to_json

100.times do |i|
  if i % 2 == 0
    puts p1
    s.puts p1
  else
    puts p2
    s.puts p2
  end
end

s.close  
