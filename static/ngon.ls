EventDispatcher =
  on: (name, fn) ->
    @events = {} unless @events?

    if not @events[name]?
      @events[name] = fn
    else if @events[name] instanceof Array 
      @events[name].push(fn)
    else
      @events[name] = [@events[name], fn]
    @

  once: (name, fn) ->
    f = ->
      @removeListener name, f
      fn.apply @, arguments

    f.listener = fn
    @on name, f
    @

  removeListener: (name, fn) ->
    if @events? and @events[name]?
      handler = @events[name]
      if handler instanceof Array
        pos = -1

        for i, idx in handler
          if i == fn || (i.listener? && i.listener == fn)
            pos = idx
            break

        return @ if pos < 0

        handler.splice pos, 1

        if !handler.length
          delete @events[name]

      else if handler == fn || (handler.listener? && handler.listener == fn)
        delete @events[name]
    @

  removeAllListeners: (name) ->
    if not name?
      @events = {}
      return @

    if @events? && @events[name]
      @events[name] = null
    @


  dispatch: (name, args) ->
    return false unless @events?

    # make sure args is an array
    args = [args]

    handler = @events[name]
    return false unless handler?

    if handler instanceof Function
      handler.apply @, args
    else if handler instanceof Array
      listeners = handler.slice!
      listeners.map -> it.apply @, args
    else
      return false
    true

EventDispatcher.addListener = EventDispatcher.on


class Socket implements EventDispatcher
  (opts = {}) -> 
    loc = document.location
    defaultOpts =
      { url: "ws://#{loc.hostname}:#{loc.port}/"
      , reconnect: true
      , reconnectDelay: 500
      , autoConnect: false
      }
    @opts = defaultOpts <<< opts
    @firstConnection = true
    @connected = false

    tryConnect = ~>
      @ws = new WebSocket @opts.url
      @ws.onopen = ~> 
        @connected = true
        @dispatch if @firstConnection then \connect else \reconnect
        @firstConnection = false
      @ws.onmessage = (e) ~> 
        console.log "<-", e.data
        @dispatch \message, e.data
      @ws.onerror = (e) ~> @dispatch \error, e
      @ws.onclose = ~>
        if @connected then @dispatch \disconnect
        @connected = false
        if @opts.reconnect
          setTimeout tryConnect, @opts.reconnectDelay

    @connect = ->
      return @ if @connected
      tryConnect!
      @

    @connect! if @opts.autoConnect

  close: -> 
    @opts.reconnect = false
    @ws.close!
    @

  send: (msg) -> 
    console.log "->", msg
    @ws.send msg
    @


class Packets implements EventDispatcher
  ->
    @receiverIDs = 0
    @receivers = {}

    handlePacket = (data) ~>
      packet = @decode data
      if packet?
        if packet.id?
          handler = @receivers[packet.id]
          if handler?
            handler packet.data
            delete @receivers[packet.id]
        else
          @dispatch packet.endpoint, packet.data

    M.socket.addListener \message, handlePacket

  decode: (data) ->
    parts = data.match /([^:]+)?:([^:]+):?([\s\S]*)?/
    if not parts? then return null
    
    id = parts[1]
    packet = {endpoint: parts[2]}
    if id? then packet.id = id

    try packet.data = JSON.parse parts[3]

    packet

  encode: (endpoint, payload=null, id=null) ->
    packet = ""
    if id? then packet += id
    packet += ":#{endpoint}"

    if payload?
      try packet += ":" + JSON.stringify payload

    packet

  send: (endpoint, payload, fn=null) ~>
    id = null
    if fn?
      id = ++@receiverIDs
      @receivers[id] = fn

    packet = @encode endpoint, payload, id
    M.socket.send packet
    @

  sendV: (endpoint, payload) ~>
    @send endpoint, payload, (res) -> console.log res


class Clients implements EventDispatcher
  ->
    @clients = {0:\HTTP}

    handleConnect = ([id, name]) ~>
      @clients[id] = name
      @dispatch \connect, id

    handleDisconnect = (id) ~>
      if (@clients[id])? 
        delete @clients[id]
        @dispatch \disconnect, id

    M.packets.on \cc, handleConnect
             .on \dc, handleDisconnect

  getName: (id) ~> @clients[id]


class Stores implements EventDispatcher
  ->
    @stores = {}

    handleUpdate = ([id, o]) ~>
      if (s = @stores[id])? then s._handleMerge o

    handleIncrement = ([id, o]) ~>
      if (s = @stores[id])? then s._handleIncrement o

    handleCreate = ([id, o, lock]) ~>
      if @stores[id]?
        handleUpdate id, o
      else
        s = @add new Store id, o, lock
        @dispatch "created_#{id}", s
        # if the object has a "type" property,
        # dispatch on the value
        if o.type? then @dispatch o.type, s

    handleLock = ([cid, id]) ~>
      if (s = @stores[id])? then s._handleLockChange cid

    handleReleaseLock = (id) ~>
      if (s = @stores[id])? then s._handleLockChange null

    handleDelete = (id) ~>
      if (s = @stores[id])?
        s.dispatch \delete
        delete @stores[id]

    M.packets.on \cs, handleCreate
             .on \ms, handleUpdate
             .on \is, handleIncrement
             .on \gl, handleLock
             .on \rl, handleReleaseLock
             .on \ds, handleDelete
    
  add: (store) ~>
    @stores[store.id] = store

  read: (id) ~>
    M.packets.send \rs, id, (o) ~>
      @update [id, o]

  get: (id, cb) ~>
    if cb?
      if (s = @stores[id])? then
        cb s
      else
        @once "created_#{id}", cb

  create: (obj={}, cb=null) ~>
    M.packets.send \cs, obj, (id) ~>
      if cb? then @get id, cb


class Store implements EventDispatcher
  (@id, @data, @lockID=null) ->

  _handleMerge: (o) ~>
    @data <<< o
    for key, val of o
      @dispatch key, val
    @dispatch \__propchange, @data

  # incorrectly-typed increments are filtered by the server
  _handleIncrement: (o) ~>
    for k, v of o
      if (c = @data[k])?
        if c instanceof Array
          c.push v
        else if c instanceof Object
          c <<< v
        else 
          @data[k] = c += v
        @dispatch k, c
    @dispatch \__propchange, @data

  _handleLockChange: (id=null) ~>
    @lockID = id
    @dispatch \__lockchange, id

  ###

  get: (prop) ~>
    @data[prop]

  linkLock: (cb) ~>
    @on \__lockchange, cb
    @dispatch \__lockchange, @lockID
    @

  set: (obj) ~>
    M.packets.send \ms, [@id, obj]
    @

  increment: (obj) ~> 
    # trim increment of 0
    for k, v of obj
      if v == 0 then delete obj[k]
    M.packets.send \is, [@id, obj]
    @

  link: (f, cb=null) ~>
    if f instanceof Function
      @on \__propchange, f
      @dispatch \__propchange, @data
    else
      @on f, cb
      if (v = @data[f])? then @dispatch f, v 
    @

  lock: ~> 
    M.packets.send \gl, @id
    @

  unlock: ~> 
    M.packets.send \rl, @id
    @

  delete: ~>
    M.packets.send \ds, @id
    @



      

class Client
  ->
    @id = null
    @readSettings!
    M.packets.on \id, (id) ~> @id = id
  
  clearName: -> delete localStorage.name
  setName: (n) -> localStorage.name = n
  getName: ~>
    name = localStorage.name
    unless name? 
      name = window.prompt "Your name?", ""
    if name?
      localStorage.name = name
      name

  setSetting: (k, v) ->
    localStorage[k] = v 
    @readSettings!

  readSettings: ~>
    # mouse hide
    m = localStorage.mouseHide
    document.body.style.cursor = if m == \true then \none else \auto

  sendID: ~>
    name = @getName!
    if name?
      M.packets.send \id, [name, M.instance.name]


###############################################################################

class Instance implements EventDispatcher
  (@viewport) ->
    @zIndex = 0

    # set page title
    path = document.location.pathname.substr 1
    @name = if path.length > 0 then path else \default
    document.title = @name

    resizeViewportToFit = ~>
      xRatio = window.innerWidth / @viewport.offsetWidth
      yRatio = window.innerHeight / @viewport.offsetHeight
      @scale = Math.min xRatio, yRatio, 1
      @viewport.style.webkitTransform = "scale(#{@scale})"

    handleSetSize = ([@width, @height]) ~>
      @viewport.style.width = "#{@width}px" 
      @viewport.style.height = "#{@height}px"
      resizeViewportToFit!
      @dispatch \resize

    handleResize = (e) ~> resizeViewportToFit!

    @scale = 1
    @viewport.style.webkitTransformOrigin = "0 0"
    window.addEventListener \resize, handleResize
    resizeViewportToFit!

    M.packets.on \isize, handleSetSize

  appendChild: (el) ~> @viewport.appendChild el 

  removeChild: (el) ~> @viewport.removeChild el 

  nextZIndex: ~> @zIndex++

  scaleInput: (x) ~> 
    Math.round(x / @scale) # round to prevent large decimals on the wire

  setSize: (w, h) -> M.packets.send \isize, [w, h]

  setType: (t) -> M.packets.send \itype, t

  reload: -> window.location.reload!


###############################################################################

@M = M = {}
M.eventDispatcher = EventDispatcher

M.init = (cb=null) ~>
  M.socket = new Socket
  M.packets = new Packets
  M.instance = new Instance document.getElementById \viewport
  M.stores = new Stores
  M.clients = new Clients
  M.client = new Client

  M.packets.on \ev, (args) ->
    args = [args] unless args instanceof Array
    eval.apply window, args

  M.socket.on \connect, ~>
    M.client.sendID!

  M.socket.on \reconnect, ~>
    window.location.href = window.location.href

  if cb? then cb!
  M.socket.connect!
