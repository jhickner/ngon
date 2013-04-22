@N = N = {}

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

class Uploader
  (file) ->
    fd = new FormData
    fd.append \file, file

    xhr = new XMLHttpRequest
    xhr.upload.addEventListener \progress, @uploadProgress, false
    xhr.addEventListener \load, @uploadComplete, false
    xhr.addEventListener \lerror, @uploadFailed, false
    xhr.addEventListener \abort, @uploadCanceled, false

    loc = document.location
    xhr.open \POST, "http://#{loc.hostname}:#{loc.port}/files", true
    xhr.send fd

  uploadProgress: (e) ~>
    if e.lengthComputable
      pct = Math.round(e.loaded * 100 / e.total)

  uploadComplete: (e) ~> console.log "upload complete!"
  uploadFailed: (e) ~> console.log "upload failed!: #{e}"
  uploadCanceled: (e) ~> console.log "upload canceled!"

N.Uploader = Uploader

class DragDropTarget
  (@target, @handler) ->

    @target.addEventListener \dragenter, (e) ->
      e.stopPropagation!
      e.preventDefault!
      false

    @target.addEventListener \dragover, (e) ->
      e.stopPropagation!
      e.preventDefault!
      false

    @target.addEventListener \drop, (e) ~>
      e.stopPropagation!
      e.preventDefault!
      @handler e
      false

N.DragDropTarget = DragDropTarget

class DragDropUploader extends DragDropTarget
  (target) ->

    upload = (f) -> new Uploader f

    super target, (dropEvent) ~>
      for f in dropEvent.dataTransfer.files
        upload f

N.DragDropUploader = DragDropUploader

class Interaction implements EventDispatcher
  (@target) ->
    @clicks = [] # click count for multi clicks

    @target.addEventListener \mousedown @mouseDown
    @target.addEventListener \touchstart @touchStart

    document.addEventListener \touchmove @touchMove
    document.addEventListener \touchend @touchEnd

  # on multiple consecutive clicks dispatch an "Nclick" event
  # where N is the number of clicks
  checkMultiClick: ~>
    ts = new Date().valueOf!
    if @clicks.length == 0 || ts - @clicks[0] < 250
      @clicks.unshift ts
      clearTimeout @clickIV
      @clickIV = setTimeout (~> 
        @dispatch "#{@clicks.length}click"
        @clicks = []), 250

  cleanup: ~>
    # remove mouse events
    @removeMouseEvents!
    @target.removeEventListener \mousedown @mouseDown

    # remove touch events
    @target.removeEventListener \touchstart @touchStart
    document.removeEventListener \touchmove @touchMove
    document.removeEventListener \touchend @touchEnd

  removeMouseEvents: ~>
    document.removeEventListener \mousemove @mouseMove
    document.removeEventListener \mouseup @mouseUp

  mouseDown: (e) ~>
    @checkMultiClick!
    @startX = @lastX = e.clientX
    @startY = @lastY = e.clientY
    document.addEventListener \mousemove @mouseMove
    document.addEventListener \mouseup @mouseUp
    @dispatch \down, @
    e.preventDefault!
    false

  mouseUp: (e) ~>
    @removeMouseEvents!
    @dispatch \up, @
    e.preventDefault!
    false

  mouseMove: (e) ~>
    x = e.clientX
    y = e.clientY
    @deltaX = x - @lastX
    @deltaY = y - @lastY
    @lastX = x
    @lastY = y
    @dispatch \move, @
    e.preventDefault!
    false

  touchStart: (e) ~>
    if e.touches.length == 1
      @checkMultiClick!
      touch = e.touches[0]
      @touchID = touch.identifier
      @startX = @lastX = touch.pageX
      @startY = @lastY = touch.pageY
      @dispatch \down, @
      e.preventDefault!
      false

  touchMove: (e) ~>
    touch = e.touches[0]
    if @touchID? and touch.identifier == @touchID
      x = touch.pageX
      y = touch.pageY
      @deltaX = x - @lastX
      @deltaY = y - @lastY
      @lastX = x
      @lastY = y
      @dispatch \move, @
      e.preventDefault!
      false

  touchEnd: (e) ~>
    if e.touches.length == 0 or e.touches[0].identifier == @touchID
      @touchID = null
      @dispatch \up, @
      e.preventDefault!
      false

N.Interaction = Interaction

class Socket implements EventDispatcher
  (opts = {}) -> 
    loc = document.location
    defaultOpts =
      { url: "ws://#{loc.hostname}:#{loc.port}/ws"
      , reconnect: true
      , reconnectDelay: 1000
      , autoConnect: false
      }
    @opts = defaultOpts <<< opts
    @firstConnection = true
    @connected = false

    tryConnect = ~>
      @ws = new WebSocket @opts.url
      @ws.onopen = ~> 
        console.log "connected!"
        @connected = true
        @dispatch if @firstConnection then \connect else \reconnect
        @firstConnection = false
      @ws.onmessage = (e) ~> 
        #console.log "<-", e.data
        packet = JSON.parse e.data
        if packet?.e?
          ep = packet.e
          packet.e = ep.split '/'
          @dispatch ep, packet
          @dispatch \packet, packet
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

  close: ~> 
    @opts.reconnect = false
    @ws.close!
    @

  send: (msg) ~> 
    # reload on error
    if @ws.readyState != 1
      document.location.reload!
    #console.log "->", msg
    @ws.send JSON.stringify msg
    @

N.Socket = Socket

class Users implements EventDispatcher
  subscribe: ~>
    N.socket.send {e:\u, a:\sub}
    N.socket.on \packet, (p) ~>
      if p.e[0] == \u
        @dispatch p.a, p

  unsubscribe: ->
    N.socket.send {e:\u, a:\unsub}

N.Users = new Users

class Obj implements EventDispatcher
  (@id, initial) ->
    @ep = "o/#{@id}"

    handleUpdate = (packet) ~>
      if packet.a == \lock
        @dispatch \lock, packet.p
      else if packet.a == \unlock
        @dispatch \unlock
      else if packet.p?
        for key, val of packet.p
          @dispatch key, val

    N.send {e:@ep, a:"create", p:initial} 
    N.send {e:@ep, a:\sub}
    N.socket.on @ep, handleUpdate

  set: (...args) ~>
    if args.length == 1
      N.send {e:@ep, p:args[0]}
    else if args.length == 2
      o = {}
      o[args[0]] = args[1]
      N.send {e:@ep, p:o}
    @

  inc: (...args) ~>
    if args.length == 1
      N.send {e:@ep, p:args[0], a:"inc"}
    else if args.length == 2
      o = {}
      o[args[0]] = args[1]
      N.send {e:@ep, p:o, a:"inc"}
    @

  lock: ~> 
    N.send {e:@ep, a:"lock"}
    @

  unlock: ~> 
    N.send {e:@ep, a:"unlock"}
    @
        
N.Object = Obj  

class Objects implements EventDispatcher
  subscribe: ~>
    N.socket.send {e:\o, a:\sub}
    N.socket.on \packet, (p) ~>
      if p.e[0] == \o
        @dispatch p.a, p

  unsubscribe: ->
    N.socket.send {e:\o, a:\unsub}

  create: (id, initial) -> new Obj id, initial

N.Objects = new Objects

class Messages implements EventDispatcher
  subscribe: ~>
    N.socket.on \packet, (p) ~>
      if p.e[0] == \m
        @dispatch p.a, p

N.Messages = new Messages!

N.Messages.on \url, (p) ->
  if p.p?
    N.socket.close!
    document.location.href = p.p

N.Messages.on \eval, (p) ->
  if p.p?
    res = eval p.p
    try
      JSON.stringify res
    catch 
      res = res.toString!
    N.send {e:"m/#{p.e[1]}", a:\evalresult, p:res}

N.Messages.on \id, (p) -> 
  if p.p? then N.setID p.p

N.setID = (un) ->
  localStorage.ngon_id = un
  N.socket.close!
  document.location.reload!

N.readStoredID = ->
  name = localStorage.ngon_id
  unless name? 
    name = Math.round Math.random! * 1000
  name

N.getQueryValue = -> window.location.search.substring 1

N.getQueryParams = ->
  pl = /\+/g
  search = /([^&=]+)=?([^&]*)/g
  decode = (s) -> decodeURIComponent s.replace(pl, " ")
  query = window.location.search.substring 1
  queryParams = {}
  while m = search.exec query
     queryParams[decode m[1]] = decode m[2]
  queryParams

N.connect = (f, opts = {}) ->
  N.id = N.readStoredID!
  N.socket = s = new Socket opts
  N.send = s.send
  s.on \reconnect, -> 
    s.close!
    document.location.reload!
  s.on \connect, ->
    s.send {e:"u/#{N.id}"}
    N.Messages.subscribe!
    if f? then f!
  s.connect!
