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

@Interaction = Interaction

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

  sendJSON: (msg) ->
    @send JSON.stringify msg


loc = document.location

h = ->
  @sendJSON {e:"u/#{Math.random()*10}"} 
  @sendJSON {e:'o', a:'sub'} 
  @sendJSON {e:'u', a:'sub'} 
  @sendJSON {e:'o/foo', a:'create', p:{x:0, y:0}}

N = new Socket {url:"ws:/#{loc.hostname}:#{loc.port}/ws"}
N.on \connect, h
N.on \reconnect, h


class Block
  -> 
    @block = document.createElement \div
    @block.className = "block moveable"
    document.body.appendChild @block

    @int = new Interaction @block
    @int.on \down, -> N.sendJSON {e:'o/foo', a:'lock'}
        .on \up,   -> N.sendJSON {e:'o/foo', a:'unlock'}
        .on \move, (d) ~> N.sendJSON {e:'o/foo', a:'inc', p:{x:d.deltaX, y:d.deltaY}}
        #.on \3click, ~> @store.delete!

block = new Block

N.on \message, (d) ->
  p = JSON.parse d
  block.block.style.left = "#{p.p.x}px" if p.p?.x?
  block.block.style.top  = "#{p.p.y}px" if p.p?.y?



N.connect!
