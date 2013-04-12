class Interaction implements M.eventDispatcher
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
    @startX = @lastX = M.instance.scaleInput e.clientX
    @startY = @lastY = M.instance.scaleInput e.clientY
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
    x = M.instance.scaleInput e.clientX
    y = M.instance.scaleInput e.clientY
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
      @startX = @lastX = M.instance.scaleInput touch.pageX
      @startY = @lastY = M.instance.scaleInput touch.pageY
      @dispatch \down, @
      e.preventDefault!
      false

  touchMove: (e) ~>
    touch = e.touches[0]
    if @touchID? and touch.identifier == @touchID
      x = M.instance.scaleInput touch.pageX
      y = M.instance.scaleInput touch.pageY
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
