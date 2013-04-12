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

@DragDropTarget = DragDropTarget
