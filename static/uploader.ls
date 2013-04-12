class Uploader
  (file, @cb) ->
    fd = new FormData
    fd.append \file, file

    xhr = new XMLHttpRequest
    xhr.upload.addEventListener \progress, @uploadProgress, false
    xhr.addEventListener \load, @cb, false
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

@Uploader = Uploader

class DragDropUploader extends DragDropTarget
  (target, handler) ->

    upload = (f, dropEvent) ->
      new Uploader f, (uploadEvent) -> handler dropEvent, f

    super target, (dropEvent) ~>
      for f in dropEvent.dataTransfer.files
        upload f, dropEvent

@DragDropUploader = DragDropUploader
