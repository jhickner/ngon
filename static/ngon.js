(function(){
  var EventDispatcher, Uploader, DragDropTarget, DragDropUploader, Interaction, Socket, Object, slice$ = [].slice;
  this.NGON = {};
  EventDispatcher = {
    on: function(name, fn){
      if (this.events == null) {
        this.events = {};
      }
      if (this.events[name] == null) {
        this.events[name] = fn;
      } else if (this.events[name] instanceof Array) {
        this.events[name].push(fn);
      } else {
        this.events[name] = [this.events[name], fn];
      }
      return this;
    },
    once: function(name, fn){
      var f;
      f = function(){
        this.removeListener(name, f);
        return fn.apply(this, arguments);
      };
      f.listener = fn;
      this.on(name, f);
      return this;
    },
    removeListener: function(name, fn){
      var handler, pos, i$, len$, idx, i;
      if (this.events != null && this.events[name] != null) {
        handler = this.events[name];
        if (handler instanceof Array) {
          pos = -1;
          for (i$ = 0, len$ = handler.length; i$ < len$; ++i$) {
            idx = i$;
            i = handler[i$];
            if (i === fn || (i.listener != null && i.listener === fn)) {
              pos = idx;
              break;
            }
          }
          if (pos < 0) {
            return this;
          }
          handler.splice(pos, 1);
          if (!handler.length) {
            delete this.events[name];
          }
        } else if (handler === fn || (handler.listener != null && handler.listener === fn)) {
          delete this.events[name];
        }
      }
      return this;
    },
    removeAllListeners: function(name){
      if (name == null) {
        this.events = {};
        return this;
      }
      if (this.events != null && this.events[name]) {
        this.events[name] = null;
      }
      return this;
    },
    dispatch: function(name, args){
      var handler, listeners;
      if (this.events == null) {
        return false;
      }
      args = [args];
      handler = this.events[name];
      if (handler == null) {
        return false;
      }
      if (handler instanceof Function) {
        handler.apply(this, args);
      } else if (handler instanceof Array) {
        listeners = handler.slice();
        listeners.map(function(it){
          return it.apply(this, args);
        });
      } else {
        return false;
      }
      return true;
    }
  };
  EventDispatcher.addListener = EventDispatcher.on;
  Uploader = (function(){
    Uploader.displayName = 'Uploader';
    var prototype = Uploader.prototype, constructor = Uploader;
    function Uploader(file){
      var fd, xhr, loc;
      this.uploadCanceled = bind$(this, 'uploadCanceled', prototype);
      this.uploadFailed = bind$(this, 'uploadFailed', prototype);
      this.uploadComplete = bind$(this, 'uploadComplete', prototype);
      this.uploadProgress = bind$(this, 'uploadProgress', prototype);
      fd = new FormData;
      fd.append('file', file);
      xhr = new XMLHttpRequest;
      xhr.upload.addEventListener('progress', this.uploadProgress, false);
      xhr.addEventListener('load', this.uploadComplete, false);
      xhr.addEventListener('lerror', this.uploadFailed, false);
      xhr.addEventListener('abort', this.uploadCanceled, false);
      loc = document.location;
      xhr.open('POST', "http://" + loc.hostname + ":" + loc.port + "/files", true);
      xhr.send(fd);
    }
    prototype.uploadProgress = function(e){
      var pct;
      if (e.lengthComputable) {
        return pct = Math.round(e.loaded * 100 / e.total);
      }
    };
    prototype.uploadComplete = function(e){
      return console.log("upload complete!");
    };
    prototype.uploadFailed = function(e){
      return console.log("upload failed!: " + e);
    };
    prototype.uploadCanceled = function(e){
      return console.log("upload canceled!");
    };
    return Uploader;
  }());
  this.NGON.Uploader = Uploader;
  DragDropTarget = (function(){
    DragDropTarget.displayName = 'DragDropTarget';
    var prototype = DragDropTarget.prototype, constructor = DragDropTarget;
    function DragDropTarget(target, handler){
      var this$ = this;
      this.target = target;
      this.handler = handler;
      this.target.addEventListener('dragenter', function(e){
        e.stopPropagation();
        e.preventDefault();
        return false;
      });
      this.target.addEventListener('dragover', function(e){
        e.stopPropagation();
        e.preventDefault();
        return false;
      });
      this.target.addEventListener('drop', function(e){
        e.stopPropagation();
        e.preventDefault();
        this$.handler(e);
        return false;
      });
    }
    return DragDropTarget;
  }());
  this.NGON.DragDropTarget = DragDropTarget;
  DragDropUploader = (function(superclass){
    var prototype = extend$((import$(DragDropUploader, superclass).displayName = 'DragDropUploader', DragDropUploader), superclass).prototype, constructor = DragDropUploader;
    function DragDropUploader(target){
      var upload, this$ = this;
      upload = function(f){
        return new Uploader(f);
      };
      DragDropUploader.superclass.call(this, target, function(dropEvent){
        var i$, ref$, len$, f, results$ = [];
        for (i$ = 0, len$ = (ref$ = dropEvent.dataTransfer.files).length; i$ < len$; ++i$) {
          f = ref$[i$];
          results$.push(upload(f));
        }
        return results$;
      });
    }
    return DragDropUploader;
  }(DragDropTarget));
  this.NGON.DragDropUploader = DragDropUploader;
  Interaction = (function(){
    Interaction.displayName = 'Interaction';
    var prototype = Interaction.prototype, constructor = Interaction;
    importAll$(prototype, arguments[0]);
    function Interaction(target){
      this.target = target;
      this.touchEnd = bind$(this, 'touchEnd', prototype);
      this.touchMove = bind$(this, 'touchMove', prototype);
      this.touchStart = bind$(this, 'touchStart', prototype);
      this.mouseMove = bind$(this, 'mouseMove', prototype);
      this.mouseUp = bind$(this, 'mouseUp', prototype);
      this.mouseDown = bind$(this, 'mouseDown', prototype);
      this.removeMouseEvents = bind$(this, 'removeMouseEvents', prototype);
      this.cleanup = bind$(this, 'cleanup', prototype);
      this.checkMultiClick = bind$(this, 'checkMultiClick', prototype);
      this.clicks = [];
      this.target.addEventListener('mousedown', this.mouseDown);
      this.target.addEventListener('touchstart', this.touchStart);
      document.addEventListener('touchmove', this.touchMove);
      document.addEventListener('touchend', this.touchEnd);
    }
    prototype.checkMultiClick = function(){
      var ts, this$ = this;
      ts = new Date().valueOf();
      if (this.clicks.length === 0 || ts - this.clicks[0] < 250) {
        this.clicks.unshift(ts);
        clearTimeout(this.clickIV);
        return this.clickIV = setTimeout(function(){
          this$.dispatch(this$.clicks.length + "click");
          return this$.clicks = [];
        }, 250);
      }
    };
    prototype.cleanup = function(){
      this.removeMouseEvents();
      this.target.removeEventListener('mousedown', this.mouseDown);
      this.target.removeEventListener('touchstart', this.touchStart);
      document.removeEventListener('touchmove', this.touchMove);
      return document.removeEventListener('touchend', this.touchEnd);
    };
    prototype.removeMouseEvents = function(){
      document.removeEventListener('mousemove', this.mouseMove);
      return document.removeEventListener('mouseup', this.mouseUp);
    };
    prototype.mouseDown = function(e){
      this.checkMultiClick();
      this.startX = this.lastX = e.clientX;
      this.startY = this.lastY = e.clientY;
      document.addEventListener('mousemove', this.mouseMove);
      document.addEventListener('mouseup', this.mouseUp);
      this.dispatch('down', this);
      e.preventDefault();
      return false;
    };
    prototype.mouseUp = function(e){
      this.removeMouseEvents();
      this.dispatch('up', this);
      e.preventDefault();
      return false;
    };
    prototype.mouseMove = function(e){
      var x, y;
      x = e.clientX;
      y = e.clientY;
      this.deltaX = x - this.lastX;
      this.deltaY = y - this.lastY;
      this.lastX = x;
      this.lastY = y;
      this.dispatch('move', this);
      e.preventDefault();
      return false;
    };
    prototype.touchStart = function(e){
      var touch;
      if (e.touches.length === 1) {
        this.checkMultiClick();
        touch = e.touches[0];
        this.touchID = touch.identifier;
        this.startX = this.lastX = touch.pageX;
        this.startY = this.lastY = touch.pageY;
        this.dispatch('down', this);
        e.preventDefault();
        return false;
      }
    };
    prototype.touchMove = function(e){
      var touch, x, y;
      touch = e.touches[0];
      if (this.touchID != null && touch.identifier === this.touchID) {
        x = touch.pageX;
        y = touch.pageY;
        this.deltaX = x - this.lastX;
        this.deltaY = y - this.lastY;
        this.lastX = x;
        this.lastY = y;
        this.dispatch('move', this);
        e.preventDefault();
        return false;
      }
    };
    prototype.touchEnd = function(e){
      if (e.touches.length === 0 || e.touches[0].identifier === this.touchID) {
        this.touchID = null;
        this.dispatch('up', this);
        e.preventDefault();
        return false;
      }
    };
    return Interaction;
  }(EventDispatcher));
  this.NGON.Interaction = Interaction;
  Socket = (function(){
    Socket.displayName = 'Socket';
    var prototype = Socket.prototype, constructor = Socket;
    importAll$(prototype, arguments[0]);
    function Socket(opts){
      var loc, defaultOpts, tryConnect, this$ = this;
      opts == null && (opts = {});
      this.send = bind$(this, 'send', prototype);
      this.close = bind$(this, 'close', prototype);
      loc = document.location;
      defaultOpts = {
        url: "ws://" + loc.hostname + ":" + loc.port + "/ws",
        reconnect: true,
        reconnectDelay: 1000,
        autoConnect: false
      };
      this.opts = import$(defaultOpts, opts);
      this.firstConnection = true;
      this.connected = false;
      tryConnect = function(){
        this$.ws = new WebSocket(this$.opts.url);
        this$.ws.onopen = function(){
          console.log("connected!");
          this$.connected = true;
          this$.dispatch(this$.firstConnection ? 'connect' : 'reconnect');
          return this$.firstConnection = false;
        };
        this$.ws.onmessage = function(e){
          var packet;
          packet = JSON.parse(e.data);
          if ((packet != null ? packet.e : void 8) != null) {
            return this$.dispatch(packet.e, packet);
          }
        };
        this$.ws.onerror = function(e){
          return this$.dispatch('error', e);
        };
        return this$.ws.onclose = function(){
          if (this$.connected) {
            this$.dispatch('disconnect');
          }
          this$.connected = false;
          if (this$.opts.reconnect) {
            return setTimeout(tryConnect, this$.opts.reconnectDelay);
          }
        };
      };
      this.connect = function(){
        if (this.connected) {
          return this;
        }
        tryConnect();
        return this;
      };
      if (this.opts.autoConnect) {
        this.connect();
      }
    }
    prototype.close = function(){
      this.opts.reconnect = false;
      this.ws.close();
      return this;
    };
    prototype.send = function(msg){
      if (this.ws.readyState !== 1) {
        document.location.reload();
      }
      this.ws.send(JSON.stringify(msg));
      return this;
    };
    return Socket;
  }(EventDispatcher));
  this.NGON.Socket = Socket;
  Object = (function(){
    Object.displayName = 'Object';
    var prototype = Object.prototype, constructor = Object;
    importAll$(prototype, arguments[0]);
    function Object(id, initial){
      var handleUpdate, this$ = this;
      this.id = id;
      this.unlock = bind$(this, 'unlock', prototype);
      this.lock = bind$(this, 'lock', prototype);
      this.inc = bind$(this, 'inc', prototype);
      this.set = bind$(this, 'set', prototype);
      this.ep = "o/" + this.id;
      handleUpdate = function(packet){
        var key, ref$, val, results$ = [];
        if (packet.a === 'lock') {
          return this$.dispatch('lock', packet.p);
        } else if (packet.a === 'unlock') {
          return this$.dispatch('unlock');
        } else if (packet.p != null) {
          for (key in ref$ = packet.p) {
            val = ref$[key];
            results$.push(this$.dispatch(key, val));
          }
          return results$;
        }
      };
      NGON.send({
        e: this.ep,
        a: "create",
        p: initial
      });
      NGON.subscribe(this.ep, handleUpdate);
    }
    prototype.set = function(){
      var args, o;
      args = slice$.call(arguments);
      if (args.length === 1) {
        NGON.send({
          e: this.ep,
          p: args[0]
        });
      } else if (args.length === 2) {
        o = {};
        o[args[0]] = args[1];
        NGON.send({
          e: this.ep,
          p: o
        });
      }
      return this;
    };
    prototype.inc = function(){
      var args, o;
      args = slice$.call(arguments);
      if (args.length === 1) {
        NGON.send({
          e: this.ep,
          p: args[0],
          a: "inc"
        });
      } else if (args.length === 2) {
        o = {};
        o[args[0]] = args[1];
        NGON.send({
          e: this.ep,
          p: o,
          a: "inc"
        });
      }
      return this;
    };
    prototype.lock = function(){
      NGON.send({
        e: this.ep,
        a: "lock"
      });
      return this;
    };
    prototype.unlock = function(){
      NGON.send({
        e: this.ep,
        a: "unlock"
      });
      return this;
    };
    return Object;
  }(EventDispatcher));
  this.NGON.Object = Object;
  this.NGON.watchPage = function(){
    var page;
    page = new NGON.Object('page', {
      url: 'index.html'
    });
    return page.on('url', function(url){
      var p;
      p = "/" + url;
      if (document.location.pathname !== p) {
        return document.location.pathname = p;
      }
    });
  };
  this.NGON.setUsername = function(un){
    return localStorage.ngon_username = un;
  };
  this.NGON.getUsername = function(){
    var name;
    name = localStorage.ngon_username;
    if (name == null) {
      name = window.prompt("Your name?", "");
    }
    if (name != null) {
      return NGON.setUsername(name);
    }
  };
  this.NGON.connect = function(f, opts){
    var uid, s;
    opts == null && (opts = {});
    uid = NGON.getUsername();
    NGON.socket = s = new Socket(opts);
    NGON.send = s.send;
    s.on('reconnect', function(){
      return document.location.reload();
    });
    s.on('connect', function(){
      s.send({
        e: "u/" + uid
      });
      return f();
    });
    return s.connect();
  };
  this.NGON.subscribe = function(endpoint, f){
    NGON.socket.send({
      e: endpoint,
      a: 'sub'
    });
    return NGON.socket.on(endpoint, f);
  };
  function bind$(obj, key, target){
    return function(){ return (target || obj)[key].apply(obj, arguments) };
  }
  function extend$(sub, sup){
    function fun(){} fun.prototype = (sub.superclass = sup).prototype;
    (sub.prototype = new fun).constructor = sub;
    if (typeof sup.extended == 'function') sup.extended(sub);
    return sub;
  }
  function import$(obj, src){
    var own = {}.hasOwnProperty;
    for (var key in src) if (own.call(src, key)) obj[key] = src[key];
    return obj;
  }
  function importAll$(obj, src){
    for (var key in src) obj[key] = src[key];
    return obj;
  }
}).call(this);
