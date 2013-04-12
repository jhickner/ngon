(function(){
  var EventDispatcher, Interaction, Socket, loc, h, N, Block, block;
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
  this.Interaction = Interaction;
  Socket = (function(){
    Socket.displayName = 'Socket';
    var prototype = Socket.prototype, constructor = Socket;
    importAll$(prototype, arguments[0]);
    function Socket(opts){
      var loc, defaultOpts, tryConnect, this$ = this;
      opts == null && (opts = {});
      loc = document.location;
      defaultOpts = {
        url: "ws://" + loc.hostname + ":" + loc.port + "/",
        reconnect: true,
        reconnectDelay: 500,
        autoConnect: false
      };
      this.opts = import$(defaultOpts, opts);
      this.firstConnection = true;
      this.connected = false;
      tryConnect = function(){
        this$.ws = new WebSocket(this$.opts.url);
        this$.ws.onopen = function(){
          this$.connected = true;
          this$.dispatch(this$.firstConnection ? 'connect' : 'reconnect');
          return this$.firstConnection = false;
        };
        this$.ws.onmessage = function(e){
          console.log("<-", e.data);
          return this$.dispatch('message', e.data);
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
      console.log("->", msg);
      this.ws.send(msg);
      return this;
    };
    prototype.sendJSON = function(msg){
      return this.send(JSON.stringify(msg));
    };
    return Socket;
  }(EventDispatcher));
  loc = document.location;
  h = function(){
    this.sendJSON({
      e: "u/" + Math.random() * 10
    });
    this.sendJSON({
      e: 'o',
      a: 'sub'
    });
    this.sendJSON({
      e: 'u',
      a: 'sub'
    });
    return this.sendJSON({
      e: 'o/foo',
      a: 'create',
      p: {
        x: 0,
        y: 0
      }
    });
  };
  N = new Socket({
    url: "ws:/" + loc.hostname + ":" + loc.port + "/ws"
  });
  N.on('connect', h);
  N.on('reconnect', h);
  Block = (function(){
    Block.displayName = 'Block';
    var prototype = Block.prototype, constructor = Block;
    function Block(){
      var this$ = this;
      this.block = document.createElement('div');
      this.block.className = "block moveable";
      document.body.appendChild(this.block);
      this.int = new Interaction(this.block);
      this.int.on('down', function(){
        return N.sendJSON({
          e: 'o/foo',
          a: 'lock'
        });
      }).on('up', function(){
        return N.sendJSON({
          e: 'o/foo',
          a: 'unlock'
        });
      }).on('move', function(d){
        return N.sendJSON({
          e: 'o/foo',
          a: 'inc',
          p: {
            x: d.deltaX,
            y: d.deltaY
          }
        });
      });
    }
    return Block;
  }());
  block = new Block;
  N.on('message', function(d){
    var p, ref$;
    p = JSON.parse(d);
    if (((ref$ = p.p) != null ? ref$.x : void 8) != null) {
      block.block.style.left = p.p.x + "px";
    }
    if (((ref$ = p.p) != null ? ref$.y : void 8) != null) {
      return block.block.style.top = p.p.y + "px";
    }
  });
  N.connect();
  function bind$(obj, key, target){
    return function(){ return (target || obj)[key].apply(obj, arguments) };
  }
  function importAll$(obj, src){
    for (var key in src) obj[key] = src[key];
    return obj;
  }
  function import$(obj, src){
    var own = {}.hasOwnProperty;
    for (var key in src) if (own.call(src, key)) obj[key] = src[key];
    return obj;
  }
}).call(this);
