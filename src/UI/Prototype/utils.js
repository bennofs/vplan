.pragma library

Function.prototype.method = function (name, func) {
    this.prototype[name] = func;
    return this;
}

Function.method('curry', function() {
    var slice = Array.prototype.slice
      , args = slice.apply(arguments)
      , that = this;
    return function() {
        return that.apply(this, args.concat(slice.apply(arguments)))
    }
});

Object.method('superior', function(x) {
    var that = this
      , method = that[x];
    return function() {
        return method.apply(that, arguments);
    };
});

if(typeof Object.create !== 'function') {
  Object.create = function (p) {
      var F = function() {}
      F.prototype = p
      return new F();
  }
}
