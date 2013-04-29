.pragma library

var _privs = {}

function create(key, value) {
    _privs[key.toString()] = value;
}

function priv(key) {
    return _privs[key.toString()];
}
