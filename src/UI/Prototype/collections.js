.pragma library

function map() {
    var data = {}
    var size = 0
    var that = {}

    var contains = function(k) {
        return data.hasOwnProperty(k);
    }
    that.contains = contains

    var insert = function (k,v) {
        if(contains(k)) return;
        size += 1;
        data[k.toString()] = v;
        return that;
    }
    that.insert = insert;

    var remove = function (k) {
        if(!that.contains(k)) return;
        size -= 1
        delete data[k.toString()];
        return that;
    }
    that.remove = remove

    var clear = function (k,v) {
        data = {};
        size = 0;
        return that;
    }
    that.clear = clear

    var size = function () {
        return size;
    }
    that.size = size

    var keys = function () {
        var result = []
        for(var i in data)
            if(data.hasOwnProperty(i)) result.push(i);
        return result;
    }
    that.keys = keys

    var values = function () {
        var result = []
        for(var i in data)
            if(data.hasOwnProperty(i)) result.push(data[i]);
        return result;
    }
    that.values = values;

    return that;
}

function set() {
    var that = map();
    var super_insert = that.superior('insert');
    var super_remove = that.superior('remove');
    that.insert = function (k) { return super_insert(k.toString(), k); }
    that.remove = function (k) { return super_remove(k.toString()); }
    delete that.keys;
    return that;
}
