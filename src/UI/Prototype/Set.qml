////////////////////////////////////////////////////////////////////////////////
/// An implementation of a Set-like data structure in qml //////////////////////
////////////////////////////////////////////////////////////////////////////////

import QtQuick 2.0
import "qmlprivate.js" as P

QtObject {
  id: container

  Component.onCompleted: {
    P.create(container, {});
    P.priv(container).members = {};
    P.priv(container).toElements = (function(a) {
      if(a instanceof Array) return a;
      var na = new Array();
      na.push(a);
      return na;
    });
  }

  // Add an element to the set and emit the added signal.
  function add(elems) {
    var priv = P.priv(container);
    var keys = priv.toElements(elems);
    for(var i = 0; i < keys.length; ++i) {
      added(keys[i]);
      priv.members[keys[i].toString()] = keys[i];
    }
  }

  // test whether some key is contained in the set.
  function contains(elems) {
    var priv = P.priv(container);
    if(!priv.members) return false;
    var keys = priv.toElements(elems);
    for(var i = 0; i < keys.length; ++i) {
      if(!priv.members.hasOwnProperty(keys[i].toString())) return false;
    }
    return true;
  }

  // Remove an element from the set and raise the removed signal.
  function remove(elems) {
    var priv = P.priv(container);
    var keys = priv.toElements(elems);
    for(var i = 0; i < keys.length; ++i) {
      removed(keys[i]);
      delete priv.members[keys[i].toString()];
    }
  }

  // return a list of the members of the set. Do not depend on the order of the elements, it may be
  // arbitrary
  function members() {
    var result = new Array();
    var mems = P.priv(container).members
    for(var key in mems) {
      if(mems.hasOwnProperty(key)) result.push(mems[key]);
    }
    return result;
  }

  // clear the set, removing all members. If notify is false, removed is not emitted.
  function clear(notify) {
    if(notify) {
      var allMembers = members();
      for(var i = 0; i < allMembers.length; ++i) removed(allMembers[i]);
    }
    P.priv(container).members = {};

  }

  // Invoked after an element was added to the set.
  signal added(variant key);

  // Invoked after an element was was removed from the set.
  signal removed(variant key);
}
