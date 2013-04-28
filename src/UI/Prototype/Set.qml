////////////////////////////////////////////////////////////////////////////////
/// An implementation of a Set-like data structure in qml //////////////////////
////////////////////////////////////////////////////////////////////////////////

import QtQuick 2.0
import "qmlprivate.js" as P

QtObject {
  id: container

  Component.onCompleted: {
    P.priv(container).members = {}
    P.priv(container).length = 0
    P.priv(container).toElements = (function(a) {
      if(a instanceof Array) return a;
      var na = new Array();
      na.push(a);
      return na;
    });
  }

  // add a key to a set. This function does nothing if the element is already in the set.
  // returns true when all elements were added or were already in the set, false otherwise.
  // If key is a set, this function adds all elements of the given set.
  // If key is an array, this function adds all elements of the given array.
  function add(elems) {
    var result = true;
    var keys = P.priv(container).toElements(elems);
    for(var i = 0; i < keys.length; ++i) {
      if(contains(keys[i])) continue;
      if(added(keys[i])) result = false;
      P.priv(container).length += 1;
      P.priv(container).members[keys[i].toString()] = keys[i];
    }
    return result;
  }

  // check whether a given key is contained in the set or if all the given keys are in the set.
  function contains(elems) {
    if(!P.priv(container).members) return false;
    var keys = P.priv(container).toElements(elems);
    for(var i = 0; i < keys.length; ++i) {
      if(!P.priv(container).members.hasOwnProperty(keys[i].toString())) return false;
    }
    return true;
  }

  // remove an element from the set. This function does nothing if the element
  // is not already in the set.
  // returns true if the element was not in the set or was removed, false otherwise.
  function remove(elems) {
    var result = true;
    var keys = P.priv(container).toElements(elems);
    for(var i = 0; i < keys.length; ++i) {
      if(!contains(keys[i])) continue;
      if(removed(keys[i])) result = false;
      P.priv(container).length -= 1
      delete P.priv(container).members[keys[i].toString()];
    }
    return true;
  }

  // retrieve an Array of elements in the set
  // Warning: do not depend on the order of the elements, it may be arbitrary.
  function members() {
    var result = new Array();
    for(var key in P.priv(container).members) {
      if(contains(key)) result.push(P.priv(container).members[key]);
    }
    return result;
  }

  // retrieve the number if items in the set.
  function size() {
    return P.priv(container).length;
  }

  // clear the set, removing all members
  function clear() {
    var allMembers = members();
    for(var i = 0; i < allMembers.length; ++i) remove(allMembers[i]);
  }

  // Invoked before an element that is not contained in the set is added. If this returns
  // true, the element is not added.
  signal added(variant key);

  // invoked before an element that is contained in the set is removed. If this returns
  // true, the element is not removed.
  signal removed(variant key);
}