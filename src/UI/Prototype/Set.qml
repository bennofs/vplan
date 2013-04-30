////////////////////////////////////////////////////////////////////////////////
/// An implementation of a Set-like data structure in qml //////////////////////
////////////////////////////////////////////////////////////////////////////////

import QtQuick 2.0

QtObject {
  id: container

  property var membersData;

  Component.onCompleted: {
    membersData = {}
  }

  // Add an element to the set and emit the added signal.
  function add(elem) {
      added(elem);
      membersData[elem.toString()] = elem;
  }

  // test whether some key is contained in the set.
  function contains(elem) {
    if(!membersData) return false;
    return membersData.hasOwnProperty(elem);
  }

  // Remove an element from the set and raise the removed signal.
  function remove(elem) {
    if(!contains(elem)) return;
    removed(elem);
    delete membersData[elem.toString()];
  }

  // return a list of the members of the set. Do not depend on the order of the elements, it may be
  // arbitrary
  function members() {
    var result = new Array();
    for(var key in membersData) {
      if(membersData.hasOwnProperty(key)) result.push(membersData[key]);
    }
    return result;
  }

  // clear the set, removing all members. If notify is false, removed is not emitted.
  function clear(notify) {
    if(notify) {
      var allMembers = members();
      for(var i = 0; i < allMembers.length; ++i) removed(allMembers[i]);
    }
    membersData = {};

  }

  // Invoked after an element was added to the set.
  signal added(variant key);

  // Invoked after an element was was removed from the set.
  signal removed(variant key);
}
