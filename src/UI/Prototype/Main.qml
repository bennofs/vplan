import QtQuick 2.0

Rectangle {
  width: 1000; height: 600; color: "#eee"
  id: toplevel
  
  property ListModel attributes: AttributesModel {}
  
  Rectangle {
    id: main_area
    color: "lightgreen"
    anchors.left: parent.left
    anchors.right: right_list.left
    height: parent.height
    DropArea {
      id: dropArea
      anchors.fill: parent;
      onDropped: {
        console.log("Dropped!");
        drop.accept(Qt.CopyAction);
      }
      onEntered: {
        console.log("Entered!");
      }
    }
    states: [
      State {
        name: "active"
        when: dropArea.containsDrag
        PropertyChanges { target: main_area; color: "lightblue"; }
      }
    ]
  }
    
  
  Rectangle {
    id: right_list
    anchors.right: parent.right
    width: 100; height: parent.height; color: "#DDDD99"
    
    ListView {
      id: view
      height: parent.height; width: parent.width;
      model: attributes
      interactive: false
      focus: true
      spacing: 2
      section.property: "category"
      section.criteria: ViewSection.FullString
      section.delegate: Text { text: section }
      delegate: myDelegate
    }
  }
  
  Component {
    id: myDelegate
    Rectangle {
      id: container
      width: parent.width
      height: child.height
      radius: 5
      color: "orange"
      Text {
        id: child
        text: name
        anchors.horizontalCenter: parent.horizontalCenter
      }
      Drag.active: mouseArea.drag.active
      MouseArea {
        drag.target: parent
        anchors.fill: parent
        id: mouseArea
        hoverEnabled: true
        property bool floating: false
        property bool removed: false
        property variant ix: index
        property variant data
        onPressed: {
          if(floating || removed) return;
          console.log(attributes.count);
          container.ListView.delayRemove = true;
          var sIx = ix
          var cData = {}
          data = attributes.get(ix);
          for (var i in attributes.get(ix)) {
            cData[i] = data[i]
          }
          data = cData;
          attributes.remove(ix);
          ix = sIx
          floating = true;
          parent.opacity = 0.5;
        }
        onReleased: {
          if(!floating) return;
          floating = false;
          parent.opacity = 1;
          var res = parent.Drag.drop();
          if(res == Qt.IgnoreAction) {
            attributes.insert(ix, data);
            parent.ListView.delayRemove = false;
          } else if (res == Qt.MoveAction) {
            console.log(parent.Drag.target);
          }
        }
      }
      states : [
        State {
          name: "selected"
          when: (mouseArea.containsMouse && !mouseArea.active)
        }
      ]
      transitions: [
        Transition { 
          from: "*"; to: "selected"
          PropertyAnimation { target: container; property: "color"; to: "yellow"; duration: 100}
        },
        Transition {
          from: "selected"; to: "*"
          PropertyAnimation { target: container; property: "color"; to: "orange"; duration: 100}
        }
      ]
    }
  }
  
}
