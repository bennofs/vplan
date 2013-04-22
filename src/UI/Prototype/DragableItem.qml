import QtQuick 2.0

Rectangle {
  id: "container"
  width: parent.width
  height: child.height
  radius: 5
  color: "orange" 
  Text {
    id: child
    text: name
    anchors.horizontalCenter: parent.horizontalCenter
  }
  MouseArea {
    anchors.fill: parent
    id: mouseArea
    hoverEnabled: true
  }
  states : [
    State {
      name: "selected"
      when: mouseArea.containsMouse
    }
  ]
  transitions: [
    Transition { 
      from: "*"; to: "selected"
      PropertyAnimation { target: container; property: "color"; from: "orange"; to: "lightsteelblue"; duration: 200}
    },
    Transition {
      from: "selected"; to: "*"
      PropertyAnimation { target: container; property: "color"; from: "lightsteelblue"; to: "orange"; duration: 200}
    }
  ]
}