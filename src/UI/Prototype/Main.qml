import QtQuick 2.0

Rectangle {
  width: 300; height: 200; color: "#eee"
  
  Rectangle {
    id: right_list
    anchors.right: parent.right
    width: 100; height: parent.height; color: "#DDDD99"
    
    ListView {
      height: parent.height; width: parent.width;
      model: Subjects {}
      interactive: false
      focus: true
      spacing: 2
      delegate: DragableItem {}
    }
  }
  
}
