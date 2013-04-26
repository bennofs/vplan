import QtQuick 2.0


Rectangle {
  
  width: 400; height: 400;
  
  Row {
    
    id: table
    
    property int cellWidth: 100
    property int cellHeight: 100
    
    Column {
      Rectangle {
        color: "lightgreen";
        height: 20
        width: 20
      }
      Repeater {
        model: 3
        delegate: Rectangle {
          height: table.cellHeight;
          width: 20
          color: "green";
          Text {text: index + 1; anchors.centerIn: parent}
        }
      }
    }
    
    Repeater {
      model: ScheduleModel {}
      delegate: Column {
        Rectangle {
          height: 20
          width: table.cellWidth
          color: "yellow";
          Text {text: day; anchors.centerIn: parent}
        }
        
        Repeater {
          model: lessons
          delegate: Rectangle {
            width: table.cellWidth
            height: table.cellHeight
            border.color: "grey";
            border.width: 1
            color: "lightgray"
            Column {
              
              visible: used
              anchors.leftMargin: 4
              anchors.left: parent.left
              Text { text: subject == undefined ? "" : subject  }
              Text { text: room == undefined ? "" : room }
              Text { text: teacher == undefined ? "" : "bei " + teacher}
              
            }
          }
        }
      }
    }
    
  }
  
}