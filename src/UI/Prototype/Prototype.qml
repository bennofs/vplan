import QtQuick 2.0

TableView {
  id: main
  width: 300
  height: 300
  cellWidth: 400
  cellHeight: 200
  model: ScheduleModel {}
  rows: [1,2,3,4,5,6]
  columns: ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
  flow: Grid.TopToBottom

  corner: Rectangle {
    width: 20
    height: 20
    color: "darkblue"
  }

  row: Rectangle {
    color: "lightsteelblue"
    Text { text: model.index + 1 }
  }

  column: Rectangle {
    color: "lightsteelblue"
    Text { text: model.modelData }
  }

  cell: Rectangle {
    id: cellDelegate
    anchors.margins: 1
    anchors.fill: parent
    border.color: "#777777"
    border.width: 1
    Column {
      anchors.margins: 5
      anchors.fill: parent
      Text { text: model.subject || "" }
      Text { text: model.room < 0 || !model.room ? "" : model.room }
      Text { text: model.teacher ? "bei " + model.teacher : "" }
    }
    states: [
    State {
      when: isSelected
      PropertyChanges { target: cellDelegate; color: "#AAAAAA" }
    }
    ]
  }

  ScrollBar {
    target: content
    color: "lightgray"
    anchors.bottom: parent.bottom
    height: 20
    x: 20
    width: parent.width - 40
    delegate: Rectangle {
      color: "gray"
    }
  }

  ScrollBar {
    target: content
    rotation: 90
    transformOrigin: Item.TopLeft
    color: "lightgray"
    width: main.height - 40
    height: 20
    anchors.left: parent.right
    y: 20
    delegate: Rectangle {
      color: "gray"
    }
  }

}