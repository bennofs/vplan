import QtQuick 2.0


Table {
  id: main
  width: 200
  height: 200
  model: ScheduleModel {}
  columnHeaders: ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
  rowHeaders: 6
  cellsX: 5
  cellsY: 6
  cellHeight: height / cellsY
  cellWidth: 500
  columnHeaderHeight: 20
  rowHeaderWidth: 20

  cornerDelegate: Rectangle {
    width: 20
    height: 20
    color: "blue";
  }

  rowHeaderDelegate: Rectangle {
    color: "lightblue"
    Text { anchors.centerIn: parent; text: modelData ? modelData.modelData + 1 : "" }
  }

  columnHeaderDelegate: Rectangle {
    color: "lightblue"
    Text { anchors.centerIn: parent; text: modelData ? modelData.modelData : "" }
  }

  delegate: Rectangle {
    id: cell
    property bool selected: false;
    color: "#DDDDDD";
    border.width: 1
    border.color: "lightgray"
    Column {
      anchors.centerIn: parent
      Text { text: !modelData ? "" : modelData.subject || "" }
      Text { text: !modelData ? "" : modelData.room == -1 ? "" : modelData.room }
      Text { text: !modelData ? "" : modelData.teacher ? "bei " + modelData.teacher : "" }
    }
   states: [
      State {
        name: "selected"
        when: selected
        PropertyChanges { target: cell; color: "darkgray"}
      }
    ]
  }
}
