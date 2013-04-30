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
  cellHeight: height / cellsY + 20
  cellWidth: 500 // width / cellsX
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

  ScrollBar {
    id: vertiscroll
    color: "orange"
    z: 4
    rotation: 90
    opacity: 0.4
    anchors.left: main.right
    y: 0
    height: 10
    width: main.height
    transformOrigin: Item.TopLeft
    target: content
    delegate: Component { Rectangle {
      z: 4
      width: content.visibleArea.heightRatio * main.height
      height: 10
      color: "#FF0000";
    }}
    MouseArea {
      id: mouseAreaVerti
      anchors.fill: parent
      hoverEnabled: true
      acceptedButtons: Qt.NoButton
    }

    states: [
    State {
      when: mouseAreaVerti.containsMouse
      PropertyChanges { target: vertiscroll; opacity: 0.9 }
    }
    ]
  }

  ScrollBar {
    id: horiscroll
    color: "orange"
    z: 4;
    opacity: 0.4
    anchors.bottom: main.bottom
    height: 10
    width: main.width
    target: content
    delegate: Component { Rectangle {
      z: 4
      width: content.visibleArea.widthRatio * main.width
      height: 10
      color: "#FF0000";
    }}
    MouseArea {
      id: mouseAreaHori
      anchors.fill: parent
      hoverEnabled: true
      acceptedButtons: Qt.NoButton
    }

    states: [
    State {
      when: mouseAreaHori.containsMouse
      PropertyChanges { target: horiscroll; opacity: 0.9 }
    }
    ]
  }
}
