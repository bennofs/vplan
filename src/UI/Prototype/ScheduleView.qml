import QtQuick 2.0

TableView {
  id: main
  cellWidth: 100
  cellHeight: 60
  flow: GridView.TopToBottom

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
    visible: content.width < content.contentWidth
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
    visible: content.height < content.contentHeight
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

  MouseArea {
    anchors.fill: parent
    acceptedButtons: Qt.NoButton
    onWheel: {
      var fac = 1.02;
      if(wheel.angleDelta.y < 0) fac = 1 / fac;
      cellWidth *= fac
      cellHeight *= fac
    }
  }

}