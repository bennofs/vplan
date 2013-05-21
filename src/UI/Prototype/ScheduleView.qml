import QtQuick 2.0

Rectangle {

  property alias rows: main.rows
  property alias columns: main.columns
  property alias flow: main.flow
  property alias content: main.content
  property alias table: main
  property alias model: main.model
  property alias cellHeight: main.cellHeight
  property alias cellWidth: main.cellWidth
  property int scrollbarWidth: 10

  TableView {
    id: main
    cellWidth: 100
    cellHeight: 60
    flow: GridView.TopToBottom
    width: parent.width - scrollbarWidth
    height: parent.height - scrollbarWidth
    anchors.left: parent.left
    anchors.top: parent.top

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
        TextEdit { text: model.subject || "" }
        TextEdit { text: model.room < 0 || !model.room ? "" : model.room }
        TextEdit { text: model.teacher ? "bei " + model.teacher : "" }
      }
      states: [
      State {
        when: isSelected
        PropertyChanges { target: cellDelegate; color: "#AAAAAA" }
      }
      ]
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

  ScrollBar {
    id: scrollBottom
    target: main.content
    height: main.content.width < main.content.contentWidth ? scrollbarWidth : 0
    color: "lightgray"
    y: Math.min(main.content.contentHeight + main.topHeaderHeight, main.height)
    x: 0
    width: main.width
    delegate: Rectangle {
      color: "gray"
    }
  }

  ScrollBar {
    id: scrollRight
    target: main.content
    rotation: 90
    transformOrigin: Item.TopLeft
    color: "lightgray"
    width: main.height
    visible: main.content.height < main.content.contentHeight
    height: main.content.height < main.content.contentHeight ? scrollbarWidth : 0
    x: Math.min(main.content.contentWidth + main.leftHeaderWidth, main.width)
    y: 0
    delegate: Rectangle {
      color: "gray"
    }
  }

}