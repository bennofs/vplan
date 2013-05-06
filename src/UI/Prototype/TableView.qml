import QtQuick 2.0
import "collections.js" as C
import "utils.js" as U

Rectangle {
  id: table
  property Component cell
  property Component corner
  property Component row
  property Component column

  property variant model
  property var rows
  property var columns
  property int numRows: rows.count || rows.length || rows.size || rows
  property int numColumns: columns.count || columns.length || columns.size || columns
  property int cellWidth
  property int cellHeight
  property int flow

  property alias content: mainContent

  signal selection ()
  property var selected: C.set();

  Loader {
    z: 3
    id: corner
    sourceComponent: table.corner
  }

  Flickable {
    clip: true
    z: 2
    id: topHeader
    interactive: false
    anchors.left: corner.right
    width: Math.min(cellWidth * numColumns, parent.width - corner.width)
    height: corner.height
    contentX: content.contentX
    maximumFlickVelocity: 0
    contentWidth: cellWidth * numColumns
    contentHeight: corner.height
    Grid {
      id: topHeaderContent
      columns: table.numColumns
      rows: 1
      Repeater {
        model: table.columns
        delegate: Item {
          width: cellWidth
          height: corner.height
          property var modelData: model
          Loader {
            anchors.fill: parent
            property var model: modelData
            sourceComponent: column
          }
        }
      }
    }
  }

  Flickable {
    clip: true
    z: 2
    id: leftHeader
    interactive: false
    width: corner.width
    anchors.top: corner.bottom
    anchors.bottom: parent.bottom
    contentY: content.contentY
    contentWidth: corner.width
    contentHeight: cellHeight * numRows
    Grid {
      id: leftHeaderContent
      rows: table.numRows
      columns: 1
      Repeater {
        model: table.rows
        delegate: Item {
          width: corner.width
          height: cellHeight
          property var modelData: model
          Loader {
            anchors.fill: parent
            property var model: modelData
            sourceComponent: row
          }
        }
      }
    }
  }

  Flickable {
    clip: true
    id: mainContent
    anchors.left: leftHeader.right
    anchors.top: topHeader.bottom
    anchors.right: parent.right
    anchors.bottom: parent.bottom
    contentWidth: numColumns * cellWidth
    contentHeight: numRows * cellHeight
    interactive: false
    Grid {
      id: grid
      rows: table.numRows
      columns: table.numColumns
      height: parent.contentHeight
      width: parent.contentWidth
      flow: table.flow
      Repeater {
        model: table.model
        delegate: Item {
          width: cellWidth
          height: cellHeight
          property var modelData: model
          Loader {
            anchors.fill: parent
            property var model: modelData;
            property bool isSelected: false;
            sourceComponent: table.cell
            Component.onCompleted: {
              selectionArea.selectionFinished.connect(function() {
                isSelected = selected.contains(model.index);
              })
            }
          }
        }
      }
    }
  }

  function getIndex(x,y) {
    if(flow == Grid.LeftToRight) return y * numColumns + x;
    return x * numRows + y;
  }

  function cellAction(action, x,y) {
    action(getIndex(x,y));
  }

  function rowAction(action, y) {
    for(var x = 0; x < numColumns; ++x) cellAction(action,x,y);
  }

  function columnAction(action, x) {
    for(var y = 0; y < numRows; ++y) cellAction(action,x,y);
  }

  function allAction(action) {
    for(var i = 0; i < numColumns * numRows; ++i) action(i);
  }

  function copyRect(rect) {
    return Qt.rect(rect.x, rect.y, rect.width, rect.height);
  }

  MouseArea {
    id: selectionArea
    anchors.fill: parent
    signal selectionFinished;
    property int startX;
    property int startY;
    property var action;
    property var keep: [];
    property var selectionRect;

    onPressed: {
      startX = mouse.x
      startY = mouse.y
      action = selected.insert
      if(mouse.modifiers & Qt.ShiftModifier) action = selected.remove
      else if(!(mouse.modifiers & Qt.ControlModifier)) selected.clear();
      keep = selected.values();
    }

    onPositionChanged: {
      selected.clear();
      for(var i = 0; i < keep.length; ++i) selected.insert(keep[i]);
      selectionRect = Qt.rect( Math.min(startX, mouse.x)
                             , Math.min(startY, mouse.y)
                             , Math.abs(mouse.x - startX)
                             , Math.abs(mouse.y - startY))
      var rect = copyRect(selectionRect);
      var rect2 = copyRect(selectionRect);
      testCorner(copyRect(rect));
      rect2.height += content.contentY
      testLeftHeader(copyRect(rect2));
      rect.width += content.contentX
      testTopHeader(rect);
      rect.height += content.contentY
      testItems(copyRect(rect));
      autoScroll(mouse);
      selectionFinished();
    }

    onReleased: {
      onPositionChanged(mouse);
      selection();
    }

    function autoScroll(mouse) {
      if(mouse.x >= table.width - 20) content.contentX += 50
      if(mouse.y >= table.height - 20) content.contentY += 50
      if(mouse.x <= 20) content.contentX -= 50
      if(mouse.y <= 20) content.contentY -= 50
      content.contentY = Math.max(0, Math.min(content.contentHeight - content.height, content.contentY))
      content.contentX = Math.max(0, Math.min(content.contentWidth - content.width, content.contentX))
    }

    function testCorner(rect) {
      if(corner.childAt(rect.x, rect.y)) allAction(action);
    }

    function testTopHeader(rect) {
      rect.x -= corner.width
      if(rect.x < 0) return;
      if(rect.x > topHeaderContent.width) return;
      if(rect.y > topHeaderContent.height) return;
      var cw = topHeaderContent.width / numColumns
      var indexStart = Math.floor(rect.x / cw)
        , indexEnd = Math.floor((rect.x + rect.width) / cw);
      indexEnd = Math.min(indexEnd, numColumns - 1)
      for(var c = indexStart; c <= indexEnd; ++c) columnAction(action, c);
    }

    function testLeftHeader(rect) {
      rect.y -= corner.height
      if(rect.y < 0) return
      if(rect.y > leftHeaderContent.height) return
      if(rect.x > leftHeaderContent.width) return
      var cw = leftHeaderContent.height / numRows
      var indexStart = Math.floor(rect.y / cw)
        , indexEnd = Math.floor((rect.y + rect.height) / cw)
      indexEnd = Math.min(indexEnd, numRows - 1)
      for(var r = indexStart; r <= indexEnd; ++r) rowAction(action, r);
    }

    function testItems(rect) {
      rect.x -= corner.width
      rect.y -= corner.height
      if(rect.x + rect.width < 0 || rect.x > grid.width) return
      if(rect.y + rect.height < 0 || rect.y > grid.height) return
      if(rect.x < 0) { rect.width += rect.x; rect.x = 0 }
      if(rect.y < 0) { rect.height += rect.y; rect.y = 0 }
      if(rect.x + rect.width > grid.width) rect.width = grid.width - rect.x;
      if(rect.y + rect.height > grid.height) rect.height = grid.height - rect.y;
      var xstart = Math.floor(rect.x / cellWidth)
      var xend = Math.floor((rect.x + rect.width) / cellWidth)
      var ystart = Math.floor(rect.y / cellHeight)
      var yend = Math.floor((rect.y + rect.height) / cellHeight)
      for(var x = xstart; x <= xend; ++x)
        for(var y = ystart; y <= yend; ++y) cellAction(action, x, y);
    }
  }

}