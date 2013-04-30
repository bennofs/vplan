import QtQuick 2.0

Rectangle {

  id: table

  property variant model
  property variant columnHeaders
  property variant rowHeaders
  property int cellHeight
  property int cellWidth
  property int columnHeaderHeight
  property int rowHeaderWidth
  property double cellsX
  property double cellsY
  property alias content: content
  property Set selected: Set {}
  property Component rowHeaderDelegate
  property Component columnHeaderDelegate
  property Component cornerDelegate
  property Component delegate
  signal newSelection

  Loader {
    id: topLeftCorner
    z: 2
    sourceComponent: table.cornerDelegate
    anchors.left: table.left
    anchors.top: table.top
    width: table.columnHeaderWidth
    height: table.rowHeaderHeight
    clip: true
  }

  ListView {
    id: topHeader
    interactive: false
    z: 1
    contentX: content.contentX
    orientation: ListView.Horizontal
    anchors.top: table.top
    anchors.left: topLeftCorner.right;
    anchors.right: table.right
    model: table.columnHeaders
    width: table.cellWidth
    height: table.columnHeaderHeight
    delegate: Loader {
      property variant modelData: model
      sourceComponent: table.columnHeaderDelegate
      width: table.cellWidth
      height: table.columnHeaderHeight
    }
  }

  ListView {
    id: leftHeader
    interactive: false
    z: 1
    contentY: content.contentY
    anchors.top: topLeftCorner.bottom
    anchors.bottom: table.bottom
    anchors.left: table.left
    model: table.rowHeaders
    height: table.cellHeight
    width: table.rowHeaderWidth
    delegate: Loader {
      property variant modelData: model
      sourceComponent: table.rowHeaderDelegate
      height: table.cellHeight
      width: table.rowHeaderWidth
    }
  }

  Flickable {
    id: content
    boundsBehavior: Flickable.StopAtBounds
    contentHeight: table.cellHeight * cellsY
    contentWidth: table.cellWidth * cellsX
    anchors.right: parent.right
    anchors.left: leftHeader.right
    anchors.bottom: table.bottom
    anchors.top: topHeader.bottom
    interactive: false
    GridView {
      id: inner
      z: 0
      interactive: false
      flow: GridView.TopToBottom
      cellHeight: table.cellHeight
      cellWidth: table.cellWidth
      anchors.fill: parent
      model: table.model
      delegate: Loader {
        property variant modelData: model
        sourceComponent: table.delegate
        width: table.cellWidth
        height: table.cellHeight
        onLoaded: {
          table.selected.added.connect(update(true))
          table.selected.removed.connect(update(false))
        }

        function update(value) {
          return (function(updatedIndex) {
            if(updatedIndex == index) item.selected = value;
          });
        }
      }
    }
  }

  function min(a,b) {
    if(a < b) return a;
    return b;
  }

  function abs(a) {
    if(a < 0) return -a;
    return a;
  }

  function cloneRect(a) {
    return Qt.rect(a.x, a.y, a.width, a.height);
  }

  MouseArea {
    id: mouseArea
    anchors.fill: parent
    property int startX;
    property int startY;
    property var oldMembers;
    property var action;

    Component.onCompleted: {
      oldMembers = new Array();
    }

    onPressed: {
      startX = mouse.x
      startY = mouse.y
      action = selected.add;
      if(mouse.modifiers & Qt.ShiftModifier) action = selected.remove;
      else if(!(mouse.modifiers & Qt.ControlModifier)) selected.clear(true);
      oldMembers = selected.members();
    }

    onPositionChanged: {
      var lastMembers = selected.members();
      selected.clear(false);
      for(var i = 0; i < oldMembers.length; ++i) {
        selected.add(oldMembers[i]);
      }
      var rect = Qt.rect( min(mouseArea.startX, mouseArea.mouseX)
                        , min(mouseArea.startY, mouseArea.mouseY)
                        , abs(mouseArea.mouseX - mouseArea.startX)
                        , abs(mouseArea.mouseY - mouseArea.startY));
      rect.x += content.contentX
      rect.y += content.contentY
      selectedTopLeftCorner(cloneRect(rect));
      selectedLeftHeader(cloneRect(rect));
      selectedTopHeader(cloneRect(rect));
      selectedItems(cloneRect(rect));
      for(var i = 0; i < lastMembers.length; ++i) {
        if(!selected.contains(lastMembers[i])) {
          selected.removed(lastMembers[i]);
        }
      }
      if(mouse.x >= table.width - 20) content.contentX += 50
      if(mouse.y >= table.height - 20) content.contentY += 50
      if(content.contentX > content.contentWidth - content.width) content.contentX = content.contentWidth - content.width
      if(content.contentY > content.contentHeight - content.height) content.contentY = content.contentHeight - content.height
      if(mouse.x <= 20 && content.contentX >= 50) content.contentX -= 50
      if(mouse.y <= 20 && content.contentY >= 50) content.contentY -= 50

    }

    onReleased: {
      onPositionChanged(mouse);
      newSelection();
    }

    function selectedTopLeftCorner(rect){
      if(topLeftCorner.childAt(rect.x, rect.y)) {
        for(var i = 0; i < cellsX * cellsY; ++i) action(i);
      }
    }

    function selectedTopHeader(rect) {
      rect.x -= rowHeaderWidth; // Width of top left corner
      var indexStart = topHeader.indexAt(rect.x, rect.y);
      var indexEnd = topHeader.indexAt(rect.x + rect.width, rect.y);
      if(indexStart < 0 && indexEnd < 0) return;
      if(indexStart < 0) indexStart = 0;
      if(indexEnd < 0) indexEnd = cellsX - 1;
      for(var c = indexStart; c <= indexEnd; ++c)
        for(var i = 0; i < cellsY; ++i) action(c * cellsY + i);
    }

    function selectedLeftHeader(rect) {
      rect.y -= columnHeaderHeight;
      var indexStart = leftHeader.indexAt(rect.x, rect.y);
      var indexEnd = leftHeader.indexAt(rect.x, rect.y + rect.height);
      if(indexStart < 0 && indexEnd < 0) return;
      if(indexStart < 0) indexStart = 0;
      if(indexEnd < 0) indexEnd = cellsY - 1;
      for(var l = indexStart; l <= indexEnd; ++l)
        for(var i = 0; i < cellsX; ++i) action(i * cellsY + l);
    }

    function selectedItems(rect) {
      rect.x -= rowHeaderWidth;
      rect.y -= columnHeaderHeight;
      if(rect.x < 0 && rect.x + rect.width > 0) { rect.width += rect.x; rect.x = 0; }
      if(rect.y < 0 && rect.y + rect.height > 0) { rect.height += rect.y; rect.y = 0; }
      if(rect.x + rect.width > inner.width && rect.x < inner.width)
        rect.width = inner.width - rect.x;
      if(rect.y + rect.height > inner.height && rect.y < inner.height)
        rect.height = inner.height - rect.y;
      var indexStart = inner.indexAt(rect.x, rect.y);
      var indexEnd = inner.indexAt(rect.x + rect.width, rect.y + rect.height);
      var ystart = indexStart % cellsY;
      var xstart = (indexStart - ystart) / cellsY;
      var yend = indexEnd % cellsY;
      var xend = (indexEnd - yend) / cellsY;
      for(var x = xstart; x <= xend; ++x) {
        for(var y = ystart; y <= yend; ++y) {
          action(x * cellsY + y);
        }
      }
    }
  }
 }
