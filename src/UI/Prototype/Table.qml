import QtQuick 2.0
import "qmlprivate.js" as P

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
  property Set selected: Set {}
  property Component rowHeaderDelegate
  property Component columnHeaderDelegate
  property Component cornerDelegate
  property Component delegate
  signal newSelection

  Loader {
    id: topLeftCorner
    z: 3
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
    z: 2
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
    z: 2
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

  GridView {
    id: content
    z: 5
    interactive: false
    anchors.right: parent.right
    anchors.left: leftHeader.right
    anchors.bottom: parent.bottom
    anchors.top: topHeader.bottom
    flow: GridView.TopToBottom
    cellHeight: table.cellHeight
    cellWidth: table.cellWidth
    width: cellHeight * table.cellsY
    height: cellWidth * table.cellsX
    model: table.model
    delegate: Loader {
      property variant modelData: model
      sourceComponent: table.delegate
      width: table.cellWidth
      height: table.cellHeight
      onLoaded: {
        table.selected.added.connect(update)
        table.selected.removed.connect(update)
      }

      function update(updatedIndex) {
        if(updatedIndex == index) {
          if(item.selected != undefined) item.selected = !item.selected;
        }
        return false;
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

    onPressed: {
      startX = mouse.x
      startY = mouse.y
      P.priv(mouseArea).action = selected.add;
      if(mouse.modifiers & Qt.ShiftModifier) P.priv(mouseArea).action = selected.remove;
      else if(!(mouse.modifiers & Qt.ControlModifier)) selected.clear();
      P.priv(mouseArea).oldMembers = selected.members();
    }

    onPositionChanged: {
      selected.clear();
      selected.add(P.priv(mouseArea).oldMembers);
      var rect = Qt.rect( min(mouseArea.startX, mouseArea.mouseX)
                        , min(mouseArea.startY, mouseArea.mouseY)
                        , abs(mouseArea.mouseX - mouseArea.startX)
                        , abs(mouseArea.mouseY - mouseArea.startY));
      selectedTopLeftCorner(cloneRect(rect));
      selectedLeftHeader(cloneRect(rect));
      selectedTopHeader(cloneRect(rect));
      selectedItems(cloneRect(rect));
    }

    onReleased: {
      onPositionChanged(mouse);
      newSelection();
    }

    function selectedTopLeftCorner(rect){
      if(topLeftCorner.childAt(rect.x, rect.y)) {
        for(var i = 0; i < cellsX * cellsY; ++i) P.priv(mouseArea).action(i);
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
        for(var i = 0; i < cellsY; ++i) P.priv(mouseArea).action(c * cellsY + i);
    }

    function selectedLeftHeader(rect) {
      rect.y -= columnHeaderHeight;
      var indexStart = leftHeader.indexAt(rect.x, rect.y);
      var indexEnd = leftHeader.indexAt(rect.x, rect.y + rect.height);
      if(indexStart < 0 && indexEnd < 0) return;
      if(indexStart < 0) indexStart = 0;
      if(indexEnd < 0) indexEnd = cellsY - 1;
      for(var l = indexStart; l <= indexEnd; ++l)
        for(var i = 0; i < cellsX; ++i) P.priv(mouseArea).action(i * cellsY + l);
    }

    function selectedItems(rect) {
      rect.x -= rowHeaderWidth;
      rect.y -= columnHeaderHeight;
      if(rect.x < 0 && rect.x + rect.width > 0) { rect.width += rect.x; rect.x = 0; }
      if(rect.y < 0 && rect.y + rect.height > 0) { rect.height += rect.y; rect.y = 0; }
      if(rect.x + rect.width > content.width && rect.x < content.width)
        rect.width = content.width - rect.x;
      if(rect.y + rect.height > content.height && rect.y < content.height)
        rect.height = content.height - rect.y;
      var indexStart = content.indexAt(rect.x, rect.y);
      var indexEnd = content.indexAt(rect.x + rect.width, rect.y + rect.height);
      var ystart = indexStart % cellsY;
      var xstart = (indexStart - ystart) / cellsY;
      var yend = indexEnd % cellsY;
      var xend = (indexEnd - yend) / cellsY;
      for(var x = xstart; x <= xend; ++x) {
        for(var y = ystart; y <= yend; ++y) {
          P.priv(mouseArea).action(x * cellsY + y);
        }
      }
    }
  }
 }