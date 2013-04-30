import QtQuick 2.0

Rectangle {
  id: root
  property variant delegate;
  property variant target;
  property double ratio: value / (width - slider.width);
  property int value: 0;
  property bool reversed: true;

  // First clear rotation, the apply again, so that it is also applied to our movement changes.
  Rectangle {
    transformOrigin: parent.transformOrigin
    rotation: -parent.rotation
    Loader {
      id: slider
      property double dirY: Math.sin(root.rotation * Math.PI / 180);
      property double dirX: Math.cos(root.rotation * Math.PI / 180);
      rotation: -parent.rotation
      transformOrigin: parent.transformOrigin
      sourceComponent: delegate;
      x: value * dirX
      y: value * dirY
      MouseArea {
        id: mouseArea
        property int offset;
        anchors.fill: parent
        onPressed: {
          offset = mouse.x;
        }

        onPositionChanged: {
          value += (mouse.x - offset)
        }
      }
    }
  }
  MouseArea {
    acceptedButtons: Qt.NoButton
    anchors.fill: parent
    onWheel: {
      console.log(wheel.angleDelta)
      if(wheel.angleDelta.y > 0) value -= 50;
      else value += 50;
    }
  }

  onValueChanged: {
    if(root.width <= slider.width) {
      value = 0;
      return
    }
    if(value < 0) { value = 0 }
    if(value > root.width - slider.width) { value = root.width - slider.width}
    if(!target) return;
    if(target.contentWidth > target.width) {
      var valuex = target.contentWidth - target.width;
      target.contentX = ratio * valuex * slider.dirX
    }
    if(target.contentHeight > target.height) {
      var valuey = target.contentHeight - target.height;
      target.contentY = ratio * valuey * slider.dirY
    }
  }
}