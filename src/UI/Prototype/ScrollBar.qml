import QtQuick 2.0

Rectangle {
  id: root
  property Component delegate;
  property variant target;
  property double ratio: value / (width - slider.width);
  property int value: 0;
  property int diffX: 0
  property int diffY: 0;
  property bool inUpdate: false
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

      function updateSliderWidth () {
        item.width = Math.max(
          content.visibleArea.widthRatio * content.width * dirX,
          content.visibleArea.heightRatio * content.height * dirY
        );
      }

      onLoaded: {
        if(!item.width) {
          content.visibleArea.widthRatioChanged.connect(updateSliderWidth);
          content.widthChanged.connect(updateSliderWidth);
          content.heightChanged.connect(updateSliderWidth);
          content.visibleArea.heightRatioChanged.connect(updateSliderWidth);
        }
        if(!item.height) content.heightChanged.connect(function() {
          item.height = root.height
        });
      }
    }
  }
  MouseArea {
    acceptedButtons: Qt.NoButton
    anchors.fill: parent
    onWheel: {
      if(wheel.angleDelta.y > 0) value -= 50;
      else value += 50;
    }
  }

  Component.onCompleted: {
    target.contentXChanged.connect(updatePosition);
    target.contentYChanged.connect(updatePosition);
  }

  function updatePosition() {
    if(inUpdate) return;
    inUpdate = true;
    var dx = target.contentX - ratio * (target.contentWidth - target.width) * slider.dirX - diffX
      , dy = target.contentY - ratio * (target.contentHeight - target.height) * slider.dirY - diffY
      , diffValue = dx * slider.dirX + dy * slider.dirY
    value += diffValue
    var xn = ratio * (target.contentWidth - target.width) * slider.dirX
      , yn = ratio * (target.contentHeight - target.height) * slider.dirY
    diffX = target.contentX - xn
    diffY = target.contentY - yn
    inUpdate = false;
  }

  onValueChanged: {
    if(inUpdate) return;
    inUpdate = true
    if(root.width <= slider.width) {
      value = 0;
      return
    }
    if(value < 0) { value = 0 }
    if(value > root.width - slider.width) { value = root.width - slider.width}
    if(!target) return;
    if(target.contentWidth > target.width) {
      var valuex = target.contentWidth - target.width;
      target.contentX = ratio * valuex * slider.dirX + diffX
    }
    if(target.contentHeight > target.height) {
      var valuey = target.contentHeight - target.height;
      target.contentY = ratio * valuey * slider.dirY + diffY
    }
    inUpdate = false
  }
}