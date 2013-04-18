import QtQuick 2.0

Rectangle {
    width: 200
    height: 200
    color: "#FF0000"

    Rectangle {
        color: "#00FF00"
        width: 10
        height: 10
        Drag.active: dragArea.drag.active
        MouseArea {
            drag.target: parent
            anchors.fill: parent
            id: dragArea
        }
    }

    DropArea {
        x: 75; y: 75
        width: 50; height: 50

        onDropped: {
            console.log("Hello!")
        }

        onEntered: {
            console.log("Hello2!")
        }

        Rectangle {
            anchors.fill: parent
            color: "green"

            visible: parent.containsDrag
        }
    }
}
