import QtQuick 2.0
import QtQuick.Window 2.0
import QtDesktop 1.0

Item {
    width: 200
    height: 200

    ListModel {
        id: teachers
        ListElement { name: "Müller"; title: "Frau"; shortName: "Mü" }
        ListElement { name: "Schneider"; title: "Herr"; shortName: "Sch" }
        ListElement { name: "Schmidt"; title: "Frau"; shortName: "Schm" }
    }

    Component {
        id: teacher
        Rectangle {
            Drag.active: dragArea.drag.active
            Drag.hotSpot.x: 10
            Drag.hotSpot.y: 10
            height: 50
            width: parent.width
            color: "steelblue"
            MouseArea {
                id: dragArea
                anchors.fill: parent
                drag.target: parent
                drag.axis: Drag.XAndYAxis
            }
            Text {
                id: content
                text: title + " " + name
                onLineLaidOut: {
                    console.log("Hello!")
                }
            }
        }
    }

    DropArea {
        id: droparea
        x: 0; y: 0
        width: parent.width - 150
        height: parent.height
        onDropped: {
            console.log("Hello2!");
        }
        onEntered: {
            console.log(drag.accept(Qt.CopyAction));
        }

        Rectangle {
            anchors.fill: parent
            color: "green"
            visible: parent.containsDrag
        }
    }

    Rectangle {
        gradient: Gradient {
            GradientStop { position: 0.0; color: "darkslategray" }
            GradientStop { position: 1.0; color: "dimgray" }
        }
        height: parent.height
        anchors.right: parent.right
        width: 150
        ListView {
            id: view
            spacing: 10
            height: parent.height
            width: parent.width
            model: teachers
            delegate: teacher
        }
    }
}
