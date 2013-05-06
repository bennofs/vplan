import QtQuick 2.0

Rectangle {

  ScheduleView {
    height: 700
    width: 700
    model: ScheduleModel {}
    columns: ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
    rows: 6
  }

}