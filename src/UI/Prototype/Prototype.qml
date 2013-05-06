import QtQuick 2.0

ScheduleView {
  height: 300
  width: 300
  flow: GridView.TopToBottom
  model: ScheduleModel {}
  columns: ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag"]
  rows: 6
}