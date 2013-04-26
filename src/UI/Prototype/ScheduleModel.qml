import QtQuick 2.0

ListModel {
  
  ListElement {
    day: "Montag"
    lessons:
      [ ListElement { used: true; subject: "Mathe"; teacher: "Müller"; room: 202 }
      , ListElement { used: true; subject: "Mathe"; teacher: "Müller"; room: 202 }
      , ListElement { used: false }
      ]
  }
  
  ListElement {
    day: "Dienstag"
    lessons:
      [ ListElement { used: false }
      , ListElement { used: true; subject: "Deutsch"; teacher: "Schneider"; room: 113 }
      , ListElement { used: false } 
      ]
  }
  
  ListElement {
    day: "Mittwoch"
    lessons:
      [ ListElement { used: false }
      , ListElement { used: true; subject: "Englisch"; teacher: "Richter"; room: 203 }
      , ListElement { used: true; subject: "Mathe"; teacher: "Schmidt"; room: 212 }
      ]
  }
 
}