**M4-12: zur abschließenden Nutzerabnahme empfohlen. Keine neuen Findings.**

Visueller Review vom 07.10.2026 mit dem aktiven Skill `review-shiny-ux`. Die Build-Kennung **TRX-M4-12-57cc87de8c2a** war auf beiden Prüffassungen sichtbar und stimmte mit der Übergabe überein:

- Deutsch: http://127.0.0.1:7545/
- Englisch: http://127.0.0.1:7546/

Geprüft wurden ausschließlich die neue Themen-Navigation und ihre angrenzenden Auswahl-, Fokus- und Rückkehrwege. Prüfbasis waren sichtbare Bedienzustände und Screenshots, bedient mit Maus und Tastatur. Die akzeptierte Fassung M4-10 bleibt außerhalb dieses Zusatzes der geschützte Ausgangsstand.

Die vollständige Vorwärtsfolge auf Deutsch und Rückwärtsfolge auf Englisch stimmen mit Übersicht und Dropdown überein:

| Position | Deutsch | Englisch |
| --- | --- | --- |
| 1 | Ein Spektrum lesen | Reading a spectrum |
| 2 | Beleuchtungsstärke, EDI und DER | Illuminance, EDI and DER |
| 3 | Lichtfarbe und Farbwiedergabe | Light colour and colour rendering |
| 4 | Alter und Auge | Age and the eye |
| 5 | Import, Skalierung und Export | Import, scaling and export |
| 6 | Materialwirkung im Überblick | Material effects at a glance |
| 7 | Einfallend und austretend | Incident and outgoing light |
| 8 | Wie ein Material das Spektrum verändert | How a material changes the spectrum |
| 9 | Vom Material zum Empfänger: F | From material to receiver: F |
| 10 | Schritte und Gesamtwirkung im Lichtpfad | Steps and combined effects in the light path |

Bestätigte Ergebnisse:

- Positionsanzeige und beide Zielbeschriftungen passen zum jeweils dargestellten Thema. Der Übergang 5 ↔ 6 funktioniert in beiden Richtungen. Am Anfang ist Zurück, am Ende Weiter deaktiviert; es gibt keinen Umlauf.
- Direktauswahl im Dropdown, Auswahl über die Übersicht und wiederholte Weiter-/Zurück-Wechsel bleiben konsistent. Die Übersicht zeigt keine irreführende laufende Themenposition.
- Tab und Shift+Tab erreichen die neuen Buttons in nachvollziehbarer Reihenfolge. Enter und Leertaste aktivieren sie. Die Dropdown-Auswahl ließ sich über den browserseitigen Tastaturpfad öffnen, mit End ändern und mit Enter übernehmen.
- Nach einem Wechsel über die neuen Weiter-/Zurück-Buttons beginnt die Ansicht wieder oben. Der Fokus liegt sichtbar auf der Überschrift des Erläuterungsbereichs; ein Fokusverlust oder Verbleib am Ende des vorherigen Themas wurde dabei nicht beobachtet.
- Nach mehreren Themenwechseln führt der kontextuelle Rückweg in beiden Sprachen zurück zur Einführung. Der Fokus kehrt zu „Erläuterungen öffnen“ beziehungsweise „Open explanations“ zurück.
- Bei 1440 × 1000, 390 × 844 und 320 × 844 sind die geprüften Navigationszustände in beiden Sprachen bedienbar. Lange Zielnamen umbrechen vollständig. Die schmalen Ansichten stapeln die großen Schaltflächen ohne Überlagerung oder abgeschnittene Aktionen.

**UI-/UX-Wertung: gut und abnahmefähig.** Positionsanzeige, konkrete Zieltitel, klare Richtungspfeile und große Trefferflächen unterstützen das fortlaufende Lesen. Die neue Navigation fügt sich in die bestehende Gestaltung ein und erhält den Kontext-Rückweg.

Ausgewählte Evidenz:

- [Desktop: Tastaturfokus und erstes Thema](02-de-first-navigation-keyboard.png)
- [Desktop: Ende ohne Umlauf](04-de-desktop-end.png)
- [Deutsch, 390 px](05-de-390-navigation.png)
- [Deutsch, 320 px: lange Ziele](06-de-320-long-target.png)
- [Deutsch, 320 px: Fokus und Scrollposition nach Wechsel](07-de-320-after-switch.png)
- [Englisch, 390 px: erstes Thema](09-en-390-first-topic.png)
- [Englisch, 320 px: lange Ziele](11-en-320-long-targets.png)

Die Viewport-Vorgabe wurde zurückgesetzt. Ausschließlich die eigenen Prüftabs 3 und 4 wurden geschlossen. Nächster Freigabeschritt ist die abschließende Nutzerabnahme dieses Zusatzes.
