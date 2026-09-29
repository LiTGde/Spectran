# Spectran: Vergleich des App-Starts am 29.09.2026

Die Materialerweiterung erhöht den gemessenen Startaufwand. Im wiederholten lokalen Browserstart liegt B08 bei **1,020 Sekunden**, das ursprüngliche Spectran bei **0,613 Sekunden**. Das sind **0,407 Sekunden beziehungsweise 66,5 % mehr**. Hauptursache ist die bereits beim Öffnen der Einführung ausgeführte Initialisierung des Materialmoduls samt reaktiven Folgeaktualisierungen.

## Browservergleich

Je fünf neue Shiny-Sitzungen nach einer separat protokollierten ersten Sitzung, im selben Codex In-app Browser. Die Tabelle enthält Mediane; Browserressourcen waren bei den Wiederholungen bereits im Cache.

| Version / Versuch | Startzeit | Spanne der fünf Läufe | Zeit nach Shiny-Verbindung | DOM-Elemente |
|---|---:|---:|---:|---:|
| Ursprüngliches Spectran | 0,613 s | 0,542–1,115 s | 0,361 s | 1.283 |
| Akzeptierte Materialversion B08 | 1,020 s | 0,925–1,679 s | 0,789 s | 1.661 |
| B08, nur Material-Server deaktiviert | 0,657 s | 0,611–1,043 s | 0,385 s | 1.547 |
| Neue Tabellen und D65-Einstieg, B09-Entwicklungsstand | 0,974 s | 0,873–1,722 s | 0,728 s | 1.660 |

Im isolierten Versuch bleibt die Materialoberfläche vorhanden; lediglich `transmissionServer()` wird im temporären R-Prozess durch leere reaktive Rückgaben ersetzt. Die Quelldateien bleiben unverändert. Damit entfallen 0,363 Sekunden, also rund **89 % des zusätzlichen Medianaufwands** von B08 gegenüber dem Original. Der Eingriff entfernt auch die vom Modul ausgelösten Browseraktualisierungen; er misst deshalb mehr als die reine Laufzeit seines Funktionsaufrufs. Die kleinen Unterschiede zwischen B08 und dem neuen Stand erlauben angesichts der Streuung keine Aussage über eine Beschleunigung.

## Frische R-Prozesse

Fünf Läufe je Stand, abwechselnde Reihenfolge, dieselbe R-Installation und dieselben Abhängigkeiten. Dies sind neue Prozesse mit vorhandenem Betriebssystem-Dateicache, keine Messungen nach einem Rechnerneustart. Alle Stände werden gleich mit `pkgload::load_all()` gestartet, wie der lokale Showcase.

| Stand | Paket laden | App-Oberfläche konstruieren | Erstes HTML erzeugen | HTML-Größe |
|---|---:|---:|---:|---:|
| Ursprüngliches Spectran | 0,719 s | 0,056 s | 0,088 s | 113.202 Byte |
| Erster Transmissions-Prototyp | 0,822 s | 0,079 s | 0,113 s | 168.495 Byte |
| Materialversion B08 | 0,784 s | 0,086 s | 0,111 s | 160.916 Byte |
| Neuer Entwicklungsstand | 0,799 s | 0,087 s | 0,111 s | 161.179 Byte |

Diese Phasen und die Browsermessung haben unterschiedliche Startpunkte und dürfen nicht unbesehen addiert werden. Der Paketladevorgang ist ein einmaliger Prozessaufwand. Ein späterer Browseraufruf eines schon laufenden Showcase lädt das R-Paket nicht erneut.

## Welche Faktoren beitragen

1. **Materialmodul beim Sitzungsstart.** Der isolierte Versuch liefert hierfür den stärksten Nachweis. Das R-Profil zeigt im Median ungefähr 145 ms allein im Aufruf von `transmissionServer()`, darunter 35 ms für den Verlauf, 15 ms für Tooltip-Registrierungen und 10 ms für die Anwendungskomponente. Diese Zeiten enthalten Unteraufrufe und überlappen. Hinzu kommen anschließende Initialisierungs-, Validierungs- und UI-Aktualisierungen. Der Start erzeugt diese Infrastruktur schon vor dem ersten Besuch der Materialseite.
2. **Mehr Oberfläche und Browserzustand.** B08 erzeugt etwa 48 kB mehr initiales HTML und 378 zusätzliche DOM-Elemente. Konstruktion und HTML-Erzeugung kosten zusammen etwa 53 ms mehr als im Original. Auch mit deaktiviertem Material-Server bleibt deshalb ein kleiner Mehrbedarf bestehen.
3. **Zusätzlicher R-Code und Abhängigkeiten.** Das Laden benötigt in dieser Messreihe etwa 65 ms mehr als im Original. Der erste Transmissions-Prototyp ist hier sogar langsamer als B08. Die neue TUB-/Reflexionserweiterung erklärt somit keinen großen Sprung beim eigentlichen Paketladen.
4. **Materialdaten und Normierung sind kein großer Startkostentreiber.** Das Profil zeigt nur kleine Stichprobenanteile für Katalogzugriff und Kurvenvorbereitung, typischerweise im Bereich einer 5-ms-Profilstichprobe. Es wird die ausgewählte Kurve vorbereitet, kein vollständiger Normvergleich über alle Materialien gerechnet. Bei der ersten Browsermessung liegen die erfassten Ressourcen beider Stände bei rund 3,07 MB. Die Materialerweiterung lädt keine zusätzliche große Sammlung von Farbbildern.

Der sinnvollste Ansatz aus dieser Ursachenanalyse ist, den Material-Server und seine aufwendigen Ausgaben erst beim ersten Öffnen der Materialseite zu initialisieren. Das würde Startarbeit auf den Zeitpunkt ihrer Nutzung verschieben. Danach sollten ausgeblendete Vorschau-/Verlaufsausgaben auf unnötige Aktualisierung geprüft werden. Eine Kürzung der 55 TUB-Beispiele wird durch die Messungen nicht begründet. Im Rahmen dieses Vergleichs wurde keine solche Architekturänderung vorgenommen.

## Messgrenzen und Reproduktion

Referenz ist Commit `97208f8`, unmittelbar vor dem Transmissions-Prototyp. `R/`, `DESCRIPTION` und `NAMESPACE` stimmen mit dem gespeicherten `origin/main` bei `5bad285` überein. Der Prototyp ist `f6408dd`. B08 ist `MAT-UX-B08-4d3bf2c8519b`, aus der unveränderten eingefrorenen Kopie. Der neue Messstand enthält die angeforderte Tabellenaufteilung und den automatischen D65-Einstieg; die späteren Änderungen betreffen nur Tabellenbeschriftungen, die Darstellung gerundeter Nullabweichungen und den Ausklapppfeil. Sie wurden nicht als Performance-Optimierung behandelt.

R 4.6.1, macOS ARM64, gemeinsame Projektbibliothek. Die genauen Paketversionen stehen in den gespeicherten `*-sessionInfo.txt`. Keine Hintergrund-Paketinstallation oder Paketprüfung lief während der wiederholten Browsermessungen. Andere Tätigkeiten auf dem Rechner, Browser-JIT und Betriebssystem-Caches sind nicht vollständig kontrolliert. Es gibt keine Messung auf einem entfernten Shiny-Server oder über eine langsame Internetverbindung.

Die Browserzeit läuft ab Navigation bis zum letzten `shiny:idle` vor einer Ruhephase von 750 ms. Die Ruhephase selbst wird nicht zur ausgewiesenen Zeit addiert. Das Messskript zeichnet zusätzlich Verbindungs-, DOM-, Lade- und Ressourcenzeiten auf. Dies ist ein definierter Shiny-Bereitschaftsindikator, keine standardisierte Web-Vitals-Messung und keine Garantie, dass ein externes Tutorialvideo fertig geladen ist. R-Profile werden im Abstand von 5 ms erfasst und können selbst etwas Aufwand verursachen; alle verglichenen Browserstände sind gleich instrumentiert.

Dateien:

- `browser-measurements.csv` und `browser-summary.csv`: einzelne Sitzungen und Zusammenfassung, einschließlich der getrennten ersten Sitzungen.
- `process-measurements.csv` und `process-summary.csv`: alle frischen Prozesse und Mediane.
- `profile-measurements.csv`, `profile-summary.csv` und `*.Rprof`: Stichprobenprofile. Inklusive Zeiten verschachtelter Funktionen nicht addieren.
- `probe.R`, `process-benchmark.R`, `serve-profile.R`, `summarise.R`: tatsächlich verwendete R-Skripte.

Zum Wiederholen die Referenzstände mit `git archive` in temporäre Verzeichnisse exportieren, die Pfade in `process-benchmark.R` setzen und das Skript mit `Rscript --vanilla` ausführen. `serve-profile.R QUELLPFAD LABEL PORT` startet die entsprechende lokale Browsermessung. Das Label `b08_no_material_server` aktiviert ausschließlich im Messprozess den beschriebenen Eingriff. Nach einer ersten Sitzung je fünf neue Sitzungen abwechselnd öffnen; anschließend `summarise.R` ausführen. Die Pfade in den Skripten dokumentieren den tatsächlichen Messlauf und können für einen neuen Ausgabestand angepasst werden.

Alle Berechnungen und Zusammenfassungen erfolgen in R; JavaScript erfasst lediglich die Browserzeitstempel. Die Messdateien liegen unter `tests/verification` und werden durch `.Rbuildignore` vom CRAN-Paket ausgeschlossen.

## Anschließende Umsetzung

Auf ausdrücklichen Nutzerwunsch wurde danach die bedarfsgesteuerte Initialisierung umgesetzt und separat mit dem unmittelbar vorherigen Stand verglichen. Ergebnisse, Wartezeit beim ersten Materialaufruf und Rohdaten stehen in `../startup-lazy-2026-09-29/README.md`. Die obigen Messungen dokumentieren unverändert den Zustand vor dieser Optimierung.
