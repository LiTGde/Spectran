# Startoptimierung durch bedarfsgesteuerte Initialisierung

Am 29.09.2026 wurde der Material-Server so geändert, dass er erst beim ersten Besuch von Transmission / Reflexion initialisiert wird. Seine reaktiven Verbindungen zu Import, Übernahme und Wiederherstellung bleiben über feste Vermittlungsfunktionen verbunden. Nach der Initialisierung bleibt dasselbe Modul während der gesamten Sitzung aktiv. Ein Tabwechsel baut es nicht erneut auf.

## Wiederholter Vergleich vor und nach der Änderung

Je fünf neue Sitzungen mit warmem Browsercache, abwechselnd vor/nach der Änderung; jeweils eine zusätzliche erste Sitzung separat protokolliert und nicht im Median. Die Ausgangssituation ist die Einführung ohne importiertes Spektrum. Der erste Materialaufruf erzeugt daher in beiden Ständen die automatische D65-Basis mit 100 lx und die G1-Vorschau. Nach einem Wechsel zu Import wird dieselbe Materialseite erneut geöffnet.

| Vorgang | Vorher, B10 | Bedarfsgesteuerter Start, B11 |
|---|---:|---:|
| App-Start, Median | 1,186 s | 0,636 s |
| Spanne der fünf App-Starts | 0,900–2,435 s | 0,623–0,913 s |
| Erster Materialaufruf, Median | 2,416 s | 2,714 s |
| Spanne der ersten Materialaufrufe | 2,206–2,427 s | 2,618–3,237 s |
| Erneuter Materialaufruf, Median | 0,019 s | 0,019 s |

Der Start wird um **0,550 Sekunden beziehungsweise 46,4 %** kürzer. Der erste Materialaufruf kostet im Median **0,298 Sekunden mehr**. Die Änderung verschiebt einen Teil der Arbeit auf die erste Nutzung; sie beseitigt diese Arbeit nicht. Wer nur die bisherigen Import-/Auswertungsfunktionen nutzt, initialisiert den Material-Server gar nicht. Alle 55 TUB-Beispiele bleiben verfügbar.

Der zuvor durchgeführte Vergleich mit dem ursprünglichen Spectran ergab 0,613 s Startzeit. Die optimierte Version liegt in derselben Größenordnung. Diese Zahl stammt jedoch aus der früheren Messreihe, nicht aus dem hier gepaarten Vorher-/Nachher-Vergleich. Details und die ursprüngliche Ursachenanalyse: `../startup-2026-09-29/README.md`.

## Referenzen und Messverfahren

Vorher: eingefrorenes `MAT-UX-B10-74fadd8bd37d`, SHA-256 `74fadd8bd37d4ff2f14ad02b79e5adf62a54e312312d037396eab56f3ced3472`, Pfad `/private/tmp/spectran-ux-review/B10/Spectran`. Nachher: Autorstand mit denselben Laufzeitdateien wie `MAT-UX-B11-b06d34913f5d`, SHA-256 `b06d34913f5d7fb5e1a2f423cc97bc21789772c36e8f97413b6f6809f100fa59`. Die nach der Messung ergänzten NEWS-Einträge verändern keine Laufzeitlogik. Die Menü-ID wurde vor der optimierten Messung wiederhergestellt, damit der bisherige Zeilenumbruch erhalten bleibt.

R 4.6.1 auf macOS ARM64, identische Projektbibliothek (Versionen in `package-versions.csv`), derselbe Codex In-app Browser. Kein Paketcheck und keine Paketinstallation liefen während dieser Wiederholungen. Fremde Systemlast, JIT und Caches sind nicht vollständig kontrolliert. Das sind lokale Messungen, keine Garantie für einen entfernten Server. Ein Ausreißer beim ersten wiederholten Vorher-Start bleibt unverändert in den Rohdaten enthalten.

App-Start: Navigation bis zum letzten `shiny:idle` vor 750 ms Ruhe. Materialaufruf: Klick auf die Seitenleisten-Verknüpfung bis zum letzten `shiny:idle` vor derselben Ruhephase. Die 750 ms werden nicht zur ausgewiesenen Zeit addiert. Der zweite Aufruf wird nach einem sichtbaren Wechsel zu Import gemessen. Ein externer Tutorialfilm muss für diesen Indikator nicht fertig geladen sein. Die browserseitigen Zeitstempel werden mit JavaScript erfasst; Auswertung, Mediane und Änderungen werden in R berechnet.

`serve-profile.R QUELLPFAD LABEL PORT` startet einen instrumentierten Vergleichsserver. Labels sind `before_lazy` und `lazy`. Die Instrumentierung ergänzt ausschließlich Zeitmessung und eine sichtbare Abschlussmarkierung. `summarise.R` erzeugt `summary.csv`, `startup-measurements.csv` und `material-measurements.csv` aus allen Sitzungsdateien. Es prüft, dass fünf vollständige Wiederholungen pro Stand und Vorgang vorliegen. Die ersten Sitzungen sind als `first_session` separat ausgewiesen. Die absoluten Pfade in den Skripten dokumentieren diesen Messlauf.

Die numerischen Methoden, Spektraldaten und Materialmodelle wurden für diese Optimierung nicht verändert. R-Integrationstests prüfen direkte D65-Aktivierung, unveränderte Modulinstanz bei wiederholter Navigation, Übernahme, Wiederherstellung, erhaltene Zweige, Importabbruch und bestätigten Import. Der unabhängige Browserreview prüft die sichtbare Bedienung zusätzlich. Die Messunterlagen sind durch `.Rbuildignore` vom Paket ausgeschlossen.
