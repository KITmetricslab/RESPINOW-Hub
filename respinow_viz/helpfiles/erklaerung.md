### Erklärung der Bedienelemente

- **Datenstand:** Wählen Sie ein Datum in der Vergangenheit, um die Nowcasts und Vorhersagen, die zu diesem Zeitpunkt gemacht wurden anzuzeigen. Dies ermöglicht den Abgleich vergangener Prognosen mit später angefallenen Daten.
- **Krankheit / Indikator**: Verschiedene epidemiologische Indikatoren können angezeigt werden. Derzeit erzeugen wir nur für eine kleine Zahl davon tatsächlich Nowcasts und Vorhersagen, dies soll aber in der Zukunft umfassender geschehen.
- **Stratifizierung**: Nowcasts können für ganz Deutschland, pro Bundesland oder pro Altersgruppe gezeigt werden. Welche Stratifizierung verfügbar ist hängt vom jeweiligen Indikator ab.
<!---
- **Grafische Darstellung**: Nowcasts können auf zwei Arten dargestellt weren: Entweder in einem interaktiven Plot, in dem für ein Bundesland/eine Altersgruppe mehrere Modelle verglichen werden können; hier ist zoomen etc. möglich. Oder in einer Übersichtsansicht, in der für ein ausgewähltes Modell alle Bundesländer oder Altersgruppen gezeigt werden.
- **Zeige Übersichtstabelle**: Zeigt eine Übersichtabelle, in der für ein gewähltes Modell und Meldedatum die Nowcasts für alle Bundesländer oder Altersgruppen aufgeführt werden. Die Tabelle zeigt die 7-Tages-Inzidenz gemäß des aktuellsten Datenstandes und des Datenstandes basierend auf dem der Nowcast erstellt wurde, den Nowcast, den resultierenden Korrekturfaktor und die Veränderung zur Vorwoche gemäß des Nowcasts.
--->


**Weitere Optionen:**

- **Maximaler Meldeverzug**: Alle Nowcasts und Vorhersagen beziehen sich auf Werte der jeweiligen Indikatoren, die über längstens vier Wochen hinweg korrigiert/vervollständigt wurden. Stark verspätete Korrekturen werden also nicht mit einbezogen. Um die Daten inklusive dieser späten Korrekturen anzuzeigen kann hier als maximaler Meldeverzug "beliebig" gewählt werden.
- **Anzeige**: Daten und Nowcasts können auf einer logarithmischen oder natürlichen Skala gezeigt werden. Exponentielles Wachstum oder Rückgang entsprechen auf einer logarithmischen Skala einer Geraden. Außerdem kann zwischen absoluten Zahlen und Zahlen pro 100.000 gewählt werden. Letzteres macht Werte z.B. über verschiedene Bundesländer hinweg besser vergleichbar.
- **Zeitreihe eingefrorener Werte**: Eine Alternative zum Nowcasting besteht darin, für jede Woche denjenigen Wert anzuzeigen, der am darauffolgenden Donnerstag verfügbar war. Auf diese Weise sind alle Datenpunkte ähnlich unvollständig und Trends können besser beurteilt werden.
- **Punktschätzer**: Es kann entweder der Erwartungswert oder der Median angezeigt werden (der Median ist der Wert für den gilt: Der wahre Wert liegt mit 50% Wahrscheinlichkeit darunter und mit 50% Wahrscheinlichkeit darüber).
- **Unsicherheitsintervall**: Soll ein Vorhersageintervall mit angezeigt werden? 95% Vorhersageintervalle sollen den wahren Wert mit 95% Wahrscheinlichkeit enthalten, 50% Vorhersageintervalle mit 50%. In der Praxis sind die wahren Werte oft seltener enthalten als gewünscht.
<!---
- **Zeitreihe nach Erscheinen in RKI-Daten**: Eine Alternative zum Nowcast der Hospitalisierungsinzidenz nach Meldedatum (Datum, an dem der erste positive Test einer Person an das lokale Gesundheitsamt gemeldet wurde) ist es, Hospitalisierungszahlen nach ihrem Auftauchen im Datensatz des RKI zu aggregieren. Diese Zahlen ändern sich in den darauffolgenden Tagen nicht mehr, sodass Trends einfacher zu interpretieren sind.
- **Nachträglich erstellte Nowcasts zeigen**: Zu Forschungszwecken sammeln wir auch Nowcasts, die erst nachtröglich, aber basierend auf dem jeweiligen Datenstand erzeugt wurden. Um Verwechslung mit in Echtzeit produzierten Nowcasts zu vermeiden zeigen wir sie standardmäßig nicht an.
- **Einheitliche y-Achsenabschnitte in Übersicht**: Falls "Überblick für ein Modell" ausgewählt ist kann hier entschieden werden, ob für alle Plots in der Übersicht der selbe y-Achsenabschnitt verwendet werden soll. Dies erleichtert den Vergleich zwischen verschiedenen Gruppen, kann aber dazu führen, dass einzelne Plots schwer lesbar sind.
--->
