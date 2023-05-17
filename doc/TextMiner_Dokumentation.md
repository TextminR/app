# TextminR Dokumentation

ausgearbeitet von Benjamin Kissinger



Im Unterricht gibt es derzeit keine Möglichkeit, für spezifische Wörter und Wortgruppen den  literaturgeschichtlichen Kontext zu ermitteln. Solch eine Möglichkeit kann für eine Reihe von  Fächern, vor allem Deutsch, zugutekommen. Dieses Projekt widmet sich diesem Problem.

Dafür wird eine KI erstellt, die aus einem Datenkorpus, welcher aus einer Sammlung von  deutsch- und englischsprachigen Texten besteht, häufig benutzte Wörter und Wortgruppen  raussucht. Für diese KI soll eine Webapp erstellt werden, welche angepasste Diagramme  anzeigen kann. Das Bildungsministerium sieht vor, dass diese dann als Schnittstelle für  ProfessorInnen im Sprachunterricht dient.

Zusätzlich werden verschiedene Methoden zur Textanalyse bereitgestellt, welche im folgenden Dokument genauer erläutert werden.





## Wordcloud

Eine Wordcloud ist eine visuelle Darstellung von Text, bei der häufig verwendete Wörter größer dargestellt werden als seltener verwendete Wörter. Dabei werden die Wörter in unterschiedlichen Farben und Schriftarten angezeigt und können in verschiedenen Formen angeordnet werden. Die Größe und Platzierung der Wörter in der Wordcloud werden durch die Häufigkeit des Auftretens der Wörter im Text bestimmt. Wordclouds werden oft verwendet, um die wichtigsten Themen oder Schlagwörter in einem Text auf einen Blick zu erfassen: 

![](.\img\wordcloud.PNG)

Um eine solche Wordcloud zu erstellen sind mehrere Schritte notwendig:

1. Geeignete GUI erstellen
2. Daten aufbereiten
3. Die Häufigkeit der Wörter ermitteln
4. Wordcloud erstellen

Außerdem müssen folgende Libraries importiert werden:

```R
library(shiny)
library(wordcloud2)
library(tidytext)
library(SnowballC)
library(dplyr)
library(tibble)
```



### Geeignete GUI erstellen

Da das Projekt mit Shiny realisiert wurde, wird strikt zwischen der GUI und der Logik unterschieden. Fangen wir also zuerst mit der GUI an:

```R
ui <- fluidPage(
  # Titel
  titlePanel("Amerikanische Reden von Präsidenten"),
  
  # Linke Seite
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_words", "Maximale Anzahl an Wörtern:",
                  min = 1, max = 500, value = 100, step = 10),
      sliderInput("years", "Von 1790 bis ...:",
                  min = 1790, max = 1903, value = 1790, step = 1)
    ),
    # Wordcloud-Bereich
    mainPanel(
      wordcloud2Output("wordcloud", width = "100%", height = "600px")
    )
  )
)
```

In der ersten Zeile definieren wir das Layout der UI. In diesem Fall haben wir uns für eine FluidPage entschieden. Anschließend kann man den Titel der WebApp mit `titlePanel("...") ` festlegen.

Als nächstes müssen wir das SidebarLayout erstellen, in welches wir die Slider für die Filter hinzufügen. In unserem Anwendungsfall brauchen wir nur zwei **Slider**. Diese kann man mit folgendem Befehl erstellen `sliderInput(*name des elements*, *Titel*, min, max, Startwert, *Größe pro Schritt*)`.

Zu guter Letzt erstellen das **MainPanel**, in welchem später die WordCloud dargestellt wird.



### Daten aufbereiten

Die Texte, die wir in diesem Beispiel verwenden, sind amerikanische Reden. Diese sind in einem Dataframe gespeichert. Eine Zeile beinhaltet genau eine Rede. Diese Texte müssen noch so angepasst werden, dass sie effizient von der Wordcloud-Library benutzt werden können. Als ersten Schritt müssen wir die sgn. Stoppwörter aus den Reden entfernen. Stoppwörter sind jene Wörter, welche kein wirkliche Information enthalten. Praktischerweise beinhaltet die Library `tidytext` ein Dataframe namens *stop_words*, welches genau diese Stoppwörter enthält.

Zusätzlich müssen wir 2 neue Spalten erstellen. Einmal eine eigene Spalte für das Jahr, damit wir die Ergebnisse nach dem Jahr Filtern können. Außerdem brauchen wir eine eigene Spalte mit den Wörtern. Diese ist notwendig, weil wir später für die Wordcloud zählen müssen,, wie oft ein Wort vorkommt. 

```R
# Filtere die Textdaten nach dem ausgewählten Jahr
    # %>% ist eine Pipe; statt jedes mal "words <-" zu verwenden, kann man es so machen und zwischenspeichern
    filtered_textdata <- textdata %>%
      mutate(year = year(date)) %>% # Extra Spalte year erstellen, um das Jahr zu vergleichen
      filter(year <= input$years) # Alle Zeilen entfernen, die <= user input sind
```

Bevor ich den Code erkläre, müssen wir klären, was folgendes Zeichen bedeutet: %>%. Das ist die sgn. **Pipe**, welche mit der Library `dplyr` importiert wird. Man kann die Pipe verwenden, wenn man das Ergebnis der Funktionen nicht immer zwischen speichern will. Man könnte den oben gezeigten Code also auch folgendermaßen schreiben:

```R
 filtered_textdata <- textdata 
 filtered_textdata <- mutate(year = year(date)) 
 filtered_textdata <- filter(year <= input$years) 
```

Mit `mutate(year = year(date))` erstellen wir, wie oben beschrieben, eine eigene Spalte für das Jahr. Mit `filter(year <= input$years) ` werden alle Einträge aus dem Dataframe mit den Reden entfernt, die kleiner als der User-Input `input$years` sind.

Anschließend müssen wir noch folgende Befehle ausführen:

```R
# Verarbeite die gefilterten Textdaten mit tidytext
    words <- filtered_textdata %>%
	  # Die Wörter in der Spalte text werden extrahiert und in eine eigene Spalte word gespeichert 
      unnest_tokens(word, text, to_lower = TRUE) %>% 
      anti_join(v) # stopwords entfernen
```

Mit `unnest_tokens(word, text, to_lower = TRUE)` speichern wir alle Wörter aus *text* in einer neuen Spalte *word*. Mit `to_lower = TRUE` werden alle Wörter klein geschrieben gespeichert.



### Die Häufigkeit der Wörter ermitteln

Nun ermitteln wir die Häufigkeit der Wörter aus den Texten:

```R
# Extrahiere die am häufigsten vorkommenden Wörter und ihre Häufigkeit
    top_words <- words %>%
      count(word, sort = TRUE) %>%
      head(input$num_words)
```

Dazu verwenden wir die `count()`-Methode. Mit `count(word, sort = TRUE)` werden die Wörter nach ihrem Vorkommen gruppiert. Anschließend wird die Häufigkeit in einer eigenen Spalte `n` gespeichert.

`head(input$num_words)` wählt genau so viele Wörter aus `top_words` aus, wie der User (`input$num_words`) haben will.



### Wordcloud erstellen

Nun ist alles vorbereitet und wir können die Wordcloud erstellen. Gemacht haben wir das mit der `wordcloud2`-Library:

```R
wordcloud2(data.frame(name = top_words$word, size = top_words$n),
               size = 1, backgroundColor = "white", fontFamily = "sans-serif")
```

Der erste Parameter ist ein Data-Frame aus den Wörtern, die angezeigt werden sollen (`name = top_words$word`) und deren Häufigkeit (`size = top_words$n`). Der zweite Parameter ist die Font-Size. Standardmäßig ist sie auf 1 gestellt. Der dritte Parameter bestimmt die Hintergrundfarbe (`backroundColor = "white"`) - wir haben uns für einen weißen Hintergrund entschieden. Mit dem vierten und letzten Parameter legen wir die Schriftart fest.