frequent_words <-function(text, k) {
  # Funktion mit Vektor der zu untersuchenden Text enthält und k, ein Faktor, der die Länge des k-mers bedeutet
  frequent_patterns <- {}
  # Funktionsinterner Vektor für häufige muster 
  count <-{}
  # Funktionsinterner Vektor für anzahl häufiger muster
  pattern_count <-function(text, pattern) {
    #Funktion um ein definiertes pattern in text zu suchen, immer, wenn es vorkommt wird der numeric count um 1 erhöht
    #pattern_count muss in der funktion definiert werden, da es auch in der funktion verwendet wird
    count <- 0
    #count wird innerhalb der Funktion als 0 definiert
    pattern_length <- nchar(pattern)
    #pattern_length wird als Charakteranzahl von pattern definiert
    for (i in 0:(nchar(text) - pattern_length)) {
      #for-Schleife von 0 bis Charakteranzahl von text - Länge von pattern
      if (substr(text, i + 1, i + pattern_length) == pattern)  count <- count + 1
        #Frage: wenn Sequenz in text bei i dem gesuchten pattern entspricht, dann count +1
    }
    return(count)}
    #return gibt nun den fertigen Wert aus, der an der entsprechenden stelle von count[]eingefügt wird
  
  for (i in 0:(nchar(text) -k)) {
    #for-Schleife von 0 bis Charakteranzahl von text - Länge von k
    pattern <-substr(text, i + 1, i + k)
    #pattern ist eine Sequenz aus Text von i so lang wie k - Referenz: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
    count[i + 1] <- pattern_count(text, pattern)
    #count[] ist ein vektor, der an jeder stelle von text, angibt, wie oft das dort beginnende k-mer noch in text vorkommt
  }
  max_count <-max(count)
  #Objekt max_count ist maximalwert in Vektor count, also die häufigkeit von dem/ den häufigsten patterns
  for (i in 0:(nchar(text) -k)) {
    #for-Schleife von 0 bis Charakteranzahl von text - Länge von k
    if (count[i + 1] == max_count)
      #Frage: ist bei Durchgang für pattern startend bei i die Stelle i + 1 in count das häufigste pattern (=max_count)
      frequent_patterns <- append(frequent_patterns, substr(text, i + 1, i + k))}
  #Objekt frequent patterns wird um Sequenz bei i + 1 erweitert Referenz: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/append
  return(unique(frequent_patterns))}
#Ausgabe der häufigsten Patterns, wobei durch unique die patterns einzeln ausgegeben werden (ohne unique würden sie in frequent_patterns so oft vorhanden sein, wie sie vorkommen) Referenz: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/unique


text <- c("A","C","G","T","T","G","C","A","T","G","T","C","G","C","A","T","G","A","T","G","C","A","T","G","A","G","A","G","C","T")
#Definieren der Genomsequenz als sample
k <- 3L
#k als integer für gewünschte Länge wiederkehrender Sequenz definieren
sample <- frequent_words(text,k)

text <- c("C","G","G","A","A","G","C","G","A","G","A","T","T","C","G","C","G","T","G","G","C","G","T","G","A","T","T","C","C","G","G","C","G","G","G","C","G","T","G","G","A","G","A","A","G","C","G","A","G","A","T","T","C","A","T","T","C","A","A","G","C","C","G","G","G","A","G","G","C","G","T","G","G","C","G","T","G","G","C","G","T","G","G","C","G","T","G","C","G","G","A","T","T","C","A","A","G","C","C","G","G","C","G","G","G","C","G","T","G","A","T","T","C","G","A","G","C","G","G","C","G","G","A","T","T","C","G","A","G","A","T","T","C","C","G","G","G","C","G","T","G","C","G","G","G","C","G","T","G","A","A","G","C","G","C","G","T","G","G","A","G","G","A","G","G","C","G","T","G","G","C","G","T","G","C","G","G","G","A","G","G","A","G","A","A","G","C","G","A","G","A","A","G","C","C","G","G","A","T","T","C","A","A","G","C","A","A","G","C","A","T","T","C","C","G","G","C","G","G","G","A","G","A","T","T","C","G","C","G","T","G","G","A","G","G","C","G","T","G","G","A","G","G","C","G","T","G","G","A","G","G","C","G","T","G","C","G","G","C","G","G","G","A","G","A","T","T","C","A","A","G","C","C","G","G","A","T","T","C","G","C","G","T","G","G","A","G","A","A","G","C","G","A","G","A","A","G","C","G","C","G","T","G","C","G","G","A","A","G","C","G","A","G","G","A","G","G","A","G","A","A","G","C","A","T","T","C","G","C","G","T","G","A","T","T","C","C","G","G","G","A","G","A","T","T","C","A","A","G","C","A","T","T","C","G","C","G","T","G","C","G","G","C","G","G","G","A","G","A","T","T","C","A","A","G","C","G","A","G","G","A","G","G","C","G","T","G","A","A","G","C","A","A","G","C","A","A","G","C","A","A","G","C","G","C","G","T","G","G","C","G","T","G","C","G","G","C","G","G","G","A","G","A","A","G","C","A","A","G","C","G","C","G","T","G","A","T","T","C","G","A","G","C","G","G","G","C","G","T","G","C","G","G","A","A","G","C","G","A","G","C","G","G")
k <- 12L
#das gleiche nochmal für extradataset
extra <- frequent_words(text,k)

show(sample)
show(extra)