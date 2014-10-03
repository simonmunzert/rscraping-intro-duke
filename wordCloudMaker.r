wordCloudMaker <- function(X, language = "english", return.data = FALSE, ...) {
  require(tm)
  require(wordcloud)
  require(RColorBrewer)
  
  Xcoll <- paste(X, sep=" ", collapse=" ")
  corpus <- Corpus(DataframeSource(data.frame(Xcoll)))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, function(x) removeWords(x, stopwords(language)))
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  tdm <- DocumentTermMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(colSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, "Blues")
  pal <- pal[-(1:4)]
  
  wordcloud(d$word, d$freq, scale=c(4,1), colors=pal, vfont=c("sans serif","plain"), ...)
  if(return.data == TRUE) {return(d)}
}
