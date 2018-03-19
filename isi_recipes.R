library(bibliometrix)
library(tidytext)
library(tm)
library(topicmodels)
library(dplyr)
library(tm.plugin.webmining)

#all articles before selection, stats on bibtex
isi.rawdata <- readFiles("savedrecs.bib")
isi.dfrawdata <- convert2df(isi.rawdata, dbsource = "isi", format = "bibtex")

#conceptual coword structure
isi.rawcs <- conceptualStructure(isi.dfrawdata, field = "DE_TM", minDegree = 7, k.max = 4, stemming = FALSE, labelsize = 10)
isi.rawcs$km.res
#best repr
isi.rawcs <- conceptualStructure(isi.dfrawdata, field = "DE_TM", minDegree = 14, k.max = 5, stemming = FALSE, labelsize = 10)


#work with selected pdf files
#first set the workspace where all pdf files are located
files <- list.files("isiarticles/",pattern = "pdf$")
rpdf <- readPDF(control = list(text = "-layout"))
isi.pdf <- Corpus(URISource(files), readerControl = list(reader = rpdf))
isi.pdftdm <- TermDocumentMatrix(isi.pdf, control=list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = TRUE, removeNumbers = TRUE))
isi.pdfdtm <- DocumentTermMatrix(isi.pdf, control=list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE))

#select terms
isi.dtmTerms <- Terms(isi.pdfdtm)
head(isi.dtmTerms)
isi.pdftidy <- tidy(isi.pdfdtm)
isi.pdfsentiment %>%
  + count(sentiment, term, wt = count) %>%
  + ungroup() %>%
  + filter(n >= 200) %>%
  + mutate(n =ifelse(sentiment == "negative", -n, n)) %>%
  + mutate(term=reorder(term,n)) %>%
  + ggplot(aes(term, n, fill = sentiment)) +
  + geom_bar(stat="identity") +
  + ylab("Contribution to sentiment") +
  + coord_flip()

#sentiment analysis
isi.pdfsentiment <- isi.pdftidy %>%
  inner_join(get_sentiments("bing"), by = c(term ="word"))

#tf-idf analysis, book chapter 3
isi.pdf.tf_idf <- isi.pdftidy %>%
  + bind_tf_idf(term, document, count) %>%
  + arrange(desc(tf_idf))
 isi.pdf.tf_idf

 
#word topic probabilities
library(topicmodels)
isi.pdf.ap_lda <- LDA(isi.pdfdtm, k=3, control=list(seed=1234))
isi.pdf.ap_topics <- tidy(isi.pdf.ap_lda, matrix = "beta")
ilibrary(dplyr)
library(ggplot2)
isi.pdf.aptop_terms <- isi.pdf.ap_topics %>%
  + group_by(topic) %>%
  + top_n(10, beta) %>%
  + ungroup %>%
  + arrange(topic, -beta)
isi.pdf.aptop_terms %>%
  + mutate(term = reorder(term, beta)) %>%
  + ggplot(aes(term, beta, fill=factor(topic))) +
  + geom_col(show.legend = FALSE) +
  + facet_wrap(~ topic, scales = "free") +
  + coord_flip()


#radar chart
library(fmsb)
mbaryear=barplot(isi.summary$AnnualProduction$Articles, border=F,names.arg = isi.summary$AnnualProduction$`Year   `, las = 2, col = rgb(0.2, 0.12, 0.47), ylab = "articles")

#country scientific collaboration
isi.metaTag <- metaTagExtraction(isi.df, Field = "AU_CO", sep = ";")
netmatrix <- biblioNetwork(isi.metaTag, analysis = "collaboration", network = "countries", sep = ";")
networkPlot(netmatrix, n =20, type = "circle", size = TRUE, remove.multiple = FALSE)


#author's coupling
isi.selected <- readFiles("savedrecs_selected.bib")
isi.selectedf <- convert2df(isi.selected, dbsource = "isi", format = "bibtex")
isi.netmatrix <- biblioNetwork(isi.selectedf, analysis = "coupling", network = "authors", sep = ";")
isi.netmatrixnormalized <- normalizeSimilarity(isi.netmatrix, type="salton")
networkPlot(isi.netmatrixnormalized, n=30, Title = "Author's Coupling", type = "kamada", size = FALSE,remove.multiple = TRUE, halo = TRUE, curved = TRUE, edgesize = 0.2)


#salva dados para arquivo
cat("ISI Summary", capture.output(isi.results), file = "res.txt",sep = "n",append = TRUE)
