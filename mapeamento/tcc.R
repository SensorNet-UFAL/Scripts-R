library(tictoc)
tic("Início") 
library(bibliometrix)
library(tm)
D <- readFiles("savedrecs.bib")
M <- convert2df(D, dbsource = "isi", format = "bibtex")

#https://tm4ss.github.io/docs/Tutorial_2_Read_data_Zipf.html

df <- data.frame(c(M$TI,M$AB))
df <- as.data.frame(df)
#df <- data.frame(matrix(unlist(df)), stringsAsFactors = FALSE)

corpus<- Corpus(VectorSource(df$c.M.TI..M.AB.))
inspect(corpus)                 

#### achando numero de topicos
library(ldatuning)
library(topicmodels)
corpus.dtm <- DocumentTermMatrix(corpus, control=list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE))

rowTotals <- apply(corpus.dtm, 1, sum)
corpus.dtm <- corpus.dtm[rowTotals>0, ]

tunningresult <- FindTopicsNumber(
  corpus.dtm,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 3L,
  verbose = TRUE)

FindTopicsNumber_plot(tunningresult)

### visualizar os tópicos
library(tidytext)
library(magrittr)
library(dplyr)
library(ggplot2)
corpus.lda <- LDA(corpus.dtm, k =15, control = list(seed=77), method = "Gibbs")
corpus.topics <- tidy(corpus.lda, matrix = "beta")
corpus.top_terms <- corpus.topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup %>%
  arrange(topic, -beta)

corpus.top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free")+
  coord_flip()

###### pegando os artigos relativos aos tópicos
corpus.gamma <- tidy(corpus.lda, matrix = "gamma")
corpus.gamma %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma))+
  geom_boxplot()+
  facet_wrap(~ title)

M$DI[as.numeric(head(corpus.gamma$document))]

####
freqs <- col_sums(corpus.dtm)
words <- colnames(corpus.dtm)
wordlist <- data.frame(words, freqs)
wordIndexes <- order(wordlist[, "freqs"], decreasing = TRUE)
wordlist <- wordlist[wordIndexes, ]
head(wordlist, 10)

plot(wordlist$freqs , type = "l", log="xy", lwd=2, main = "Classificação da Frequência de Palavras", xlab="log-Rank", ylab ="log-Frequency")

plot(wordlist$freqs, type = "l", log="xy",lwd=2, main = "Classificação da Frequência de Palavras", xlab="log-Rank", ylab = "Frequency")
englishStopwords <- stopwords("en")
stopwords_idx <- which(wordlist$words %in% englishStopwords)
low_frequent_idx <- which(wordlist$freqs < 10)
insignificant_idx <- union(stopwords_idx, low_frequent_idx)
meaningful_range_idx <- setdiff(1:nrow(wordlist), insignificant_idx)
lines(meaningful_range_idx, wordlist$freqs[meaningful_range_idx], col = "green", lwd=2, type="p", pch=20)

M$DI[as.numeric(head(corpus.gamma$document[corpus.gamma$topic=="11"]))]

toc()
