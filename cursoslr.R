#Esse arquivo foi criado para o curso de Revisão Sistemática
# apresentado no LCCV, no ano de 2018 por Randy Quindai
# os slides do curso podem ser encontrados no seguinte endereço
# https://sites.google.com/site/sensornetufal/how-to-s/reviso-sistemtica
# LaCCAN - Laboratório de Computação Científica e Análise Numérica
# Grupo de pesquisa : SensorNet-UFAL


library(bibliometrix)
#lê arquivo .bib
#converte .bib num DataFrame pronto a ser processado pelo R
isi.rawdata <- readFiles("savedrecs.bib")
isi.dfrawdata <- convert2df(isi.rawdata, dbsource = "isi", format = "bibtex")

isi.rawcs <- conceptualStructure(isi.dfrawdata, field = "ID", minDegree = 4, k.max = 5, stemming = FALSE, labelsize = 10)
isi.rawcs <- conceptualStructure(isi.dfrawdata, field = "DE_TM", minDegree = 14, k.max = 5, stemming = FALSE, labelsize = 10)

#para visualizar campos da variavel criada, use $
isi.rawcs$km.res
#---------------------------

library(bibliometrix)
results <- biblioAnalysis(isi.dfrawdata, sep=";")
plot(x = results, k = 10, pause = FALSE)
results


#*************************-Processar pdfs
library(tm)
#lê arquivos .pdf localizados no diretório pasta/
#rodar esse comando no diretório pai de pasta/
files <- list.files("pasta/",pattern = "pdf$")

#definindo uma funcao
rpdf <- readPDF(control = list(text = "-layout"))

#muda diretorio para o diretorio dos arquivos .pdf
setwd("pasta/")

#usa a funcao rpdf
#as variaveis isi.pdf e isi.pdfdtm podem ser exploradas usando $
#verifique e tente extrair algum conteudo
isi.pdf <- Corpus(URISource(files), readerControl = list(reader = rpdf))
isi.pdfdtm <- DocumentTermMatrix(isi.pdf, control=list(removePunctuation = TRUE, stopwords = TRUE, tolower = TRUE, stemming = FALSE, removeNumbers = TRUE))



#trabalhando o Corpus criado para extrair alguma informacao
library(dplyr)
library(ggplot2)
library(tidytext)
isi.pdftidy <- tidy(isi.pdfdtm)

#Analise de sentimento, busca efetuada no texto dos artigos
#explore um pouco essa variavel usando $
isi.pdfsentiment <- isi.pdftidy %>%
  inner_join(get_sentiments("bing"), by = c(term ="word"))

isi.pdfsentiment %>%
count(sentiment, term, wt = count) %>%
ungroup() %>%
filter(n >= 200) %>%
mutate(n =ifelse(sentiment == "negative", -n, n))%>%
mutate(term=reorder(term,n)) %>%
ggplot(aes(term, n, fill = sentiment)) +
geom_bar(stat="identity") +
ylab("Analisando o sentimento da amostra") +
coord_flip()


#para mais detalhes sobre cada comando ou biblioteca no r use help("comando")
help("tidytext")
