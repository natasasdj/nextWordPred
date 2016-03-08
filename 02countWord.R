# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# counts of words


con <- file("data/en_US/test.txt", "r") 
t=readLines(con, -1) 
close(con)
library("openNLP")
require("NLP")

#### split text on sentences
# replace numbers with #no
# remove punctuation
# add start of sentence <#s> and end of sentence <#e>
sent_token_annotator <- Maxent_Sent_Token_Annotator()
#word_token_annotator <- Maxent_Word_Token_Annotator()
ss<-character()
for (text in t){
        s<-as.String(text) 
        t_a <- annotate(s, sent_token_annotator)
        sent<-s[t_a]
        no_expr<-"(^|\\s)[+-]?((\\d+(\\.\\d*)?)|(\\.\\d+))($|\\s)"
        sent<-gsub("no_expr", " #no ", sent)
        sent<-gsub("[[:punct:]]", "", sent)
        sent<-paste("<#s>",sent,"<#e>")
        ss<-c(ss,sent)
}

library("tm")
tC<-VCorpus(VectorSource(ss))
tC
inspect(tC)
lapply(tC, as.character)
tC <- tm_map(tC, stripWhitespace)
tC <- tm_map(tC,  content_transformer(tolower))
dtm <- DocumentTermMatrix(tC)
tdm <- TermDocumentMatrix(tC)
dtm
tdm
inspect(dtm)
inspect(tdm)
findFreqTerms(dtm, 2)
findFreqTerms(tdm, 5)
findAssocs(dtm, "and", 0.5)
freq <- colSums(as.matrix(dtm))
freq

freq <- sort(freq, decreasing=TRUE)[-c(1,2)]
head(freq)
### plot word frequencies
library(ggplot2)  
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>2), aes(word, freq))    
p <- p + geom_bar(stat="identity")  
p <- p + geom_bar()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   
head(wf)
word=names(freq)
freq2<-freq[freq>2]
plot(freq2,xaxt="n")
axis(1, at=1:length(freq2),labels=names(freq2), col.axis="red", las=2)
barplot(freq2,ylim=c(0,max(freq2)),las=2)

# bigrams 
library(RWeka)
#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm_bi <- DocumentTermMatrix(tC, control = list(tokenize = BigramTokenizer))
inspect(dtm_bi[1:2,1:50])
freq <- colSums(as.matrix(dtm_bi))
head(freq)
freq <- sort(freq, decreasing=TRUE)
names(freq2)
head(freq)
freq1<-freq[freq>1]
# filter with start and end word
freq2<-freq1[grep(pattern="(<#s>|<#e>)",x=names(freq2),value=TRUE,invert=TRUE)]
plot(freq2,xaxt="n")
axis(1, at=1:length(freq2),labels=names(freq2), col.axis="red", las=2)
barplot(freq2,ylim=c(0,max(freq2)),las=2)

# 3-grams 
library(RWeka)
#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm_bi <- DocumentTermMatrix(tC, control = list(tokenize = BigramTokenizer))
inspect(dtm_bi[1:2,1:50])
freq <- colSums(as.matrix(dtm_bi))
head(freq)
freq <- sort(freq, decreasing=TRUE)
names(freq2)
head(freq)
freq1<-freq[freq>1]
# filter with start and end word
freq2<-freq1[grep(pattern="(<#s>|<#e>)",x=names(freq2),value=TRUE,invert=TRUE)]
plot(freq2,xaxt="n")
axis(1, at=1:length(freq2),labels=names(freq2), col.axis="red", las=2)
barplot(freq2,ylim=c(0,max(freq2)),las=2)

# 4-grams 
library(RWeka)
#Tokenizer for n-grams and passed on to the term-document matrix constructor
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm_bi <- DocumentTermMatrix(tC, control = list(tokenize = BigramTokenizer))
inspect(dtm_bi[1:2,1:50])
freq <- colSums(as.matrix(dtm_bi))
head(freq)
freq <- sort(freq, decreasing=TRUE)
names(freq2)
head(freq)
freq1<-freq[freq>1]
# filter with start and end word
freq2<-freq1[grep(pattern="(<#s>|<#e>)",x=names(freq2),value=TRUE,invert=TRUE)]
plot(freq2,xaxt="n")
axis(1, at=1:length(freq2),labels=names(freq2), col.axis="red", las=2)
barplot(freq2,ylim=c(0,max(freq2)),las=2)

