# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# count of n-grams in news

source("00change_locale.R")
source("03Ngrams_funs.R")



#################### news #################

con <- file("data/en_US/en_US.news.txt", "rb") 
a=readLines(con, -1,encoding="UTF-8") 

length(a)
# 1010242
# 40835
#if max<m max<-m
close(con)

counts<-NULL
set.seed(15)
sampleInd<-seq(from=sample(100,size=1),to=length(a),by=100)
length(sampleInd)
counts$sampleLines <- length(sampleInd)
# 10102
newsSample <- a[sampleInd]
head(newsSample)

newsSentences<-sentences(newsSample)
head(newsSentences)
tail(newsSentences)
counts$sentences<-length(newsSentences)
# 19253

# frequency of news words

newsWords<-words(newsSentences)
counts$words <- length(newsWords)
# 413849
head(newsWords)
newsWordsFreq<-table(newsWords)
newsWordsFreq<-newsWordsFreq[order(newsWordsFreq,decreasing=TRUE)]
dim(newsWordsFreq)
counts$uniqueWords <- dim(newsWordsFreq)
#30518
write.table(newsWordsFreq,"results/newsWordsFreq.csv")

# frequency of news bigrams

newsBigrams<-bigrams(newsSentences)
counts$bigrams <- length(newsBigrams)
# 390768
newsBigramsFreq<-table(newsBigrams)
newsBigramsFreq<-newsBigramsFreq[order(newsBigramsFreq,decreasing=TRUE)]
counts$uniqueBigrams<-dim(newsBigramsFreq)
# 187144
write.table(newsBigramsFreq,"results/newsBigramsFreq.csv")

# frequency of news 3-grams

newsTrigrams<-trigrams(newsSentences)
counts$trigrams <- length(newsTrigrams)
# 367784
#newsTrigrams[1:30]
newsTrigramsFreq<-table(newsTrigrams)
newsTrigramsFreq<-newsTrigramsFreq[order(newsTrigramsFreq,decreasing=TRUE)]
counts$uniqueTrigrams<-dim(newsTrigramsFreq)
# 311139
write.table(newsTrigramsFreq,"results/newsTrigramsFreq.csv")

# frequency of news 4-grams

newsQuadrigrams<-quadrigrams(newsSentences)
counts$quadrigrams <- length(newsQuadrigrams)
# 344836
#newsQuadrigrams[1:30]
newsQuadrigramsFreq<-table(newsQuadrigrams)
newsQuadrigramsFreq<-newsQuadrigramsFreq[order(newsQuadrigramsFreq,decreasing=TRUE)]
counts$uniqueQuadrigrams<-dim(newsQuadrigramsFreq)
# 334088
write.table(newsQuadrigramsFreq,"results/newsQuadrigramsFreq.csv")

write.table(unlist(counts),"results/newsCounts.csv")


############# most frequent words, bigrams, 3-grams, 4-gram ##############

################ how many words cover 50%, 90% of text

# Next, we calculate how much percentage of text we cover with $n$ most frequent words.
# 
# ```{r,echo=FALSE}
# l<-length(freq)
# s <- 0
# sv<-numeric()
# for (f in freq){
#         s<-s+f
#         sv<-c(sv,s)     
# }
# p<-sv*100/sum(freq)
# 
# plot(p,pch=".",xlab="number of the most frequent words",ylab="percentage",main="Text coverage")
# #n<-which(p>99)[1]
# #freq[n]
# 
# ```
############### combining tables #############################
#######tapply(T, names(T), sum)



### quiz:

grep("a case of ",names(newsQuadrigramsFreq),value=TRUE)
grep("would mean the ",names(newsQuadrigramsFreq),value=TRUE)
grep("make me the ",names(newsQuadrigramsFreq),value=TRUE)
grep("struggling but the ",names(newsQuadrigramsFreq),value=TRUE)
grep("date at the ",names(newsQuadrigramsFreq),value=TRUE)
grep("be on my ",names(newsQuadrigramsFreq),value=TRUE)
grep("in quite some",names(newsQuadrigramsFreq),value=TRUE)
# time
grep("with his little ",names(newsQuadrigramsFreq),value=TRUE)
grep("faith during the ",names(newsQuadrigramsFreq),value=TRUE)
grep("you must be ",names(newsQuadrigramsFreq),value=TRUE)



