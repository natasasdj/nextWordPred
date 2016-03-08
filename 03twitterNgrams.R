# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# count of n-grams in twitter

source("00change_locale.R")
source("03Ngrams_funs.R")



#################### twitter #################

con <- file("data/en_US/en_US.twitter.txt", "rb") 
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
twitterSample <- a[sampleInd]
head(twitterSample)

# sentences of twitter

twitterSentences<-sentences(twitterSample)
head(twitterSentences)
tail(twitterSentences)
counts$sentences<-length(twitterSentences)
# 19253

# frequency of twitter words

twitterWords<-words(twitterSentences)
counts$words <- length(twitterWords)
# 413849
head(twitterWords)
twitterWordsFreq<-table(twitterWords)
twitterWordsFreq<-twitterWordsFreq[order(twitterWordsFreq,decreasing=TRUE)]
dim(twitterWordsFreq)
counts$uniqueWords <- dim(twitterWordsFreq)
#30518
write.table(twitterWordsFreq,"results/twitterWordsFreq.csv")

# frequency of twitter bigrams

twitterBigrams<-bigrams(twitterSentences)
counts$bigrams <- length(twitterBigrams)
# 390768
twitterBigramsFreq<-table(twitterBigrams)
twitterBigramsFreq<-twitterBigramsFreq[order(twitterBigramsFreq,decreasing=TRUE)]
counts$uniqueBigrams<-dim(twitterBigramsFreq)
# 187144
write.table(twitterBigramsFreq,"results/twitterBigramsFreq.csv")

# frequency of twitter 3-grams

twitterTrigrams<-trigrams(twitterSentences)
counts$trigrams <- length(twitterTrigrams)
# 367784
#twitterTrigrams[1:30]
twitterTrigramsFreq<-table(twitterTrigrams)
twitterTrigramsFreq<-twitterTrigramsFreq[order(twitterTrigramsFreq,decreasing=TRUE)]
counts$uniqueTrigrams<-dim(twitterTrigramsFreq)
# 311139
write.table(twitterTrigramsFreq,"results/twitterTrigramsFreq.csv")

# frequency of twitter 4-grams

twitterQuadrigrams<-quadrigrams(twitterSentences)
counts$quadrigrams <- length(twitterQuadrigrams)
# 344836
#twitterQuadrigrams[1:30]
twitterQuadrigramsFreq<-table(twitterQuadrigrams)
twitterQuadrigramsFreq<-twitterQuadrigramsFreq[order(twitterQuadrigramsFreq,decreasing=TRUE)]
counts$uniqueQuadrigrams<-dim(twitterQuadrigramsFreq)
# 334088
write.table(twitterQuadrigramsFreq,"results/twitterQuadrigramsFreq.csv")

write.table(unlist(counts),"results/twitterCounts.csv")


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

grep("a case of ",names(twitterQuadrigramsFreq),value=TRUE)
grep("would mean the ",names(twitterQuadrigramsFreq),value=TRUE)
grep("make me the ",names(twitterQuadrigramsFreq),value=TRUE)
grep("struggling but the ",names(twitterQuadrigramsFreq),value=TRUE)
grep("date at the ",names(twitterQuadrigramsFreq),value=TRUE)
grep("be on my ",names(twitterQuadrigramsFreq),value=TRUE)
grep("in quite some",names(twitterQuadrigramsFreq),value=TRUE)
# time
grep("with his little ",names(twitterQuadrigramsFreq),value=TRUE)
grep("faith during the ",names(twitterQuadrigramsFreq),value=TRUE)
grep("you must be ",names(twitterQuadrigramsFreq),value=TRUE)


