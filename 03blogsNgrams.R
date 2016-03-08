# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# count of n-grams in blogs

source("00change_locale.R")
source("03Ngrams_funs.R")



#################### blogs #################

con <- file("data/en_US/en_US.blogs.txt", "r") 
a=readLines(con, -1,encoding="UTF-8") 

length(a)
# 899288
# 40835
#if max<m max<-m
close(con)
counts<-NULL
set.seed(15)
sampleInd<-seq(from=sample(100,size=1),to=length(a),by=100)
length(sampleInd)
counts$lines <- length(sampleInd)
# 8993
blogSample <- a[sampleInd]
head(blogSample)

# sentences of blogs

blogSentences<-sentences(blogSample)
head(blogSentences)
tail(blogSentences)
length(blogSentences)
counts$sentences<-length(blogSentences)
# 22984

# frequency of blog words

blogWords<-words(blogSentences)
counts$words <- length(blogWords)
# 413849
head(blogWords)
blogWordsFreq<-table(blogWords)
blogWordsFreq<-blogWordsFreq[order(blogWordsFreq,decreasing=TRUE)]
dim(blogWordsFreq)
counts$uniqueWords <- dim(blogWordsFreq)
#30518
write.table(blogWordsFreq,"results/blogWordsFreq.csv")

# frequency of blog bigrams

blogBigrams<-bigrams(blogSentences)
counts$bigrams <- length(blogBigrams)
# 390768
blogBigramsFreq<-table(blogBigrams)
blogBigramsFreq<-blogBigramsFreq[order(blogBigramsFreq,decreasing=TRUE)]
counts$uniqueBigrams<-dim(blogBigramsFreq)
# 187144
write.table(blogBigramsFreq,"results/blogBigramsFreq.csv")

# frequency of blog 3-grams

blogTrigrams<-trigrams(blogSentences)
counts$trigrams <- length(blogTrigrams)
# 367784
#blogTrigrams[1:30]
blogTrigramsFreq<-table(blogTrigrams)
blogTrigramsFreq<-blogTrigramsFreq[order(blogTrigramsFreq,decreasing=TRUE)]
counts$uniqueTrigrams<-dim(blogTrigramsFreq)
# 311139
write.table(blogTrigramsFreq,"results/blogTrigramsFreq.csv")

# frequency of blog 4-grams

blogQuadrigrams<-quadrigrams(blogSentences)
counts$quadrigrams <- length(blogQuadrigrams)
# 344836
#blogQuadrigrams[1:30]
blogQuadrigramsFreq<-table(blogQuadrigrams)
blogQuadrigramsFreq<-blogQuadrigramsFreq[order(blogQuadrigramsFreq,decreasing=TRUE)]
counts$uniqueQuadrigrams<-dim(blogQuadrigramsFreq)
# 334088
write.table(blogQuadrigramsFreq,"results/blogQuadrigramsFreq.csv")

write.table(unlist(counts),"results/blogCounts.csv")

################# histograms of words, bigrams, 3-grams and 4-grams #################

freq<-blogWordsFreq[grep(pattern="(sss|eee)",x=names(blogWordsFreq),value=TRUE,invert=TRUE)]
plot(freq,type="l")
plot(freq[1:10],type="l")
plot(freq[10:100],type="l",xlim=c(0,100))
plot(freq[100:length(freq)],type="l")
plot(freq[100:1000],type="l")
plot(freq[1000:length(freq)],type="l")
freq[which(freq<3)][1:50]
freq[100:200]
freq[100]
hist(freq,breaks=20000,freq=FALSE)
hist(freq,breaks=20000,xlim=c(0,20),freq=FALSE)
hist(freq,breaks=10000,xlim=c(2,4),ylim=c(0,4000))
hist(freq,breaks=2000,xlim=c(20,100),ylim=c(0,1000))
hist(freq,breaks=25000,xlim=c(0,20),freq=FALSE)
axis(1, at=1:length(freq2),labels=names(freq2), col.axis="red", las=2)
barplot(freq2,ylim=c(0,max(freq2)),las=2)
names(freq[freq==1])[1:100]
names(freq[1:100])

sent<-gsub("[^'[:^punct:]]", "", sent, perl=T)
# match<-regmatches(vtext, 
#            gregexpr("[[:^punct:]]", vtext))
# unlist(match)
t<-grep(pattern="[^[:alnum:]]",x=blogWords,value=TRUE)
grep(pattern=" eee$ ",x=blogWords,value=TRUE)
t<-t[t!="<#s>"&t!="<#e>"&t!="#no"]
t<-grep(pattern="[^'[:alnum:]]",x=t,value=TRUE)

t1<-grep(pattern="\xE2\x80\x98",x=t,value=TRUE,,useBytes=TRUE)
gsub(quotes,"'",t[2],useBytes=TRUE)


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

grep("a case of ",names(blogQuadrigramsFreq),value=TRUE)
grep("would mean the ",names(blogQuadrigramsFreq),value=TRUE)
grep("make me the ",names(blogQuadrigramsFreq),value=TRUE)
grep("struggling but the ",names(blogQuadrigramsFreq),value=TRUE)
grep("date at the ",names(blogQuadrigramsFreq),value=TRUE)
grep("be on my ",names(blogQuadrigramsFreq),value=TRUE)
grep("in quite some",names(blogQuadrigramsFreq),value=TRUE)
# time
grep("with his little ",names(blogQuadrigramsFreq),value=TRUE)
grep("faith during the ",names(blogQuadrigramsFreq),value=TRUE)
grep("you must be ",names(blogQuadrigramsFreq),value=TRUE)


#sum(grepl("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",a))
