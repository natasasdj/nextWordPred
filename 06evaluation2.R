# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# evaluation of next word prediction

# text 
# make sentences
# for each sentence:
# make words
# for each word make a prediction
# count how many correct and incorrect predictions
# count: 
        # first word
        # second word
        # third word
        # fourth and further words

library(data.table)
unigram<-readRDS("results/unigram2.rds")
bigram<-readRDS("results/bigram2.rds")
trigram<-readRDS("results/trigram2.rds")
quadrigram<-readRDS("results/quadrigram2.rds")

f1<-function(x,y){!(is.na(x) | x %in% y)}
f2<-function(x,y){!(x %in% y)}

unigramPred <- function(no=3,prevPred=""){
        if (no<1) return(NULL)
        pred<-unigram
        pred<-pred[sapply(pred,f2,y=prevPred)]
        l<-length(pred)
        if (no>3) no<-3
        l<-length(pred)
        if (l>=no) return(pred[1:no])
        pred
}

bigramPred<-function(word,no=3,prevPred=""){
        if (no<1) return(NULL)
        pred<-as.character(bigram[word,2:4,with=F])
        pred<-pred[sapply(pred,f1,y=prevPred)]
        if (no>3) no<-3
        l<-length(pred)
        if (l>=no) return(pred[1:no])
        c(pred,unigramPred(no-l,c(prevPred,pred)))
}

trigramPred<-function(word1,word2,no=3,prevPred=""){
        if (no<1) return(NULL)
        pred<-as.character(trigram[paste(word1,word2),2:4,with=F])
        pred<-pred[sapply(pred,f1,y=prevPred)]
        if (no>3) no<-3
        l<-length(pred)
        if (l>=no) return(pred[1:no])
        c(pred,bigramPred(word2,no-l,c(prevPred,pred)))
        
}

quadrigramPred<-function(word1,word2,word3,no=3){
        pred<-as.character(quadrigram[paste(word1,word2,word3),2:4,with=F])
        pred<-pred[!is.na(pred)]
        if (no>3) no<-3
        if (no<1) return(NULL)
        l<-length(pred)
        if (l>=no) return(pred[1:no])
        c(pred,trigramPred(word2,word3,no-l,pred))
        
}



unkWord<-function(w){
        if (is.na(bigram[w,2,with=FALSE])) "#unk" else w
        
}

wordPred<-function(words,no=3){
        words<-sapply(words,unkWord,USE.NAMES = F)
        l<-length(words)
        if (l==1) p<-bigramPred(words,no)
        if (l==2) p<-trigramPred(words[1],words[2],no) 
        if (l>=3) p<-quadrigramPred(words[l-2],words[l-1],words[l],no)
        if (is.null(p)) "null" else { p[p=="eee"]<-".";p }
}



source("00change_locale.R")
source("03Ngrams_funs.R")
collectSampleText2<-function(textFile,start=1,by=100){
        con <- file(textFile, "rb") 
        a=readLines(con, -1,encoding="UTF-8") 
        close(con)
        set.seed(15)
        sampleInd<-seq(from=start,to=length(a),by=by)
        sampleText <- a[sampleInd]
        sentences(sampleText) 
}

textFile<-"data/en_US/en_US.blogs.txt"
text1<-collectSampleText2(textFile)
textFile<-"data/en_US/en_US.news.txt"
text2<-collectSampleText2(textFile)
textFile<-"data/en_US/en_US.twitter.txt"
text3<-collectSampleText2(textFile)
text<-c(text1,text2,text3)
length(text)

p1<-0; n1<-0; p2<-0; n2<-0; p3<-0; n3<-0; p4<-0; n4<-0;
j<-1
for (sentence in text){
        if (j%%10==0) cat(j," ") 
        j<-j+1
        #cat("sentence:",sentence,"\n")
        words <- unlist(strsplit(sentence, "\\s+"))
        l<-length(words)
        if (l<3) next
        pred<-wordPred(words[1])
        if (words[2] %in% pred) {p1<-p1+1} else {n1<-n1+1}
        #cat("word 1:",words[2],"pred:",pred,"\n")
        if (l<4) next
        pred<-wordPred(c(words[1],words[2]))
        if (words[3] %in% pred) {p2<-p2+1} else {n2<-n2+1}
        #cat("word 2:",words[3],"pred:",pred,"\n")
        if (l<5) next
        pred<-wordPred(c(words[1],words[2],words[3]))
        #cat("word 3:",words[4],"pred:",pred,"\n")
        if (words[4] %in% pred) {p3<-p3+1} else {n3<-n3+1}
        if (l<6) next
        for (i in 2:(l-3)){
                pred<-wordPred(c(words[i],words[i+1],words[i+2]))
                if (words[i+3] %in% pred) {p4<-p4+1} else {n4<-n4+1}
                #cat("word ",i+2,":",words[i+3],"pred:",pred,"\n")
        }        
}

cat("p1",p1,"n1",n1,"p2",p2,"n2",n2,"p3",p3,"n3",n3,"p4",p4,"n4",n4)
cat("r1",p1/(p1+n1),"r2",p2/(p2+n2),"r3",p3/(p3+n3),"r4",p4/(p4+n4))
cat((p1+p2+p3+p4)/(p1+p2+p3+p4+n1+n2+n3+n4))


