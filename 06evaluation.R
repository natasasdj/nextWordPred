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



wh<-readRDS("results/whBNTpred.RData")

sentence<-"sss i love to eat bananas eee"

unigramPred <- wh[["_pred"]]

bigramPred<-function(word,start){
        pred<-wh[[word]][["_pred"]]
        if (is.null(pred)) return(unigramPred)
        return(pred)
}

trigramPred<-function(word1,word2){
        pred<-wh[[word1]][[word2]][["_pred"]]
        if (is.null(pred)) pred<-bigramPred(word2)
        return(pred)
}

quadrigramPred<-function(word1,word2,word3){
        pred<-wh[[word1]][[word2]][[word3]][["_pred"]]
        if (is.null(pred)) pred<-trigramPred(word2,word3)
        return(pred)
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
        if (j%%1000==0) cat(j,"\n") 
        j<-j+1
        #cat("sentence:",sentence,"\n")
        words <- unlist(strsplit(sentence, "\\s+"))
        l<-length(words)
        if (l<3) next
        pred<-bigramPred(words[1])
        if (pred==words[2]) {p1<-p1+1} else {n1<-n1+1}
        #cat("word 1:",words[2],"pred:",pred,"\n")
        if (l<4) next
        pred<-trigramPred(words[1],words[2])
        if (pred==words[3]) {p2<-p2+1} else {n2<-n2+1}
        #cat("word 2:",words[3],"pred:",pred,"\n")
        if (l<5) next
        pred<-quadrigramPred(words[1],words[2],words[3])
        #cat("word 3:",words[4],"pred:",pred,"\n")
        if (pred==words[4]) {p3<-p3+1} else {n3<-n3+1}
        if (l<6) next
        for (i in 2:(l-3)){
                pred<-quadrigramPred(words[i],words[i+1],words[i+2])
                if (pred==words[i+3]) {p4<-p4+1} else {n4<-n4+1}
                #cat("word ",i+2,":",words[i+3],"pred:",pred,"\n")
        }        
}

cat("p1",p1,"n1",n1,"p2",p2,"n2",n2,"p3",p3,"n3",n3,"p4",p4,"n4",n4)
cat("r1",p1/(p1+n1),"r2",p2/(p2+n2),"r3",p3/(p3+n3),"r4",p4/(p4+n4))



