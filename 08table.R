library(data.table)
wh<-readRDS("results/whBNTpred3wum.rds")
unigram<-wh[["_pred"]]
saveRDS(unigram,"results/unigram2.rds")



# df<-data.frame(prevWords=character(),nextWord1=character(),nextWord2=character(),nextWord3=character(),stringsAsFactors=FALSE)
# i<-1
# for (v1 in ls(wh)){
#         if (v1=="_pred") next
#         df[i,]<-c(v1,wh[[v1]][["_pred"]])
#         i<-i+1
#         
# }

k<-length(ls(wh))-1
df<-data.frame(prevWords=rep(NA,k),nextWord1=rep(NA,k),nextWord2=rep(NA,k),nextWord3=rep(NA,k),stringsAsFactors=FALSE)
i<-1
for (v1 in ls(wh)){
        if (v1=="_pred") next
        df[i,]<-c(v1,wh[[v1]][["_pred"]])
        i<-i+1
        
}
object.size(df)
bigram<-as.data.table(df)
setkey(bigram,prevWords)
bigram["sun"]
saveRDS(bigram,"results/bigram2.rds")



k<-0
for (v1 in ls(wh)){
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
                if (v2=="_pred") next
                k<-k+1
        }
}
# k 357734
df<-data.frame(prevWords=rep(NA,k),nextWord1=rep(NA,k),nextWord2=rep(NA,k),nextWord3=rep(NA,k),stringsAsFactors=FALSE)        
i<-1        
for (v1 in ls(wh)){
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
                if (v2=="_pred") next
                if (i%%1000==0) cat(i," ")
                df[i,]<-c(paste(v1,v2),wh[[v1]][[v2]][["_pred"]])
                i<-i+1
        }
}  
object.size(df)
trigram<-as.data.table(df)
setkey(trigram,prevWords)
trigram["i love"]
saveRDS(trigram,"results/trigram2.rds")


k<-0
for (v1 in ls(wh)){
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
                if (v2=="_pred") next
                for (v3 in ls(wh[[v1]][[v2]])){
                        if (v3=="_pred") next
                        k<-k+1
                        
                }
                
        }        
}
# 651410
df<-data.frame(prevWords=rep(NA,k),nextWord1=rep(NA,k),nextWord2=rep(NA,k),nextWord3=rep(NA,k),stringsAsFactors=FALSE)        
i<-1
for (v1 in ls(wh)){
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
                if (v2=="_pred") next
                for (v3 in ls(wh[[v1]][[v2]])){
                        if (v3=="_pred") next
                        if (i%%1000==0) cat(i," ")
                        df[i,]<-c(paste(v1,v2,v3),wh[[v1]][[v2]][[v3]][["_pred"]])
                        i<-i+1
                        
                }
                
        }        
}
object.size(df)
quadrigram<-as.data.table(df)
setkey(quadrigram,prevWords)
quadrigram["i love to"]
saveRDS(quadrigram,"results/quadrigram2.rds")