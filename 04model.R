# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# input: words - character vector of words
# output:
source("00change_locale.R")
source("03Ngrams_funs.R")


sentence1<-"sss I am N and I am a girl eee"
sentence2<-"sss who said I am a girl eee"
sentence3<- "sss I live in A eee"
sentence4<-"sss I am a boy eee"
sentences<-c(sentence1,sentence2,sentence3,sentence4)

grep(pattern="^sss\\d+\\s.*\\d+",x=blogSentences,value=TRUE)
############### collecting ########################

#sentences<-blogSentences
collectSampleText<-function(textFile){
        con <- file(textFile, "rb") 
        a=readLines(con, -1,encoding="UTF-8") 
        close(con)
        set.seed(15)
        sampleInd<-seq(from=sample(100,size=1),to=length(a),by=100)
        sampleText <- a[sampleInd]
        sentences(sampleText) 
}



  
wh<-new.env()
textFile<-"data/en_US/en_US.blogs.txt"
textSentences<-collectSampleText(textFile)
wh<-textNgrams(textSentences,wh)
textFile<-"data/en_US/en_US.news.txt"
textSentences<-collectSampleText(textFile)
wh<-textNgrams(textSentences,wh)
textFile<-"data/en_US/en_US.twitter.txt"
textSentences<-collectSampleText(textFile)
wh<-textNgrams(textSentences,wh)

saveRDS(wh,file="results/whBNT.RData")

printPredT<-function(wh,verbose=FALSE){
        wh[["_pred"]]
        for (v1 in ls(wh)){
                if (verbose) cat("v1",v1,"\n")
                if (v1=="_count"|v1=="_pred") next
                cat(v1, wh[[v1]][["_count"]],wh[[v1]][["_pred"]],"\n")
                for (v2 in ls(wh[[v1]])){
                        if (verbose) cat("v2",v2,"\n")
                        if (v2=="_count"|v2=="_pred") next
                        cat(v1,v2,wh[[v1]][[v2]][["_count"]],wh[[v1]][[v2]][["_pred"]],"\n")
                        for (v3 in ls(wh[[v1]][[v2]])){
                                if (verbose) cat("v3",v3,"\n")
                                if (v3=="_count"|v3=="_pred") next
                                cat(v1,v2,v3,wh[[v1]][[v2]][[v3]][["_count"]],wh[[v1]][[v2]][[v3]][["_pred"]],"\n")
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        if (verbose) cat("v4",v4,"\n")
                                        if (v4=="_count"|v4=="_pred") next
                                        cat(v1,v2,v3,v4,wh[[v1]][[v2]][[v3]][[v4]][["_count"]],wh[[v1]][[v2]][[v3]][[v4]][["_pred"]],"\n")
                                        
                                }
                        }
                }
                
                
        }
        
}



############ printing words, unigrams, bigrams, trigrams



printNgramsT<-function(wh,verbose=FALSE){
        for (v1 in ls(wh)){
                if (verbose) cat("v1",v1,"\n")
                if (v1=="_count"|v1=="_pred") next
                cat(v1, wh[[v1]][["_count"]],"\n")
                for (v2 in ls(wh[[v1]])){
                        if (verbose) cat("v2",v2,"\n")
                        if (v2=="_count"|v2=="_pred") next
                        cat("\t\t",v2,wh[[v1]][[v2]][["_count"]],"\n")
                        for (v3 in ls(wh[[v1]][[v2]])){
                                if (verbose) cat("v3",v3,"\n")
                                if (v3=="_count"|v3=="_pred") next
                                cat("\t\t\t\t",v3,wh[[v1]][[v2]][[v3]][["_count"]],"\n")
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        if (verbose) cat("v4",v4,"\n")
                                        if (v4=="_count"|v4=="_pred") next
                                        cat("\t\t\t\t\t\t",v4,wh[[v1]][[v2]][[v3]][[v4]][["_count"]],"\n")
                                        
                                }
                        }
                }
                
                
        }
        
}

# printing functions

printNgrams<-function(wh){
        for (v1 in ls(wh)){
                if (v1=="_pred") next
                cat(v1, wh[[v1]][["_count"]],"\n")
                for (v2 in ls(wh[[v1]])){
                        if (v2=="_count"|v2=="_pred") next
                        cat(v1,v2,wh[[v1]][[v2]][["_count"]],"\n")
                        for (v3 in ls(wh[[v1]][[v2]])){
                                if (v3=="_count"|v3=="_pred") next
                                cat(v1,v2,v3,wh[[v1]][[v2]][[v3]][["_count"]],"\n")
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        if (v4=="_count"|v4=="_pred") next
                                        cat(v1,v2,v3,v4,wh[[v1]][[v2]][[v3]][[v4]][["_count"]],"\n")
                                        
                                }
                        }
                }
                
                
        }
}

printPred<-function(wh,verbose=FALSE){
        wh[["_pred"]]
        for (v1 in ls(wh)){
                if (verbose) cat("v1",v1,"\n")
                if (v1=="_count"|v1=="_pred") next
                cat(v1, wh[[v1]][["_count"]],wh[[v1]][["_pred"]],"\n")
                for (v2 in ls(wh[[v1]])){
                        if (verbose) cat("v2",v2,"\n")
                        if (v2=="_count"|v2=="_pred") next
                        cat(v1,v2,wh[[v1]][[v2]][["_count"]],wh[[v1]][[v2]][["_pred"]],"\n")
                        for (v3 in ls(wh[[v1]][[v2]])){
                                if (verbose) cat("v3",v3,"\n")
                                if (v3=="_count"|v3=="_pred") next
                                cat(v1,v2,v3,wh[[v1]][[v2]][[v3]][["_count"]],wh[[v1]][[v2]][[v3]][["_pred"]],"\n")
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        if (verbose) cat("v4",v4,"\n")
                                        if (v4=="_count"|v4=="_pred") next
                                        cat(v1,v2,v3,v4,wh[[v1]][[v2]][[v3]][[v4]][["_count"]],wh[[v1]][[v2]][[v3]][[v4]][["_pred"]],"\n")
                                        
                                }
                        }
                }
                
                
        }
        
}






###### testing printing

printClass<-function(wh,verbose=FALSE){
        for (v1 in ls(wh)){
                if (v1=="_count"|v1=="_pred") next
                if (class(wh[[v1]])!="environment") cat("v1",v1,"\n")                
        for (v2 in ls(wh[[v1]])){
                if (v2=="_count"|v2=="_pred") next
                if (class(wh[[v1]][[v2]])!="environment") cat("v1",v1,"v2",v2,"\n")
        for (v3 in ls(wh[[v1]][[v2]])){
                if (v3=="_count"|v3=="_pred") next
                if (class(wh[[v1]][[v2]][[v3]])!="environment") cat("v1",v1,"v2",v2,"v3",v3,"\n")
        for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                if (v4=="_count"|v4=="_pred") next
                if (class(wh[[v1]][[v2]][[v3]][[v4]])!="environment") cat("v1",v1,"v2",v2,"v3",v3,"v4",v4,"\n")                        
        }
        }
        }                
        }
        
}





######################### printing predictions ################
w<-ls(wh)[1:10]

printPredNgrams<-function(wh){
        cat("unigrams:","\n")
        for (v1 in ls(wh)){ 
                if (v1=="_pred") next
                cat(v1, wh[[v1]][["_count"]],"  ")        
        }
        cat("\nprediction: ",wh[["_pred"]],"\n")
        
        cat("bigrams:","\n")
        for (v1 in ls(wh)){
                if (v1=="_pred") next
                nl<-FALSE
                for (v2 in ls(wh[[v1]])){
                        if (v2=="_count"|v2=="_pred") next
                        nl<-TRUE
                        cat(v1,v2,wh[[v1]][[v2]][["_count"]],"  ")
                }
                if (nl) cat("\nprediction: ",wh[[v1]][["_pred"]],"\n")
                
                
        }
        
        cat("trigrams:","\n")
        for (v1 in ls(wh)){
                if (v1=="_pred") next
                for (v2 in ls(wh[[v1]])){
                        if (v2=="_count"|v2=="_pred") next
                        nl<-FALSE
                        for (v3 in ls(wh[[v1]][[v2]])){
                                if (v3=="_count"|v3=="_pred") next
                                nl<-TRUE
                                cat(v1,v2,v3,wh[[v1]][[v2]][[v3]][["_count"]]," ")
                                
                        }
                        if (nl) cat("\nprediction: ",wh[[v1]][[v2]][["_pred"]],"\n")
                }
                
                
        }
        
        
        cat("quadrigrams:","\n")
        for (v1 in ls(wh)){
                if (v1=="_pred") next
                for (v2 in ls(wh[[v1]])){
                        if (v2=="_count"|v2=="_pred") next
                        for (v3 in ls(wh[[v1]][[v2]])){
                                if (v3=="_count"|v3=="_pred") next
                                nl<-FALSE
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        if (v4=="_count"|v4=="_pred") next
                                        nl<-TRUE
                                        cat(v1,v2,v3,v4,wh[[v1]][[v2]][[v3]][[v4]][["_count"]],"  ")
                                        
                                }
                                if (nl) cat("\nprediction: ",wh[[v1]][[v2]][[v3]][["_pred"]],"\n")
                        }
                }
                
                
        }
        
}


########### printing part of trees #######

cat("unigrams:","\n")
for (v1 in ls(wh)[1:100]){ 
        if (v1=="_pred") next
        cat(v1, wh[[v1]][["_count"]],"  ")        
}
cat("\nprediction: ",wh[["_pred"]],"\n")

words<-ls(wh)
nos<-grep(pattern="\\d+",x=words,value=TRUE

cat("words:",ls(wh),"\n")
cat("words counts:")
for (v1 in ls(wh)){
        if (v1=="_pred") next
        cat(v1,wh[[v1]][["_count"]],"  ")
}
cat("\n")
cat("bigrams:")
for (v1 in ls(wh)){ 
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
               if (v2=="_count"|v2=="_pred") next
        cat(v1,v2,wh[[v1]][[v2]][["_count"]],"  ")               
               
        }
        
}
cat("\n")















# unigram prediction
m<-0; pred<-NULL
for (v1 in ls(wh)){
        if (v1=="_pred") next
        c<-wh[[v1]][["_count"]]
        if (c>m){
                pred<-v1
                m<-c
        }
        cat(v1,c,"  ")
}
wh[["_pred"]]<-pred
cat("pred:",wh[["_pred"]])
#rm("_pred",envir=wh)




# bigram prediction
for (v1 in ls(wh)){
        if (v1 == "_pred") next
        m<-0; pred<-NULL
        for (v2 in ls(wh[[v1]])){
                if (v2=="_count" | v2 == "_pred") next
                c<-wh[[v1]][[v2]][["_count"]]
                if (c>m){
                        pred<-v2
                        m<-c
                }
                cat(v1,v2,c,"  ")               
                
        }
        wh[[v1]][["_pred"]]<-pred
        
}


#########printing
for (v1 in ls(wh)){
        if (v1 == "_pred") next
        cat(v1,wh[[v1]][["_pred"]],"  ")
}

for (v1 in ls(wh)){
        if (v1 == "_pred") next
        cat(v1,wh[[v1]][["_count"]],"  ")
        m<-0; pred<-NULL
        cat("\n")
        for (v2 in ls(wh[[v1]])){
                if (v2=="_count" | v2 == "_pred") next
                cat(v1,v2,wh[[v1]][[v2]][["_count"]],"  ")               
                
        }
        cat("\n")
        cat("pred:", v1, wh[[v1]][["_pred"]])
        cat("\n")        
}
cat("pred:", wh[["_pred"]])




################## printing counts ##################




for (v1 in ls(wh)){        
        cat(v1, wh[[v1]][["_count"]],"  ")        
}

for (v1 in ls(wh)){
        if (v1=="_pred") next
        nl<-FALSE
        for (v2 in ls(wh[[v1]])){
                if (v2=="_count"|v2=="_pred") next
                nl<-TRUE
                cat(v1,v2,wh[[v1]][[v2]][["_count"]],"  ")
        }
        if (nl) cat("\n")
        
        
}

for (v1 in ls(wh)){
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
                if (v2=="_count"|v2=="_pred") next
                nl<-FALSE
                for (v3 in ls(wh[[v1]][[v2]])){
                        if (v3=="_count"|v3=="_pred") next
                        nl<-TRUE
                        cat(v1,v2,v3,wh[[v1]][[v2]][[v3]][["_count"]]," ")
                
                }
                if (nl) cat("\n")
        }
        
        
}

for (v1 in ls(wh)){
        if (v1=="_pred") next
        for (v2 in ls(wh[[v1]])){
                if (v2=="_count"|v2=="_pred") next
                for (v3 in ls(wh[[v1]][[v2]])){
                        if (v3=="_count"|v3=="_pred") next
                        nl<-FALSE
                        for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                if (v4=="_count"|v4=="_pred") next
                                nl<-TRUE
                                cat(v1,v2,v3,v4,wh[[v1]][[v2]][[v3]][[v4]][["_count"]],"  ")
                                
                        }
                        if (nl) cat("\n")
                }
        }
        
        
}




