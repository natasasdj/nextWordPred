# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# saving memory - delete unnecessary entries

wh<-readRDS("results/whBNTpred3wu2.rds")
i<-0
for (v1 in ls(wh)){
        if (v1=="_pred") next
        rm(list="_count",envir=wh[[v1]])
        i<-i+1
        if (i%%1000==0) cat(i,"\n")
        for (v2 in ls(wh[[v1]])){
                if (v2=="_pred") next
                rm(list="_count",envir=wh[[v1]][[v2]])
                for (v3 in ls(wh[[v1]][[v2]])){
                        if (v3=="_pred") next
                        rm(list="_count",envir=wh[[v1]][[v2]][[v3]])
                        for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                if (v4=="_pred") next()
                                rm(list=v4,envir=wh[[v1]][[v2]][[v3]])
                        }
                }
        }
}

#saveRDS(wh,"results/whBNTpred3w2.rds")
j<-0
for (v1 in ls(wh)){
        if (v1=="_pred") next
        if (sum(!is.na(wh[[v1]][["_pred"]]))==0) {
                j<-j+1
                cat("v1",v1,"\n")
                rm(list=v1,envir=wh)
                next()
        }
        for (v2 in ls(wh[[v1]])){
                if (v2=="_pred") next
                if (sum(!is.na(wh[[v1]][[v2]][["_pred"]]))==0) {
                        j<-j+1
                        cat("v1v2",v1,v2,"\n")
                        rm(list=v2,envir=wh[[v1]])
                        next
                } 
                for (v3 in ls(wh[[v1]][[v2]])){
                        if (v3=="_pred") next
                        if (sum(!is.na(wh[[v1]][[v2]][[v3]][["_pred"]]))==0) {
                                j<-j+1
                                cat("v1v2v3",v1,v2,v3,"\n")
                                rm(list=v3,envir=wh[[v1]][[v2]])
                                next
                        }
                }
                
        }        
}
  
saveRDS(wh,"results/whBNTpred3wum.rds")




