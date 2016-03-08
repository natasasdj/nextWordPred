# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# check unigram statistics and decide to prune, or to put unk word

wh<-readRDS("results/whBNT.RData")
w<-ls(wh)
wl<-length(w)
# 58871
wordsCount<-rep(0,wl)
for (i in 1:wl){
        if (i%%1000==0) cat(i,"\n")
        v<-w[i]
        if (v=="_pred") next
        wordsCount[i]<-wh[[v]][["_count"]]
}
names(wordsCount)<-w
wh[["sss"]][["_count"]]
sum(wordsCount==1)
ind<-wordsCount==1
wordsCount[ind][30000:30100]
ind<-grep("'",names(wordsCount))
length(ind)
# 2925
head(wca)
head(wordsCount[ind])
wca<-sort(wordsCount[ind],decreasing=TRUE)
wca[1:20]
wc<-sort(wordsCount,decreasing=TRUE)
wc[1:20]


############ prediction ##############

predictNgrams<-function(){
        m1<-0;p1<-NULL
        i<-1
        for (v1 in ls(wh)){
                cat(i,"\n")
                #cat("v1",v1,"\n")
                i<-i+1
                if (v1=="_pred") next
                if (v1!="sss" & v1!="eee"){
                        c<-wh[[v1]][["_count"]]
                        if (c>m1 ){
                                p1<-v1
                                m1<-c
                        }     
                }                
                m2<-0;p2<-NULL
                for (v2 in ls(wh[[v1]])){
                        #cat("    v2",v2,"\n")
                        if (v2=="_count"|v2=="_pred") next
                        c<-wh[[v1]][[v2]][["_count"]]
                        if (c>m2){
                                p2<-v2
                                m2<-c
                        }
                        m3<-0;p3<-NULL
                        for (v3 in ls(wh[[v1]][[v2]])){
                                #cat("        v3",v3,"\n")
                                if (v3=="_count"|v3=="_pred") next
                                c<-wh[[v1]][[v2]][[v3]][["_count"]]
                                if (c>m3){
                                        p3<-v3
                                        m3<-c
                                }
                                
                                m4<-0;p4<-NULL
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        #cat("               v4",v4,"\n")
                                        if (v4=="_count"|v4=="_pred") next
                                        c<-wh[[v1]][[v2]][[v3]][[v4]][["_count"]]
                                        if (c>m4){
                                                p4<-v4
                                                m4<-c
                                        }
                                        
                                        
                                }
                                wh[[v1]][[v2]][[v3]][["_pred"]]<-p4
                        }
                        wh[[v1]][[v2]][["_pred"]]<-p3
                }
                wh[[v1]][["_pred"]]<-p2
                
        }
        wh[["_pred"]]<-p1     
        
}

predictNgrams2<-function(verbose=FALSE){
        m1<-c(0,0,0);p1<-c(NA,NA,NA)
        if (verbose) cat("m1",m1,"p1",p1,"\n")
        i<-1
        for (v1 in ls(wh)){
                if (i%%1000==0) cat(i,"\n")
                #v1<-ls(wh)[i]
                if (verbose) cat("v1",v1,"\n")
                i<-i+1
                if (v1=="_pred") next 
                if (v1!="sss" & v1!="eee" & v1!="#unk"){  
                c<-wh[[v1]][["_count"]]
                if (verbose) cat("c",c,"\n")
                if (c>m1[1]){
                        p1<-c(v1,p1[1:2])
                        m1<-c(c,m1[1:2])
                } else if (c>m1[2]){
                        p1<-c(p1[1],v1,p1[2])
                        m1<-c(m1[1],c,m1[2])
                } else if (c>m1[3]){
                        p1<-c(p1[1],p1[2],v1)
                        m1<-c(m1[1],m1[2],c)
                }
                if (verbose) cat("m1",m1,"p1",p1,"\n")
                } 

                m2<-c(0,0,0);p2<-c(NA,NA,NA)
                for (v2 in ls(wh[[v1]])){
                        #cat("    v2",v2,"\n")
                        if (v2=="_count"|v2=="_pred") next
                        if (v2!="#unk"){ 
                        c<-wh[[v1]][[v2]][["_count"]]
                        if (c>m2[1]){
                                p2<-c(v2,p2[1:2])
                                m2<-c(c,m2[1:2])
                        } else if (c>m2[2]){
                                p2<-c(p2[1],v2,p2[2])
                                m2<-c(m2[1],c,m2[2])
                        } else if (c>m2[3]){
                                p2<-c(p2[1],p2[2],v2)
                                m2<-c(m2[1],m2[2],c)
                        }
                        }
                        m3<-c(0,0,0);p3<-c(NA,NA,NA)
                        for (v3 in ls(wh[[v1]][[v2]])){
                                #cat("        v3",v3,"\n")
                                if (v3=="_count"|v3=="_pred") next
                                if (v3!="#unk"){
                                c<-wh[[v1]][[v2]][[v3]][["_count"]]
                                if (c>m3[1]){
                                        p3<-c(v3,p3[1:2])
                                        m3<-c(c,m3[1:2])
                                } else if (c>m3[2]){
                                        p3<-c(p3[1],v3,p3[2])
                                        m3<-c(m3[1],c,m3[2])
                                } else if (c>m3[3]){
                                        p3<-c(p3[1],p3[2],v3)
                                        m3<-c(m3[1],m3[2],c)
                                }
                                }
                                m4<-c(0,0,0);p4<-c(NA,NA,NA)
                                for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                                        #cat("               v4",v4,"\n")
                                        if (v4=="_count"|v4=="_pred") next
                                        if (v4!="#unk"){
                                        c<-wh[[v1]][[v2]][[v3]][[v4]][["_count"]]
                                        if (c>m4[1]){
                                                p4<-c(v4,p4[1:2])
                                                m4<-c(c,m4[1:2])
                                        } else if (c>m4[2]){
                                                p4<-c(p4[1],v4,p4[2])
                                                m4<-c(m4[1],c,m4[2])
                                        } else if (c>m4[3]){
                                                p4<-c(p4[1],p4[2],v4)
                                                m4<-c(m4[1],m4[2],c)
                                        }
                                        }
                                        
                                }
                                wh[[v1]][[v2]][[v3]][["_pred"]]<-p4
                        }
                        wh[[v1]][[v2]][["_pred"]]<-p3
                }
                wh[[v1]][["_pred"]]<-p2
                
        }
        wh[["_pred"]]<-p1     
        
}

wh<-readRDS("results/whBNTunk.RData")
predictNgrams()
saveRDS(wh,file="results/whBNTpred.RData")
# i am not sure if i am going to be a great player
wh[["_pred"]]
wh[["sss"]][["_pred"]]
wh[["sss"]][["i"]][["am"]][["_pred"]]
wh[["i"]][["am"]][["not"]][["_pred"]]
wh[["am"]][["not"]][["sure"]][["_pred"]]
wh[["not"]][["sure"]][["if"]][["_pred"]]
wh[["sure"]][["if"]][["i"]][["_pred"]]
wh[["if"]][["i"]][["am"]][["_pred"]]
wh[["i"]][["am"]][["going"]][["_pred"]]
wh[["am"]][["going"]][["to"]][["_pred"]]
wh[["going"]][["to"]][["be"]][["_pred"]]
wh[["to"]][["be"]][["a"]][["_pred"]]
wh[["be"]][["a"]][["great"]][["_pred"]]
wh[["a"]][["great"]][["player"]][["_pred"]]


s1<-"I love to eat bananas"
s2<-"I love to eat"
s3<-"I love to work "
s4<-"You love to eat mangoes"
s5<-"You like to play sport"
s6<-"They like to play the guitar"
text<-c(s1,s2,s3,s4,s5,s6)
text<-sentences(text)
wh<-new.env()
wh<-textNgrams(text,wh)
printNgramsT(wh)
predictNgrams()
printPredT(wh)
predictNgrams2()
printPred(wh)
wh<-readRDS("results/whBNTunk.RData")
predictNgrams2()
saveRDS(wh,file="results/whBNTpred3w.rds")

######### corrected unknown words
wh<-readRDS("results/whBNTunk.RData")
predictNgrams2()
saveRDS(wh,file="results/whBNTpred3wu2.rds")
