# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

######## unknown words - first word

unknown_words1<-function(verbose=FALSE){
i<-1
for (v1 in ls(wh)){
        cat(i,"\n"); i<-i+1
        if (verbose) cat("v1",v1,"\n")
        if (wh[[v1]][["_count"]]!=1) {if (verbose) cat("count>1\n");next}
        if (is.null(wh[["#unk"]])){
                if (verbose) cat("make #unk\n")
                wh[["#unk"]]<-wh[[v1]]
         } else {                
                if (verbose) cat("update #unk\n")
                v2<-setdiff(ls(wh[[v1]]),"_count")
                if (verbose) cat("v2",v2,"\n")
                if (length(v2)==0) {rm(list=v1,envir=wh);next}
                if (is.null(wh[["#unk"]][[v2]])){
                        if (verbose) cat("make #unk v2\n")
                        wh[["#unk"]][[v2]]<-wh[[v1]][[v2]]
                        rm(list=v1,envir=wh)
                        next
                 }
                if (verbose) cat("update #unk v2\n")
                wh[["#unk"]][[v2]][["_count"]]<-
                        wh[["#unk"]][[v2]][["_count"]] +  wh[[v1]][[v2]][["_count"]]
                v3<-setdiff(ls(wh[[v1]][[v2]]),"_count")
                if (verbose) cat("v3",v3,"\n")
                if (length(v3)==0) {rm(list=v1,envir=wh);next}
                if (is.null(wh[["#unk"]][[v2]][[v3]])){
                        if (verbose) cat("make #unk v2 v3\n")
                        wh[["#unk"]][[v2]][[v3]]<-wh[[v1]][[v2]][[v3]]
                        rm(list=v1,envir=wh)
                        next
                }
                if (verbose) cat("update #unk v2 v3\n")
                wh[["#unk"]][[v2]][[v3]][["_count"]]<-
                        wh[["#unk"]][[v2]][[v3]][["_count"]] +  wh[[v1]][[v2]][[v3]][["_count"]]
                v4<-setdiff(ls(wh[[v1]][[v2]][[v3]]),"_count")
                if (verbose) cat("v4",v4,"\n")
               if (length(v4)==0) {rm(list=v1,envir=wh);next}
               if (is.null(wh[["#unk"]][[v2]][[v3]][[v4]])){
                       if (verbose) cat("make #unk v2 v3 v4\n")
                       wh[["#unk"]][[v2]][[v3]][[v4]]<-wh[[v1]][[v2]][[v3]][[v4]]
                       rm(list=v1,envir=wh)
                       next
               }
               if (verbose) cat("update #unk v2 v3 v4\n")
               wh[["#unk"]][[v2]][[v3]][[v4]][["_count"]]<-
                       wh[["#unk"]][[v2]][[v3]][[v4]][["_count"]] +  wh[[v1]][[v2]][[v3]][[v4]][["_count"]]               
                
         }       
        rm(list=v1,envir=wh)
}
}

unknown_words2<-function(verbose=FALSE){
        
        for (v1 in ls(wh)){
                if (verbose) cat("v1",v1,"\n")
        for (v2 in ls(wh[[v1]])){        
                if (verbose) cat("v2",v2,"\n")
                if (v2=="_count") next
                if (!is.null(wh[[v2]])) {if (verbose) cat("count>1\n");next}
                if (is.null(wh[[v1]][["#unk"]])){
                        if (verbose) cat("make v1 #unk\n")
                        wh[[v1]][["#unk"]]<-wh[[v1]][[v2]]
                } else {                
                        if (verbose) cat("update v1 #unk\n")
                        v3<-setdiff(ls(wh[[v1]][[v2]]),"_count")
                        if (verbose) cat("v3",v3,"\n")
                        if (length(v3)==0) {rm(list=v2,envir=wh[[v1]]);next}
                        if (is.null(wh[[v1]][["#unk"]][[v3]])){
                                if (verbose) cat("make v1 #unk v3\n")
                                wh[[v1]][["#unk"]][[v3]]<-wh[[v1]][[v2]][[v3]]
                                rm(list=v2,envir=wh[[v1]])
                                next
                        }
                        if (verbose) cat("update v1 #unk v3\n")
                        wh[[v1]][["#unk"]][[v3]][["_count"]]<-
                                wh[[v1]][["#unk"]][[v3]][["_count"]] +  wh[[v1]][[v2]][[v3]][["_count"]]
                        v4<-setdiff(ls(wh[[v1]][[v2]][[v3]]),"_count")
                        if (verbose) cat("v4",v4,"\n")
                        if (length(v4)==0) {rm(list=v2,envir=wh[[v1]]);next}
                        if (is.null(wh[["#unk"]][[v2]][[v3]][[v4]])){
                                if (verbose) cat("make #unk v2 v3 v4\n")
                                wh[[v1]][["#unk"]][[v3]][[v4]]<-wh[[v1]][[v2]][[v3]][[v4]]
                                rm(list=v2,envir=wh[[v1]])
                                next
                        }
                        if (verbose) cat("update v1 #unk v3 v4\n")
                        wh[[v1]][["#unk"]][[v3]][[v4]][["_count"]]<-
                                wh[[v1]][["#unk"]][[v3]][[v4]][["_count"]] +  wh[[v1]][[v2]][[v3]][[v4]][["_count"]]               
                        
                }       
                rm(list=v2,envir=wh[[v1]])
        }
        }
}



unknown_words3<-function(verbose=FALSE){
        
        for (v1 in ls(wh)){
                if (verbose) cat("v1",v1,"\n")
        for (v2 in ls(wh[[v1]])){        
                if (verbose) cat("v2",v2,"\n")
                if (v2=="_count") next
        for (v3 in ls(wh[[v1]][[v2]])){
                if (verbose) cat("v3",v3,"\n")
                if (v3=="_count") next
                if (!is.null(wh[[v3]])) {if (verbose) cat("count>1\n");next}
                if (is.null(wh[[v1]][[v2]][["#unk"]])){
                        if (verbose) cat("make v1 v2 #unk\n")
                        wh[[v1]][[v2]][["#unk"]]<-wh[[v1]][[v2]][[v3]]
                } else {                
                         if (verbose) cat("update v1 v2 #unk\n")
                         v4<-setdiff(ls(wh[[v1]][[v2]][[v3]]),"_count")
                         if (verbose) cat("v4",v4,"\n")
                         if (length(v4)==0) {rm(list=v3,envir=wh[[v1]][[v2]]);next}
                         if (is.null(wh[[v1]][[v2]][["#unk"]][[v4]])){
                                 if (verbose) cat("make v1 v2 #unk v4\n")
                                 wh[[v1]][[v2]][["#unk"]][[v4]]<-wh[[v1]][[v2]][[v3]][[v4]]
                                 rm(list=v3,envir=wh[[v1]][[v2]])
                                 next
                         }
                        
                         if (verbose) cat("update v1 #unk v3 v4\n")
                         wh[[v1]][[v2]][["#unk"]][[v4]][["_count"]]<-
                                 wh[[v1]][[v2]][["#unk"]][[v4]][["_count"]] +  wh[[v1]][[v2]][[v3]][[v4]][["_count"]]               
                                       
                         
                }
                rm(list=v3,envir=wh[[v1]][[v2]])
        }
        }
        }
}

unknown_words4<-function(verbose=FALSE){
        
        for (v1 in ls(wh)){
                if (verbose) cat("v1",v1,"\n")
        for (v2 in ls(wh[[v1]])){        
                if (verbose) cat("v2",v2,"\n")
                if (v2=="_count") next
        for (v3 in ls(wh[[v1]][[v2]])){
                if (verbose) cat("v3",v3,"\n")       
                if (v3=="_count") next
        for (v4 in ls(wh[[v1]][[v2]][[v3]])){
                if (verbose) cat("v4",v4,"\n")
                if (v4=="_count") next
                if (!is.null(wh[[v4]])) {if (verbose) cat("count>1\n");next}
                if (is.null(wh[[v1]][[v2]][[v3]][["#unk"]])){
                        if (verbose) cat("make v1 v2 v3 #unk\n")
                        wh[[v1]][[v2]][[v3]][["#unk"]]<-wh[[v1]][[v2]][[v3]][[v4]]
                } else {                
                        if (verbose) cat("update v1 v2 v3 #unk\n")
                }        
                                        
                rm(list=v4,envir=wh[[v1]][[v2]][[v3]])
        }
        }
        }
        }
}

#### test word hash:

test_wh<-function(){
        wh<-new.env()
        wh[["a"]]<-new.env();wh[["b"]]<-new.env();wh[["c"]]<-new.env();
        wh[["d"]]<-new.env();wh[["e"]]<-new.env();
        wh[["a"]][["_count"]]<-5; wh[["b"]][["_count"]]<-4; wh[["c"]][["_count"]]<-3;
        wh[["d"]][["_count"]]<-1; wh[["e"]][["_count"]]<-1;
        
        wh[["a"]][["b"]]<-new.env(); wh[["a"]][["c"]]<-new.env(); 
        wh[["b"]][["a"]]<-new.env(); wh[["b"]][["d"]]<-new.env();
        wh[["a"]][["b"]][["_count"]]<-2;wh[["a"]][["c"]][["_count"]]<-2;
        wh[["b"]][["a"]][["_count"]]<-1;wh[["b"]][["d"]][["_count"]]<-1;
        wh[["d"]][["e"]]<-new.env(); 
        wh[["e"]][["a"]]<-new.env();
        wh[["d"]][["e"]][["_count"]]<-1;
        wh[["e"]][["a"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]]<-new.env(); wh[["a"]][["c"]][["e"]]<-new.env(); 
        wh[["b"]][["a"]][["d"]]<-new.env(); wh[["b"]][["d"]][["a"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["_count"]]<-2;
        wh[["b"]][["a"]][["d"]][["_count"]]<-1;wh[["b"]][["d"]][["a"]][["_count"]]<-1;
        wh[["d"]][["e"]][["a"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]]<-new.env();
        wh[["d"]][["e"]][["a"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]][["b"]]<-new.env(); 
        wh[["b"]][["a"]][["d"]][["e"]]<-new.env(); wh[["b"]][["d"]][["a"]][["c"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["d"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["b"]][["_count"]]<-2;
        wh[["b"]][["a"]][["d"]][["e"]][["_count"]]<-1;wh[["b"]][["d"]][["a"]][["c"]][["_count"]]<-1;
        wh[["d"]][["e"]][["a"]][["b"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]][["c"]]<-new.env();
        wh[["d"]][["e"]][["a"]][["b"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        
        wh
}


test_wh1<-function(){
        wh<-new.env()
        wh[["a"]]<-new.env();wh[["b"]]<-new.env();wh[["c"]]<-new.env();
        wh[["d"]]<-new.env();wh[["e"]]<-new.env();
        wh[["a"]][["_count"]]<-5; wh[["b"]][["_count"]]<-1; wh[["c"]][["_count"]]<-1;
        wh[["d"]][["_count"]]<-1; wh[["e"]][["_count"]]<-1;
        
        wh[["a"]][["b"]]<-new.env(); wh[["a"]][["c"]]<-new.env(); 
        wh[["b"]][["a"]]<-new.env(); wh[["b"]][["d"]]<-new.env();
        wh[["a"]][["b"]][["_count"]]<-2;wh[["a"]][["c"]][["_count"]]<-2;
        wh[["b"]][["a"]][["_count"]]<-1;wh[["b"]][["d"]][["_count"]]<-1;
        wh[["d"]][["e"]]<-new.env(); 
        wh[["e"]][["a"]]<-new.env();
        wh[["d"]][["e"]][["_count"]]<-1;
        wh[["e"]][["a"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]]<-new.env(); wh[["a"]][["c"]][["e"]]<-new.env(); 
        wh[["b"]][["a"]][["d"]]<-new.env(); wh[["b"]][["d"]][["a"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["_count"]]<-2;
        wh[["b"]][["a"]][["d"]][["_count"]]<-1;wh[["b"]][["d"]][["a"]][["_count"]]<-1;
        wh[["d"]][["e"]][["a"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]]<-new.env();
        wh[["d"]][["e"]][["a"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]][["b"]]<-new.env(); 
        wh[["b"]][["a"]][["d"]][["e"]]<-new.env(); wh[["b"]][["d"]][["a"]][["c"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["d"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["b"]][["_count"]]<-2;
        wh[["b"]][["a"]][["d"]][["e"]][["_count"]]<-1;wh[["b"]][["d"]][["a"]][["c"]][["_count"]]<-1;
        wh[["d"]][["e"]][["a"]][["b"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]][["c"]]<-new.env();
        wh[["d"]][["e"]][["a"]][["b"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        
        wh
}

test_wh2<-function(){
        wh<-new.env()
        wh[["a"]]<-new.env();wh[["b"]]<-new.env();wh[["c"]]<-new.env();
        wh[["d"]]<-new.env();wh[["e"]]<-new.env();
        wh[["a"]][["_count"]]<-5; wh[["b"]][["_count"]]<-4; wh[["c"]][["_count"]]<-3;
        wh[["d"]][["_count"]]<-1; wh[["e"]][["_count"]]<-1;
        
        wh[["a"]][["b"]]<-new.env(); wh[["a"]][["c"]]<-new.env(); 
        wh[["b"]][["c"]]<-new.env(); wh[["b"]][["d"]]<-new.env();
        wh[["a"]][["b"]][["_count"]]<-2;wh[["a"]][["c"]][["_count"]]<-2;
        wh[["b"]][["c"]][["_count"]]<-2;wh[["b"]][["d"]][["_count"]]<-2;
        wh[["d"]][["a"]]<-new.env(); 
        wh[["e"]][["a"]]<-new.env();
        wh[["d"]][["a"]][["_count"]]<-1;
        wh[["e"]][["a"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]]<-new.env(); wh[["a"]][["c"]][["e"]]<-new.env(); 
        wh[["b"]][["c"]][["d"]]<-new.env(); wh[["b"]][["d"]][["a"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["_count"]]<-2;
        wh[["b"]][["c"]][["d"]][["_count"]]<-2;wh[["b"]][["d"]][["a"]][["_count"]]<-2;
        wh[["d"]][["a"]][["a"]]<-new.env(); 
        wh[["e"]][["a"]][["b"]]<-new.env();
        wh[["d"]][["a"]][["a"]][["_count"]]<-1;
        wh[["e"]][["a"]][["b"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]][["b"]]<-new.env(); 
        wh[["b"]][["c"]][["d"]][["e"]]<-new.env(); wh[["b"]][["d"]][["a"]][["c"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["d"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["b"]][["_count"]]<-2;
        wh[["b"]][["c"]][["d"]][["e"]][["_count"]]<-2;wh[["b"]][["d"]][["a"]][["c"]][["_count"]]<-2;
        wh[["d"]][["a"]][["a"]][["b"]]<-new.env(); 
        wh[["e"]][["a"]][["b"]][["c"]]<-new.env();
        wh[["d"]][["a"]][["a"]][["b"]][["_count"]]<-1;
        wh[["e"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        
        wh
}


test_wh3<-function(){
        wh<-new.env()
        wh[["a"]]<-new.env();wh[["b"]]<-new.env();wh[["c"]]<-new.env();
        wh[["d"]]<-new.env();wh[["e"]]<-new.env();
        wh[["a"]][["_count"]]<-5; wh[["b"]][["_count"]]<-4; wh[["c"]][["_count"]]<-3;
        wh[["d"]][["_count"]]<-1; wh[["e"]][["_count"]]<-1;
        
        wh[["a"]][["b"]]<-new.env(); wh[["a"]][["c"]]<-new.env(); 
        wh[["b"]][["c"]]<-new.env(); wh[["b"]][["d"]]<-new.env();
        wh[["a"]][["b"]][["_count"]]<-2;wh[["a"]][["c"]][["_count"]]<-2;
        wh[["b"]][["c"]][["_count"]]<-2;wh[["b"]][["d"]][["_count"]]<-2;
        wh[["d"]][["a"]]<-new.env(); 
        wh[["e"]][["a"]]<-new.env();
        wh[["d"]][["a"]][["_count"]]<-1;
        wh[["e"]][["a"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]]<-new.env(); wh[["a"]][["c"]][["e"]]<-new.env(); 
        wh[["b"]][["c"]][["d"]]<-new.env(); wh[["b"]][["d"]][["a"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["_count"]]<-2;
        wh[["b"]][["c"]][["d"]][["_count"]]<-2;wh[["b"]][["d"]][["a"]][["_count"]]<-2;
        wh[["d"]][["a"]][["b"]]<-new.env(); 
        wh[["e"]][["a"]][["b"]]<-new.env();
        wh[["d"]][["a"]][["b"]][["_count"]]<-1;
        wh[["e"]][["a"]][["b"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]][["b"]]<-new.env(); 
        wh[["b"]][["c"]][["d"]][["e"]]<-new.env(); wh[["b"]][["d"]][["a"]][["c"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["d"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["b"]][["_count"]]<-2;
        wh[["b"]][["c"]][["d"]][["e"]][["_count"]]<-2;wh[["b"]][["d"]][["a"]][["c"]][["_count"]]<-2;
        wh[["d"]][["a"]][["b"]][["c"]]<-new.env(); 
        wh[["e"]][["a"]][["b"]][["c"]]<-new.env();
        wh[["d"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        wh[["e"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        
        wh
}

test_wh4<-function(){
        wh<-new.env()
        wh[["a"]]<-new.env();wh[["b"]]<-new.env();wh[["c"]]<-new.env();
        wh[["d"]]<-new.env();wh[["e"]]<-new.env();
        wh[["a"]][["_count"]]<-5; wh[["b"]][["_count"]]<-4; wh[["c"]][["_count"]]<-3;
        wh[["d"]][["_count"]]<-1; wh[["e"]][["_count"]]<-1;
        
        wh[["a"]][["b"]]<-new.env(); wh[["a"]][["c"]]<-new.env(); 
        wh[["b"]][["c"]]<-new.env(); wh[["b"]][["d"]]<-new.env();
        wh[["a"]][["b"]][["_count"]]<-2;wh[["a"]][["c"]][["_count"]]<-2;
        wh[["b"]][["c"]][["_count"]]<-2;wh[["b"]][["d"]][["_count"]]<-2;
        wh[["d"]][["a"]]<-new.env(); 
        wh[["e"]][["a"]]<-new.env();
        wh[["d"]][["a"]][["_count"]]<-1;
        wh[["e"]][["a"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]]<-new.env(); wh[["a"]][["c"]][["e"]]<-new.env(); 
        wh[["b"]][["c"]][["d"]]<-new.env(); wh[["b"]][["d"]][["a"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["_count"]]<-2;
        wh[["b"]][["c"]][["d"]][["_count"]]<-2;wh[["b"]][["d"]][["a"]][["_count"]]<-2;
        wh[["d"]][["a"]][["b"]]<-new.env(); 
        wh[["e"]][["a"]][["b"]]<-new.env();
        wh[["d"]][["a"]][["b"]][["_count"]]<-1;
        wh[["e"]][["a"]][["b"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["c"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]][["b"]]<-new.env(); 
        wh[["b"]][["c"]][["d"]][["e"]]<-new.env(); wh[["b"]][["d"]][["a"]][["c"]]<-new.env();
        wh[["a"]][["b"]][["c"]][["d"]][["_count"]]<-2;wh[["a"]][["c"]][["e"]][["b"]][["_count"]]<-2;
        wh[["b"]][["c"]][["d"]][["e"]][["_count"]]<-2;wh[["b"]][["d"]][["a"]][["c"]][["_count"]]<-2;
        wh[["d"]][["a"]][["b"]][["c"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]][["c"]]<-new.env();
        wh[["d"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        
        wh
}

test_wh5<-function(){
        wh<-new.env()
        wh[["a"]]<-new.env();wh[["b"]]<-new.env();wh[["c"]]<-new.env();
        wh[["d"]]<-new.env();wh[["e"]]<-new.env();
        wh[["a"]][["_count"]]<-5; wh[["b"]][["_count"]]<-4; wh[["c"]][["_count"]]<-3;
        wh[["d"]][["_count"]]<-1; wh[["e"]][["_count"]]<-1;
        
        wh[["a"]][["b"]]<-new.env(); wh[["a"]][["c"]]<-new.env(); 
        wh[["b"]][["b"]]<-new.env(); wh[["b"]][["d"]]<-new.env();
        wh[["a"]][["b"]][["_count"]]<-2;wh[["a"]][["c"]][["_count"]]<-2;
        wh[["b"]][["b"]][["_count"]]<-1;wh[["b"]][["d"]][["_count"]]<-1;
        wh[["d"]][["e"]]<-new.env(); 
        wh[["e"]][["a"]]<-new.env();
        wh[["d"]][["e"]][["_count"]]<-1;
        wh[["e"]][["a"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]]<-new.env(); 
        wh[["a"]][["c"]][["d"]]<-new.env();
        wh[["b"]][["b"]][["a"]]<-new.env(); wh[["b"]][["d"]][["a"]]<-new.env();
        wh[["a"]][["b"]][["d"]][["_count"]]<-1;wh[["a"]][["c"]][["e"]][["_count"]]<-1;
        wh[["b"]][["b"]][["a"]][["_count"]]<-1;wh[["b"]][["d"]][["a"]][["_count"]]<-1;
        wh[["a"]][["c"]][["d"]][["_count"]]<-1
        wh[["d"]][["e"]][["a"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]]<-new.env();
        wh[["d"]][["e"]][["a"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["_count"]]<-1;
        
        wh[["a"]][["b"]][["d"]][["d"]]<-new.env(); wh[["a"]][["c"]][["e"]][["b"]]<-new.env(); 
        wh[["b"]][["b"]][["a"]][["e"]]<-new.env(); wh[["b"]][["d"]][["a"]][["c"]]<-new.env();
        wh[["a"]][["c"]][["d"]][["c"]]<-new.env()
        wh[["a"]][["c"]][["d"]][["b"]]<-new.env()
        wh[["b"]][["b"]][["a"]][["d"]]<-new.env()
        wh[["b"]][["b"]][["a"]][["a"]]<-new.env()
        wh[["a"]][["b"]][["d"]][["d"]][["_count"]]<-1;wh[["a"]][["c"]][["e"]][["b"]][["_count"]]<-1;
        wh[["b"]][["b"]][["a"]][["e"]][["_count"]]<-1;wh[["b"]][["d"]][["a"]][["c"]][["_count"]]<-1;
        wh[["a"]][["c"]][["d"]][["c"]][["_count"]]<-1
        wh[["a"]][["c"]][["d"]][["b"]][["_count"]]<-1
        wh[["b"]][["b"]][["a"]][["d"]][["_count"]]<-1
        wh[["b"]][["b"]][["a"]][["a"]][["_count"]]<-1
        wh[["d"]][["e"]][["a"]][["b"]]<-new.env(); 
        #wh[["e"]][["a"]][["b"]][["c"]]<-new.env();
        wh[["d"]][["e"]][["a"]][["b"]][["_count"]]<-1;
        #wh[["e"]][["a"]][["b"]][["c"]][["_count"]]<-1;
        
        wh
}


#wh<-test_wh5()
wh<-readRDS("results/whBNT.RData")

unknown_words1()
unknown_words2()
unknown_words3()
unknown_words4()
saveRDS(wh,file="results/whBNTunk.RData")
