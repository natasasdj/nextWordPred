# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016


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




replace_abbr<-function(text){       
        text<-gsub("(^|\\s)ain't($|\\s)"," am not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)aren't($|\\s)"," are not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)can't($|\\s)"," can not  ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)cannot($|\\s)"," can not  ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)couldn't($|\\s)"," could not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)could've($|\\s)"," could have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)didn't($|\\s)"," did not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)don't($|\\s)"," do not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)doesn't($|\\s)"," does not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)gonna($|\\s)"," going to ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)hadn't($|\\s)"," had not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)hasn't($|\\s)"," has not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)haven't($|\\s)"," have not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)he's($|\\s)"," he is ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)he'll($|\\s)"," he will ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)he'd($|\\s)"," he would ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)how's($|\\s)"," how is ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)how'll($|\\s)"," how will ",text,useBytes=TRUE) 
        text<-gsub("(^|\\s)here's($|\\s)"," here is ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)i'm($|\\s)"," i am ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)i'll($|\\s)"," i will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)i've($|\\s)"," i have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)i'd($|\\s)"," i would ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)it's($|\\s)"," it is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)it'll($|\\s)"," it will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)it'd($|\\s)"," it will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)isn't($|\\s)"," is not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)let's($|\\s)"," let us ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)must've($|\\s)"," must have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)mustn't($|\\s)"," must not  ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)needn't($|\\s)"," need not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)she's($|\\s)"," she is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)she'll($|\\s)"," she will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)she'd($|\\s)"," she would ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)shouldn't($|\\s)"," should not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)should've($|\\s)"," should have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)that's($|\\s)"," that is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)that'll($|\\s)"," that will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)there's($|\\s)"," there is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)there're($|\\s)"," there are ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)they're($|\\s)"," they are ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)they've($|\\s)"," they have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)they'll($|\\s)"," they will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)they'd($|\\s)"," they would ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)wanna($|\\s)"," want to ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)wasn't($|\\s)"," was not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)won't($|\\s)"," will not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)weren't($|\\s)"," were not ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)wouldn't($|\\s)"," would not ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)would've($|\\s)"," would have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)we're($|\\s)"," we are ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)we've($|\\s)"," we have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)we'll($|\\s)"," we will ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)we'd($|\\s)"," we would ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)what's($|\\s)"," what is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)who's($|\\s)"," who is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)where's($|\\s)"," where is ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)who'd($|\\s)"," who would ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)who've($|\\s)"," who have ",text,useBytes=TRUE)       
        text<-gsub("(^|\\s)you're($|\\s)"," you are ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)you'll($|\\s)"," you will ",text,useBytes=TRUE)        
        text<-gsub("(^|\\s)you've($|\\s)"," you have ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)you'd($|\\s)"," you would ",text,useBytes=TRUE)
        text<-gsub("(^|\\s)y'all($|\\s)"," you all ",text,useBytes=TRUE)
        
}


processText<-function(text){        
        no_expr<-"(^|\\s)\\d+($|\\s)"
        no_expr1<-"(^|\\s)(\\d+)(['[:alpha:]]+['[:alnum:]]*)($|\\s)"
        no_expr2<-"(^|\\s)['[:alpha:]]+\\d+['[:alnum:]]*($|\\s)"
        singleQuotes<-"(\xE2\x80\x98|\xE2\x80\x99|\xE2\x80\x9A|\xE2\x80\x9B)"
        text<-gsub(singleQuotes,"'",text,useBytes=TRUE)
        text<-gsub("'+","'",text,useBytes=TRUE)
        doubleQuotes<-"(\xE2\x80\x9C|\xE2\x80\x9D|\xE2\x80\x9E|\xE2\x80\x9F)"
        text<-gsub(doubleQuotes,"",text,useBytes=TRUE)
        text<-gsub("[^'[:^punct:]]", "", text, perl=T)
        text<-gsub("(^|\\s)'|'($|\\s)", "", text)
        text<-gsub("[^'[:alnum:][:space:]]", " ", text)
        text<-gsub("\\s", "  ", text)
        text<-gsub("(^|\\s)1st($|\\s)", " first ", text)
        text<-gsub("(^|\\s)2nd($|\\s)", " second ", text)
        text<-gsub("(^|\\s)3rd($|\\s)", " third ", text)
        text<-gsub("(^|\\s)\\d+th($|\\s)", " #noth ", text)
        text<-gsub(no_expr1," \\2 \\3 ",text)
        text<-gsub(pattern=no_expr,replacement=" #no ", text)
        text<-gsub(pattern=no_expr2,replacement=" #an ", text)
        text<-tolower(text) 
        text<-replace_abbr(text)
        text<-gsub("([[:alpha:]]+)'s", "\\1", text)
        text<-paste("sss",text)                
        return(text)
}


unkWord<-function(w){
        if (is.na(bigram[w,2,with=FALSE])) "#unk" else w
        
}

pred<-function(text){
        text<-processText(text)
        # cat(text,"\n")
        words<-unlist(strsplit(text, "\\s+"))
        words<-sapply(words,unkWord,USE.NAMES = F)
        l<-length(words)
        if (l==1) p<-bigramPred(words)
        if (l==2) p<-trigramPred(words[1],words[2]) 
        if (l>=3) p<-quadrigramPred(words[l-2],words[l-1],words[l])
        if (is.null(p)) "null" else { p[p=="eee"]<-".";p }
}






