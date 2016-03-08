# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# functions for processing text into sentnces
# functions for processing a sentence into n-grams


#### split text on sentences
# replace numbers with #no
# remove punctuation
# add start of sentence <#s> and end of sentence <#e>
library("openNLP")
require("NLP")

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


sentences<-function(text){
        sent_token_annotator <- Maxent_Sent_Token_Annotator()
        #word_token_annotator <- Maxent_Word_Token_Annotator()
        ss<-character()
        i<-0
        
        no_expr<-"(^|\\s)\\d+($|\\s)"
        no_expr1<-"(^|\\s)(\\d+)(['[:alpha:]]+['[:alnum:]]*)($|\\s)"
        no_expr2<-"(^|\\s)['[:alpha:]]+\\d+['[:alnum:]]*($|\\s)"
        for (t in text){
                i<-i+1
                s<-as.String(t) 
                t_a <- annotate(s, sent_token_annotator)
                sent<-s[t_a]
                singleQuotes<-"(\xE2\x80\x98|\xE2\x80\x99|\xE2\x80\x9A|\xE2\x80\x9B)"
                sent<-gsub(singleQuotes,"'",sent,useBytes=TRUE)
                sent<-gsub("'+","'",sent,useBytes=TRUE)
                doubleQuotes<-"(\xE2\x80\x9C|\xE2\x80\x9D|\xE2\x80\x9E|\xE2\x80\x9F)"
                sent<-gsub(doubleQuotes,"",sent,useBytes=TRUE)
                sent<-gsub("[^'[:^punct:]]", "", sent, perl=T)
                sent<-gsub("(^|\\s)'|'($|\\s)", "", sent)
                sent<-gsub("[^'[:alnum:][:space:]]", " ", sent)
                sent<-gsub("\\s", "  ", sent)
                sent<-gsub("(^|\\s)1st($|\\s)", " first ", sent)
                sent<-gsub("(^|\\s)2nd($|\\s)", " second ", sent)
                sent<-gsub("(^|\\s)3rd($|\\s)", " third ", sent)
                sent<-gsub("(^|\\s)\\d+th($|\\s)", " #noth ", sent)
                #sent<-gsub("(^|\\s)\\d+'s($|\\s)", " #no's ", sent)
                
                #while (sum(grepl(pattern=no_expr1,sent))>0){
                        sent<-gsub(no_expr1," \\2 \\3 ",sent)
                #}
                
                #while (sum(grepl(pattern=no_expr,sent))>0){
                        sent<-gsub(pattern=no_expr,replacement=" #no ", sent)
                #}
                #while (sum(grepl(pattern=no_expr2,sent))>0){
                        sent<-gsub(pattern=no_expr2,replacement=" #an ", sent)
                #}             
                sent<-tolower(sent) 
                sent<-replace_abbr(sent)
                sent<-gsub("([[:alpha:]]+)'s", "\\1", sent)
                sent<-paste("sss",sent,"eee")                
                ss<-c(ss,sent)
        }
        return(ss)
}

words<-function(sentences){ 
        unlist(strsplit(sentences, "\\s+"))
}

bigrams<-function(sentences){
        bb<-character()
        for (s in sentences){
                b<-bigrams1(sentence) 
                bb<-c(bb,b)
        }
        bb
}

bigrams<-function(sentences){
        bb<-character()
        for (s in sentences){
                b<-bigrams1(s) 
                bb<-c(bb,b)
        }
        bb
}

trigrams<-function(sentences){
        bb<-character()
        for (s in sentences){
                b<-trigrams1(s) 
                bb<-c(bb,b)
        }
        bb
}
quadrigrams<-function(sentences){
        bb<-character()
        for (s in sentences){
                b<-quadrigrams1(s) 
                bb<-c(bb,b)
        }
        bb
}


bigrams1<-function(sentence){
        words<-strsplit(sentence, "\\s+")[[1]]
        l<-length(words)
        if (l<2) return(character())
        paste(words[1:(l-1)],words[2:l])
}

trigrams1<-function(sentence){
        words<-strsplit(sentence, "\\s+")[[1]]
        l<-length(words)
        if (l<3) return(character())
        paste(words[1:(l-2)],words[2:(l-1)],words[3:l])
}

quadrigrams1<-function(sentence){
        words<-strsplit(sentence, "\\s+")[[1]]
        l<-length(words)
        if (l<4) return(character())
        paste(words[1:(l-3)],words[2:(l-2)],words[3:(l-1)],words[4:l])
}

textNgrams<-function(textSentences,wh){
        s<-0
        for (sentence in textSentences){
                s<-s+1
                if (s%%1000==0) cat("sentence:",s,"\n")
                words <- unlist(strsplit(sentence, "\\s+"))
                l <- length(words)
                for (i in 1:l){
                        # unigrams
                        w1<-words[i]
                        if (is.null(wh[[w1]])){
                                wh[[w1]]<-new.env()
                                wh[[w1]][["_count"]]<-1
                        } else {
                                wh[[w1]][["_count"]] <- wh[[w1]][["_count"]] + 1      
                        }
                        
                        # bigrams
                        if (l>i){
                                w2<-words[i+1]
                                if (is.null(wh[[w1]][[w2]])){
                                        wh[[w1]][[w2]]<-new.env()
                                        wh[[w1]][[w2]][["_count"]]<-1
                                } else {
                                        wh[[w1]][[w2]][["_count"]] <- wh[[w1]][[w2]][["_count"]] + 1      
                                }       
                        } 
                        
                        #trigrams
                        if (l>i+1){
                                w3<-words[i+2]
                                if (is.null(wh[[w1]][[w2]][[w3]])){
                                        wh[[w1]][[w2]][[w3]]<-new.env()
                                        wh[[w1]][[w2]][[w3]][["_count"]]<-1
                                } else {
                                        wh[[w1]][[w2]][[w3]][["_count"]] <- wh[[w1]][[w2]][[w3]][["_count"]] + 1      
                                }       
                        } 
                        
                        # quadrigrams
                        if (l>i+2){
                                w4<-words[i+3]
                                if (is.null(wh[[w1]][[w2]][[w3]][[w4]])){
                                        wh[[w1]][[w2]][[w3]][[w4]]<-new.env()
                                        wh[[w1]][[w2]][[w3]][[w4]][["_count"]]<-1
                                } else {
                                        wh[[w1]][[w2]][[w3]][[w4]][["_count"]] <- wh[[w1]][[w2]][[w3]][[w4]][["_count"]] + 1      
                                }       
                        }        
                        
                }
        }
        wh
        
        
        
}