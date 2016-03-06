
####### very slow #############
# l<-1; w<-0; uw<-0
# uwords<-vector()
# for (line in a[10001:100000]){
#         if (l%%100==0) cat("line: ",l,"\n")
#         singleQuotes<-"(\xE2\x80\x98|\xE2\x80\x99|\xE2\x80\x9A|\xE2\x80\x9B)"
#         line<-gsub(singleQuotes,"'",line,useBytes=TRUE)
#         doubleQuotes<-"(\xE2\x80\x9C|\xE2\x80\x9D|\xE2\x80\x9E|\xE2\x80\x9F)"
#         line<-gsub(doubleQuotes,"",line,useBytes=TRUE)
#         line<-gsub("[^'[:^punct:]]", "", line, perl=T)
#         line<-gsub("(^|\\s)'", "", line)
#         line<-gsub("[^'[:alnum:][:space:]]", " ", line)
#         no_expr<-"(^|\\s)[+-]?((\\d+(\\.\\d*)?)|(\\.\\d+))($|\\s)"
#         line<-gsub(pattern=no_expr,replacement=" #no ", line)
#         line<-tolower(line) 
#         words<-unlist(strsplit(line, "\\s+"))
#         w<-w+length(words)
#         for (word in words){
#                 if(is.na(uwords[word])){
#                         uwords[[word]]<-1
#                         uw<-uw+1
#                 }
#         }
#         l<-l+1
# }
# 
preprocess<-function(line){
        singleQuotes<-"(\xE2\x80\x98|\xE2\x80\x99|\xE2\x80\x9A|\xE2\x80\x9B)"
        line<-gsub(singleQuotes,"'",line,useBytes=TRUE)
        line<-gsub("'+","'",line,useBytes=TRUE)
        doubleQuotes<-"(\xE2\x80\x9C|\xE2\x80\x9D|\xE2\x80\x9E|\xE2\x80\x9F)"
        line<-gsub(doubleQuotes,"",line,useBytes=TRUE)
        line<-gsub("[^'[:^punct:]]", "", line, perl=T)
        line<-gsub("(^|\\s)'|'($|\\s)", "", line)
#        line<-gsub("(^|\\s)1st ", " first ", line)
#        line<-gsub("(^|\\s)2nd ", " second ", line)
#        line<-gsub("(^|\\s)3rd ", " third ", line)
#        line<-gsub("(^|\\s)\\d+th", "#noth ", line)
        line<-gsub("[^'[:alnum:][:space:]]", " ", line)
        no_expr<-"(^|\\s)[+-]?((\\d+(\\.\\d*)?)|(\\.\\d+))($|\\s)"       
        line<-gsub(pattern=no_expr,replacement=" #no ", line)               
        line<-tolower(line) 
}


########## fast
## with environment
wordsCount<-function(a){
        l<-1; w<-0; uw<-0
        uwords<-new.env()
        for (line in a){
                if (l%%1000==0) cat("line: ",l,"\n")
                line<-preprocess(line)
                words<-unlist(strsplit(line, "\\s+"))
                w<-w+length(words)
                for (word in words){               
                        if(nchar(word)>0){
                                if (is.null(uwords[[word]])){
                                        uwords[[word]]<-1
                                        uw<-uw+1
                                }
                        }
                }
                l<-l+1
        }
        res<-c(w,uw)
        #names(res)<-c("words","wordsUnique")
        return(res)
        
} 

con <- file("data/en_US/en_US.blogs.txt", "rb") 
a=readLines(con, -1,encoding="UTF-8") 

length(a)
# 899288
close(con)
res <- wordsCount(a)
blogLines<-length(a)
blogWords<-res[1]
blogWordsUnique<-res[2]


con <- file("data/en_US/en_US.news.txt", "rb") 
a=readLines(con, -1,encoding="UTF-8") 
length(a)
# 1010242
close(con)
res <- wordsCount(a)
newsLines<-length(a)
newsWords<-res[1]
# 34232188
newsWordsUnique<-res[2]
#358890


con <- file("data/en_US/en_US.twitter.txt", "rb") 
a=readLines(con, -1,encoding="UTF-8") 
length(a)
# 899288
close(con)
res <- wordsCount(a)
twitterLines<-length(a)
twitterWords<-res[1]
twitterWordsUnique<-res[2]


counts<-data.frame(blogs=c(blogLines,blogWords,blogWordsUnique),
                   news=c(newsLines,newsWords,newsWordsUnique),
                   twitter=c(twitterLines,twitterWords,twitterWordsUnique))
rownames(counts)<-c("lines","words","unique words")
write.table(counts,"results/countsAll.csv")
