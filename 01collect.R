# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

# data collection
con <- file("data/en_US/en_US.blogs.txt", "r") 
a=readLines(con, -1) 
m=max(nchar(a))
length(a)
# 899288
m
# 40835
#if max<m max<-m
close(con)

symbolPatt<-"[[:alpha:]]+'[[:alpha:]]+"
v<-a
ap<-unlist(regmatches(v,gregexpr(symbolPatt,v)))
apt<-table(ap)
apt<-apt[order(apt,decreasing=TRUE)]
con <- file("data/en_US/en_US.news.txt", "r") 
a=readLines(con, -1) 
length(a)
# 77259
m=max(nchar(a))
m
#[1] 5760
close(con)

con <- file("data/en_US/en_US.twitter.txt", "r") 
a=readLines(con, -1) 
length(a)
# 2360148
m=max(nchar(a))
m
# 213
close(con)

sum(grepl("love",a))
# 90956
sum(grepl("hate",a))
# 22138
sum(grepl("love",a))/sum(grepl("hate",a))
# 4.108592
sum(grepl(" love ",a))/sum(grepl(" hate ",a))
# 4.616849

grep("biostats",a,value=TRUE)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing",a))
