# Next Word Prediction
# Data Science Capstone Project
# Johns Hopkins University on Coursera
# Author: Natasa Sarafijanovic-Djukic
# January 2016

library(data.table)
cat("start download")
unigram<-readRDS("unigram2.rds")
bigram<-readRDS("bigram2.rds")
trigram<-readRDS("trigram2.rds")
quadrigram<-readRDS("quadrigram2.rds")
cat("end download")