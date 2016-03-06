Next Word Prediction
========================================================
author: Natasa sarafijanovic-Djukic 
date: January 24, 2016
transition: rotate
Data Science Coursera Capstone Project    
Johns Hopkins University

Project Overview
========================================================

The main goal of the project is to build a shiny application that predicts the next word as we type a text.

The data provided by Coursera is the [HC Corpora dataset](www.corpora.heliohost.org), which contains entries from blogs, news, and twitter. 

Prediction Algorithm
========================================================

- N-gram language model (unigram, bigram, trigram, and four-gram)
- Stupid back-off technique for prediction
- Dealing with unknown words: all words that appear only once are replaced in a model with a special word "#unk"

- Preprocessing:  
        - tokenize text into sentences  
        - remove all punctuation except apostrophe  
        - replace numbers with special word "#no"  
        - replacing English contractions (such as I'm) with full phrases (I am)  

Shiny Application
========================================================

[Try it!](https://natasasdj.shinyapps.io/wordPred/)  

![alt text](shiny.png)


Shiny Application - Description
========================================================

This application allows a user to type a partial sentence and it predicts the next word following the user's input. The application predicts the three most probable words.

- As a user is typing in the box 'Input text' , the next word is shown in the box 'Next word'.  
The next word is shown only when a user types the blank symbol.

- The input text is reset:  with the button 'reset' or if a user types '.'

