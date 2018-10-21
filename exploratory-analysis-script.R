library(ggplot2)
library(lubridate)
library(tidyverse)
library(stringr)
library(tidytext)
library(sentimentr)
library(tm)
library(NLP)
library(dplyr)
library(syuzhet)
library(ggrepel)
library(zoo)
library(scales)

load('Project1/dawn_comments_total_new.Rda')
dawnComments <- comments_total
rm(comments_total)
load('Project1/dawnArticles_new.Rda')

dawnArticles$title <- as.character(dawnArticles$title) #converting title to characters
dawnArticles$text <- as.character(dawnArticles$text) #converting article text to characters
dawnComments$text <- as.character(dawnComments$text) #converting comment text to characters


dawnTotal$year <- lubridate::year(dawnTotal$date.y)
#Total Comments Per Year
ggplot(dawnTotal,aes(year))+
        geom_histogram()+
        xlab('Year')+
        ylab('Number of Comments')+
        theme_linedraw()+
        scale_x_continuous(breaks = seq(2011,2018,1))

#Comments on an article vs Date
ggplot(dawnTotal_year_s,aes(date,n_comments,label=title,group=1))+
        geom_point()+
        geom_label_repel(data          = subset(dawnTotal_year_s, n_comments> quantile(dawnTotal_year_s$n_comments,0.99)),
                         segment.size  = 0.3,
                         size=4,
                         color='black',
                         box.padding   = 0.35,
                         segment.color = 'grey50')+
        xlab('Date')+
        ylab('Number of Comments')+
        theme_minimal()+
        geom_line(size=1,color='grey',aes(y=rollmean(n_comments, 100, fill=NA)))+
        geom_hline(yintercept = quantile(dawnTotal_year_s$n_comments,0.99),linetype=2)
dawnComments$date <- as.POSIXct(strptime(dawnComments$date,format='%b %d, %Y %l:%M%p')) #converting comment date time to a valid format



#Comments per article - 2014
dawnArticles$year <- lubridate::year(dawnArticles$date)

dawnArticle_month <- subset(dawnArticles,year==2014) %>%
        group_by(lubridate::month(date)) %>%
        summarise(articles = n(),
                  comments = sum(n_comments))


names(dawnArticle_month)[1] <- 'month'
ggplot(dawnArticle_month,aes(month,comments/articles,group=1))+
        geom_point(aes(size=comments))+
        geom_line(color='grey')+
        geom_line(color= 'green',aes(y=articles))+
        scale_x_continuous(breaks = seq(1,12,1),labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'))+
        theme_minimal()


#comments per article
dawnArticle_year <- subset(dawnArticles,n_comments>0) %>%
        group_by(year) %>%
        summarise(articles = n(),
                  comments = sum(n_comments))


ggplot(dawnArticle_year,aes(year,comments/articles,group=1))+
        geom_point(color='black',aes(size=comments))+
        geom_line(color='grey')+
        geom_line(color= 'green',aes(y=articles))+
        scale_x_continuous(breaks = seq(2011,2018,1))+
        ylab('Comments Per Article')+
        xlab('Year')+
        theme_minimal()


#author with most comments per article

dawnArticles_author <- dawnArticles %>%
        group_by(author) %>%
        summarise(total = n(),
                  a_score_mean = mean(text_score),
                  comments = mean(n_comments))

dawnArticles_author <- subset(dawnArticles_author,!grepl('Report',author)&!grepl('Dawn',author)&!grepl('Newspaper',author)&!grepl('AFP',author)&!grepl('APP',author)&!grepl('AP',author))


a<-subset(dawnArticles_author,total>4 & !grepl('Agencies',author) &!grepl('Entertainment',author) &!grepl('Editorial',author))

ggplot(subset(a, comments > quantile(comments,0.75)),aes(author,comments))+
        geom_bar(stat='identity',aes(fill = author))+
        xlab('Writer')+
        ylab('Comments Per Article')


