t<- sentiment_by(dawnArticles$title) #Finding Article Title Score
dawnArticles$title_count <- t$word_count
dawnArticles$title_score <- t$ave_sentiment
dawnArticles$title_sd <- t$sd

t<- sentiment_by(dawnArticles$text) #Finding Article Text Score
dawnArticles$text_count <- t$word_count
dawnArticles$text_score <- t$ave_sentiment
dawnArticles$text_sd <- t$sd

s <-sentiment_by(dawnComments$text) #Finding Comment Score
dawnComments$text_count <- s$word_count
dawnComments$text_score <- s$ave_sentiment
dawnComments$text_sd <- s$sd

#Finding out the positive, negative, and neutral words in each comment

for(i in (1:nrow(dawnComments))){
        print(i)
        res <- table(factor(get_sentiment(get_tokens(dawnComments$text[i]),method = 'bing'),levels = c('-1','0','1')))
        
        dawnComments$pos[i] <-as.numeric(res[3])
        dawnComments$neg[i] <-as.numeric(res[2])
        dawnComments$neutral[i] <-as.numeric(res[1])
        
}


dawnTotal <- merge(dawnComments,dawnArticles,by = 'url')



#Mean Sentiment Score for comments per article



dawnTotal_group <- dawnTotal %>%
        group_by(year,date.y,author.y,title) %>%
        summarise(title_score = mean(title_score),
                  article_score = mean(text_score.y),
                  comments_total = n(),
                  score_mean=mean(text_score.x),
                  score_median = median(text_score.x))


b<- dawnTotal_group

ggplot(b,aes(year,score_mean,group = 1))+
        geom_point(color='blue',position=position_jitter(h=0),alpha=1/2,aes(size=comments))+
        geom_line(color='orange',stat='summary',fun.y=median,size=1)+
        geom_line(color='red',stat='summary',fun.y=mean,size=1)+
        scale_y_continuous(breaks = seq(-0.5,0.5,0.1),limits=c(-0.5,0.5))+
        xlab('Year')+
        ylab('Negative Polarity')+
        scale_x_continuous(breaks = seq(2011,2018,1))+
        geom_hline(yintercept=0,linetype=2)

#Mean sentiment score for comments on an article - 2015


dawnTotal_year_s <- subset(dawnTotal_group,year == 2015)

ggplot(dawnTotal_year_s,aes(date.y,score_mean,label=title))+
        geom_point(color='blue',alpha=0.75,aes(size=comments),position = position_jitter(h=0))+
        geom_label_repel(data          = subset(dawnTotal_year_s,(score_mean> quantile(dawnTotal_year_s$score_mean,0.95) | score_mean< quantile(dawnTotal_year_s$score_mean,0.05))),
                         #nudge_y       = 5 - subset(dawnTotal_week,year==2018 & (score_mean> quantile(dawnTotal_week$score_mean,0.95) | score_mean< quantile(dawnTotal_week$score_mean,0.05)))$score_mean,
                         segment.size  = 0.3,
                         size=3,
                         color='black',
                         box.padding   = 0.35,
                         segment.color = 'grey50')+
        geom_line(size=1,color='grey',aes(y=rollmean(score_mean, 7, na.pad=TRUE)))+
        xlab('Time')+
        ylab('Polarity')+
        geom_hline(yintercept=0)+
        geom_hline(color = 'grey',linetype = 2,yintercept=quantile(dawnTotal_year_s$score_mean,0.01))+
        geom_hline(color = 'grey',linetype = 2, yintercept=quantile(dawnTotal_year_s$score_mean,0.99))+
        scale_y_continuous(breaks = seq(-0.4,0.4,0.1),limits=c(-0.45,0.45))+
        scale_x_date(breaks = date_breaks("1 months"),labels=date_format('%B'))+
        theme_classic()

# Mean sentiment score for comments on an article Vs Year


dawnTotal_year_s <- dawnTotal_group

ggplot(dawnTotal_year_s,aes(date.y,score_mean,label=title))+
        geom_point(color='blue',alpha=0.75,aes(size=comments),position = position_jitter(h=0))+
        geom_label_repel(data          = subset(dawnTotal_year_s,(score_mean> quantile(dawnTotal_year_s$score_mean,0.99) | score_mean< quantile(dawnTotal_year_s$score_mean,0.01))),
                         #nudge_y       = 5 - subset(dawnTotal_week,year==2018 & (score_mean> quantile(dawnTotal_week$score_mean,0.95) | score_mean< quantile(dawnTotal_week$score_mean,0.05)))$score_mean,
                         segment.size  = 0.3,
                         size=3,
                         color='black',
                         box.padding   = 0.35,
                         segment.color = 'grey50')+
        geom_line(size=1,color='grey',aes(y=rollmean(score_mean, 7, na.pad=TRUE)))+
        xlab('Time')+
        ylab('Polarity')+
        geom_hline(yintercept=0)+
        geom_hline(color = 'grey',linetype = 2,yintercept=quantile(dawnTotal_year_s$score_mean,0.01))+
        geom_hline(color = 'grey',linetype = 2, yintercept=quantile(dawnTotal_year_s$score_mean,0.99))+
        scale_y_continuous(breaks = seq(-0.4,0.4,0.1),limits=c(-0.45,0.45))+
        scale_x_date(breaks = date_breaks("1 years"),labels=date_format('%Y'))+
        theme_classic()

#Article Score Vs Comment Score


#left
ggplot(dawnTotal_group,aes(article_score,score_mean,size=comments))+
        geom_point(color = 'blue',alpha=1/3)+
        xlab('Article Polarity')+
        ylab('Comments Polarity')+
        scale_x_continuous(breaks = seq(-0.4,0.4,0.1), limits = c(-0.4,0.4))+
        scale_y_continuous(breaks = seq(-0.4,0.4,0.1),limits = c(-0.4,0.4))+
        theme_linedraw()+
        geom_hline(yintercept = 0,linetype = 2,color='grey')+
        geom_vline(xintercept = 0, linetype = 2, color='grey')

#right
ggplot(subset(dawnTotal_group,article_score > 0.1 | article_score < (-0.1)),aes(article_score,score_mean,size=comments))+
        geom_point(color = 'blue',alpha=1/3)+
        xlab('Article Polarity')+
        ylab('Comments Polarity')+
        scale_x_continuous(breaks = seq(-0.4,0.4,0.1), limits = c(-0.4,0.4))+
        scale_y_continuous(breaks = seq(-0.4,0.4,0.1),limits = c(-0.4,0.4))+
        theme_linedraw()+
        geom_hline(yintercept = 0,linetype = 2,color='grey')+
        geom_vline(xintercept = 0, linetype = 2, color='grey')

# Author Polarity



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

dawnArticles_author_c <- dawnTotal %>%
        group_by(author.y) %>%
        summarise(total = n(),
                  total = n(),
                  score = mean(text_score.x))

x <- merge(dawnArticles_author,dawnArticles_author_c,by.x='author',by.y='author.y')


y<- subset(x,!grepl('Desk',author) &!grepl('Corresp',author) & !grepl('Agencies',author) &!grepl('Entertainment',author) &!grepl('Editorial',author)&!grepl('Report',author)&!grepl('Dawn',author)&!grepl('Newspaper',author)&!grepl('AFP',author)&!grepl('APP',author)&!grepl('AP',author))
names(y) <- c('author','total','score_article','avg_comments','total_comments','score_comment')

# Authors with the most positive comments
y<- subset(x,!grepl('Desk',author) &!grepl('Corresp',author) & !grepl('Agencies',author) &!grepl('Entertainment',author) &!grepl('Editorial',author)&!grepl('Report',author)&!grepl('Dawn',author)&!grepl('Newspaper',author)&!grepl('AFP',author)&!grepl('APP',author)&!grepl('AP',author))
names(y) <- c('author','total','score_article','avg_comments','total_comments','score_comment')
y<- subset(y,total>3)
y<- subset(y,quantile(score_comment,0.75)<score_comment)

a <- melt(y,id.vars= c('author'))

a<- subset(a,variable == 'score_article' | variable =='score_comment' )

ggplot(a,aes(author,value, fill = variable))+
        geom_bar(stat='identity',position = 'dodge')+
        xlab('Writer')+
        ylab('Polarity')

# Authors with the most negative comments
y<- subset(x,!grepl('Desk',author) &!grepl('Corresp',author) & !grepl('Agencies',author) &!grepl('Entertainment',author) &!grepl('Editorial',author)&!grepl('Report',author)&!grepl('Dawn',author)&!grepl('Newspaper',author)&!grepl('AFP',author)&!grepl('APP',author)&!grepl('AP',author))
names(y) <- c('author','total','score_article','avg_comments','total_comments','score_comment')
y<- subset(y,total>3)
y<- subset(y,quantile(score_comment,0.25)>score_comment)

a <- melt(y,id.vars= c('author'))

a<- subset(a,variable == 'score_article' | variable =='score_comment' )

ggplot(a,aes(author,value, fill = variable))+
        geom_bar(stat='identity',position = 'dodge')+
        xlab('Writer')+
        ylab('Polarity')

#