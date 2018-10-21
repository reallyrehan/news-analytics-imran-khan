
#LOADING PACKAGES
library('rvest')
library('dplyr')

#FUNCTION TO CONVERT DATE LATER
get_date <- function(x){
        if(grepl('Updated ',x)){
                x <-substr(x,9,nchar(x))
        }
        x <- as.Date(x,'%B %d, %Y')
        return(x)
}



links <- read.csv('Project1/dawnlinks.csv') #Reading links from the CSV
links<-distinct(links)# in case links arent unique #Removing any duplicates



#Starting the loop to iterate through all the links we collected

for(i in 1:nrow(links)){
        print(i)
        
        url<- as.character(links[i,1])
        webpage <- read_html(url) #reading the link
        
        
        article_writer<- html_text(html_nodes(webpage,'.float-sm-left a'))[1] #Picks the article author
        article_text <- html_text(html_nodes(webpage,'.modifier--blockquote-center-narrow')) #Picks the article text
        article_title <- html_text(html_nodes(webpage,'.story__link'))[1] #Picks the article Title
        article_date <- get_date(html_text(html_nodes(webpage,'.font--arial'))[2]) #Picks the article date and converts it into a date format
        
        if(length(article_writer)==0){ #EXCEPTIONS where article writer isn't mentioend
                article_writer<-''
        }
        
        df <- data.frame(article_title,article_writer,paste(article_text,collapse="\n"),article_date,url) #Making a data frame for the Article with the title, Writer, Date, URL
        names(df) <- c('title','author','text','date','url')
        df$articleIndex <- i
        
        comment_author <- html_text(html_nodes(webpage,'.comment__author')) #Picking the comment writers
        comment_text <- html_text(html_nodes(webpage,'.comment__body')) #Picking the comments
        comment_date <- html_text(html_nodes(webpage,'.comment__date')) #Picking the comment dates
        
        df$n_comments <- length(comment_text) #Adding number of comments for each article to the article data frame
        
        
        if(i == 1){
                articles_total<- df 
        }
        else{
                articles_total <- rbind(articles_total,df)
        }
        
        
        if(length(comment_text) == 0 ){
                next
        }
        
        articleIndex <- i 
        type <- 'comment'
        df <- data.frame(articleIndex,type,comment_date,comment_author,comment_text,url) #Making a comments data frame
        names(df)<- c('articleIndex','type','date','author','text','url')
        
        if(i == 1){
                comments_total<- df
        }
        else{
                comments_total <- rbind(comments_total,df)
        }
        
}




save(comments_total,file = 'Project1/dawn_comments_total_new.Rda') #Saving comments
save(articles_total,file= 'Project1/dawn_articles_total_new.Rda') #Saving Articles