library(twitteR)

consumer_key <- "..................."
consumer_secret <- "................................................"
access_token <- "................................................"
access_secret <- "................................................"
#Keys removed

#Sets up Twitter Authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret) 

#Extracting tweets containing the hashtag #AamirLiaquat, for multiple keywords you can use the + sign. The tells the API the number of tweets to pull and the 'since' parameter tells it the date to check for. The last parameter tells it the number of times to retry in case of failure
tw = twitteR::searchTwitter('Imran Khan exclude:retweets', n = 10000, since = '2018-01-01', retryOnRateLimit = 2)

#Converting it into a DataFrame
twDf = twitteR::twListToDF(tw)


