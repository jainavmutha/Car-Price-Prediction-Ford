## use the library to access reddit API ##
library(RedditExtractoR)

companyX <- "ford"
company<- find_thread_urls(subreddit=)

## get the content from ALL of the urls above ##
onethread <- get_thread_content(company$url)

### make a dataframe of all the comments and write it out to a csv file ###
commentsdf <- onethread$comments
write.csv(commentsdf,"comments.csv")

# install.packages("syuzhet")
library(syuzhet)


rawdata <- read.csv('comments.csv')

sent <- get_sentiment(rawdata$comment,method="syuzhet")
sent2 <- get_sentiment(rawdata$comment,method="afinn")

finaldata <- cbind(rawdata,sent,sent2)
write.csv(finaldata, "fordcomments.csv", row.names=TRUE)

# Define a function to categorize sentiment
categorize_sentiment <- function(value) {
  if (value <= -5) {
    return("very negative")
  } else if (value > -5 && value <= -0.01) {
    return("negative")
  } else if (value > -0.01 && value <= 0.01) {
    return("neutral")
  } else if (value > 0.01 && value <= 5) {
    return("positive")
  } else {
    return("very positive")
  }
}

# Apply the function to create new columns
finaldata$cat_sent <- sapply(finaldata$sent, categorize_sentiment)
finaldata$cat_sent2 <- sapply(finaldata$sent2, categorize_sentiment)

write.csv(finaldata, "caractorizedfordcomments.csv", row.names=TRUE)



# Install necessary libraries if not already installed
install.packages("wordcloud")

# Load the libraries
library(wordcloud)

comments <- finaldata$comment

# Combine all comments into a single character vector
text <- paste(comments, collapse = " ")

# Preprocess the text
text <- tolower(text)
text <- removePunctuation(text)
text <- removeNumbers(text)
text <- removeWords(text, stopwords("english"))
text <- removeWords(text, stopwords("english"))

# Create a word cloud
wordcloud(words = strsplit(text, " ")[[1]], min.freq = 10, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"))