library(betareg)

#Load processed data
setwd('/home/delores/Desktop/DeepMile/')
data <- read.csv("regression_table.csv")

response <- data[,1:5]
predictors <- data[6:ncol(data)]

#We transform the response variables into beta distributed random variables.

response <- apply(response, 2, as.numeric)
#First we transform the variables into [0,1]
#y' = (y – a)/(b – a), where 
#b = "highest possible score"
#a = "lowest possible score"

a <- 1
b <- 5
response.scaled <- (response - a)/(b - a)

#Values of y=1 or y=0 are not permitted in beta regression, 
#so we use the transformation recommend by Smithson and Verkuilen (2006):
#y'' = (y'*(N – 1) + 1/2)/N
#See link for details:
#http://psychology3.anu.edu.au/people/smithson/details/betareg/Smithson_Verkuilen06.pdf
N <- nrow(response)
response.scaled.squeezed <- data.frame((response.scaled*(N - 1) + 1/2)/N)


ind <- which(names(predictors) == 'gender')
regression.table <- data.frame(response.scaled.squeezed, predictors[-ind])



model.O <- betareg( avgOpennessIntellect ~ 
                    (tweet_negative_word_count + tweet_positive_word_count + profile_negative_word_count + 
                    profile_positive_word_count + hashtag_count + user_mention_count + favorites_count + 
                    followers_count + klout_score + nchar_profile + nchar_tweets), 
                    data=regression.table, link = "logit")


model.A <- betareg( avgAgreeableness ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + profile_negative_word_count + 
                      profile_positive_word_count + hashtag_count + user_mention_count + favorites_count + 
                      followers_count + klout_score + nchar_profile + nchar_tweets), 
                      data=regression.table, link = "logit")

model.C <- betareg( avgConscientiousness ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + profile_negative_word_count + 
                      profile_positive_word_count + hashtag_count + user_mention_count + favorites_count + 
                      followers_count + klout_score + nchar_profile + nchar_tweets), 
                      data=regression.table, link = "logit")

model.E <- betareg( avgExtraversion ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + profile_negative_word_count + 
                      profile_positive_word_count + hashtag_count + user_mention_count + favorites_count + 
                      followers_count + klout_score + nchar_profile + nchar_tweets), 
                      data=regression.table , link = "logit")

model.N <- betareg( avgNeuroticism ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + profile_negative_word_count + 
                      profile_positive_word_count + hashtag_count + user_mention_count + favorites_count + 
                      followers_count + klout_score + nchar_profile + nchar_tweets), 
                      data=regression.table , link = "logit")

table.O <- summary(model.O)$coefficients$mean
table.C <- summary(model.C)$coefficients$mean
table.E <- summary(model.E)$coefficients$mean
table.A <- summary(model.A)$coefficients$mean
table.N <- summary(model.N)$coefficients$mean













