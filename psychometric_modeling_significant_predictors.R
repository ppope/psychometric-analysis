#Transform response data into beta-distributed random variables.
#Build beta regressions models for each OCEAN score
#Test significance of 

###################################
### PREPARE DATA FOR REGRESSION ###
###################################

library(betareg)

#Load regression table.
setwd('/home/delores/Desktop/DeepMile/assignment/')
data <- read.csv("regression_table.csv")

#Need to set each variable to their proper types.
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
data.transformed <- data.frame(response.scaled.squeezed, predictors)

#####################
### BUILD MODELS  ###
#####################

#We build models using all available predictors.

model.O2 <- betareg( avgOpennessIntellect ~ 
                      (tweet_negative_word_count +  klout_score), 
                    data=data.transformed, link = "logit")


model.A2 <- betareg( avgAgreeableness ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + hashtag_count + user_mention_count + 
                         klout_score + nchar_tweets + gender), 
                    data=data.transformed, link = "logit")

model.C2 <- betareg( avgConscientiousness ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + hashtag_count + user_mention_count), 
                    data=data.transformed, link = "logit")

model.E2 <- betareg( avgExtraversion ~ gender, 
                    data=data.transformed , link = "logit")

model.N2 <- betareg( avgNeuroticism ~ 
                      (tweet_negative_word_count + tweet_positive_word_count + user_mention_count + favorites_count), 
                    data=data.transformed , link = "logit")


########################
### EVALUATE MODELS  ###
########################


#Partition Data into k=10 subsets
#   1. Create a random permutation of the data.
#   2. Divide into 10 parts (Note: since N=316 and not a multiple of 10, we make the first k-1 partitions of size 32, and the kth partition of size 28.)
#   3. Build models on training set from k-1 partitions
#   4. Calculate training error on the partition left out (sum of squared errors)
#   5. Repeat k times (leaving each partition out once)

k <- 10
set.seed(2)
permuted.inds <- sample(nrow(data.transformed), size=nrow(data.transformed), replace=FALSE)
partition.ind.list <- list()
N <- nrow(data.transformed)
partition.size <- ceiling(N/k)

#Define a list containing indices of each k partition
for (i in 1:k){
  
  if (i == 10) {partition.inds <- ((partition.size*(i-1))+1):N}
  else {partition.inds <- ((partition.size*(i-1))+1):(i*partition.size)}
  partition.ind.list[[i]] <- partition.inds
  
}

#Define list with indices for the training sets (contains k-1 partitions).
#The testing set will be the remaining indices. 
training.inds.list <- list()
for (i in 1:k){
  training.inds.list[[i]] <- unlist(partition.ind.list[-i])
}


error.O <- c()
error.C <- c()
error.E <- c()
error.A <- c()
error.N <- c()

for (i in 1:k){
  
  training.data <- data.transformed[training.inds.list[[i]],]
  testing.data <- data.transformed[-training.inds.list[[i]],]
  
  model.O.cv <- betareg( avgOpennessIntellect ~ 
                        (tweet_negative_word_count +  klout_score), 
                      data=data.transformed, link = "logit")
  
  
  model.A.cv <- betareg( avgAgreeableness ~ 
                        (tweet_negative_word_count + tweet_positive_word_count + hashtag_count + user_mention_count + 
                           klout_score + nchar_tweets + gender), 
                      data=data.transformed, link = "logit")
  
  model.C.cv <- betareg( avgConscientiousness ~ 
                        (tweet_negative_word_count + tweet_positive_word_count + hashtag_count + user_mention_count), 
                      data=data.transformed, link = "logit")
  
  model.E.cv <- betareg( avgExtraversion ~ gender, 
                      data=data.transformed , link = "logit")
  
  model.N.cv <- betareg( avgNeuroticism ~ 
                        (tweet_negative_word_count + tweet_positive_word_count + user_mention_count + favorites_count), 
                      data=data.transformed , link = "logit")
  
  
  predictions.O <- predict(model.O.cv, newdata=testing.data)
  predictions.C <- predict(model.C.cv, newdata=testing.data)
  predictions.E <- predict(model.E.cv, newdata=testing.data)
  predictions.A <- predict(model.A.cv, newdata=testing.data)
  predictions.N <- predict(model.N.cv, newdata=testing.data)
  
  
  error.O[i] <- sum((testing.data$avgOpennessIntellect - predictions.O)^2)
  error.C[i] <- sum((testing.data$avgConscientiousness - predictions.C)^2)
  error.E[i] <- sum((testing.data$avgExtraversion - predictions.E)^2)
  error.A[i] <- sum((testing.data$avgAgreeableness - predictions.A)^2)
  error.N[i] <- sum((testing.data$avgNeuroticism - predictions.N)^2)
  
}

mean.error.O2 <- mean(error.O)
mean.error.C2 <- mean(error.C)
mean.error.E2 <- mean(error.E)
mean.error.A2 <- mean(error.A)
mean.error.N2 <- mean(error.N)

#mean.error.O = 0.5530735
#mean.error.C = 0.811232
#mean.error.E = 0.8235343
#mean.error.A = 0.4978544
#mean.error.N = 0.9378057












