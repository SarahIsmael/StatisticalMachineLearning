#source code for logistic regression
set.seed(1) # for reproducible results
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
data <- read.csv("training_data.csv",header=T)
testdata <- read.csv("songs_to_classify.csv",header=T)
data$acousticness <- exp(data$acousticness^2)
testdata$acousticness <- exp(testdata$acousticness^2)
data$speechiness <- exp(data$speechiness)
testdata$speechiness <- exp(testdata$speechiness)
glm.fit <- glm(formula=label~loudness+speechiness+acousticness, data = data, family=binomial)
#cv.err <- cv.glm(data, glm.fit, cost, K=4)$delta
glm.probs <- predict(object = glm.fit, newdata=testdata,type="response")
glm.pred <- rep(0,length(glm.probs))
glm.pred[glm.probs>0.5] <- 1
