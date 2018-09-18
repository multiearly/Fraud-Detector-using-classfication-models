library(readr)

card <- read_csv("creditcard.csv")
str(card)

card$Class <- factor(card$Class)
card <- na.omit(card)

#Split Dataset
train.test.split <- sample(2, nrow(card), replace = T,
                           prob = c(0.7, 0.3))
train = card[train.test.split == 1,]
test = card[train.test.split == 2,]

table(test$Class)

#Prediction
glm.model <- glm(Class ~ ., data = train, family = binomial)
glm.predict <- predict(glm.model, test, type = "response")

#Confusion Matrix
matrix = table(test$Class, glm.predict > 0.5)
str(matrix)
acr = (matrix[1,1] + matrix[2,2]) / sum(matrix)
acr
sprintf("Accuracy of logistic regression model is %s percent", round(acr*100,2))
