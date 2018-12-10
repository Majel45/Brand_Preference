surveycomplete [ , 3:5] <- lapply(surveycomplete[ , 3:5], factor)

surveycomplete$brand <- factor(surveycomplete$brand, levels = c(0,1), 
                               labels = c("Acer", "Sony") )

surveycomplete$brand <- as.factor(surveycomplete$brand)

summary(surveycomplete)

is.na(surveycomplete)

na.omit(surveycomplete)

inTrain <- createDataPartition(y= surveycomplete$brand, p= 0.75, list = FALSE)

training <- surveycomplete [inTrain, ]

test <- surveycomplete [-inTrain, ]

dim(training)

set.seed(12345)

folds <- createFolds(y= surveycomplete$brand, k = 10, list = TRUE, returnTrain = TRUE)

sapply(folds, length)

folds [[1]][1:10]

knn.brandmodel <- train(brand ~., data = training, method = "knn")

knn.brandmodel

knn.predict <- predict(knn.brandmodel, newdata = test)

confusionMatrix(test$brand, knn.predict)

SurveyIncomplete [ , 3:5] <- lapply(SurveyIncomplete[ , 3:5], factor)

incomplete.knn.predict <- predict(knn.brandmodel, newdata = SurveyIncomplete)

summary(incomplete.knn.predict)

postResample(incomplete.knn.predict, knn.predict)

library(ISLR)

cutcredit <- cut2(training$credit, g=3)

table(cutcredit)


featurePlot(x= training[, c( "credit", "age", "elevel")], 
            y= training$brand, plot = "pairs")

qplot(salary, age, col = brand, data = training)


plot(training$brand, training$salary, pch = 19, col = "blue", 
     xlab = "brand", ylab = "salary", main = "Correlation between Brand and Salary")

qq + geom_smooth(method = 'lm', formula = y ~ x)

p1 <- qplot(cutcredit, age, data = training, fill = cutcredit)

p1

t1 <- table(cutcredit, training$brand)

t1

prop.table(t1, 1)

qplot(elevel, color = brand, data = training, geom = "density")

plot(knn.brandmodel, uniform= TRUE, main = "KNN Model")




