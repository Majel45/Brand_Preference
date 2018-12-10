read.csv(surveycomplete)

str(surveycomplete)

surveycomplete [ , 3:5] <- lapply(surveycomplete [ , 3:5], factor)

surveycomplete$brand <- factor(surveycomplete$brand, levels = c(0,1), 
                                 labels = c("Acer", "Sony") )

summary(surveycomplete)

is.na(surveycomplete)

tree <- ctree(surveycomplete$brand ~., surveycomplete, controls = 
                ctree_control(maxdepth = 3))

plot(tree)

ggplot(surveycomplete, aes(x = age, y = salary, 
                             color = brand)) + geom_point()


set.seed(123)

indexes <- createDataPartition(surveycomplete$brand, p= 0.75, list = FALSE)

training <- surveycomplete [indexes, ]

test <- surveycomplete [-indexes, ]

knn.model <- train(brand ~., data = training, method = "knn")

knn.model

knn.predict <- predict(knn.model, newdata = test)

confusionMatrix(test$brand, knn.predict)

knn.model2 <- train(brand ~., data = training, 
                    method = "knn", preProcess = c("center", "scale"))

knn.model2

knn.predict2 <- predict(knn.model2, newdata = test)

confusionMatrix(test$brand, knn.predict2)

incomplete.knn.predict2 <- predict(knn.model2, newdata = SurveyIncomplete)

summary(incomplete.knn.predict2)

postResample(incomplete.knn.predict2, knn.predict2)

knn.model3 <- train(brand ~ salary + age, data = training, method = "knn")

knn.model3

knn.predict3 <- predict(knn.model3, newdata = test)

confusionMatrix(test$brand, knn.predict3)

incomplete.knn.predict3 <- predict(knn.model3, newdata = SurveyIncomplete)

summary(incomplete.knn.predict3)

postResample(incomplete.knn.predict3, knn.predict3)

knn.model4 <- train(brand ~ salary + age, data = training, method = "knn", 
                    preProcess = c("center", "scale"))

knn.model4

knn.predict4 <- predict(knn.model4, newdata = test)

confusionMatrix(test$brand, knn.predict4)

incomplete.knn.predict4 <- predict(knn.model4, newdata = SurveyIncomplete)

summary(incomplete.knn.predict4)

postResample(incomplete.knn.predict4, knn.predict4)

rf.model <- train(brand ~., data = training, method = "rf", prox = TRUE)

rf.model

rf.predict <- predict(rf.model, newdata = test)

confusionMatrix(test$brand, rf.predict)

incomplete.rf.model <- predict(rf.model, newdata = SurveyIncomplete)

summary(incomplete.rf.model)

postResample(incomplete.rf.model, rf.predict)

rf.model2 <- train(brand ~., data = training, method = "rf")

rf.model2

rf.predict2 <- predict(rf.model2, newdata = test)

confusionMatrix(test$brand, rf.predict2)

rf.model3 <- train(brand ~ salary + age, data = training, method = "rf")

rf.model3

rf.predict3 <- predict(rf.model3, newdata = test)

confusionMatrix(test$brand, rf.predict3)

rf.model4 <- train(brand ~ salary + age, data = training, method = "rf", 
                   preProcess = c("center", "scale") )

rf.model4

rf.predict4 <- predict(rf.model4, newdata = test)

confusionMatrix(test$brand, rf.predict4)

rf.model5<- train(brand ~., data = training, method = "rf", 
                   tuneLength = 4 )

rf.model5

rf.predict5 <- predict(rf.model5, newdata = test)

confusionMatrix(test$brand, rf.predict5)

incomplete.rf.model <- predict(rf.model5, newdata = SurveyIncomplete)

summary(incomplete.rf.model)

postResample(incomplete.rf.model, rf.predict5)

str(incomplete)

incomplete [ , 3:5] <- lapply(incomplete[ , 3:5], factor)

incomplete$brand <- factor(incomplete$brand, levels = c(0,1), labels = c("Acer", "Sony"))

summary(incomplete)

prediction <- predict(knn.model4, newdata = incomplete)

summary(prediction)

count <- data.frame(Brand = c("Acer", "Sony"), 
                    Count = c(sum(prediction == "Acer"), sum(prediction == "Sony")))

pie <- ggplot (count, aes(x = "", y = Count, fill = Brand)) +
  geom_bar(width= 1, stat = "identity")

pie <- pie + coord_polar("y", start = 0) + geom_label(count$Count, show.legend = F)


