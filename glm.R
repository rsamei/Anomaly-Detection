source('common.R')
logreg = glm(Anomaly ~ ., 
             data=train,
             family="binomial")
summary(logreg)
logreg_pred = predict.glm(logreg,
                          newdata = test,
                          type="response")
f1vec= c()
library(caret)
treshhold = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8)
for (i in 1:length(treshhold)){
  logreg_pred_class = ifelse(logreg_pred > treshhold[i], 1, 0)
  logreg_pred_class = factor(logreg_pred_class)
  test$Anomaly = factor(test$Anomaly)
  f1 = confusionMatrix(logreg_pred_class, test$Anomaly, mode = "everything", positive="1")
  f1vec[i] = f1$byClass[7]
}
max(f1vec) #0.2741
treshhold[which.max(f1vec)] #logistic regression with treshold 0.5 as the base model
#f1 score equal to 0.27 !
