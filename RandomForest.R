source('common.R')
library(tidymodels)
train$Anomaly = factor(train$Anomaly)

rf_spec = rand_forest(mtry = sqrt(ncol(train)),trees = 500) %>% 
  set_engine('ranger', importance = 'permutation') %>% 
  set_mode('classification')
rf_fit =rf_spec %>%  fit(Anomaly ~ ., data = train[,-1])
test = na.omit(test)
rf_pred = rf_fit %>% 
  predict(test)
data.frame(actual= factor(test$Anomaly),
           pred = rf_pred$.pred_class) %>% 
  conf_mat(actual,pred)
prop.table(table(train$Anomaly))
classification_perf = function(pred,obs){
  overallerrorrate = mean(pred!=obs)
  acc = mean(pred == obs)
  cf = table(pred,obs)
  sensitivity = cf[2,2]/sum(cf[,2])
  specificity = cf[1,1]/sum(cf[,1])
  percision = cf[2,2] / sum(cf[2,])
  f1 = 2 * ((percision * sensitivity) / (percision + sensitivity))
  return(c(overallerr = overallerrorrate,
           sens = sensitivity,
           spec = specificity,
           perc = percision,
           accuracy = acc,
           f1_score = f1
  ))
}
classification_perf(rf_pred$.pred_class,factor(test$Anomaly))
#overallerr sensitivity specificity percision   accuracy   f1_score 
#0.1179648  0.6813320  0.9169191  0.5876926  0.8820352  0.6310575
library(vip) 
vip(rf_fit) # RollingSD_800 most important feature
