# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
gss_tbl<-read_sav("../data/GSS2016.sav")%>% #it seems like all the IAP and DKs are already marked as NA's?
  filter(!is.na(MOSTHRS))%>% #remove NAs from MOSTHRS
  rename(`work hours`=MOSTHRS)%>% #renamed column
  select(-HRS1,-HRS2)%>% #removed these 2 columns
  select(where(~mean(is.na(.))<0.75))%>% #removed columns that had less than 75% NA's
  sapply(as.numeric)%>% #don't know why this fixes stuff downstream, but nothing worked.
  as_tibble()

# Visualization
ggplot(gss_tbl,aes(`work hours`))+ #plotted as a histogram
  geom_histogram()

# Analysis 
set.seed(12) #setting seed
rows<-sample(nrow(gss_tbl)) #shuffle rows
gss_tbl_shuffled<-gss_tbl[rows,] 
split<-round(nrow(gss_tbl)*0.75) #creating row to split on
train_tbl<-gss_tbl_shuffled[1:split,] #creating train vs. test
test_tbl<-gss_tbl_shuffled[(split+1):nrow(gss_tbl),]

# keeping the folds the same across models
folds<-createFolds(train_tbl$`work hours`, 10)

# setting the trControl, cross validation 10 fold 
controlled_things<-trainControl(method="cv", 
                                number=10, 
                                verboseIter=T,
                                indexOut = folds
                                search='grid')
# OLS Regression model
ols_reg<- train(`work hours` ~ .,
  train_tbl,
  method = "lm",
  na.action=na.pass, #take out nas 
  metric = "Rsquared",
  preProcess=c("center","scale","medianImpute"), #impute median
  trControl=controlled_things)
#Fitting final model on full training set
p_ols<-predict(ols_reg,test_tbl,na.action=na.pass)#predicting
r2_ols<-cor(p_ols,test_tbl$`work hours`)^2 #getting r2

# Elastic Net
elastic_net<- train(
  `work hours`~.,
  train_tbl,
  metric = "Rsquared",
  preProcess=c("center","scale","medianImpute"),
  method = "glmnet",
  na.action=na.pass,
  trControl=controlled_things,
  tuneGrid = expand.grid(alpha = c(0,1), lambda=seq(0.0001,0.1,length=10))#took from slides
)
#Fitting alpha = 1, lambda = 0.1 on full training set
p_elastic<-predict(elastic_net,test_tbl,na.action=na.pass)#predicting
r2_elastic<-cor(p_elastic,test_tbl$`work hours`)^2#getting r2

# Random Forest
random_forest<- train(
  `work hours`~.,
  train_tbl,
  metric = "Rsquared",
  preProcess=c("center","scale","medianImpute"),
  method = "ranger",
  na.action=na.pass,
  trControl=controlled_things,
  tuneGrid = expand.grid(mtry = c(2, 3, 7, 10, 20, 50,100, 150), splitrule = "variance", min.node.size = 5) #used the datacamp logic
)
#Fitting mtry = 100, splitrule = variance, min.node.size = 5 on full training set
p_rf<-predict(random_forest,test_tbl,na.action=na.pass)#predicting
r2_rf<-cor(p_rf,test_tbl$`work hours`)^2#getting r2

# eXtreme Gradient Boosting
eXtreme<- train(
  `work hours`~.,
  train_tbl,
  metric = "Rsquared",
  preProcess=c("center","scale","medianImpute"),
  method = "xgbLinear",
  na.action=na.pass,
  trControl=controlled_things,
  tuneGrid = expand.grid(nrounds=c(2,5,7),
                         alpha = c(0,0.5,1),
                         lambda=c(0,0.5,1),
                         eta=c(0.5,1))#found online example
)
#Fitting nrounds = 7, lambda = 0, alpha = 0.5, eta = 0.5 on full training set
p_eX<-predict(eXtreme,test_tbl,na.action=na.pass)#predicting
r2_eX<-cor(p_eX,test_tbl$`work hours`)^2#getting r2

# Publication
#first making separate vectors 
cv_r2<-c(max(ols_reg$results$Rsquared),
               max(elastic_net$results$Rsquared),
              max(random_forest$results$Rsquared),
               max(eXtreme$results$Rsquared))#make vector of cv r squareds
ho_r2<-c(r2_ols,r2_elastic,r2_rf,r2_eX)#make vector of holdout r squared
names<-c("OLS regression", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting")
#creating table
table1_tbl<-tibble(algo=names,
                   cv_rsq=str_remove(formatC(cv_r2, format="f",digits=2),"^0"),
                   ho_rsq=str_remove(formatC(ho_r2, format="f",digits=2),"^0"))
  
# 1. The ols regression model was the worst model, by far, while the other models were pretty similar. This suggests that the ols model is not the ideal model to use for this data, and this might have to do with this model being overly simple for the data. As the data is pretty extensive and vary quite widely from questions about marriage status to questions about work hours, a more complicated model is probably needed for this endeavor.
# 2. The R squared of the holdouts were significantly lower (around half) compared to the kfolds. Thus, the model had a harder time with data it was not familiar with at all. It seems this may be due to us trading bias for lower out of sample predictive accuracy. 
# 3. I would use the random forest model because out of all of the other models, it performed the highest on the test data (despite the CV R squared being relatively similar across elastic net, random forest, and XGB). This may be because I used better hyperparameters, though, that I got from the slides (versus the hyperparameters I got from datacamp or the internet). Irregardless, I felt more confident that this model worked better than the others.