#Author: Sara Shakouri 
#Date : 26-08-2018

#### Import the csv file into R ######
shoes<- read.csv("womens_shoes_listings.csv")

##### Importing required libraries ####
library(data.table)
library(ggplot2)
library(reshape2)
library(caret)
library(dplyr)
library(ranger)
library(plotly)

set.seed(40567)

####### Convering into data table #####
shoes<- as.data.table(shoes)

#### Start visualizing and checking the data first:#######
### Check the feature names ####
name<- names(shoes)

#### Check the distribution of the target variable first #####
table(shoes$IsSold)/length(shoes$IsSold)*100

####    0        1 
#### 55.76623 44.23377 
#### This suggest the target value is pretty much balanced which is an ideal case ######

##### Checking for the missing values in all the features #####

for (i in 1:length(name)){
print(shoes[is.na(name[i]),.N])
}

##### There are no missing values in the features #########

p1<- shoes %>% plot_ly(x=~IsSold , y =~NoOfPhotos , type = "box") 
p2<- shoes %>% plot_ly(x=~IsSold , y =~EstimatedListingValue , type = "box") 
p3<- shoes %>% plot_ly(x=~EstimatedListingValue , type = "histogram") 
p1


#### Building a classifier using only numeric features #######
###### shuffling the data to make sure there is no ordering and biases in data ########## 
rows <- sample(nrow(shoes))
shoes <- shoes[rows, ]
rm(rows)

#### Creatining train and test datasets #########
#### createDataPartition() could also be used from caret package to split the data ######
shoes$IsSold_2<- ifelse(shoes$IsSold==1,"Y","N")
shoes$IsSold_2<- factor(shoes$IsSold_2,levels=c("Y","N"))
shoes$IsSold_2<- relevel(shoes$IsSold_2, ref="Y")


##### For the modeling I'll be using decision trees which are scale invariant, however, 
### the code below can be used to transform the features if another 
##### algorithm such as svm was prefered.

# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# shoes$ReservePrice_trans<- normalize(shoes$ReservePrice)
# shoes$EstimatedListingValue_trans<- normalize(shoes$EstimatedListingValue)

split <- round(nrow(shoes) * .70)

# Create train
mytrain<- shoes[1:split,]

# Create test
mytest<- shoes[(split+1):nrow(shoes),]



##### Check the target variable in the train and test sets #####
table(mytrain$IsSold)

###   0     1 
### 14016 11089 

table(mytest$IsSold)
####   0    1 
#### 5984 4775 

### Looks fine! #####

#### Using XGBoost for modeling ######

xgb_grid_1 <- expand.grid(
  nrounds= c(4,8,10,25,40),
  max_depth = c(2,4,6,8,10,15,25),
  eta=0.1,
  gamma=c(0.3,0.5,0.7,1),
  colsample_bytree = 0.5,
  min_child_weight=1,
  subsample = 0.5
  
)


xgb_trcontrol <- trainControl(
  method="cv",
  number = 5,
  verboseIter = TRUE,
  returnData=FALSE,
  returnResamp = "all",
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)
#mytrain<- as.matrix(mytrain)

xgb_caret <- train(
  IsSold_2 ~ NoOfPhotos + EstimatedListingValue + IsListedByTopSeller + IsPayNow + ReservePrice +
    IsGallery + FreeShipping + IsNew + IsBuyNowOffered,
  data=mytrain,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid_1,
  method="xgbTree",
  metric="ROC"
  
)

pred_xgboost <- predict(xgb_caret,mytest)

confusionMatrix(mytest$IsSold_2,pred_xgboost)

########## Model with random forest with caret package ######################

##########  Create resampling (cross-validation) ################


myControl <- trainControl(
  method = "cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE
)

mymodel_rf <- train(IsSold_2 ~ NoOfPhotos + EstimatedListingValue + IsListedByTopSeller + IsPayNow + ReservePrice +
                      IsGallery + FreeShipping + IsNew + IsBuyNowOffered,
                    data=mytrain,
                    method="ranger",
                    trControl = myControl,
                    metric="ROC",
                    tuneLength=10,
                    importance='impurity')

varImp(mymodel_rf)

pred_rf<- predict(mymodel_rf,mytest)

confusionMatrix(pred_rf,mytest$IsSold_2)

#####  Including the text features ########

###### Feature engeeniering ######
#### 
t<- table(shoes$colour,shoes$style)
t1<- melt(t,varnames = c("Var.2","Var.1"))
t1$Var.1<- as.factor(t1$Var.1)

p<- ggplot(t1, aes(x=Var.1, y=Var.2, fill=value)) + geom_tile() +
  geom_text(aes(label=t1$value), colour="white")+ 
  theme(axis.text.x=element_text(angle=90,size = 15),axis.text.y=element_text(size = 15))+labs(y="Colour",x="Style")
print(p)


 ##### It is obvious from the heat map that style"8901" and colour='#1B1B1B' are very popular. Thus, I've made a new feature
##### called fav_col_size which takes a value of 1 if style and colour are "8901" and '#1B1B1B' and 0 if else. ####
shoes$fav_col_size<- ifelse(shoes$colour=='#1B1B1B' & shoes$style=="8901",1,0)

######## size-colour heat map #########
t<- table(shoes$colour,shoes$size)
t1<- melt(t,varnames = c("Var.2","Var.1"))
t1$Var.1<- as.factor(t1$Var.1)

p<- ggplot(t1, aes(x=Var.1, y=Var.2, fill=value)) + geom_tile() +
  geom_text(aes(label=t1$value), colour="white")+ 
  theme(axis.text.x=element_text(angle=90,size = 15),axis.text.y=element_text(size = 15))+labs(y="Colour",x="Size")
print(p)

#### The size-colour heatmap shows that colour #1B1B1B is popular in all sizes, so this wont enhance the signal and will not be used to 
##### create a new feature.#########

t<- table(shoes$style,shoes$size)
t1<- melt(t,varnames = c("Var.2","Var.1"))
t1$Var.1<- as.factor(t1$Var.1)
t1$Var.2<- as.factor(t1$Var.2)

p<- ggplot(t1, aes(x=Var.1, y=Var.2, fill=value)) + geom_tile() +
  geom_text(aes(label=t1$value), colour="white")+ 
  theme(axis.text.x=element_text(angle=90,size = 15),axis.text.y=element_text(size = 15))+labs(y="style",x="Size")
print(p)
#### The size-style heatmap shows that style 8901 is popular in all sizes, so this wont enhance the signal and will not be used to 
##### create a new feature.#########

shoes$Title<- as.character(shoes$Title)
shoes$brand<- as.factor(shoes$brand)
shoes$IsSold<- as.character(shoes$IsSold)
shoes$IsSold_2<- as.character(shoes$IsSold_2)


shoes_dummy<- dummy.data.frame(shoes, dummy.class="factor" )

shoes_dummy$IsSold<- NULL
shoes_dummy$Title<- NULL
shoes_dummy$IsSold_2<- as.factor(shoes_dummy$IsSold_2)
shoes_dummy$fav_col_size<- ifelse(shoes_dummy$`shoe_color#1B1B1B`==1 & shoes_dummy$shoe_style8901==1,1,0)
shoes_dummy$IsSold_2<- relevel(shoes_dummy$IsSold_2, ref="Y")


split <- round(nrow(shoes_dummy) * .70)

# Create train
mytrain_2<- shoes_dummy[1:split,]

# Create test
mytest_2<- shoes_dummy[(split+1):nrow(shoes_dummy),]



xgb_grid_1 <- expand.grid(
  nrounds= c(4,8,10,25,40),
  max_depth = c(2,4,6,8,10,15,25),
  eta=0.1,
  gamma=c(0.3,0.5,0.7,1),
  colsample_bytree = 0.5,
  min_child_weight=1,
  subsample = 0.5
)


xgb_trcontrol <- trainControl(
  method="cv",
  number = 5,
  verboseIter = TRUE,
  returnData=FALSE,
  returnResamp = "all",
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)


xgb_caret_2 <- train(x = as.matrix(mytrain_2 %>%
                                     select(-IsSold_2)),
                     y = as.factor(mytrain_2$IsSold_2),
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid_1,
  method="xgbTree",
  metric="ROC"
  
)

pred_xgboost_2 <- predict(xgb_caret_2,as.matrix(mytest_2 %>%
                                                  select(-IsSold_2)))
varImp(xgb_caret_2)

test <- chisq.test(as.factor(mytrain_2$EstimatedListingValue), mytrain_2$IsSold_2)
print(test)

confusionMatrix(mytest_2$IsSold_2,pred_xgboost_2)

########## Model with random forest with caret package ######################


colnames(shoes)[11]<- "Shoe_size"
colnames(shoes)[12]<- 'shoe_style'
colnames(shoes)[13]<- 'shoe_color'
shoes$fav_col_size<- ifelse(shoes$shoe_color=='#1B1B1B' & shoes$shoe_style=="8901",1,0)
shoes$IsSold_2<- as.factor(shoes$IsSold_2)
shoes$IsSold_2<- relevel(shoes$IsSold_2, ref="Y")

split <- round(nrow(shoes) * .70)

# Create train
mytrain_2<- shoes[1:split,]

# Create test
mytest_2<- shoes[(split+1):nrow(shoes),]




myControl <- trainControl(
  method = "cv", number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, 
  verboseIter = TRUE
)


mymodel_rf_2 <- train(x= mytrain_2 %>%
                                  select(-IsSold_2,-Title,-IsSold),
                      y= mytrain_2$IsSold_2,
                    method="ranger",
                    trControl = myControl,
                    metric="ROC",
                    tuneLength=14,
                    importance = 'impurity')

plot(varImp(mymodel_rf_2))

pred_rf_2<- predict(mymodel_rf_2,mytest_2 %>%
                      select(-IsSold_2,-Title,-IsSold))

confusionMatrix(pred_rf_2,mytest_2$IsSold_2)


