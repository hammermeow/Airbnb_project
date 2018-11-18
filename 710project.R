library(readr)
output_run <- read_csv("C:/Users/inner/Downloads/output_run.csv")
ratingclass<-output_run
range(output_run$review_scores_rating)
table(output_run$review_scores_rating)
#in the data, rating >=95 6626 rows
hist(output_run$review_scores_rating)
class(ratingclass$review_scores_rating)
ratingclass$review_scores_rating <-ifelse(ratingclass$review_scores_rating>=95,'High','Low')
getwd()
write.csv(ratingclass,'C:/Users/inner/Downloads/ratingclass.csv')
#data for salford

#remove superhost
ratingclass1=ratingclass[,-3]
ratingclass1$review_scores_rating=factor(ratingclass1$review_scores_rating)


#split into train/test sets
set.seed(1)###For reproducible results
trainset=sample(1:nrow(ratingclass1), 0.8*nrow(ratingclass1))
train=ratingclass1[trainset,]
testdata=ratingclass1[-trainset,]


#CART TREE
library(rpart)
cart <- rpart(review_scores_rating ~.,data =train,
               minsplit=30, cp=0.01,
               method = "class")
print(cart)
library("rpart.plot")
rpart.plot(cart,  main ="Classification Tree")


#C5.0 TREE
library("C50")
c50fit <- C5.0(review_scores_rating ~.,data =train, rules = TRUE)
summary(c50fit)



#test models
#CART TREE
pred.cart<- predict(cart, newdata = testdata,type="class")
### factor neighbourhood_cleansed has new levels Lincoln Park # only 1 property
testdata1=testdata[testdata$neighbourhood_cleansed!= "Lincoln Park",]
pred.cart<- predict(cart, newdata = testdata1,type="class")
table(pred.cart,testdata1$review_scores_rating)
#pred.cart High  Low
#     High 1196  486
#     Low    93  131
(1196+131)/1907 #0.6958574

#C5.0 TREE
pred.c50 <- predict(c50fit, newdata = testdata1,type="class")
table(pred.c50 ,testdata1$review_scores_rating)
#pred.c50 High  Low
#   High  1203  451
#   Low     86  166
(1203+166)/1907 #0.7178815

#over sampling
library(ROSE)
table(train$review_scores_rating)
data.balanced.ou <- ovun.sample(review_scores_rating~., data=train,
                                N=nrow(train), p=0.5, 
                                seed=1, method="both")$data
table(data.balanced.ou$review_scores_rating)
data.balanced.under <- ovun.sample(review_scores_rating~., data=train, 
                                   p=0.5, seed=1, 
                                   method="under")$data

table(data.balanced.under$review_scores_rating)
data.balanced.over <- ovun.sample(review_scores_rating~., data=train, 
                                  p=0.5, seed=1, 
                                  method="over")$data

table(data.balanced.over$review_scores_rating)



#CART TREE
library(rpart)
cart1 <- rpart(review_scores_rating ~.,data =data.balanced.over,
                minsplit=30, cp=0.01,
                method = "class")
print(cart1)
library("rpart.plot")
rpart.plot(cart1,  main ="Classification Tree")


#C5.0 TREE
library("C50")
c50fit1 <- C5.0(review_scores_rating ~.,data =data.balanced.over, rules = TRUE)
summary(c50fit1)


#test models
#CART TREE
pred.cart1<- predict(cart1, newdata = testdata,type="class")
#factor host_response_time has new levels None # 3 properties
testdata2=testdata[testdata$host_response_time!='None',] 

pred.cart1<- predict(cart1, newdata = testdata2,type="class")
#factor neighbourhood_cleansed has new levels Lincoln Park
testdata2=testdata2[testdata2$neighbourhood_cleansed!= "Lincoln Park",]#1 property

pred.cart1<- predict(cart1, newdata = testdata2,type="class")
table(pred.cart1,testdata2$review_scores_rating)
#pred.cart1 High Low
#     High  836 238
#     Low   450 379
(836+379)/1903 #0.6384656


#C5.0 TREE
pred.c501 <- predict(c50fit1, newdata = testdata2,type="class")
table(pred.c501 ,testdata2$review_scores_rating)
#pred.c501 High Low
#     High  931 285
#     Low   355 332
(931+332)/1903


#random forest###problem with bootstrap sampling
library(randomForest)
sum(is.na(train))
#https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/na.fail
train=na.omit(train)
sum(is.na(train))
RF=randomForest(review_scores_rating ~.,data =train)
RF


# Bagging model (5 base models)
set.seed(2)
s1 <- train[sample(dim(train)[1], replace = TRUE),]
# Repeat the above for s2 through s5
s2 <- train[sample(dim(train)[1], replace = TRUE),]
s3 <- train[sample(dim(train)[1], replace = TRUE),]
s4 <- train[sample(dim(train)[1], replace = TRUE),]
s5 <- train[sample(dim(train)[1], replace = TRUE),]

# Repeat the above for s2, s3, s4, s5
cart1 <- rpart(review_scores_rating ~.,data =s1,
               minsplit=30, cp=0.01,
               method = "class")
p1 <- predict(cart1, newdata = testdata)
testdata3=testdata[testdata$host_response_time!='None',]
p1 <- predict(cart1, newdata = testdata3)
testdata3=testdata3[testdata3$neighbourhood_cleansed!='Lincoln Park'& 
                     testdata3$neighbourhood_cleansed!= 'Terra Nova',]
p1 <- predict(cart1, newdata = testdata3)
pred1 <- ifelse(p1[,1] > p1[,2], "Pred: High", "Pred: Low")

cart2 <- rpart(review_scores_rating ~.,data =s2,
               minsplit=30, cp=0.01,
               method = "class")
p2 <- predict(cart2, newdata = testdata)
testdata4=testdata[testdata$host_response_time!='None',]
p2 <- predict(cart2, newdata = testdata4)
testdata4=testdata4[!(testdata4$neighbourhood_cleansed %in% c('Egger Highlands', 'Lincoln Park', 'Rancho Del Rey')),]
p2 <- predict(cart2, newdata = testdata4)
pred2 <- ifelse(p2[,1] > p2[,2], "Pred: High", "Pred: Low")


cart3 <- rpart(review_scores_rating ~.,data =s3,
               minsplit=30, cp=0.01,
               method = "class")
p3 <- predict(cart3, newdata = testdata)
testdata5=testdata[testdata$host_response_time!='None',]
p3 <- predict(cart3, newdata = testdata5)
testdata5=testdata5[!(testdata5$neighbourhood_cleansed %in% c('Lincoln Park', 'Rancho Del Rey')),]
p3 <- predict(cart3, newdata = testdata5)
pred3 <- ifelse(p3[,1] > p3[,2], "Pred: High", "Pred: Low")


testdata6=testdata4[!(testdata4$neighbourhood_cleansed %in% 
                        c('Egger Highlands', 'Lincoln Park', 'Rancho Del Rey','Terra Nova')),]
p1 <- predict(cart1, newdata = testdata6)
pred1 <- ifelse(p1[,1] > p1[,2], "Pred: High", "Pred: Low")
p2 <- predict(cart2, newdata = testdata6)
pred2 <- ifelse(p2[,1] > p2[,2], "Pred: High", "Pred: Low")
p3 <- predict(cart3, newdata = testdata6)
pred3 <- ifelse(p3[,1] > p3[,2], "Pred: High", "Pred: Low")


preds <- c(pred1, pred2, pred3)
recs <- as.integer(names(preds)); fin.pred <- rep(0, dim(testdata6)[1])
for(i in 1:dim(testdata6)[1]){
  t <- table(preds[which(recs==as.integer(rownames(testdata6))[i])])
  fin.pred[i] <- names(t)[t == max(t)]
}
bag.t <- table(fin.pred, testdata6$review_scores_rating) # Contingency table
bag.t



t=table(preds[which(recs==as.integer(rownames(testdata6))[888])])           
fin.pred[888]=names(t)[t == max(t)]
#fin.pred     High  Low
#  Pred: High 1170  463
#  Pred: Low   112  152

(1170+152)/1897 #0.6968898
