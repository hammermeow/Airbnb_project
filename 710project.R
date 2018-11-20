library(readr)
output_run <- read_csv("C:/Users/inner/Downloads/Airbnb/output_run.csv")
ratingclass<-output_run
range(output_run$review_scores_rating)
table(output_run$review_scores_rating)
#in the data, rating >=95 6626 rows
hist(output_run$review_scores_rating)
class(ratingclass$review_scores_rating)
ratingclass$review_scores_rating <-ifelse(ratingclass$review_scores_rating>=95,'High','Low')
table(ratingclass$review_scores_rating)
# remove superhost
# ratingclass1=ratingclass[,-3]
ratingclass$review_scores_rating=factor(ratingclass$review_scores_rating)
ratingclass$zipcode=factor(ratingclass$zipcode)
#data for salford
getwd()
write.csv(ratingclass,'C:/Users/inner/Downloads/Airbnb/ratingclass.csv')


#split into train/test sets
set.seed(1)###For reproducible results
trainset=sample(1:nrow(ratingclass), 0.8*nrow(ratingclass))
train=ratingclass[trainset,]
testdata=ratingclass[-trainset,]


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
table(pred.cart,testdata$review_scores_rating)
# pred.cart High  Low
# High 1189  479
# Low   101  138
(1189+138)/1907 #0.6958574

#C5.0 TREE
pred.c50 <- predict(c50fit, newdata = testdata,type="class")
table(pred.c50 ,testdata$review_scores_rating)
# pred.c50 High  Low
# High 1200  470
# Low    90  147
(1200+147)/1907 #0.706345

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
testdata1=testdata[testdata$host_response_time!='None',] 
pred.cart1<- predict(cart1, newdata = testdata1,type="class")
table(pred.cart1,testdata1$review_scores_rating)
# pred.cart1 High Low
# High  980 298
# Low   307 319
(980+319)/1904 #0.6822479


#C5.0 TREE
pred.c501 <- predict(c50fit1, newdata = testdata1,type="class")
table(pred.c501 ,testdata1$review_scores_rating)
# pred.c501 High Low
# High  927 279
# Low   360 338
(927+338)/1904 #0.6643908


#random forest#
library(randomForest)
# sum(is.na(train))
# #https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/na.fail
# train=na.omit(train)
# sum(is.na(train))
library(dplyr)
ratingclass1=ratingclass %>% mutate_if(is.character, as.factor)
RF=randomForest(review_scores_rating ~ .,data =ratingclass1,
                importance=TRUE, 
                proximity=TRUE,
                na.action=na.omit)
RF
importance(RF)        
varImpPlot(RF)

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
p1 <- predict(cart1, newdata = testdata1)
pred1 <- ifelse(p1[,1] > p1[,2], "Pred: High", "Pred: Low")

cart2 <- rpart(review_scores_rating ~.,data =s2,
               minsplit=30, cp=0.01,
               method = "class")

p2 <- predict(cart2, newdata = testdata1)
pred2 <- ifelse(p2[,1] > p2[,2], "Pred: High", "Pred: Low")


cart3 <- rpart(review_scores_rating ~.,data =s3,
               minsplit=30, cp=0.01,
               method = "class")
p3 <- predict(cart3, newdata = testdata1)
pred3 <- ifelse(p3[,1] > p3[,2], "Pred: High", "Pred: Low")

cart4 <- rpart(review_scores_rating ~.,data =s4,
               minsplit=30, cp=0.01,
               method = "class")
p4 <- predict(cart4, newdata = testdata1)
pred4 <- ifelse(p4[,1] > p4[,2], "Pred: High", "Pred: Low")
cart5 <- rpart(review_scores_rating ~.,data =s5,
               minsplit=30, cp=0.01,
               method = "class")
p5 <- predict(cart5, newdata = testdata1)
pred5 <- ifelse(p5[,1] > p5[,2], "Pred: High", "Pred: Low")

preds <- c(pred1, pred2, pred3,pred4, pred5)
recs <- as.integer(names(preds)); fin.pred <- rep(0, dim(testdata1)[1])
for(i in 1:dim(testdata1)[1]){
  t <- table(preds[which(recs==as.integer(rownames(testdata1))[i])])
  fin.pred[i] <- names(t)[t == max(t)]
}
bag.t <- table(fin.pred, testdata1$review_scores_rating) # Contingency table
bag.t



t=table(preds[which(recs==as.integer(rownames(testdata1))[888])])           
fin.pred[888]=names(t)[t == max(t)]
# fin.pred     High  Low
# Pred: High 1165  449
# Pred: Low   122  168
(1165+168)/1904 #0.700105

getwd()
