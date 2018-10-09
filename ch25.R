# Prepare the data
library(readr)
risk <- read_csv("A_MA710/DMPA_data_sets/Data sets/ClassifyRisk")

choose <- runif(dim(risk)[1], 0, 1)


train <- risk [which(choose <= 0.75),]
test<- risk [which(choose > 0.75),]
# Original CART model for predicting risk
library(rpart)
cart.o <- rpart(risk ~ marital_status+mortgage+loans+income+age,
                data = train,
                method = "class")
p.0 <- predict(cart.o, newdata = test)
pred1 <- ifelse(p.0[,1] > p.0[,2], "Pred: bad loss", "Pred: good risk")
o.t <- table(pred1, test$risk)
o.t

# Bagging model (5 base models)
s1 <- train[sample(dim(train)[1], replace = TRUE),]
# Repeat the above for s2 through s5
s2 <- train[sample(dim(train)[1], replace = TRUE),]
s3 <- train[sample(dim(train)[1], replace = TRUE),]
s4 <- train[sample(dim(train)[1], replace = TRUE),]
s5 <- train[sample(dim(train)[1], replace = TRUE),]

# Repeat the above for s2, s3, s4, s5
cart1 <- rpart(risk ~ marital_status+mortgage+loans+income+age,
               data = s1, method = "class")
p1 <- predict(cart1, newdata = test)
pred1 <- ifelse(p1[,1] > p1[,2], "Pred: bad loss", "Pred: good risk")

cart2 <- rpart(risk ~ marital_status+mortgage+loans+income+age,
               data = s2, method = "class")
p2 <- predict(cart2, newdata = test)
pred2 <- ifelse(p2[,1] > p2[,2], "Pred: bad loss", "Pred: good risk")


cart3 <- rpart(risk ~ marital_status+mortgage+loans+income+age,
               data = s3, method = "class")
p3 <- predict(cart3, newdata = test)
pred3 <- ifelse(p3[,1] > p3[,2], "Pred: bad loss", "Pred: good risk")

cart4 <- rpart(risk ~ marital_status+mortgage+loans+income+age,
               data = s4, method = "class")
p4 <- predict(cart4, newdata = test)
pred4 <- ifelse(p4[,1] > p4[,2], "Pred: bad loss", "Pred: good risk")

cart5 <- rpart(risk ~ marital_status+mortgage+loans+income+age,
               data = s5, method = "class")
p5 <- predict(cart5, newdata = test)
pred5 <- ifelse(p5[,1] > p5[,2], "Pred: bad loss", "Pred: good risk")

preds <- c(pred1, pred2, pred3, pred4, pred5)
recs <- as.integer(names(preds)); fin.pred <- rep(0, dim(test)[1])
for(i in 1:dim(test)[1]){
  t <- table(preds[which(recs==as.integer(rownames(test))[i])])
  fin.pred[i] <- names(t)[t == max(t)]
}
bag.t <- table(fin.pred, test$risk) # Contingency table
bag.t
