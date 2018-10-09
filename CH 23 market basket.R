library(arules)
testing<- as(Adult2[,c(6,7,8,10)], "transactions")
rules<- apriori(testing,
                parameter=list(supp=0.15,
                               conf=0.80,
                               maxlen=3))
inspect(sort(rules))

library(arules)
heart$sex <-factor(heart$sex)
heart$fbs <-factor(heart$fbs)
heart$exang <-factor(heart$exang)
heart$num<-factor(heart$num)
hearttest<- as(heart[,c(2,6,9,14)], "transactions")
rules<- apriori(hearttest,
                parameter=list(supp=0.15,
                               conf=0.70,
                               maxlen=3))
inspect(sort(rules))
