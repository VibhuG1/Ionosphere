library(rpart)
library(mlbench)
data("Ionosphere")
head(Ionosphere)
nrow(Ionosphere)

set.seed(42)


Ionosphere[,"train"] <- ifelse(runif(nrow(Ionosphere))<0.8,1,0)

trainset <- Ionosphere[Ionosphere$train==1,]
testset <- Ionosphere[Ionosphere$train==0,]

trainColNum <- grep("train",names(trainset))

trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]


typeColNum <- grep("Class",names(Ionosphere))

rpart_model <- rpart(Class~.,data = trainset, method="class")


plot(rpart_model);text(rpart_model)

rpart_model

rpart_predict <- predict(rpart_model,testset[,-typeColNum],type="class")
mean(rpart_predict==testset$Class)

#confusion matrix
table(pred=rpart_predict,true=testset$Class)


#cost-complexity pruning

printcp(rpart_model)
opt <- which.min(rpart_model$cptable[,"xerror"])
opt

cp <- rpart_model$cptable[opt, "CP"]

cp
#prune tree
pruned_model <- prune(rpart_model,cp)

#plot tree
plot(pruned_model);
text(pruned_model)
pruned_model


#proportion of correct predictions using test set
rpart_pruned_predict <- predict(pruned_model,testset[,-typeColNum],type="class")
mean(rpart_pruned_predict==testset$Class)



