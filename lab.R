set.seed(12345)
help(par)
par(mar = rep(2,4))
data_Matrix <-matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

par(mar=rep(0.2,4))
heatmap(data_Matrix)

install.packages("titanic")
library(titanic)
devtools::install_github("paulhendricks/titanic")
data(titanic_train)
names(titanic_train)
table(is.na(titanic_train))
titanic<-na.omit(titanic_train)
table(is.na(titanic))
str(titanic)
titanic$Sex<-as.factor(titanic$Sex)
titanic$Embarked<-as.factor(titanic$Embarked)
?as.data.frame.factor


##random forest model
library(randomForest)
titanic_model<-titanic[,c(2,3,5,6,7,8,12)]
model1<-randomForest(Survived~.,data=titanic_model,importance=TRUE)
model1
importance(model1)
varImpPlot(model1)

##decision tree
library(rpart)
library(rpart.plot)
cv.ct <- rpart(Survived~.,data=titanic_model, method = "class",minbucket = 50, cp = 0.00001,maxdepth = 7, xval = 5 )
printcp(cv.ct)
pruned.ct <- prune(cv.ct, cp = 0.00001)
prp(pruned.ct, type = 1, extra = 1, , under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))  


### hclust
out.dist=dist(titanic_model[,-1],method="euclidean")    
cluster<-hclust(out.dist, method = "complete", members=NULL)
plot(cluster)

### ctree
library(party)
ctree.model = ctree(Survived~.,data=titanic_model )
ctree.model
plot(ctree.model)



