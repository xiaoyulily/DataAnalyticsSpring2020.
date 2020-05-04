setwd("C:\\Users\\Xiaoyu Li\\Desktop\\DA")
data<-read.csv("spotify_top_100_2019.csv")
head(data)
summary(data)
str(data)


# detect missing values
table(is.na(data))

# divide genres into several bigger groups 
unique(data$top.genre)
data["genre"]<-0
for(i in 1:dim(data)[1]){
  if (data[i,"top.genre"]=='canadian pop'|data[i,"top.genre"]=='dance pop'|data[i,"top.genre"]=='pop'|data[i,"top.genre"]=='colombian pop'|data[i,"top.genre"]=='art pop'|data[i,"top.genre"]=='boy band'|data[i,"top.genre"]=='modern rock'|data[i,"top.genre"]=='contemporary country'|data[i,"top.genre"]=='aussietronica'){
    data[i,"genre"]<-"Pop"
  }

  if (data[i,"top.genre"]=='dfw rap'|data[i,"top.genre"]=='country rap'|data[i,"top.genre"]=='rap'|data[i,"top.genre"]=='emo rap'|data[i,"top.genre"]=='pop rap'|data[i,"top.genre"]=='melodic rap'|data[i,"top.genre"]=='florida rap'){
    data[i,"genre"]<-'Rap'
  }

  if (data[i,"top.genre"]=='canadian contemporary r&b'|data[i,"top.genre"]=='alternative r&b'){
    data[i,"genre"]<-'R&B'
}
  if (data[i,"top.genre"]=='canadian hip hop'|data[i,"top.genre"]=='atl hip hop'|data[i,"top.genre"]=='conscious hip hop'|data[i,"top.genre"]=='detroit hip hop'|data[i,"top.genre"]=='hip hop'){
    data[i,"genre"]<-'Hip hop'
}
  
  if (data[i,"top.genre"]=='electronic trap'|data[i,"top.genre"]=='pop house'|data[i,"top.genre"]=='edm'|data[i,"top.genre"]=='big room'|data[i,"top.genre"]=='electropop'|data[i,"top.genre"]=='electro house'|data[i,"top.genre"]=='complextro'|data[i,"top.genre"]=='downtempo'){
    data[i,"genre"]<-'Eletronic'  
}
  if (data[i,"top.genre"]=='latin'){
    data[i,"genre"]<-'Latin'
}
  if (data[i,"top.genre"]=='indie folk'){
    data[i,"genre"]<-'Folk'
  }
}
unique(data$genre)

# plot the distribution of these new genre groups
data["quantity"]<-1
bar<-aggregate(data$quantity,by=list(data$genre),FUN=sum)
names(bar)<-c("genre","quantity")
bar<-bar[order(-bar$quantity),]
barplot(bar$quantity,names.arg = bar$genre,ylim = c(0,50),horiz=F,main="The distribution of new genre groups")

# plot the distribution of the values of popularity 
hist(data$popularity,ylim=c(0,60),xlim=c(0,100))

# plot the heatmap of the correlation between variables
corr<-data[,5:14]
library(corrplot)
corrplot(cor(corr),method="shade",rect.col = "black",tl.col = "black")


# modeling prat 1: cluster
# encoding the genre variable 
model_data<-data[,c(5:15)]
model_data["Genre"]<-0
for(i in 1:100){
  if (model_data[i,11]=="Pop"){
    model_data[i,"Genre"]=1
  }
  if (model_data[i,11]=="Rap"){
    model_data[i,"Genre"]=2
  }
  if (model_data[i,11]=="Eletronic"){
    model_data[i,"Genre"]=3
  }
  if (model_data[i,11]=="R&B"){
    model_data[i,"Genre"]=4
  }
  if (model_data[i,11]=="Hip hop"){
    model_data[i,"Genre"]=5
  }
  if (model_data[i,11]=="Latin"){
    model_data[i,"Genre"]=6
  }
  if (model_data[i,11]=="Folk"){
    model_data[i,"Genre"]=7
  }
}

model_data1<-model_data[,c(-11)]

# remove the outliers
library(dplyr)
model_data1<-filter(model_data1,popularity>40)

# build the Kmeans model
# create a loop function to choose the most suitable value of k
kclusters<-{}
for(i in 1:10){
  kclusters[i]<-kmeans(model_data1,i,nstart= 20)$tot.withinss} 
kclusters

# plot a graph to show the relationship between k and tot.withinss
plot(1:10,kclusters, type="b",xlab= "Number of clusters", ylab= "Within cluster sum of squares",main = "The relationship between k and tot.withinss") 


# construct the kmeans model
kmean <- kmeans(model_data1,4,nstart = 20)

# plot to show the relationship between genre and popularity in each cluster
plot(model_data1[c("popularity", "Genre")], col = kmean$cluster,main="The relationship between genre and popularity")


# modeling part 2: classification
# research the distribution of the values of popularity
summary(model_data1$popularity)
boxplot(model_data1$popularity,main="The distribution of the value of popularity")

# divide the values of popularity into three classes and drop genre column
model_data2<-model_data1[,-11]
model_data2$popularity<-cut(model_data1$popularity, br=c(0,71,79,100), labels = c("less popular", "median popular", "most popular"))

# split the dataset
set.seed(1)
train.rows <- sample(rownames(model_data2), dim(model_data2)[1]*0.7)
train_data<-model_data2[train.rows,]
test.rows<-setdiff(rownames(model_data2),train.rows)
test_data<-model_data2[test.rows,]


# build a knn classifier 
library(class)
KNNpred <- knn(train = train_knn, test = test_knn, cl =train_knn$popularity, k = 10)

# generate confusion matrix of KNN
library(caret)
library(e1071)
confusionMatrix(KNNpred, as.factor(test_knn$popularity))


# build a random forest model
library(randomForest)

# Use a loop function to identify the right mtry for this model
a<-c()
for(i in 2:6){
  model_test<-randomForest(popularity~.,data=train_data,mtry=i,importance=TRUE)
  pred_test<-predict(model_test,test_data, type = "class")
  a[i-1]<-mean(pred_test==test_data$popularity)
}
a

# construct a random forest
model<-randomForest(popularity~.,data=train_data,mtry=2,importance=TRUE)
model

# predict by using random forest
pred<-predict(model,test_data, type = "class")

# generate a confusion matrix of random forest
confusionMatrix(pred, as.factor(test_data$popularity))

# get feature importance 
feature_importance<-model$importance


# build a svm model using a new training dataset after dropping the unrelated variable
train_data2<-train_data[,-4]
test_data2<-test_data[,-4]

svm_model <- svm(popularity~., data =train_data2)
summary(svm_model)

# predict the test data by using svm
pred_svm<-predict(svm_model,test_data2,type="class")

# generate a confusion matrix of svm
confusionMatrix(pred_svm, as.factor(test_data2$popularity))


                