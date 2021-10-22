library(ggplot2)
data("iris")

ggplot(iris, aes(
  x=Petal.Length,
  y=Sepal.Length,
  colour=Species))+
geom_point()+
ggtitle("Iris Species by Petal and Sepal Length")

sapply(iris, class)

View(iris)

head(iris)

levels(iris$Species)

#percentage of species
percentage<-prop.table(table(iris$Species))*100
cbind(freq=table(iris$Species), percentage=percentage)

summary(iris)

library(caret)
dataset<-iris

#80% train data
validation_index<-createDataPartition(dataset$Species, p=0.8, list = FALSE)
validation_index

#20% validation data
validation<-dataset[-validation_index,]
validation

#80% for training and test model
dataset<-dataset[validation_index,]

#accomodate numeric data type / column 1-4
x<-dataset[,1:4]
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris))[i]}

#accomodate the column 5 / species
y<-dataset[,5]

library(ellipse)

featurePlot(x=x, y=y, plot = "ellipse")

featurePlot(x=x, y=y, plot = "box")

scales<-list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#estimate the accuracy with 10-fold crossvalidation
control<- trainControl(method="cv", number=10)
metric<-"Accuracy"

#LDA algorithm
set.seed(7)
fit.lda<-train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

##CART algorithm
set.seed(7)
fit.cart<-train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

#kNN algorithm
set.seed(7)
fit.knn<-train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

#SVM algorithm
set.seed(7)
fit.svm<-train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

#Random Forest Algorithm
set.seed(7)
fit.rf<-train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


results<-resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)

print(fit.lda)
print(fit.knn)
print(fit.cart)
print(fit.rf)
print(fit.svm)

predictions<-predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
