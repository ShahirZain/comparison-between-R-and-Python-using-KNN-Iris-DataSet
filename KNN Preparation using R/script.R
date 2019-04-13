require(ggvis)
require(class)

data("iris")
str(iris)
table(iris$Species)
head(iris)
iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
cor(iris$Petal.Length, iris$Petal.Width)

set.seed(9850)
gp<-runif(nrow(iris))
iris2<- iris[order(gp),]
head(iris2)
summary(iris2[,c(1,2,3,4)])
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}

iris_n <- as.data.frame(lapply(iris2[,c(1,2,3,4)],normalize))
summary(iris_n)
iris_train <- iris_n[1:129,-5]
iris_test<- iris_n[130:150,-5]
iris_train_target <- iris2[1:129,5]
iris_test_target <- iris2[130:150,5]
knn_model_1 <- knn(train=iris_train, test=iris_test, cl = iris_train_target, k=13)
knn_model_1
acc <- data.frame(iris_test_target,knn_model_1)
cm <- as.matrix(table(acc))
sum(diag(cm))/length(iris_test_target)

