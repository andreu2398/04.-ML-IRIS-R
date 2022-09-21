# PACKAGES, LOADING DATA AND CREATION THE VALIDATION DATASET
{
#install.packages("caret")
#install.packages("kernlab")
library(caret)
library(kernlab)
library(randomForest)

#loading the data
data(iris)
dataset <- iris

#create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species , p = 0.8 , list = FALSE)

#select 20% of the data for validation
validation <- dataset[-validation_index,]

#use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]
}

# SUMMARIZE THE DATASET
{
#1. Dimensions of dataset
dim(dataset)

#2. List types for each attribute
sapply(dataset , class)

#3. take a peek at the first 5 rows of data
head(dataset)

#4. List the levels for the class
levels(dataset$Species)

#5. Summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage = percentage)

#6. Summarize attribute distributions
summary(dataset)
}

# VISUALIZE DATASET
{
#1. Split input and output
x <- dataset[,1:4]
y <- dataset[,5]

#2. Boxplot for each attribute on one image
par(mfrow = c(1,4))
for(i in 1:4) {
    boxplot(x[,i] , main = names(iris)[i])
}

#3. Barplot for class breakdown
plot(y)

#4. Scatterplot matrix
featurePlot(x=x , y=y , plot = "ellipse")

#5. Box and whisker plots for each attribute
featurePlot(x=x , y=y , plot = "box")

#6. Density plots for each attribute by class value
scales <- list(x = list(relation = "free") , y = list(relation = "free"))
featurePlot(x=x , y=y , plot = "density" , scales = scales)
}

#EVALUATE SOME ALGORITHMS
{
# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv" , number = 10)
metric <- "Accuracy"

# Build Models
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~. , data = dataset , method = "lda" , metric = metric , trControl = control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~. , data = dataset , method = "rpart" , metric = metric , trControl = control)
# kNN
set.seed(7)
fit.knn <- train(Species~. , data = dataset , method = "knn" , metric = metric , trControl = control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~. , data = dataset , method = "svmRadial" , metric = metric , trControl = control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~. , data = dataset , method = "rf" , metric = metric , trControl = control)

# Summarize accuracy of models
results <- resamples(list(lda = fit.lda , cart = fit.cart , knn=fit.knn , svn = fit.svm , rf = fit.rf))
summary(results)

# Compare accuracy of models
dotplot(results)

# Summarize the Best Model
print(fit.lda)
}

# MAKE PREDICTIONS

# Estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda , validation)
confusionMatrix(predictions , validation$Species)



