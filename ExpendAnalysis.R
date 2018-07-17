# Load Libraries
library(caret)
library(ellipse)
library(e1071)
library(randomForest)
library(plyr)
library(sp)


# Load Data
RawImport <- read.delim("DataExp2.txt", header = TRUE)

# Evaluate Load
head(RawImport)
summary(RawImport)
classes <- c()
for (i in 1:5) {classes[i] <- as.vector(class(RawImport[,i]))}
classes


# Clean Load
CleanImport <- RawImport
CleanImport[,1] <- as.factor(CleanImport[,1])
CleanImport[,2] <- as.factor(CleanImport[,2])
cleanclasses <- c()
for (i in 1:5) {cleanclasses[i] <- as.vector(class(CleanImport[,i]))}
cleanclasses
CleanImport[,5] <- CleanImport[,5]*-1
head(CleanImport)
colnames(CleanImport)[1] <- "Date"
colnames(CleanImport)[2] <- "DoW"
colnames(CleanImport)[5] <- "Exp"
remove(i,classes,cleanclasses)
drops <- c("Date")
CleanImport <- CleanImport[ , !(names(CleanImport) %in% drops)]


# Partition Data
ValIndex <- createDataPartition(CleanImport$DoW, p = .85, list = FALSE)
ValiData <- CleanImport[-ValIndex,]
TrainData <- CleanImport[ValIndex,]


#Data evaluation
dim(TrainData)
sapply(TrainData, class)
head(TrainData)
summary(TrainData)
levels(TrainData$DoW)


# Review x and y variables
TrainDataX <- TrainData[,2:4]
TrainDataY <- TrainData[,1]
featurePlot(x = TrainDataX,
            y = TrainDataY,
            plot = "ellipse"
            )
featurePlot(x = TrainDataX,
            y = TrainDataY,
            plot = "density",
            scales = list(x = list(relation = "free"),
                          y = list(relation = "free")
                          )
            )


# Split train data for 9/1 Train/Test with Cross Validation
# and assign Accuracy as success metric
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"


# Build Models
# Linear Discriminant Analysis (LDA)
# LDA can't handle the Logical second column,
# So removed here
set.seed(19)
TrainDataLDA <- TrainData[,c(1,3:4)]
fit.lda <- train(
        DoW ~ ., 
        data = TrainDataLDA, 
        method = "lda",
        metric = metric,
        trControl = control)

# Classification and Regression Trees (CART)
set.seed(19)
fit.cart <- train(
        DoW ~ ., 
        data=TrainData, 
        method = "rpart",
        metric = metric,
        trControl = control)

# k-Nearest Neighbors (kNN)
set.seed(19)
fit.knn <- train(
        DoW ~ ., 
        data=TrainData, 
        method = "knn",
        metric = metric,
        trControl = control)

# Support Vector Machines (SVM) with a linear kernel
set.seed(19)
fit.svm <- train(
        DoW ~ ., 
        data=TrainData, 
        method = "svmRadial",
        metric = metric,
        trControl = control)

# Random Forest (RF)
set.seed(19)
fit.rf <- train(
        DoW ~ ., 
        data=TrainData, 
        method = "rf",
        metric = metric,
        trControl = control)

# Summarize results
results <- resamples(
        list(lda = fit.lda,
             cart = fit.cart,
             knn = fit.knn,
             svm = fit.svm,
             rf = fit.rf)
)

summary(results)


# Plot results
dotplot(results)


# Print results
print(fit.lda)
print(fit.cart)
print(fit.knn)
print(fit.svm)
print(fit.rf)


# Predict based on training
predictions <- predict(fit. , data.for.validation)
confusionMatrix(predictions, data.for.validation$ )

