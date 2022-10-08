############################################
# ML: Building a Linear Regression Model   #
# Data Professor                           #
# http://youtube.com/dataprofessor         #
# http://github.com/dataprofessor          #
# http://facebook.com/dataprofessor        #
# https://www.instagram.com/data.professor #
############################################

install.packages("mlbench")
install.packages("caret")
library(tidyverse)
library(mlbench)
library(caret)

data(BostonHousing)
head(BostonHousing)
sum(is.na(BostonHousing))

set.seed(100)
TrainingIndex <- caret::createDataPartition(BostonHousing$medv, p=0.8, list=FALSE)
TrainingSet <- BostonHousing[TrainingIndex,]
TestingSet <- BostonHousing[-TrainingIndex,]

###############################

# Build Training model
Model <- train(medv ~ ., data = TrainingSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none")
)

# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set

summary(Model.training)
summary(Model.testing)

# Model performance (Displays scatter plot and performance metrics)
# Scatter plots of training set and testing set 
plot(TrainingSet$medv, Model.training, col = "blue" )
plot(TestingSet$medv, Model.testing, col = "blue" )

# Model performance summary
# In the performance metric, see that 
summary(Model)


# Calculate Pearson's correlation coefficient
R.training <- cor(TrainingSet$medv, Model.training)
R2.training <- R.training^2
R.testing <- cor(TestingSet$medv, Model.testing)
R2.testing <- R.testing^2
