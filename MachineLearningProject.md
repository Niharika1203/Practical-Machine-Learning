MachineLearningProject
Niharika

Introduction
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement â a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

Preprocessing
At the preprocessing step we load the dataset and prepare it for the analysis part.

Load data and explore data
# Loading libraries
library(caret)
## Loading required package: ggplot2
library(randomForest)
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## The following object is masked from 'package:ggplot2':
## 
##     margin
library(rpart)
library(rpart.plot)

# Defining Seed
SEED_VALUE = 4242

train_set <- read.csv("pml-training.csv", sep = ",", na.strings = c("", "NA", "#DIV/0!"))
test_set <- read.csv("pml-testing.csv", sep = ",", na.strings = c("", "NA", "#DIV/0!"))

dim(train_set)
## [1] 19622   160
dim(test_set)
## [1]  20 160
Clean the data
Removing NAs in the training and test set:

# Remove colums with NAs
train_set <- train_set[, colSums(is.na(train_set)) == 0] 

# Adapt to test set:
test_set <- test_set[colnames(train_set[, !(names(train_set) %in% c("classe"))])] 

# To remove not used columns which are only there for identification
train_set <- train_set[, -(1:5)]
test_set  <- test_set[, -(1:5)]

# To print names without zeros
print(names(train_set))
##  [1] "new_window"           "num_window"           "roll_belt"           
##  [4] "pitch_belt"           "yaw_belt"             "total_accel_belt"    
##  [7] "gyros_belt_x"         "gyros_belt_y"         "gyros_belt_z"        
## [10] "accel_belt_x"         "accel_belt_y"         "accel_belt_z"        
## [13] "magnet_belt_x"        "magnet_belt_y"        "magnet_belt_z"       
## [16] "roll_arm"             "pitch_arm"            "yaw_arm"             
## [19] "total_accel_arm"      "gyros_arm_x"          "gyros_arm_y"         
## [22] "gyros_arm_z"          "accel_arm_x"          "accel_arm_y"         
## [25] "accel_arm_z"          "magnet_arm_x"         "magnet_arm_y"        
## [28] "magnet_arm_z"         "roll_dumbbell"        "pitch_dumbbell"      
## [31] "yaw_dumbbell"         "total_accel_dumbbell" "gyros_dumbbell_x"    
## [34] "gyros_dumbbell_y"     "gyros_dumbbell_z"     "accel_dumbbell_x"    
## [37] "accel_dumbbell_y"     "accel_dumbbell_z"     "magnet_dumbbell_x"   
## [40] "magnet_dumbbell_y"    "magnet_dumbbell_z"    "roll_forearm"        
## [43] "pitch_forearm"        "yaw_forearm"          "total_accel_forearm" 
## [46] "gyros_forearm_x"      "gyros_forearm_y"      "gyros_forearm_z"     
## [49] "accel_forearm_x"      "accel_forearm_y"      "accel_forearm_z"     
## [52] "magnet_forearm_x"     "magnet_forearm_y"     "magnet_forearm_z"    
## [55] "classe"
print(names(test_set))
##  [1] "new_window"           "num_window"           "roll_belt"           
##  [4] "pitch_belt"           "yaw_belt"             "total_accel_belt"    
##  [7] "gyros_belt_x"         "gyros_belt_y"         "gyros_belt_z"        
## [10] "accel_belt_x"         "accel_belt_y"         "accel_belt_z"        
## [13] "magnet_belt_x"        "magnet_belt_y"        "magnet_belt_z"       
## [16] "roll_arm"             "pitch_arm"            "yaw_arm"             
## [19] "total_accel_arm"      "gyros_arm_x"          "gyros_arm_y"         
## [22] "gyros_arm_z"          "accel_arm_x"          "accel_arm_y"         
## [25] "accel_arm_z"          "magnet_arm_x"         "magnet_arm_y"        
## [28] "magnet_arm_z"         "roll_dumbbell"        "pitch_dumbbell"      
## [31] "yaw_dumbbell"         "total_accel_dumbbell" "gyros_dumbbell_x"    
## [34] "gyros_dumbbell_y"     "gyros_dumbbell_z"     "accel_dumbbell_x"    
## [37] "accel_dumbbell_y"     "accel_dumbbell_z"     "magnet_dumbbell_x"   
## [40] "magnet_dumbbell_y"    "magnet_dumbbell_z"    "roll_forearm"        
## [43] "pitch_forearm"        "yaw_forearm"          "total_accel_forearm" 
## [46] "gyros_forearm_x"      "gyros_forearm_y"      "gyros_forearm_z"     
## [49] "accel_forearm_x"      "accel_forearm_y"      "accel_forearm_z"     
## [52] "magnet_forearm_x"     "magnet_forearm_y"     "magnet_forearm_z"
dim(train_set) 
## [1] 19622    55
dim(test_set)
## [1] 20 54
The training set and the test set
Splitting the training set into a smaller training set and a test set to predict the out-of-sample error:-

set.seed(SEED_VALUE)

# Split the train set
ids <- createDataPartition(train_set$classe, p=0.6, list=FALSE)
train_train_set <- train_set[ids, ]
train_test_set <- train_set[-ids, ]

dim(train_train_set) 
## [1] 11776    55
dim(train_test_set)
## [1] 7846   55
Train the models
To find a good model we train a a decison tree and a random forrest classifier both with a cross validation fold of size 5.

Decision Tree
set.seed(SEED_VALUE)
rpart_classifier <- train(classe ~., data=train_train_set, method="rpart",trControl=trainControl("cv",5))

rpart_classifier
## CART 
## 
## 11776 samples
##    54 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 9421, 9420, 9422, 9421, 9420 
## Resampling results across tuning parameters:
## 
##   cp          Accuracy   Kappa     
##   0.03203607  0.5124968  0.36439161
##   0.05140009  0.4719012  0.29812336
##   0.11378738  0.3484191  0.09807497
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.03203607.
The best cross-validated model has a accuracy of 57.2%.

Random Forrest
set.seed(SEED_VALUE)
random_forrest_classifier <- train(classe ~., data=train_train_set, method="rf",trControl=trainControl("cv",5))

random_forrest_classifier
## Random Forest 
## 
## 11776 samples
##    54 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 9421, 9420, 9422, 9421, 9420 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa    
##    2    0.9906585  0.9881823
##   28    0.9959235  0.9948434
##   54    0.9940554  0.9924803
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 28.
The best cross-validated model has a accuracy of 99.1%.

Evaluate the models
To find the best of the two prediction models we evaluate the out-of-sample error with help of the split off test set.

Decision Tree
set.seed(SEED_VALUE)

rpart_predictor <- predict(rpart_classifier, train_test_set)
confusionMatrix(rpart_predictor, train_test_set$classe)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1816  312  158  351  323
##          B   86  703  124  337  243
##          C  326  503 1086  598  214
##          D    0    0    0    0    0
##          E    4    0    0    0  662
## 
## Overall Statistics
##                                           
##                Accuracy : 0.5438          
##                  95% CI : (0.5327, 0.5549)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.4149          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.8136   0.4631   0.7939   0.0000  0.45908
## Specificity            0.7962   0.8752   0.7467   1.0000  0.99938
## Pos Pred Value         0.6135   0.4709   0.3982      NaN  0.99399
## Neg Pred Value         0.9149   0.8717   0.9449   0.8361  0.89136
## Prevalence             0.2845   0.1935   0.1744   0.1639  0.18379
## Detection Rate         0.2315   0.0896   0.1384   0.0000  0.08437
## Detection Prevalence   0.3773   0.1903   0.3476   0.0000  0.08488
## Balanced Accuracy      0.8049   0.6691   0.7703   0.5000  0.72923
As seen in the results the decision tree has significant problems by classifing the C and D classes. Its overall accuracy rate is 54.4%.

Random Forrest
set.seed(SEED_VALUE)

random_forrest_predictor <- predict(random_forrest_classifier, train_test_set)
confusionMatrix(random_forrest_predictor, train_test_set$classe)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231    4    0    0    0
##          B    0 1513    2    0    0
##          C    0    0 1366    8    0
##          D    0    1    0 1277    4
##          E    1    0    0    1 1438
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9973          
##                  95% CI : (0.9959, 0.9983)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9966          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9967   0.9985   0.9930   0.9972
## Specificity            0.9993   0.9997   0.9988   0.9992   0.9997
## Pos Pred Value         0.9982   0.9987   0.9942   0.9961   0.9986
## Neg Pred Value         0.9998   0.9992   0.9997   0.9986   0.9994
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1928   0.1741   0.1628   0.1833
## Detection Prevalence   0.2849   0.1931   0.1751   0.1634   0.1835
## Balanced Accuracy      0.9994   0.9982   0.9987   0.9961   0.9985
Overall the random forrest shows very good results with a accuracy rate of 99.9%. Therefore it is used to predict the new values.

Predict new data
To predict the values of new instances we use the trained random forrest classifier.

set.seed(SEED_VALUE)

predictions <- predict(random_forrest_classifier, newdata=test_set)
print(predictions)
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E