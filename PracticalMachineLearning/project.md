Practical Machine Learning wih Weight Lifting Exercise Dataset
==============================================================

This is an analysis performed as partial requirement for completing the [Practical Machine Learning](https://www.coursera.org/course/predmachlearn) course, as part of the [Data Science Specialization](https://www.coursera.org/specialization/jhudatascience/1?utm_medium=courseDescripTop) by [Johns Hopkins University](http://www.jhu.edu/), through [Coursera](https://www.coursera.org/).

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify *how well* they do it. 

This project aims at using data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants to **predict** the manner in which they did the exercise. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways.

## Dataset

More information about the dataset is available from the website here: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset). 

The training data for this project are available [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv). 

The test data are available [here](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv). 

The data for this project come from this source: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har).



## Getting and Cleaning the data

The following lines read *training* and *testing* raw data from the *csv* files:


```r
raw.training = read.csv("pml-training.csv")
raw.testing  = read.csv("pml-testing.csv")
```

The raw data contains, though, many columns with NA and empty values, which are not useful for the prediction and must be discarded:




```r
relevant.vars <- names(raw.training)[apply(raw.training, 2, function(x){ table(is.na(x))[1]==nrow(raw.training) })]

training <- raw.training[,relevant.vars]
testing  <- raw.testing[,relevant.vars[-length(relevant.vars)]]

relevant.vars <- melt(apply(training,2,function(x) sum(ifelse(x=="",1,0)))==0)
relevant.vars <- rownames(relevant.vars)[relevant.vars$value==TRUE]

training <- training[,relevant.vars]
testing  <- testing[,relevant.vars[-length(relevant.vars)]]

training <- training[,names(training[-c(1:7,length(training))])]
testing  <- testing[,names(testing[-c(1:7)])]
```

Finally, we remove multicollinearity, by analysing the correlations among numerical variables.


```r
highCorr <- findCorrelation(cor(training), cutoff = .75)

testing  <- testing[, -highCorr]

classe   <- raw.training$classe
training <- cbind(classe, training[, -highCorr]) 
```

## Learning the Prediction Model

Given that the *training* and *testing* sets are already individually provided, we use the [createDataPartition](http://caret.r-forge.r-project.org/splitting.html) function for dividing the *training* set into *training* and *cross validation* sets, with 70% and 30% of the observations, respectively. 


```r
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
training   <- training[ inTrain, ]
validating <- training[-inTrain, ]
```

The number of observations for *training*, *cross validation* and *testing* sets is as follows:


```r
c(nrow(training), nrow(validating), nrow(testing))
```

```
## [1] 13737  4121    20
```

By running the [random-forest algorithm](http://en.wikipedia.org/wiki/Random_forest) over the *training* set, we gerenate the *prediction model*, as follows:


```r
fit <- randomForest(classe~., data=training, importance=TRUE, ntree=10)
print(fit)
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training, importance = TRUE,      ntree = 10) 
##                Type of random forest: classification
##                      Number of trees: 10
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 5.75%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3776   47   14   23   13     0.02505
## B   83 2383   75   43   46     0.09392
## C   13   68 2198   66   26     0.07296
## D   27   18   65 2103   21     0.05864
## E   12   51   32   39 2364     0.05364
```

## Prediction and Accuracy over Cross Validation dataset

Once we have the learned model, we can now run a prediction with it over the *cross validation* data. The generated [confusion matrix](http://en.wikipedia.org/wiki/Confusion_matrix) is as follows:


```r
prediction.validation = predict(fit, newdata=validating)
confusionMatrix(validating$classe, prediction.validation)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1169    0    0    0    0
##          B    0  793    0    0    0
##          C    0    0  715    0    0
##          D    0    0    1  683    0
##          E    0    0    0    0  760
## 
## Overall Statistics
##                                     
##                Accuracy : 1         
##                  95% CI : (0.999, 1)
##     No Information Rate : 0.284     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 1         
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    1.000    0.999    1.000    1.000
## Specificity             1.000    1.000    1.000    1.000    1.000
## Pos Pred Value          1.000    1.000    1.000    0.999    1.000
## Neg Pred Value          1.000    1.000    1.000    1.000    1.000
## Prevalence              0.284    0.192    0.174    0.166    0.184
## Detection Rate          0.284    0.192    0.174    0.166    0.184
## Detection Prevalence    0.284    0.192    0.174    0.166    0.184
## Balanced Accuracy       1.000    1.000    0.999    1.000    1.000
```


```r
accuracy <- sum(as.numeric(prediction.validation==validating$classe)) * 100 / nrow(validating)
message("The accuracy achieved over the cross validation set is ", 
        format(round(accuracy, 2), nsmall = 2), "%")
```

```
## The accuracy achieved over the cross validation set is 99.98%
```

## Prediction over Testing dataset

Now we rebuild the regression model over *training* and *cross validation* sets together, and finally perform the model prediction over the *testing* set:


```r
training <- rbind(training, validating)

fit <- randomForest(classe~., data=training, importance = TRUE, ntree=10)

prediction.test = predict(fit, newdata=testing)
prediction.test
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```


```r
table(prediction.test)
```

```
## prediction.test
## A B C D E 
## 7 8 1 1 3
```


