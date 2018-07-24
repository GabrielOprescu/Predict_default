library(caret)
library(ROCR)

load('date_prelucrate.rda')

set.seed(101)

inTraining = createDataPartition(y, p = 0.75, list = FALSE)

x_train = x[inTraining, ]
y_train = y[inTraining, ] 

x_test = x[-inTraining, ]
y_test = y[-inTraining, ]



set.seed(100)

model_boost <- xgboost(data = x_train, 
                     label = y_train, 
                     max.depth = 5,
                     subsample = 0.5,
                     eta = 1, 
                     nthread = 2, 
                     nround = 20, 
                     objective = "binary:logistic",
                     verbose = 2)

pred_train = predict(model_boost, newdata = x_train, type = 'class')

hist(pred, main = 'Distributia predictiei')

pred_rocr = prediction(pred_train, y_train)

perf_rocr = performance(pred_rocr, measure = "tpr", x.measure = "fpr")

plot(perf_rocr)
abline(a = 0, b = 1)

confusionMatrix(factor(as.numeric(pred_train > 0.5)),
                factor(y_train),
                positive = "1")
