library(ggplot2)
library(plyr)
library(ROCR)
adult <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', 
                           sep = ',', fill = F, strip.white = T)
colnames(adult) <- c('age', 'workclass', 'fnlwgt', 'educatoin', 
                     'educatoin_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                     'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
adult$educatoin <- NULL
adult$fnlwgt <- NULL
adult$relationship <- NULL
# histogram of age by income group
ggplot(adult) + aes(x=as.numeric(age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
# histogram of age by gender group
ggplot(adult) + aes(x=as.numeric(age), group=sex, fill=sex) + 
  geom_histogram(binwidth=1, color='black')
summary(adult$workclass)
levels(adult$workclass)[1] <- 'Unknown'
# combine into Government job
adult$workclass <- gsub('^Federal-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^Local-gov', 'Government', adult$workclass)
adult$workclass <- gsub('^State-gov', 'Government', adult$workclass) 

# combine into Sele-Employed job
adult$workclass <- gsub('^Self-emp-inc', 'Self-Employed', adult$workclass)
adult$workclass <- gsub('^Self-emp-not-inc', 'Self-Employed', adult$workclass)

# combine into Other/Unknown
adult$workclass <- gsub('^Never-worked', 'Other', adult$workclass)
adult$workclass <- gsub('^Without-pay', 'Other', adult$workclass)
adult$workclass <- gsub('^Other', 'Other/Unknown', adult$workclass)
adult$workclass <- gsub('^Unknown', 'Other/Unknown', adult$workclass)

adult$workclass <- as.factor(adult$workclass)

summary(adult$workclass)
# barplot of job type by income group
# get the counts by industry and income group
count <- table(adult[adult$workclass == 'Government',]$income)["<=50K"]
count <- c(count, table(adult[adult$workclass == 'Government',]$income)[">50K"])
count <- c(count, table(adult[adult$workclass == 'Other/Unknown',]$income)["<=50K"])
count <- c(count, table(adult[adult$workclass == 'Other/Unknown',]$income)[">50K"])
count <- c(count, table(adult[adult$workclass == 'Private',]$income)["<=50K"])
count <- c(count, table(adult[adult$workclass == 'Private',]$income)[">50K"])
count <- c(count, table(adult[adult$workclass == 'Self-Employed',]$income)["<=50K"])
count <- c(count, table(adult[adult$workclass == 'Self-Employed',]$income)[">50K"])
count <- as.numeric(count)

# create a dataframe
industry <- rep(levels(adult$workclass), each = 2)
income <- rep(c('<=50K', '>50K'), 4)
df <- data.frame(industry, income, count)
df
# calculate the percentages
df <- ddply(df, .(industry), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df <- ddply(df, .(industry), transform, pos = (cumsum(count) - 0.5 * count))
df$label <- paste0(sprintf("%.0f", df$percent), "%")

# bar plot of counts by industry with in group proportions 
ggplot(df, aes(x = industry, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income by Industry')
# create a dataframe
df1 <- data.frame(table(adult$income, adult$educatoin_num))
names(df1) <- c('income', 'education_num', 'count')
df1
# calculate the percentages
df1 <- ddply(df1, .(education_num), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df1 <- ddply(df1, .(education_num), transform, pos = (cumsum(count) - 0.5 * count))
df1$label <- paste0(sprintf("%.0f", df1$percent), "%")

# remove some in group percentage to avoid overlapped text
df1$label[which(df1$percent < 5)] <- NA

# bar plot of counts by years of education with in group proportions 
ggplot(df1, aes(x = education_num, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Years of Education')
summary(adult$occupation)
levels(adult$occupation)[1] <- 'Unknown'
adult$occupation <- gsub('Adm-clerical', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Craft-repair', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Exec-managerial', 'White-Collar', adult$occupation)
adult$occupation <- gsub('Farming-fishing', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Handlers-cleaners', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Machine-op-inspct', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Other-service', 'Service', adult$occupation)
adult$occupation <- gsub('Priv-house-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Prof-specialty', 'Professional', adult$occupation)
adult$occupation <- gsub('Protective-serv', 'Service', adult$occupation)
adult$occupation <- gsub('Tech-support', 'Service', adult$occupation)
adult$occupation <- gsub('Transport-moving', 'Blue-Collar', adult$occupation)
adult$occupation <- gsub('Unknown', 'Other/Unknown', adult$occupation)
adult$occupation <- gsub('Armed-Forces', 'Other/Unknown', adult$occupation)
adult$occupation <- as.factor(adult$occupation)
summary(adult$occupation)
# create a dataframe
df2 <- data.frame(table(adult$income, adult$occupation))
names(df2) <- c('income', 'occupation', 'count')
df2
# calculate the percentages
df2 <- ddply(df2, .(occupation), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df2 <- ddply(df2, .(occupation), transform, pos = (cumsum(count) - 0.5 * count))
df2$label <- paste0(sprintf("%.0f", df2$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df2, aes(x = occupation, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Different Occupations')
summary(adult$marital_status)
adult$marital_status <- gsub('Married-AF-spouse', 'Married', adult$marital_status)
adult$marital_status <- gsub('Married-civ-spouse', 'Married', adult$marital_status)
adult$marital_status <- gsub('Married-spouse-absent', 'Married', adult$marital_status)
adult$marital_status <- gsub('Never-married', 'Single', adult$marital_status)
adult$marital_status <- as.factor(adult$marital_status)
summary(adult$marital_status)
df3 <- data.frame(table(adult$income, adult$marital_status))
names(df3) <- c('income', 'marital_status', 'count')
df3
# calculate the percentages
df3 <- ddply(df3, .(marital_status), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df3 <- ddply(df3, .(marital_status), transform, pos = (cumsum(count) - 0.5 * count))
df3$label <- paste0(sprintf("%.0f", df3$percent), "%")

# bar plot of counts by marital status with in group proportions 
ggplot(df3, aes(x = marital_status, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level with Marital Status')
# histogram of capital_gain
ggplot(adult) + aes(x=as.numeric(capital_gain), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Gain')
# histogram of capital_loss
ggplot(adult) + aes(x=as.numeric(capital_loss), group=income, fill=income) + 
  geom_histogram(bins=10, color='black') + ggtitle('Histogram of Capital Loss')
# percentage of observatiosn with no capital gain or loss
sum(adult$capital_gain == 0)/length(adult$capital_gain)
sum(adult$capital_loss == 0)/length(adult$capital_loss)
###
adult$capital_gain <- NULL
adult$capital_loss <- NULL
adult$native_country <- NULL
df4 <- data.frame(table(adult$income, adult$race))
names(df4) <- c('income', 'race', 'count')
df4
# calculate the percentages
df4 <- ddply(df4, .(race), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df4 <- ddply(df4, .(race), transform, pos = (cumsum(count) - 0.5 * count))
df4$label <- paste0(sprintf("%.0f", df4$percent), "%")

# do not display percentage for low counts categories
df4$label[df4$race == 'Other'] <- NA
df4$label[df4$race == 'Amer-Indian-Eskimo'] <- NA

# bar plot of counts by marital status with in group proportions 
ggplot(df4, aes(x = race, y = count, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Income Level by Race')
summary(adult)
sz <- round(.8 * dim(adult)[1])  # training set size
training_set <- adult[1:sz,]
testing_set <- adult[-(1:sz),]
m1 <- glm(income ~ ., data = training_set, family = binomial('logit'))
summary(m1)
confint(m1)

m_full <- m1  # full model is the model just fitted
m_null <- glm(income ~ 1, data = training_set, family = binomial('logit'))

# backward selection
step(m_full, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'backward')
# forward selection
step(m_null, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'forward')
# create a data frame to store information regarding deviance residuals
index <- 1:dim(training_set)[1]
dev_resid <- residuals(m1)
income <- training_set$income
dff <- data.frame(index, dev_resid, income)

ggplot(dff, aes(x = index, y = dev_resid, color = income)) +
  geom_point() + 
  geom_hline(yintercept = 3, linetype = 'dashed', color = 'blue') +
  geom_hline(yintercept = -3, linetype = 'dashed', color = 'blue')
ggtitle('Plot of Deviance Residuals')

###PREDICTION
prob1<-predict(m1,training_set,type='response')
pred1<-rep('<=50k',length(prob1))
pred1[prob>=.5]<-'>50k'
tb1<-table(pred1,training_set$income)
tb1
accuracy1<-sum(diag(tb1))/sum(tb1)
accuracy1
prob <- predict(m1, testing_set, type = 'response')
pred <- rep('<=50K', length(prob))
pred[prob>=.5] <- '>50K'

# confusion matrix 
tb <- table(pred, testing_set$income)
tb
accuracy2<-sum(diag(tb))/sum(tb)
accuracy2
##regression tree
library(rpart)
tree2 <- rpart(income ~ ., data = training_set, method = 'class', cp = 1e-3)
tree2.pred.prob <- predict(tree2, newdata = testing_set, type = 'prob')
tree2.pred <- predict(tree2, newdata = testing_set, type = 'class')
# confusion matrix 
tb2 <- table(tree2.pred, testing_set$income)
tb2
###random forest
library(randomForest)
rf3 <- randomForest(income ~ ., data = training_set, ntree = 1000)
rf3.pred.prob <- predict(rf3, newdata = testing_set, type = 'prob')
rf3.pred <- predict(rf3, newdata = testing_set, type = 'class')
# confusion matrix 
tb3 <- table(rf3.pred, testing_set$income)
tb3
###SVM
library(kernlab)
svm4 <- ksvm(income ~ ., data = training_set)
svm4.pred.prob <- predict(svm4, newdata = testing_set, type = 'decision')
svm4.pred <- predict(svm4, newdata = testing_set, type = 'response')
# confusion matrix 
tb4 <- table(svm4.pred, testing_set$income)
tb4
# create a prediction object
pr <- prediction(prob, testing_set$income)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# create a data frame for TP and FP rates
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])

# CART
pr2 <- prediction(tree2.pred.prob[,2], testing_set$income)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
dd2 <- data.frame(FP = prf2@x.values[[1]], TP = prf2@y.values[[1]])

# RF
pr3 <- prediction(rf3.pred.prob[,2], testing_set$income)
prf3 <- performance(pr3, measure = "tpr", x.measure = "fpr")
dd3 <- data.frame(FP = prf3@x.values[[1]], TP = prf3@y.values[[1]])

# SVM
pr4 <- prediction(svm4.pred.prob, testing_set$income)
prf4 <- performance(pr4, measure = "tpr", x.measure = "fpr")
dd4 <- data.frame(FP = prf4@x.values[[1]], TP = prf4@y.values[[1]])



# plot ROC curve for logistic regression
g <- ggplot() + 
  geom_line(data = dd, aes(x = FP, y = TP, color = 'Logistic Regression')) + 
  geom_line(data = dd2, aes(x = FP, y = TP, color = 'CART')) + 
  geom_line(data = dd3, aes(x = FP, y = TP, color = 'Random Forest')) +
  geom_line(data = dd4, aes(x = FP, y = TP, color = 'Support Vector Machine')) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
  ggtitle('ROC Curve') + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate') 


g +  scale_colour_manual(name = 'Classifier', values = c('Logistic Regression'='#E69F00', 
                                                         'CART'='#009E73', 
                                                         'Random Forest'='#D55E00', 'Support Vector Machine'='#0072B2'))
