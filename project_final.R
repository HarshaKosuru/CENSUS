library(ggplot2)
library(plyr)
library(gridExtra)
library(gmodels)
library(grid)
library(vcd)
library(scales)
library(ggthemes)

#preprocessing train data
adult_data<-read.csv(file = "adult.csv",header = T,sep = ",")
head(adult_data)
tail(adult_data)
nrow(adult_data)  
names(adult_data)
str(adult_data)
sapply(adult_data,function(x) sum(is.na(x)))
table(adult_data$workclass)
table(adult_data$occupation)
table(adult_data$native.country)

adult_data[adult_data  == "?"] <- NA
sapply(adult_data,function(x) sum(is.na(x)))


str(adult_data)
summary(adult_data)




# now dealing with Outliers
par(mfrow =c(2,2))
boxplot(adult_data$age, main = "Age")
boxplot(adult_data$fnlwgt, main = "fnlwgt")
boxplot(adult_data$education.num,main = "education.num")
boxplot(adult_data$hours.per.week,main = "hours.per.week")


levels_factors <- function(mydata) {
  col_names <- names(mydata)
  for (i in 1:length(col_names)) {
    if (is.factor(mydata[, col_names[i]])) {
      message(noquote(paste("Covariate ", "*", 
                            col_names[i], "*", 
                            " with factor levels:", 
                            sep = "")))
      print(levels(mydata[, col_names[i]]))
    }
  }
}

levels_factors(adult_data)

#transformation of hours.per.week
ggplot(aes(x = factor(0), y = hours.per.week),
       data = adult_data) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  xlab(label = "") +
  ylab(label = "Working hours per week") +
  ggtitle("Box Plot of Working Hours per Week")



adult_data<-na.omit(adult_data)
row.names(adult_data)<-1:nrow(adult_data)
sapply(adult_data,function(x) sum(is.na(x)))

summary(adult_data$hours.per.week)

adult_data$hours_w[adult_data$hours.per.week < 40] <- "less_than_40"
adult_data$hours_w[adult_data$hours.per.week >= 40 & 
                     adult_data$hours.per.week <= 45] <- "between_40_and_45"
adult_data$hours_w[adult_data$hours.per.week > 45 &
                     adult_data$hours.per.week <= 60  ] <- "between_45_and_60"
adult_data$hours_w[adult_data$hours.per.week > 60 &
                     adult_data$hours.per.week <= 80  ] <- "between_60_and_80"
adult_data$hours_w[adult_data$hours.per.week > 80] <- "more_than_80"

adult_data$hours_w <- factor(adult_data$hours_w,
                           ordered = FALSE,
                           levels = c("less_than_40", 
                                      "between_40_and_45", 
                                      "between_45_and_60",
                                      "between_60_and_80",
                                      "more_than_80"))
summary(adult_data$hours_w)
for(i in 1:length(summary(adult_data$hours_w))){
  
  print(round(100*summary(adult_data$hours_w)[i]/sum(!is.na(adult_data$hours_w)), 2)) 
}

#transformation of native country
levels(adult_data$native.country)

Asia_East <- c("Cambodia", "China", "Hong", "Laos", "Thailand",
               "Japan", "Taiwan", "Vietnam")

Asia_Central <- c("India", "Iran")

Central_America <- c("Cuba", "Guatemala", "Jamaica", "Nicaragua", 
                     "Puerto-Rico",  "Dominican-Republic", "El-Salvador", 
                     "Haiti", "Honduras", "Mexico", "Trinadad&Tobago")

South_America <- c("Ecuador", "Peru", "Columbia")


Europe_West <- c("England", "Germany", "Holand-Netherlands", "Ireland", 
                 "France", "Greece", "Italy", "Portugal", "Scotland")

Europe_East <- c("Poland", "Yugoslavia", "Hungary")

#Then we modify the data frame by adding an additional column named "native_region". 
#We do this with the help of the "mutate" function form the "plyr" package
adult_data <- mutate(adult_data, 
                   native_region = ifelse(native.country %in% Asia_East, "East-Asia",
                                   ifelse(native.country%in% Asia_Central, "Central-Asia",
                                   ifelse(native.country%in% Central_America, "Central-America",
                                   ifelse(native.country %in% South_America, "South-America",
                                   ifelse(native.country %in% Europe_West, "Europe-West",
                                   ifelse(native.country%in% Europe_East, "Europe-East",
                                   ifelse(native.country == "United-States", "United-States","Outlying-US" ))))))))
adult_data$native_region <- factor(adult_data$native_region, ordered = FALSE)

#transformation of capital_gain and capitalloss
summary(adult_data$capital.gain)
summary(adult_data$capital.loss)
(nrow(subset(adult_data, adult_data$capital.gain == 0))/nrow(adult_data))*100
#More precisely, the percentage of zeros in "capital_gain" is very big - 91.59%:

(nrow(subset(adult_data, adult_data$capital.loss == 0))/nrow(adult_data))*100
#the percentage of zeros in "capital_loss" is also very high - 95.27%:

mean.gain <- mean(adult_data$capital.gain)

mean.loss <- mean(adult_data$capital.loss)
library(knitr)
kable(data.frame(Mean_Capital_Gain = mean.gain, Mean_Capital_Loss = mean.loss),
      caption = "Mean Capital with Zero Values Included")
#the mean values of "capital_gain" and "capital_loss" with the zero values included

#We give the mean capital gain and loss also in the case of all the zero values removed
mean.gain <- mean(subset(adult_data$capital.gain, adult_data$capital.gain > 0))

mean.loss <- mean(subset(adult_data$capital.loss, adult_data$capital.loss > 0))

kable(data.frame(Mean_Capital_Gain = mean.gain, Mean_Capital_Loss = mean.loss),
      caption = "Mean Capital Only for Nonzero Values")

#
iqr.gain <- IQR(subset(adult_data$capital.gain, adult_data$capital.gain > 0))
iqr.loss <- IQR(subset(adult_data$capital.loss, adult_data$capital.loss > 0))



q.gain <- quantile(x = subset(adult_data$capital.gain, adult_data$capital.gain > 0), 
                   probs = seq(0, 1, 0.25))

q.loss <- quantile(x = subset(adult_data$capital.loss, adult_data$capital.loss > 0),
                   probs = seq(0, 1, 0.25))


kable(x = data.frame(Capital_Gain = q.gain, Capital_Loss = q.loss),
      caption = "Quantiles of the Nonzero Capital")
#IQR of the Nonzero Capital
kable(x = data.frame(IQR_Capital_Gain = iqr.gain, IQR_Capital_Loss = iqr.loss),
      caption = "IQR of the Nonzero Capital")

#box plot 
ggplot(aes(x = factor(0), y = capital.gain),
       data = subset(adult_data, adult_data$capital.gain > 0)) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100000, 5000)) +
  ylab("Capital Gain") +
  xlab("") +  
  ggtitle("Box plot of Nonzero Capital Gain") 
#From the box plot we see that, indeed, the bulk of the data is between 3,000 and 15,000 dollars, and there are a few outliers. 
#Next we show a histogram of the nonzero capital gain
df <- adult_data[adult_data$capital.gain > 0, ]

ggplot(data = df, 
       aes(x = df$capital.gain)) +
  geom_histogram(binwidth = 5000,
                 colour = "black",
                 fill = "lightblue",
                 alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 4000, 100)) +
  labs(x = "Capital gain", y = "Count") +
  ggtitle("Histogram of Nonzero Capital Gain")
#The histogram confirms once more what we have already established
#that the majority of people with positive capital gain have a capital gain between 0 and 25,000 dollars
# there are also about 150 people with capital gain of around 100,000 dollars.
#We also note that the biggest number of people with positive capital gain are those with about 5,000 dollars.

#boxplot of non zero capital loss
ggplot(aes(x = factor(0), y = capital.loss),
       data = subset(adult_data, adult_data$capital.loss > 0)) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  ylab("Capital Loss") +
  xlab("") +  
  ggtitle("Box plot of Nonzero Capital Loss")
#From the box plot we observe that most values are between 1,700 and 2,000 dollars
#creating a nw column cap gain to mention low, mwdium and highfor capital gain
adult_data <- mutate(adult_data, 
                   cap_gain = ifelse(adult_data$capital.gain < 3464, "Low",
                                     ifelse(adult_data$capital.gain >= 3464 & 
                                              adult_data$capital.gain <= 14080, "Medium", "High")))


adult_data$cap_gain <- factor(adult_data$cap_gain,
                            ordered = TRUE,
                            levels = c("Low", "Medium", "High"))


#creating a new column cap loss to mention low, medium, high for capital loss
adult_data <- mutate(adult_data, 
                   cap_loss = ifelse(adult_data$capital.loss < 1672, "Low",
                                     ifelse(adult_data$capital.loss >= 1672 & 
                                              adult_data$capital.loss <= 1977, "Medium", "High")))


adult_data$cap_loss <- factor(adult_data$cap_loss,
                            ordered = TRUE,
                            levels = c("Low", "Medium", "High"))


#transformation of work class

summary(adult_data$workclass)

adult_data$workclass<-droplevels(adult_data$workclass)
levels(adult_data$workclass)

#preprocessing test data

adult_data_test<-read.table("adult.test",sep = ",",header = FALSE, skip = 1,na.strings = " ?")
colnames(adult_data_test) <- c("age", "workclass", "fnlwgt", "education",
                       "education.num", "marital.status", "occupation",
                       "relationship", "race", "sex", "capital.gain",
                       "capital.loss", "hours.per.week",
                       "native.country", "income")
sum(is.na(adult_data_test))
adult_data_test[adult_data_test=="?"]<-NA
sapply(adult_data_test,function(x) sum(is.na(x)))
adult_data_test<-na.omit(adult_data_test)
row.names(adult_data_test) <- 1:nrow(adult_data_test)
sapply(adult_data_test,function(x) sum(is.na(x)))
head(adult_data_test)

levels(adult_data_test$income)[1] <- "<=50K"
levels(adult_data_test$income)[2] <- ">50K"

levels(adult_data_test$income)
#creating hours_w
adult_data_test$hours_w[adult_data_test$hours.per.week < 40] <- "less_than_40"
adult_data_test$hours_w[adult_data_test$hours.per.week >= 40 & 
                  adult_data_test$hours.per.week <= 45] <- "between_40_and_45"
adult_data_test$hours_w[adult_data_test$hours.per.week > 45 &
                  adult_data_test$hours.per.week <= 60  ] <- "between_45_and_60"
adult_data_test$hours_w[adult_data_test$hours.per.week > 60 &
                  adult_data_test$hours.per.week <= 80  ] <- "between_60_and_80"
adult_data_test$hours_w[adult_data_test$hours.per.week > 80] <- "more_than_80"



adult_data_test$hours_w <- factor(adult_data_test$hours_w,
                          ordered = FALSE,
                          levels = c("less_than_40", 
                                     "between_40_and_45", 
                                     "between_45_and_60",
                                     "between_60_and_80",
                                     "more_than_80"))

#creating 
Asia_East <- c("Cambodia", "China", "Hong", "Laos", "Thailand",
               "Japan", "Taiwan", "Vietnam")

Asia_Central <- c("India", "Iran")

Central_America <- c("Cuba", "Guatemala", "Jamaica", "Nicaragua", 
                     "Puerto-Rico",  "Dominican-Republic", "El-Salvador", 
                     "Haiti", "Honduras", "Mexico", "Trinadad&Tobago")

South_America <- c("Ecuador", "Peru", "Columbia")


Europe_West <- c("England", "Germany", "Holand-Netherlands", "Ireland", 
                 "France", "Greece", "Italy", "Portugal", "Scotland")

Europe_East <- c("Poland", "Yugoslavia", "Hungary")

adult_data_test <- mutate(adult_data_test, 
                     native_region = ifelse(native.country %in% Asia_East, "East-Asia",
                                     ifelse(native.country%in% Asia_Central, "Central-Asia",
                                     ifelse(native.country%in% Central_America, "Central-America",
                                     ifelse(native.country %in% South_America, "South-America",
                                     ifelse(native.country %in% Europe_West, "Europe-West",
                                     ifelse(native.country%in% Europe_East, "Europe-East",
                                     ifelse(native.country == "United-States", "United-States","Outlying-US" ))))))))

levels(adult_data_test$native_region)<-c("United-States","Central-America","Europe-West","Outlying-US","East-Asia",      
                                         "Central-Asia","South-America","Europe-East")
adult_data_test$native_region<-as.factor(adult_data_test$native_region)

##creating capital loss and capital gain columns
adult_data_test <- mutate(adult_data_test, 
                  cap_gain = ifelse(adult_data_test$capital.gain < 3464, "Low",
                                    ifelse(adult_data_test$capital.gain >= 3464 & 
                                            adult_data_test$capital.gain <= 14080, "Medium", "High")))

adult_data_test$cap_gain <- factor(adult_data_test$cap_gain,
                           ordered = FALSE,
                           levels = c("Low", "Medium", "High"))
##capital loss
adult_data_test<- mutate(adult_data_test, 
                 cap_loss = ifelse(adult_data_test$capital.loss < 1672, "Low",
                                   ifelse(adult_data_test$capital.loss >= 1672 & 
                                            adult_data_test$capital.loss <= 1977, "Medium", "High")))


adult_data_test$cap_loss <- factor(adult_data_test$cap_loss,
                           ordered = FALSE,
                           levels = c("Low", "Medium", "High"))
adult_data_test$workclass<-droplevels(adult_data_test$workclass)
levels(adult_data_test$workclass)
levels(adult_data_test$workclass)<-c("Federal-gov","Local-gov","Private","Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay")
levels(adult_data_test$workclass)
levels(adult_data_test$education)<-c("10th","11th","12th","1st-4th","5th-6th","7th-8th",     
                                     "9th","Assoc-acdm","Assoc-voc","Bachelors","Doctorate","HS-grad",     
                                     "Masters","Preschool","Prof-school","Some-college")
levels(adult_data_test$marital.status)

levels(adult_data_test$marital.status)<-c("Married-civ-spouse","Never-married","Divorced",
                                          "Separated","Widowed","Married-spouse-absent","Married-AF-spouse")

levels(adult_data_test$occupation)
levels(adult_data_test$occupation)<-c("Prof-specialty","Craft-repair","Exec-managerial","Adm-clerical",     
                                      "Sales","Other-service","Machine-op-inspct","Transport-moving", 
                                      "Handlers-cleaners","Farming-fishing","Tech-support","Protective-serv",  
                                      "Priv-house-serv","Armed-Forces")

levels(adult_data_test$relationship)
levels(adult_data_test$relationship)<-c("Husband","Not-in-family","Own-child","Unmarried","Wife",          
                                        "Other-relative")
levels(adult_data_test$race)
levels(adult_data_test$race)<-c("Amer-Indian-Eskimo","Asian-Pac-Islander","Black","Other","White")

levels(adult_data_test$sex)
levels(adult_data_test$sex)<-c("Female","Male")

levels(adult_data_test$native.country)
levels(adult_data_test$native.country)<-c("Cambodia","Canada","China","Columbia","Cuba","Dominican-Republic",
                                          "Ecuador","El-Salvador","England","France","Germany","Greece","Guatemala",
                                          "Haiti","Holand-Netherlands","Honduras","Hong","Hungary","India","Iran",
                                          "Ireland","Italy","Jamaica","Japan","Laos","Mexico","Nicaragua","Outlying-US(Guam-USVI-etc)",
                                          "Peru","Philippines","Poland","Portugal","Puerto-Rico","Scotland","South",
                                          "Taiwan","Thailand","Trinadad&Tobago","United-States","Vietnam","Yugoslavia")
levels(adult_data_test$income)
levels(adult_data_test$income)<-c("<=50K",">50K")
levels(adult_data_test$hours_w)
levels(adult_data_test$hours_w)<-c("between_40_and_45","less_than_40","between_45_and_60","between_60_and_80",
                                   "more_than_80")


levels(adult_data_test$cap_gain)
levels(adult_data_test$cap_gain)<-c("Low","Medium","High")
levels(adult_data_test$cap_loss)<-c("Low","Medium","High")
str(adult_data_test)

write.csv(adult_data, "adult_train_df.csv", row.names = FALSE)

write.csv(adult_data_test, "adult_test_df.csv", row.names = FALSE)



######################################################################################################################################################

###Correlation of the variable with INCOME
class(adult_data$income)
levels(adult_data$income)
summary(adult_data$income)

##bar plot
ggplot(data = adult_data, 
       mapping = aes(x = adult_data$income, fill = adult_data$income)) + 
  geom_bar(mapping = aes(y = (..count..)/sum(..count..))) +
  geom_text(mapping = aes(label = scales::percent((..count..)/sum(..count..)),
                          y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Income", 
       y = "",
       fill = "Income") +
  scale_y_continuous(labels = percent)
##The graph above shows us the percentage of people earning less than 50K a year and more than 50K. 
#We see that 75.1% of the participants in the study are paid less than 50K and 24.9% are paid more than 50K.

##boxplot capital gain and capital loss with income
#boxplot for capital gain and income
ggplot(mapping = aes(x = income, y = capital.gain),
       data = subset(adult_data, adult_data$capital.gain > 0)) + 
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(0, 30000)) +
  scale_y_continuous(breaks = seq(0, 30000, 1500)) +
  labs(x = "Income", 
       y = "Capital Gain") +
  ggtitle("Box Plot of Nonzero Capital Gain by Income") 
##boxplot for capital toss and income
ggplot(mapping = aes(x = income, y = capital.loss),
       data = subset(adult_data, adult_data$capital.loss > 0)) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(0, 3000))+
  scale_y_continuous(breaks = seq(0, 3000, 200)) +
  labs(x = "Income", 
       y = "Capital Loss") +
  ggtitle("Box Plot of Nonzero Capital Loss by Income")

##Next, we will examine the relationship between the factor variables "cap_gain" and "cap_loss" and the categorical variable "income".
##cap_gain
lg_cap.gain <- lapply(X = levels(adult_data$income), FUN = function(v){
  
  df <- subset(adult_data, adult_data$income == v)    
  
  df <- within(df, cap_gain <- factor(cap_gain, 
                                      levels = names(sort(table(cap_gain), 
                                                          decreasing = TRUE))))
  
  ggplot(data = df, 
         aes(x = cap_gain, 
             fill = cap_gain)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -.1) +
    labs(x = "Capital Gain", 
         y = "",
         fill = "Capital Gain") +
    theme(legend.position = 'none') +
    ggtitle(paste("Income", v, sep = "")) +  
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg_cap.gain, ncol = 2)
#From the bar plot we see that 0.0% of the people who earn less than 50K a year have a high capital gain. 
nrow(subset(adult_data, adult_data$cap_gain == " High" &
              adult_data$income == " <=50K"))
#As we can see from the output above, there are, indeed, no people with low annual income and high capital gain. 
# Bar plot of cap_loss by income:

lg_cap.loss <- lapply(levels(adult_data$income), function(v){
  
  df <- subset(adult_data, adult_data$income == v)    
  
  df <- within(df, cap_loss <- factor(cap_loss, 
                                      levels = names(sort(table(cap_loss), 
                                                          decreasing = TRUE))))    
  
  ggplot(data = df, 
         aes(x = cap_loss, 
             fill = cap_loss)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -.1) +
    labs(x = "Capital Loss", 
         y = "",
         fill = "Capital Loss") +
    theme(legend.position = 'none') +
    ggtitle(paste("Income", v, sep="")) +  
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg_cap.loss, ncol = 2)


###variable 'age' with 'income'
summary(adult_data$age)
IQR(adult_data$age)
ggplot(mapping = aes(x = factor(0), y = age),
       data = adult_data) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  coord_cartesian(ylim = c(10, 100)) +
  scale_y_continuous(breaks = seq(10, 100, 5)) +
  ylab("Age") +
  xlab("") +  
  ggtitle("Box Plot of Age") +
  scale_x_discrete(breaks = NULL)
##histogram 
qplot(x = adult_data$age, 
      data = adult_data, 
      binwidth = 5, 
      color = I('black'), 
      fill = I('#F29025'),
      xlab = "Age",
      ylab = "Count",
      main = "Histogram of Age") +
  scale_x_continuous(breaks = seq(0, 95, 5)) +   
  scale_y_continuous(breaks = seq(0, 4500, 500))
#From the plot we see that the majority of people earning more than 50K a year are between 33 and 55 years old
#the greater number of people who earn less than 50K a year are between 18 and 45

ggplot(data = adult_data, aes(age, fill = income)) + 
  geom_density(alpha = 0.2) +
  scale_x_continuous(breaks = seq(0, 95, 5))
##The density plot above clearly shows that age and income are correlated - people of greater age have higher income.

ggplot(data = adult_data, mapping = aes(x = age)) + 
  geom_histogram(binwidth = 5,
                 color = "black",
                 fill = "lightblue",
                 alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 95, 5)) + 
  facet_wrap(~income) +
  ggtitle("Histogram of Age by Income")
##boxplot
ggplot(aes(x = income, y = age),
       data = adult_data) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = "point", 
               shape = 16, 
               cex = 2, 
               col = "red") +
  coord_cartesian(ylim = c(10, 90))+
  scale_y_continuous(breaks = seq(10, 90, 5)) +
  ylab("Age") +
  xlab("Income") +  
  ggtitle("Box Plot of Age by Income")
#Once again we see the relationship between age and income

##variables age and gender

ggplot(aes(x = age, y = hours.per.week),
       data = adult_data) + 
  geom_line(mapping = aes(color = sex), 
            stat = 'summary', 
            fun.y = mean) +
  geom_smooth(mapping = aes(color = sex)) + 
  scale_x_continuous(breaks = seq(10, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 55, 5)) +  
  labs(x = "Age", y = "Mean Hours per Week") +
  ggtitle("Age vs. Mean Hours per Week by Gender")

##box plots for hours per week 
#  box plot of hours per week:
ggplot(aes(x = factor(0), y = hours.per.week),
       data = adult_data) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = "point", 
               color = "red",
               shape = 19,
               cex = 2) +
  coord_cartesian(ylim = c(30, 50)) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(30, 50, 1)) +
  ylab("Hours per Week") +
  xlab("") +  
  ggtitle("Box plot of Hours per Week")

##hours per week vs income
ggplot(aes(x = income, y = hours.per.week),
       data = adult_data) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = 'point', 
               shape = 19, 
               color = "red", 
               cex = 2) +
  coord_cartesian(ylim = c(10, 100))+
  scale_y_continuous(breaks = seq(10, 100, 10)) +
  ylab("Hours per Week") +
  xlab("Income") +  
  ggtitle("Box plot of Hours per Week by Income") 
#
summary(subset(adult_data$hours.per.week, adult_data$income == "<=50K"))

summary(subset(adult_data$hours.per.week, adult_data$income == ">50K"))
##Both medians (50-th percentiles) are equal to 40 hours per week, but the mean value for income<=50K is approximately 39 compared to 46 for income>50K.
#we show a graph of the mean working hours per week versus age, grouped by income
ggplot(mapping = aes(x = age, y = hours.per.week),
       data = adult_data) + 
  geom_line(mapping = aes(color = income), 
            stat = 'summary', 
            fun.y = mean) +
  geom_smooth(mapping = aes(color = income)) +
  scale_x_continuous(breaks = seq(10, 100, 5)) +
  labs(x = "Age", 
       y = "Mean Hours per Week") +
  ggtitle("Mean Hours per Week vs. Age")

##hours_w vs income
summary(adult_data$hours_w)

##bar plot for hours-w
adult_data <- within(adult_data, hours_w <- factor(hours_w, levels = 
                                                 names(sort(table(hours_w), 
                                                            decreasing = TRUE))))

ggplot(adult_data, 
       aes(x = adult_data$hours_w, fill = adult_data$hours_w)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = 0.3) +
  labs(x = "Hours per week", 
       y = "",
       fill = "Hours per week") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Bar Plot of Hours per Week") +  
  scale_y_continuous(labels = percent)
##We observe that 55.1% of all individuals work between 40 and 45 hours a week
##bar plots 
lg_hpw <- lapply(levels(adult_data$income), function(v){
  
  df <- subset(adult_data, adult_data$income == v)  
  
  df <- within(df, hours_w <- factor(hours_w, 
                                     levels = names(sort(table(hours_w), 
                                                         decreasing = TRUE))))
  
  ggplot(data = df, 
         aes(x = hours_w, 
             fill = hours_w)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -.1,
              size = 3) +
    labs(x = "Hours per week", 
         y = "",
         fill = "Hours per week") +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("Income", v, sep="")) + 
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg_hpw, ncol = 2)
##We see that approximately the same percentage of people in both categories of "income" work between 40 and 45 hours a week

##variable native_region

summary(adult_data$native_region)
#The majority of people come from the United States and Central America.
adult_data$native_region <- factor(adult_data$native_region, 
                                 levels = 
                                   names(sort(table(adult_data$native_region),decreasing = TRUE)))

ggplot(adult_data, 
       aes(x = adult_data$native_region, fill = adult_data$native_region)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Region", 
       y = "",
       fill = "Regions") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_y_continuous(labels = percent)
##As we can see from the graphs above, 91.2% of the participants in the study come from the USA.
#the percentage of people earning less than 50K and more than 50K among all individuals belonging to a given native region
lp_region <- lapply(levels(adult_data$native_region), function(v){
  
  df <- subset(adult_data, adult_data$native_region == v) 
  
  ggplot(data = df, 
         aes(x = income, 
             fill = income)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = c(2, -0.1),
              size = 4) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(v) +    
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +     
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lp_region[1:4], ncol = 2)

grid.arrange(grobs = lp_region[5:8], ncol = 2)

##workclass
table(adult_data$workclass)
##
adult_data$workclass <- factor(adult_data$workclass, 
                             levels = 
                               names(sort(table(adult_data$workclass),                                                    decreasing = TRUE)))

ggplot(adult_data, 
       aes(x = adult_data$workclass, fill = adult_data$workclass)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Employment type", 
       y = "",
       fill = "Employment type") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +   
  scale_y_continuous(labels = percent)
nrow(subset(adult_data , adult_data$workclass == "Never-worked"))
#Based on the summary statistic of "workclass" we observed that there are no people in the category " Never-worked"
nrow(subset(adult_data , adult_data$workclass == "Without-pay" & adult_data$income == ">50K"))
#First we create the character vector "modified.work", whose elements are the levels of the nominal variable "workclass"
modified.work <- levels(adult_data$workclass)

modified.work

modified.work <- modified.work[!is.element(modified.work, 
                                           c("Never-worked",
                                             "Without-pay"))]

modified.work              

##
lg.workclass.mod <- lapply(modified.work, function(v){
  
  ggplot(data = subset(adult_data, adult_data$workclass == v), 
         aes(x = subset(adult_data, adult_data$workclass == v)$income, 
             fill = subset(adult_data, adult_data$workclass == v)$income)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = c(2, 1.5)) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(v) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg.workclass.mod[1:3], ncol = 2)

grid.arrange(grobs = lg.workclass.mod[4:6], ncol = 2)

##from the graphs above, we can see that the percentage of individuals having an income of more than 50K is biggest for the category "Self-emp-inc" 

##variable "education"
summary(adult_data$education)
#The majority of people have a high school degree - 9840
##college degreee - 6678 and bachelor degree - 5044.

#bar plot
adult_data$education <- factor(adult_data$education, 
                             levels = 
                               names(sort(table(adult_data$education),decreasing = TRUE)))

ggplot(adult_data, 
       aes(x = adult_data$education, fill = adult_data$education)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Education", 
       y = "",
       fill = "Education") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)
nrow(subset(adult_data, adult_data$education == " Preschool" &
             adult_data$income == ">50K" ))
#we will remove the factor level "Preschool" before we continue further with the analysis. 
modified.edu <- levels(adult_data$education)

modified.edu

modified.edu <- modified.edu[!is.element(modified.edu, "Preschool")]

modified.edu

##we display the bar plot of each education category grouped by income
lg.mod.edu <- lapply(modified.edu, function(v){
  
  ggplot(data = subset(adult_data, adult_data$education == v), 
         aes(x = subset(adult_data, adult_data$education == v)$income, 
             fill = subset(adult_data, adult_data$education == v)$income)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust =  c(2, 0.5),
              size = 3) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(v) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +    
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg.mod.edu[1:4], ncol = 2)

grid.arrange(grobs = lg.mod.edu[5:8], ncol = 2)

grid.arrange(grobs = lg.mod.edu[9:12], ncol = 2)

grid.arrange(grobs = lg.mod.edu[13:15], ncol = 2)
##The categories " 1st-4th"," 5th-6th"," 7th-8th"," 9th"," 10th"," 11th" and " 12th" have a very small percentage of people with income greater than 50K a year.

#variable "marital status"
summary(adult_data$marital.status)
#the biggest number of people are married to a civilian spouse - 14065.
#A significant number of individuals belong to the group " Never-married" - 9726
adult_data$marital.status <- factor(adult_data$marital.status, 
                                  levels = 
                                    names(sort(table(adult_data$marital.status),decreasing = TRUE)))

ggplot(adult_data, 
       aes(x = adult_data$marital.status, fill = adult_data$marital.status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Marital Status", 
       y = "",
       fill = "Marital Status") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

##
lp_marital <- lapply(levels(adult_data$marital.status), function(v){
  
  ggplot(data = subset(adult_data, adult_data$marital.status == v),
         aes(x = subset(adult_data, adult_data$marital.status == v)$income,
             fill = subset(adult_data, adult_data$marital.status == v)$income)) +   
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ),
              stat = "count",
              vjust = c(2, -0.1)) +
    labs(x = "Income",
         y = "",
         fill = "Income") +
    ggtitle(v) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lp_marital[1:3], ncol = 2)

grid.arrange(grobs = lp_marital[4:7], ncol = 2)

#As we see from the graphs above, the biggest percentage of employees with income higher than 50K are those from the category " Married-AF-spouse"

##the variable "occupation"
summary(adult_data$occupation)

adult_data$occupation <- factor(adult_data$occupation, 
                              levels = 
                                names(sort(table(adult_data$occupation),                                                   decreasing = TRUE)))

ggplot(adult_data,
       aes(x = adult_data$occupation, fill = adult_data$occupation)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ),
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Occupation",
       y = "Percentage",
       fill = "Occupation") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)
#We notice (see the code below) that there are no women working in the 
##" Armed-Forces" and there are no men working in the " Priv-house-serv" sector who are earning more than 50K a year.

nrow(subset(adult_data, adult_data$sex == "Female" &
              adult_data$occupation == "Armed-Forces"))

nrow(subset(adult_data, adult_data$sex == "Male" &
              adult_data$occupation == "Priv-house-serv" &
              adult_data$income == " >50K"))
###armed formes occupation has 0 % so we remove 
modified.occup.f <- levels(adult_data$occupation)
modified.occup.f

modified.occup.f <- modified.occup.f[!is.element(modified.occup.f, 
                                                 c("Armed-Forces"))]
modified.occup.f

##Below we display bar plots of women's income grouped by occupation

lp.occupation.f <- lapply(modified.occup.f, function(v){
  
  ggplot(data = subset(adult_data, adult_data$occupation == v 
                       & adult_data$sex == "Female"), 
         aes(x = subset(adult_data, adult_data$occupation == v 
                        & adult_data$sex == "Female")$income, 
             fill = subset(adult_data, adult_data$occupation == v 
                           & adult_data$sex == "Female")$income)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = c(2, -0.1)) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(paste("Women in \n", v, sep="")) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 10, face = "bold")) + 
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lp.occupation.f[1:4], ncol = 2)

grid.arrange(grobs = lp.occupation.f[5:9], ncol = 3)

grid.arrange(grobs = lp.occupation.f[10:13], ncol = 3)

#the biggest percentage of women with income greater than 50K is in the category " Prof-specialty" - 25.5%

summary(adult_data[adult_data$sex == "Female", ]$occupation)
adult_data$occupation<-droplevels(adult_data$occupation)
levels(adult_data$occupation)
##no men who are working in the " Priv-house-serv" sector are earning more than 50K a year, we leave out " Armed-Forces" and " Priv-house-serv"
modified.occup.m <- levels(adult_data$occupation)
modified.occup.m

modified.occup.m <- modified.occup.m[!is.element(modified.occup.m, 
                                                 "Priv-house-serv")]
modified.occup.m

##bar plot for males
lp.occupation.m <- lapply(modified.occup.m, function(v){
  
  ggplot(data = subset(adult_data, adult_data$occupation == v 
                       & adult_data$sex == "Male"), 
         aes(x = subset(adult_data, adult_data$occupation == v 
                        & adult_data$sex == "Male")$income, 
             fill = subset(adult_data, adult_data$occupation == v 
                           & adult_data$sex == "Male")$income)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = c(2, 1),
              size = 3) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(paste("Men in", v, sep="")) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +     
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lp.occupation.m[1:4], ncol = 2)

grid.arrange(grobs = lp.occupation.m[5:8], ncol = 2)

grid.arrange(grobs = lp.occupation.m[9:10], ncol = 2)

grid.arrange(grobs = lp.occupation.m[11:13], ncol = 2)

#58.3% of the male employees with executive-managerial position have an income of more than 50K a year
summary(adult_data[adult_data$sex == "Male" , ]$occupation)
adult_data$occupation<-droplevels(adult_data$occupation)
levels(adult_data$occupation)


###variable relation ship
summary(adult_data$relationship)
##percentage 
adult_data$relationship <- factor(adult_data$relationship, 
                                levels = 
                                  names(sort(table(adult_data$relationship),decreasing = TRUE)))

ggplot(adult_data, 
       aes(x = adult_data$relationship, fill = adult_data$relationship)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Relationship", 
       y = "",
       fill = "Relationship") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_y_continuous(labels = percent)
##

ggplot(adult_data, 
       aes(x = adult_data$marital.status, fill = adult_data$marital.status)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1,
            size = 3.5) +
  labs(x = "Marital Status", 
       y = "",
       fill = "Marital Status") +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent)

##summary
summary(adult_data[adult_data$relationship == "Not-in-family", ]$marital.status)
summary(adult_data[adult_data$relationship == "Husband", ]$marital.status)
summary(adult_data[adult_data$relationship == "Other-relative", ]$marital.status)
summary(adult_data[adult_data$relationship == "Own-child", ]$marital.status)
summary(adult_data[adult_data$relationship == "Unmarried", ]$marital.status)
summary(adult_data[adult_data$relationship == "Wife", ]$marital.status)

#Next bar plots of income by relationship status. 
ggplot(adult_data, aes(x=adult_data$relationship, fill=adult_data$income)) +
  geom_bar(position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by relationship") +   
  scale_y_continuous(breaks = seq(0,7000,500))
##bar plots with percentage having income <50k and >50k

lg.relationship <- lapply(levels(adult_data$relationship), function(v){
  
  ggplot(data = subset(adult_data, adult_data$relationship == v), 
         aes(x = subset(adult_data, adult_data$relationship == v)$income, 
             fill = subset(adult_data, adult_data$relationship == v)$income)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = c(2, -0.1),
              size = 3) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +     
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg.relationship[1:3], ncol = 2)

grid.arrange(grobs = lg.relationship[4:6], ncol = 2)


##variable race
summary(adult_data$race) 
##major share was by white 
##percentage 
adult_data$race <- factor(adult_data$race, 
                        levels = 
                          names(sort(table(adult_data$race),decreasing = TRUE)))

ggplot(adult_data, 
       aes(x = adult_data$race, fill = adult_data$race)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = c(-0.2, -0.2, -0.2, -0.2, 3)) +
  labs(x = "Race", 
       y = "",
       fill = "Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_y_continuous(labels = percent)
##86 percent of the population participated in the study are whites
##plots for individual 
lg.race <- lapply(levels(adult_data$race), function(v){
  
  ggplot(data = subset(adult_data, adult_data$race == v), 
         aes(x = subset(adult_data, adult_data$race == v)$income, 
             fill = subset(adult_data, adult_data$race == v)$income)) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = c(2, -0.1)) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold")) +     
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg.race, ncol = 3)


##variable sex with income

summary(adult_data$sex)
##percentage 
ggplot(adult_data, 
      aes(x = adult_data$sex, fill = adult_data$sex)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..) ), 
            stat = "count",
            vjust = -.1) +
  labs(x = "Gender", 
       y = "Percentage",
       fill = "Gender") +
  scale_y_continuous(labels = percent)

## use of bar plot to classify the number of male and femal earn <50k and >50k
ggplot(adult_data, aes(x = adult_data$sex, fill = adult_data$income)) +
  geom_bar(position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by gender") +   
  scale_y_continuous(breaks = seq(0,14500,1000))

##

gender.income <- lapply(levels(adult_data$sex), function(v){
  
  ggplot(data = subset(adult_data, adult_data$sex == v), 
         aes(x = subset(adult_data, adult_data$sex == v)$income, 
             fill = subset(adult_data, adult_data$sex == v)$income))+
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -0.1,
              size = 3) +
    labs(x = "Income", 
         y = "",
         fill = "Income") +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(hjust = 1)) +     
    scale_y_continuous(labels = percent) })

grid.arrange(grobs = gender.income, ncol = 2)
##86.6% of the female earn <50k nad 68.6% of the male earn <50k
# Number of men and women earning less than 50K and more than 50K:
table(adult_data$sex,adult_data$income)


##work by gender 
lg.gender.workclass <- lapply(levels(adult_data$sex), function(v){
  
  df <- subset(adult_data, adult_data$sex == v)  
  
  df <- within(df, workclass <- factor(workclass, 
                                       levels = names(sort(table(workclass), 
                                                           decreasing = TRUE))))
  
  
  ggplot(data = df, 
         aes(x = df$workclass, 
             fill = df$workclass))+
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -0.1,
              size = 3) +
    labs(x = "Workclass", 
         y = "",
         fill = "Workclass") +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +     
    scale_y_continuous(labels = percent) })

grid.arrange(grobs = lg.gender.workclass, ncol = 2)

##occupation by gender
lg.gender.occupation <- lapply(levels(adult_data$sex), function(v){
  
  df <- subset(adult_data, adult_data$sex == v)  
  
  df <- within(df, occupation <- factor(occupation, 
                                        levels = names(sort(table(occupation), 
                                                            decreasing = TRUE))))
  
  ggplot(data = df, 
         aes(x = df$occupation, 
             fill = subset(adult_data, adult_data$sex == v)$occupation))+
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -0.1,
              hjust = 0.3,
              size = 3) +
    labs(x = "Occupation", 
         y = "",
         fill = "Occupation") +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +     
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg.gender.occupation, ncol = 2)
### education grouped by gender
lg.gender.education <- lapply(levels(adult_data$sex), function(v){
  
  df <- subset(adult_data, adult_data$sex == v)  
  
  df <- within(df, education <- factor(education, 
                                       levels = names(sort(table(education), 
                                                           decreasing = TRUE))))
  
  
  ggplot(data = df, 
         aes(x = df$education, 
             fill = subset(adult_data, adult_data$sex == v)$education))+
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                  y = (..count..)/sum(..count..) ), 
              stat = "count",
              vjust = -0.1,
              size = 2.5) +
    labs(x = "Education", 
         y = "",
         fill = "Education") +
    ggtitle(paste(v)) +  
    theme(legend.position = 'none',
          plot.title = element_text(size = 11, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +     
    scale_y_continuous(labels = percent) })


grid.arrange(grobs = lg.gender.education, ncol = 2)

########################################################################################################################


##Test for indepedence variables
# Two-way contingency table with Pearson's chi-square 
# test of independence for the variables "sex" and "income":
CrossTable(adult_data$sex, adult_data$income, 
           prop.chisq = TRUE,
           chisq = TRUE)
# Pearson's chi-square test of independence for the variables
# "sex" and "income"
chisq.test(adult_data$sex, adult_data$income)
##The p-value is less than 0.05, which means that at the 0.05 significance level we fail to accept the null hypothesis 
##that the two categorical variables are independent.


##independence test for "race" and "income"
chisq.test(adult_data$race,adult_data$income)

CrossTable(adult_data$race, adult_data$income, 
           prop.chisq = TRUE,
           chisq = TRUE)
#We reject the null hypothesis at the 0.05 significance level, because the p-values is less than 0.05.
##This means there is strong indication that "race" and "income" are correlated.
  
##independence test for "workclass" and "income"
chisq.test(table(adult_data$workclass, adult_data$income))

chisq.test(table(adult_data$workclass, adult_data$income))$expected

table(adult_data$workclass)

##we see that there are no participants in the study who identify themselves as belonging to the category " Never-worked"
#Therefore we remove this unused factor level from the categorical variable"workclass"

adult_data$workclass <- droplevels(adult_data$workclass)

levels(adult_data$workclass)

summary(adult_data$workclass)
##After we removed the cells with zero expected counts, we perform again the Pearson's chi-square test
CrossTable(adult_data$workclass, adult_data$income, 
           prop.chisq = TRUE,
           chisq = TRUE)
##The p-value is very small, which means that we reject the null hypothesis at the 0.05 significance level.

##independence tets for "occupation" and "income"
chisq.test(adult_data$occupation, adult_data$income)
##We get a warning message again. Therefore we check if there are any cells with expected count less than 5
chisq.test(adult_data$occupation, adult_data$income)$expected 
#Since there is only one problematic cell, we consider the Pearson's test trustworthy.

# independence tests for "education" and "income"
chisq.test(adult_data$education, adult_data$income)

##independence test for "marital status" and "income"
chisq.test(adult_data$marital.status, adult_data$income) 

##independence test for "native region" and "income"
chisq.test(adult_data$native_region, adult_data$income)

##independence test for "hours_W" and "income"
chisq.test(adult_data$hours_w, adult_data$income)

###########################################################################################################################################



#############################model building##############################

library(ggplot2)
library(scales)
library(plyr)
library(vcd)
library(ggthemes)
library(caret)
library(GoodmanKruskal)
library(ResourceSelection)
library(randomForest)
library(e1071)
library(nnet)
library(DMwR)
adult_train<-read.csv("adult_train_df.csv")
adult_test<-read.csv("adult_data_test.csv",header = T,sep = ",",na.strings = NULL)
str(adult_train)
str(adult_data_test)

names(adult_train)
covariates<-paste("age","workclass","fnlwgt","education","education.num",
                  "marital.status","occupation","relationship","race","sex","capital.gain",
                  "capital.loss","hours.per.week","native.country","income","hours_w",
                  "native_region","cap_gain","cap_loss",sep = "+" )


form <- as.formula(paste("income ~", covariates))

start_time <- proc.time()
glm.model <- glm(formula = form,
                 data = adult_train, 
                 family = binomial(link ="logit"),
                 x = TRUE)

# The option "x=TRUE" returns the design matrix
time.logistic <- proc.time() - start_time
time.logistic
summary(glm.model)
summary(glm.model)$coefficients[, 1:2]
##finding the linear combos
findLinearCombos(glm.model$x)
findLinearCombos(glm.model$x)$remove
colnames(glm.model$x)[findLinearCombos(glm.model$x)$remove]
### we found that education and education.num gives the sam informtion so we remove the varibale education.num
covariates_new <- paste("age", "workclass", "education","marital.status",
                        "occupation", "relationship", "race", "sex",
                        "native_region", "hours_w", "cap_gain",
                        "cap_loss", sep = "+")
form_new <- as.formula(paste("income ~", covariates_new))
start_time <- proc.time()
glm.model.wld <- glm(formula = form_new,
                     data = adult_train, 
                     family = binomial(link = "logit"),
                     x = TRUE,
                     y = TRUE)
time.logistic <- proc.time() - start_time
time.logistic
##findinlinear combos
findLinearCombos(glm.model.wld$x)
### we removed the colinearity

summary(glm.model.wld)

GKmatrix <- GKtauDataframe(adult_train[, c("age", "workclass", 
                                        "education", "education.num",
                                        "marital.status", 
                                        "occupation", "relationship",
                                        "race", "sex", "hours_w",
                                        "native_region", "cap_gain",
                                        "cap_loss")])

plot(GKmatrix)
##from the graph usiing tau value we can say say that education and education.num is collinear
##All other ?? values are close to zero, except for T(relationship, marital_status), T(relationship, sex), and T(marital_status, relationship)
# The T value of 0.42 suggests that being a female or male can determine the type of relationship that an individial is in.

tab <- xtabs(~ sex + relationship, data = adult_train)

ptab <- prop.table(tab, 1)

print(format(round(ptab, 2), scientific = FALSE))

assocstats(tab)$cramer

tab1 <- xtabs(~ marital.status + relationship, data = adult_train)

assocstats(tab1)$cramer

#generalised variance inflation factor
library(car)
vif(glm.model.wld)
##all GVIF(1/(2???Df)) are less than the treshold value of 5-???=2.23601
##there is no collinearity among the variables


###goodness of fit

summary(glm.model.wld)$null.deviance

summary(glm.model.wld)$deviance




##knowing to approve or reject the null hypothesis
k <- length(glm.model.wld$coefficients)
D_M <- glm.model.wld$deviance
D_0 <- glm.model.wld$null.deviance

1 - pchisq(q = D_0 - D_M, df = k - 1)
#The p-value is approximately 0 and, hence, smaller than 0.05. 
#This means that we reject the null hypothesis that there is no significant difference between the grand mean model (intercept-only model) and the model "lm.model.wld 



head(glm.model.wld$fitted.values)


predicted.probs <- predict(glm.model.wld, type = "response")

# predicted.probs <- glm.model.wld$fitted.values

head(predicted.probs) # returns probabilities

#
observed.values <- ifelse(adult_train$income== ">50K",1,0)

predicted.probs <- predict(glm.model.wld, type = "response")

predicted.response <- ifelse(predicted.probs > 0.5, 1, 0)


head(predicted.response, 20)

head(observed.values,20)

mean(observed.values == predicted.response)
##there is a 84.89% match between observed and predicted values of the dependent variable.

##Overall significance of the categorical covariates in the model

anova(glm.model.wld, test = "LRT")

## Significance of the estimated model parameters
length(glm.model.wld$coefficients)

summary(glm.model.wld)

##
levels(adult_train$education)
summary(adult_train$education)
summary(adult_train$workclass)
confint.default(glm.model.wld)
summary(adult_train$education)

##performance of fitted line
attributes(adult_train$income)
##prediction of the train
predicted.income.train <- ifelse(predicted.probs >0.5, ">50K", "<=50K")
mean(predicted.income.train == adult_train$income)
predicted.income.train<-as.factor(predicted.income.train)

#confusion matrix
stat.log.train <- confusionMatrix(data =predicted.income.train, 
                                  reference =adult_train$income,
                                  positive =levels(adult_train$income)[2])

stat.log.train


##for test data

predicted.income.test <- predict(glm.model.wld, 
                                 newdata = adult_test, 
                                 type ="response") 

predicted.income.test <- ifelse(predicted.income.test >0.5, ">50K", "<=50K")
mean(predicted.income.test == adult_data_test$income)

predicted.income.test<-as.factor(predicted.income.test)
##confusion matrix
stat.log.test <- confusionMatrix(data = predicted.income.test, 
                                 reference = adult_data_test$income,
                                 positive = levels(adult_data_test$income)[2])

stat.log.test

##random forest

set.seed(1234)

covariates <- paste("age", "workclass", "education", 
                    "marital.status", "occupation", "relationship",
                    "race", "sex", "native_region", "hours_w",
                    "cap_gain", "cap_loss", sep = "+")

form <- as.formula(paste("income ~", covariates))

start_time <- proc.time()
ModRF <- randomForest(formula = form,
                      data = adult_train)
time.rf.over <- proc.time() - start_time
time.rf.over
ModRF
mean(predict(ModRF, newdata = adult_train) == adult_train$income)
mean(predict(ModRF, newdata = adult_data_test) == adult_data_test$income)
plot(ModRF)
print(ModRF)
confusionMatrix(data = predict(ModRF, newdata = adult_train),
                reference = adult_train$income,
                positive = levels(adult_data_test$income)[2])

###
set.seed(1234)

start_time <- proc.time()
ModRF.small <- randomForest(formula = form, 
                            data = adult_train, 
                            ntree = 200)
time.rf.small.over <- proc.time() - start_time
time.rf.small.over
print(ModRF.small)
mean(predict(ModRF.small, newdata = adult_train) == adult_train$income)

mean(predict(ModRF.small, newdata = adult_data_test) == adult_data_test$income)

##SVM
covariates <- paste("age", "workclass", "education", 
                    "marital.status", "occupation", "relationship",
                    "race", "sex", "native_region", "hours_w",
                    "cap_gain", "cap_loss", sep = "+")

form <- as.formula(paste("income ~", covariates))

my.grid <- expand.grid(C = c(0.1, 0.5, 1)) 

  