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

adult_data_test<-read.table("adult.test",sep = ",",header = F,skip = 1,na.strings = "?")
colnames(adult_data_test) <- c("age", "workclass", "fnlwgt", "education",
                       "education.num", "marital.status", "occupation",
                       "relationship", "race", "sex", "capital.gain",
                       "capital.loss", "hours.per.week",
                       "native.country", "income")
