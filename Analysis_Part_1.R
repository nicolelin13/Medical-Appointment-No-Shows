########################################
########################################
# Medical Appointment No Shows
########################################
########################################

########################################
# import data/libraries
# biref summary of data
########################################
library(lubridate)
org = read.csv("KaggleV2-May-2016.csv", sep=",", header=T, na.strings=c("","."))
head(org)
names(org)
summary(org)

MA= read.csv("KaggleV2-May-2016.csv", sep=",", header=T, na.strings=c("","."))
head(MA)
names(MA)
summary(MA)

class(MA$ScheduledDay)
class(MA$AppointmentDay)
head(MA$ScheduledDay)
#2016-04-29T18:38:08
head(MA$AppointmentDay)
#2016-04-29T00:00:00Z

########################################
# preprocess data
# change data type
# delete not meaningful cases
########################################
# Date/Time
MA$ScheduledDay = ymd_hms(MA$ScheduledDay)
MA$AppointmentDay = ymd_hms(MA$AppointmentDay)
MA$SWeekday = as.factor(weekdays(as.Date(MA$ScheduledDay,"%Y-%m-%d")))
MA$SDate = as.Date(MA$ScheduledDay,"%Y-%m-%d")
MA$STime = (as.POSIXlt(MA$ScheduledDay,format = "%Y-%m-%d %H-%M-%S"))$hour
MA$AWeekday = as.factor(weekdays(as.Date(MA$AppointmentDay,"%Y-%m-%d")))
MA$ADate = date(as.Date(MA$AppointmentDay,"%Y-%m-%d"))
# Create awaiting time
MA$Awaitday = MA$ADate-MA$SDate

# Change the variable type
MA$Scholarship = as.logical(MA$Scholarship)
MA$Hipertension = as.logical(MA$Hipertension)
MA$Diabetes = as.logical(MA$Diabetes)
MA$Alcoholism = as.logical(MA$Alcoholism)
MA$Handcap = as.logical(MA$Handcap)
MA$SMS_received = as.logical(MA$SMS_received)

# No missing value in the dataset
# Age: range from -1 to 115
as.data.frame(table(MA$Age))
#1 case with age = -1
#97    95   24
#98    96   17
#99    97   11
#100   98    6
#101   99    1
#102  100    4
#103  102    2
#104  115    5

# delete age=-1
MA = MA[MA$Age!=-1,] #delete 1 obs

# Awaitday check
as.data.frame(table(MA$Awaitday))
# range [-6,179]
# 1 case await day is -6
# 4 cases await day is -1
MA = MA[MA$Awaitday!=-6,] #delete 1 obs
MA = MA[MA$Awaitday!=-1,] #delete 4 obs

# Neighbourhood
as.data.frame(table(MA$Neighbourhood))
# 81 neighborhoods mentioned

# delete PatientId and AppointmentID
# delte SDate and Adate
#drops = c("PatientId","AppointmentID","ScheduledDay",
#          "AppointmentDay","")
#MA = MA[,!(names(MA) %in% drops)]

# create a new variable Show-- change No.Show to Show
MA$Show = NA
MA$Show[MA$No.show == "No"]="Yes"
MA$Show[MA$No.show == "Yes"]="No"
MA$Show = as.factor(MA$Show)
head(MA$No.show)
head(MA$Show)

# Schedual time
# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year and the month
MA$Syear=as.numeric(as.POSIXlt(MA$SDate)$year+1900)
MA$Smonth=as.numeric(as.POSIXlt(MA$SDate)$mon+1)
# turn months into ordered facors to control the appearance/ordering in the presentation
MA$Smonthf=factor(MA$Smonth,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# the day of week is again easily found
MA$SWeekday = as.POSIXlt(MA$SWeekday)$wday
# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
MA$Syearmonth=as.yearmon(MA$SDate)
MA$Syearmonthf=factor(MA$Syearmonth)
# then find the "week of year" for each day
MA$Sweek = as.numeric(format(MA$SDate,"%W"))

# Appointment time
# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year and the month
MA$Ayear=as.numeric(as.POSIXlt(MA$ADate)$year+1900)
MA$Amonth=as.numeric(as.POSIXlt(MA$ADate)$mon+1)
# turn months into ordered facors to control the appearance/ordering in the presentation
MA$Amonthf=factor(MA$Amonth,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
# the day of week is again easily found
#MA$AWeekday = as.POSIXlt(MA$AWeekday)$wday
# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
MA$Ayearmonth=as.yearmon(MA$ADate)
MA$Ayearmonthf=factor(MA$Ayearmonth)
# then find the "week of year" for each day
MA$Aweek = as.numeric(format(MA$ADate,"%W"))

as.data.frame(table(MA$AppointmentDay))
# only covered April, May, June


########################################
# data analysis
# visualization
########################################
library(ggplot2)
library(gridExtra)
library(gtools)

# numeric variable -- age
age1 = 
  ggplot(MA,aes(Age))+geom_histogram(breaks=seq(0, 120, by=10))+
  labs(title="Histogram of Age") +
  #scale_x_continuous(limits = c(0, 115)) +
  theme(plot.title=element_text(size=12)) 

ggplot(MA,aes(Age))+geom_histogram(bins = 20)+
  labs(title="Histogram of Age") +
  scale_x_continuous(limits = c(0, 115)) +
  theme(plot.title=element_text(size=12))
age2 = ggplot(MA,aes(x=Show,y=Age,fill = Show))+geom_boxplot()+
  labs(title="Boxplot of Age by Show")+theme(plot.title=element_text(size=12))

age3 = ggplot(MA, aes(x=Show, y=Age, fill=Show)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.2, fill="white")+
  labs(title="Plot of Age by Show",x="Show", y = "Age") + 
  scale_fill_brewer(palette="Dark2") 

grid.arrange(age1, age3,ncol=2)

ggplot(MA, aes(Age,fill=Show))+geom_density(alpha=.5)+
  labs(title="Density curve of Age by Show")+theme(plot.title=element_text(size=12))+ 
  scale_fill_brewer(palette="Dark2") 

MA$Age_cuts = cut(MA$Age, breaks = seq(0, 120, by = 10),include.lowest=TRUE)
table(MA$Age_cuts)
prop.table(table(MA$Age_cuts, MA$Show),1)

# Realizing the age over 100 years old is not representative, 
# delete the cases over 100
MA = MA[MA$Age<=100,]

# Awaiting time
# numeric variable -- Awaitday
range(MA$Awaitday)
MA$Awaitday = as.integer(MA$Awaitday)
MA$Awaitdaycut = cut(MA$Awaitday, breaks = seq(0, 180, by = 10),include.lowest=TRUE)
table(MA$Awaitdaycut)
prop.table(table(MA$Awaitdaycut, MA$Show),1)
Awaitdaytable = as.data.frame(prop.table(table(MA$Awaitdaycut, MA$Show),1))
head(Awaitdaytable)
ggplot(Awaitdaytable,aes(Awaitdaycut,fill=Show))+geom_histogram(stat="count")

colnames(Awaitdaytable) = c("Awaitday", "Show","Percentage")
ggplot(Awaitdaytable,aes(x=Awaitday,y=Percentage,color=Show))+geom_point(size = 3)+
  labs(title="Plot of Awaitday by Show",x="Awaitday Period", y = "Percentage of Show/No Show") + 
  scale_fill_brewer(palette="Dark2")+coord_flip()

wait1 = 
  ggplot(MA,aes(Awaitday))+geom_histogram(breaks=seq(0, 180, by=10))+
  labs(title="Histogram of Awaitday") +
  #scale_x_continuous(limits = c(0, 115)) +
  theme(plot.title=element_text(size=12)) 

wait2 = ggplot(MA,aes(x=Show,y=Awaitday,fill = Show))+geom_boxplot()+
  labs(title="Boxplot of Awaitday by Show")+theme(plot.title=element_text(size=12))

wait3 = ggplot(MA, aes(x=Show, y=Awaitday, fill=Show)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.05, fill="white")+
  labs(title="Plot of Awaitday by Show",x="Show", y = "Age") + 
  scale_fill_brewer(palette="Dark2") 

grid.arrange(wait1, wait3,ncol=2)

ggplot(MA, aes(Awaitday,fill=Show))+geom_density(alpha=.5)+
  labs(title="Density curve of Age by Show")+theme(plot.title=element_text(size=12))+ 
  scale_fill_brewer(palette="Dark2") 


# Factor variables
library(plyr)
# Gender
c1 = ggplot(MA,aes(x=Gender,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Gender over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c11 = ggplot(MA,aes(x=Gender))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Gender over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Scholarship
c2 = ggplot(MA,aes(x=Scholarship,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Scholarship over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c21 = ggplot(MA,aes(x=Scholarship))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Scholarship over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Hipertension
c3 = ggplot(MA,aes(x=Hipertension,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Hipertension over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c31 = ggplot(MA,aes(x=Hipertension))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Hipertension over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Diabetes
c4 = ggplot(MA,aes(x=Diabetes,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Diabetes over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c41 = ggplot(MA,aes(x=Diabetes))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Diabetes over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Alcoholism
c5 = ggplot(MA,aes(x=Alcoholism,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Alcoholism over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c51 = ggplot(MA,aes(x=Alcoholism))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Alcoholism over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Handcap
c6 = ggplot(MA,aes(x=Handcap,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Handcap over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2")
c61 = ggplot(MA,aes(x=Handcap))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Handcap over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# SMS_received
c7 = ggplot(MA,aes(x=SMS_received,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of SMS_received over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c71 = ggplot(MA,aes(x=SMS_received))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of SMS_received over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 

grid.arrange(c1, c2, c3, c4,
             c5, c6, c7,
             ncol=4)

grid.arrange(c11, c21, c31, c41,
             c51, c61, c71,
             ncol=4)

# Awaiting time
# numeric variable -- Awaitday
range(MA$Awaitday)
MA$Awaitday = as.integer(MA$Awaitday)
MA$Awaitdaycut = cut(MA$Awaitday, breaks = seq(0, 180, by = 10),include.lowest=TRUE)
table(MA$Awaitdaycut)
prop.table(table(MA$Awaitdaycut, MA$Show),1)
Awaitdaytable = as.data.frame(prop.table(table(MA$Awaitdaycut, MA$Show),1))
head(Awaitdaytable)
ggplot(Awaitdaytable,aes(Awaitdaycut,fill=Show))+geom_histogram(stat="count")

colnames(Awaitdaytable) = c("Awaitday", "Show","Percentage")
ggplot(Awaitdaytable,aes(x=Awaitday,y=Percentage,color=Show))+geom_point(size = 3)+
  labs(title="Plot of Awaitday by Show",x="Awaitday Period", y = "Percentage of Show/No Show") + 
  scale_fill_brewer(palette="Dark2")+coord_flip()

wait1 = 
  ggplot(MA,aes(Awaitday))+geom_histogram(breaks=seq(0, 180, by=10))+
  labs(title="Histogram of Awaitday") +
  #scale_x_continuous(limits = c(0, 115)) +
  theme(plot.title=element_text(size=12)) 

wait2 = ggplot(MA,aes(x=Show,y=Awaitday,fill = Show))+geom_boxplot()+
  labs(title="Boxplot of Awaitday by Show")+theme(plot.title=element_text(size=12))

wait3 = ggplot(MA, aes(x=Show, y=Awaitday, fill=Show)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.05, fill="white")+
  labs(title="Plot of Awaitday by Show",x="Show", y = "Age") + 
  scale_fill_brewer(palette="Dark2") 

grid.arrange(wait1, wait3,ncol=2)

ggplot(MA, aes(Awaitday,fill=Show))+geom_density(alpha=.5)+
  labs(title="Density curve of Age by Show")+theme(plot.title=element_text(size=12))+ 
  scale_fill_brewer(palette="Dark2") 

# STime
# numeric variable -- STime
range(MA$STime)
MA$STime = as.integer(MA$STime)
MA$STimecut = cut(MA$STime, breaks = seq(5, 21, by = 2))
table(MA$STimecut)
prop.table(table(MA$STimecut, MA$Show),1)
STimetable = as.data.frame(prop.table(table(MA$STimecut, MA$Show),1))
head(STimetable)
ggplot(STimetable,aes(STime,fill=Show))+geom_histogram(stat="count")

colnames(STimetable) = c("STime", "Show","Percentage")
ggplot(STimetable,aes(x=STime,y=Percentage,color=Show))+geom_point(size = 3)+
  labs(title="Plot of STime by Show",x="STime Period", y = "Percentage of Show/No Show") + 
  scale_fill_brewer(palette="Dark2")+coord_flip()

STime1 = 
  ggplot(MA,aes(STime))+geom_histogram(breaks=seq(0, 24, by=3))+
  labs(title="Histogram of STime") +
  #scale_x_continuous(limits = c(0, 24)) +
  theme(plot.title=element_text(size=12)) 

STime2 = ggplot(MA,aes(x=Show,y=Awaitday,fill = Show))+geom_boxplot()+
  labs(title="Boxplot of Awaitday by Show")+theme(plot.title=element_text(size=12))

STime3 = ggplot(MA, aes(x=Show, y=STime, fill=Show)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.05, fill="white")+
  labs(title="Plot of STime by Show",x="Show", y = "Schedual Time") + 
  scale_fill_brewer(palette="Dark2") 

grid.arrange(STime1, STime3,ncol=2)

ggplot(MA, aes(Awaitday,fill=Show))+geom_density(alpha=.5)+
  labs(title="Density curve of Age by Show")+theme(plot.title=element_text(size=12))+ 
  scale_fill_brewer(palette="Dark2") 





# Factor variables in time
library(plyr)
# SMonthf
c1 = ggplot(MA,aes(x=SMonth,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of SMonth over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c11 = ggplot(MA,aes(x=SMonth))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of SMonth over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Amonthf
c2 = ggplot(MA,aes(x=Scholarship,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Scholarship over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c21 = ggplot(MA,aes(x=Scholarship))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Scholarship over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Hipertension
c3 = ggplot(MA,aes(x=Hipertension,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Hipertension over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c31 = ggplot(MA,aes(x=Hipertension))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Hipertension over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Diabetes
c4 = ggplot(MA,aes(x=Diabetes,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Diabetes over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c41 = ggplot(MA,aes(x=Diabetes))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Diabetes over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Alcoholism
c5 = ggplot(MA,aes(x=Alcoholism,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Alcoholism over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c51 = ggplot(MA,aes(x=Alcoholism))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Alcoholism over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# Handcap
c6 = ggplot(MA,aes(x=Handcap,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of Handcap over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2")
c61 = ggplot(MA,aes(x=Handcap))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of Handcap over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
# SMS_received
c7 = ggplot(MA,aes(x=SMS_received,fill=Show))+geom_bar(position="dodge")+
  labs(title="Bar Chart of SMS_received over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 
c71 = ggplot(MA,aes(x=SMS_received))+geom_bar(aes(fill = Show))+
  labs(title="Bar Chart of SMS_received over Show") +theme(plot.title=element_text(size=11))+
  scale_fill_brewer(palette="Dark2") 

grid.arrange(c1, c2, c3, c4,
             c5, c6, c7,
             ncol=4)

grid.arrange(c11, c21, c31, c41,
             c51, c61, c71,
             ncol=4)
as.data.frame(table(MA$AppointmentDay))
# only covered April, May, June
as.data.frame(table(MA$ScheduledDay))



########################################
# Experimental Result
# Sample method
# KNN, Naive Bay, Decision Tree
# Logistic Regression
# SVM, Kernel SVM
# Ensemble
########################################

keep = c("Gender","Age","Scholarship","Hipertension",
         "Diabetes","Alcoholism","Handcap","SMS_received",
         "Show","Awaitday","SWeekday","AWeekday",
         "STime","Smonthf","Amonthf")
MAF = MA[,(names(MA) %in% keep)]

# training vs. test dataset
set.seed(123)
ind = sample(3,nrow(MAF),replace = TRUE, prob = c(0.7,0.15,0.15))
MAFtrain = MAF[ind==1,]
MAFvalidation = MAF[ind==2, ]
MAFtest = MAF[ind==3,]

# smaple method-- 10-fold cross validation with 3 repeats in training
library(caret)
library(klaR)
library(combinat)
library(e1071)
train_control = trainControl(method="repeatedcv", number=10, repeats=2)
train_control2 = trainControl(method="repeatedcv", number=10, repeats=2,
                              classProbs = TRUE,summaryFunction=twoClassSummary)


########################################
# Experimental Result
# Naive Bay
########################################
modelnb = train(Show~.,data=MAFtrain,trControl=train_control,method="nb")
print(modelnb)
summary(modelnb)
modelnb_pred = predict(modelnb, newdata = MAFvalidation)
modelnb_perf = table(modelnb_pred,MAFvalidation$Show,
                     dnn = c("Predicted","Actural"))
modelnb_perf
confusionMatrix(modelnb_perf,positive = "Yes") 
confusionMatrix(modelnb_perf,positive = "No")
confusionMatrix(modelnb_perf)
#Actural
#Predicted    No   Yes
#No      5     9
#Yes  3304 13218
#Accuracy : 0.7989         
#95% CI : (0.7927, 0.805)
nbtable = as.matrix(confusionMatrix(modelnb_perf))
BDtest(nbtable,pr =0.5, conf.level = 0.95)

########################################
# Experimental Result
# Logistic Regression
########################################
set.seed(123)
lg = train(Show~.,data=MAFtrain,trControl=train_control2,
           method="glm")
print(lg)
print(lg$finalModel)
summary(lg)
lg_pred = predict(lg, newdata = MAFvalidation)
lg_perf = table(lg_pred,MAFvalidation$Show,
                dnn = c("Predicted","Actural"))
lg_perf
confusionMatrix(lg_perf)
lgtable = as.matrix(confusionMatrix(lg_perf))
BDtest(lgtable,pr =0.5, conf.level = 0.95)

lg2 = train(Show~.,data=MAFtrain,trControl=train_control2,
            method="glm",metric="Sens")
print(lg2)
print(lg2$finalModel)
summary(lg2)
lg_pred2 = predict(lg2, newdata = MAFvalidation)
lg_perf2 = table(lg_pred2,MAFvalidation$Show,
                 dnn = c("Predicted","Actural"))
lg_perf2
confusionMatrix(lg_perf2)
lgtable2 = as.matrix(confusionMatrix(lg_perf2))
BDtest(lgtable2,pr =0.5, conf.level = 0.95)

########################################
# Experimental Result
# Decision Tree
########################################

# Decision Tree
library(rattle)
library(rpart)
library(rpart.plot)
library(party)
library(bdpv)
#modeltree = rpart(Show~.-Show,data = MAFtrain, 
#                  parms = list(split="information"),
#                  control=rpart.control(minsplit=2, minbucket=1, cp=0.00001))
#modeltree$cptable
#modeltree$results

modeltree = train(Show~.,data=MAFtrain,trControl=train_control2,method="rpart",
                  parms = list(split = "information"),metric="ROC",
                  control=rpart.control(minsplit=2, minbucket=1, cp=0.00001),
                  tuneLength = 20)
modeltree
plot(modeltree$finalModel)
prp(modeltree$finalModel)
modeltree_pred = predict(modeltree,MAFvalidation)
modeltree_perf = table(modeltree_pred,MAFvalidation$Show,
                       dnn = c("Predicted","Actural"))
modeltree_perf
confusionMatrix(modeltree_perf) 
dbtable = as.matrix(confusionMatrix(modeltree_pred,MAFvalidation$Show))
BDtest(dbtable,pr =0.5, conf.level = 0.95)

#Metric Sens
modeltree2 = train(Show~.,data=MAFtrain,trControl=train_control2,method="rpart",
                   parms = list(split = "information"),metric="Sens",
                   control=rpart.control(minsplit=2, minbucket=10, cp=0.0001),
                   tuneLength = 20)
plot(modeltree2)
plot(modeltree2$finalModel)
modeltree_pred2 = predict(modeltree2,MAFvalidation)
modeltree_perf2 = table(modeltree_pred2,MAFvalidation$Show,
                        dnn = c("Predicted","Actural"))
modeltree_perf2
confusionMatrix(modeltree_perf2)
dbtable2 = as.matrix(confusionMatrix(modeltree_perf2))
BDtest(dbtable2,pr =0.5, conf.level = 0.95)

#Metric Sensitivity
modeltree3 = train(Show~.,data=MAFtrain,trControl=train_control2,method="rpart",
                   metric="Sens",
                   control=rpart.control(minsplit=2, minbucket=10, cp=0.0001),
                   tuneLength = 20)
modeltree_pred3 = predict(modeltree3,MAFvalidation)
modeltree_perf3 = table(modeltree_pred3,MAFvalidation$Show,
                        dnn = c("Predicted","Actural"))
modeltree_perf3
confusionMatrix(modeltree_perf3) 
confusionMatrix(modeltree_perf3,positive = "No")
summary(modeltree3)
dbtable3 = as.matrix(confusionMatrix(modeltree_perf3))
BDtest(dbtable3,pr =0.5, conf.level = 0.95)

#weight 7:3
modeltree4 = train(Show~.,data=MAFtrain,trControl=train_control2,method="rpart",
                   metric="Sens",parms = list(prior=c(0.7,0.3),split = "information"),
                   control=rpart.control(minsplit=2, minbucket=10, cp=0.0001),
                   tuneLength = 20)
modeltree_pred4 = predict(modeltree4,MAFvalidation)
modeltree_perf4 = table(modeltree_pred4,MAFvalidation$Show,
                        dnn = c("Predicted","Actural"))
modeltree_perf4
confusionMatrix(modeltree_perf4)
dbtable4 = as.matrix(confusionMatrix(modeltree_perf4))
BDtest(dbtable4,pr =0.5, conf.level = 0.95)

#weight 6:4
modeltree5 = train(Show~.,data=MAFtrain,trControl=train_control2,method="rpart",
                   metric="Sens",parms = list(prior=c(0.6,0.4),split = "information"),
                   control=rpart.control(minsplit=2, minbucket=10, cp=0.0001),
                   tuneLength = 20)
modeltree_pred5 = predict(modeltree5,MAFvalidation)
modeltree_perf5 = table(modeltree_pred5,MAFvalidation$Show,
                        dnn = c("Predicted","Actural"))
modeltree_perf5
confusionMatrix(modeltree_perf5)
dbtable5 = as.matrix(confusionMatrix(modeltree_perf5))
BDtest(dbtable5,pr =0.5, conf.level = 0.95)



########################################
# Experimental Result
# Random Forest
########################################
# random forest
library(randomForest)
library(caret)
library(e1071)
train_rf = randomForest(Show~.,data = MAFtrain,ntree=100,cp=0.0001,importance=TRUE)
train_rf
importance(train_rf,type=2)
# compare with validation dataset
rf_pred = predict(train_rf,MAFvalidation)
rf_perf = table(rf_pred,MAFvalidation$Show,
                dnn = c('Predicted','Actural'))
rf_perf
confusionMatrix(rf_perf)
rftable = as.matrix(confusionMatrix(rf_perf))
BDtest(rftable,pr =0.5, conf.level = 0.95)

# random forest ntree=200
train_rf2 = randomForest(Show~.,data = MAFtrain,ntree=200,cp=0.0001,importance=TRUE)
train_rf2
importance(train_rf2,type=2)
# compare with validation dataset
rf_pred2 = predict(train_rf2,MAFvalidation)
rf_perf2 = table(rf_pred2,MAFvalidation$Show,
                 dnn = c('Predicted','Actural'))
rf_perf2
confusionMatrix(rf_perf2)
rftable2 = as.matrix(confusionMatrix(rf_perf2))
BDtest(rftable2,pr =0.5, conf.level = 0.95)

# random forest ntree=300
# too big for caluculate
# change the weight of class
train_rf3 = randomForest(Show~.,data = MAFtrain,ntree=100,cp=0.0001,
                         importance=TRUE,classwt=c(0.7,0.3))
train_rf3
importance(train_rf3,type=2)
# compare with validation dataset
rf_pred3 = predict(train_rf3,MAFvalidation)
rf_perf3 = table(rf_pred3,MAFvalidation$Show,
                 dnn = c('Predicted','Actural'))
rf_perf3
confusionMatrix(rf_perf3)
rftable3 = as.matrix(confusionMatrix(rf_perf3))
BDtest(rftable3,pr =0.5, conf.level = 0.95)

# cchange the weight
train_rf4 = randomForest(Show~.,data = MAFtrain,ntree=100,cp=0.0001,
                         importance=TRUE,classwt=c(0.8,0.2))
train_rf4
importance(train_rf4,type=2)
# compare with validation dataset
rf_pred4 = predict(train_rf4,MAFvalidation)
rf_perf4 = table(rf_pred4,MAFvalidation$Show,
                 dnn = c('Predicted','Actural'))
rf_perf4
confusionMatrix(rf_perf4)
rftable4 = as.matrix(confusionMatrix(rf_perf4))
BDtest(rftable4,pr =0.5, conf.level = 0.95)

# cchange the weight
train_rf5 = randomForest(Show~.,data = MAFtrain,ntree=100,cp=0.0001,
                         importance=TRUE,classwt=c(0.6,0.4))
train_rf5

importance(train_rf5,type=2)
# compare with validation dataset
rf_pred5 = predict(train_rf5,MAFvalidation)
rf_perf5 = table(rf_pred5,MAFvalidation$Show,
                 dnn = c('Predicted','Actural'))
rf_perf5
confusionMatrix(rf_perf5)
rftable5 = as.matrix(confusionMatrix(rf_perf5))
BDtest(rftable5,pr =0.5, conf.level = 0.95)

########################################
# Experimental Result
# Boosting
########################################

library(C50)
set.seed(123)
c50 = train(Show~.,data = MAFtrain, trControl=train_control,
            method="C5.0",metric="Accuracy")
c50_pred = predict(c50,MAFvalidation)
c50_perf = table(c50_pred,MAFvalidation$Show,
                 dnn = c("Predicted","Actural"))
c50_perf
summary(c50)
confusionMatrix(c50_perf)
c50table = as.matrix(confusionMatrix(c50_perf))
BDtest(c50table,pr =0.5, conf.level = 0.95)

set.seed(123)
c502 = train(Show~.,data = MAFtrain, trControl=train_control,
             method="C5.0",metric="Sens")
c50_pred2 = predict(c502,MAFvalidation)
c50_perf2 = table(c50_pred2,MAFvalidation$Show,
                  dnn = c("Predicted","Actural"))
c50_perf2
confusionMatrix(c50_perf2)
c50table2 = as.matrix(confusionMatrix(c50_perf2))
BDtest(c50table2,pr =0.5, conf.level = 0.95)

c503 = train(Show~.,data = MAFtrain, trControl=train_control2,
             method="C5.0",metric="Accuracy")
c50_pred3 = predict(c503,MAFvalidation)
c50_perf3 = table(c50_pred3,MAFvalidation$Show,
                  dnn = c("Predicted","Actural"))
c50_perf3
c50table3 = as.matrix(confusionMatrix(c50_perf3))
BDtest(c50table3,pr =0.5, conf.level = 0.95)

c504 = train(Show~.,data = MAFtrain, trControl=train_control2,
             method="C5.0",metric="Sens")
c50_pred4 = predict(c504,MAFvalidation)
c50_perf4 = table(c50_pred4,MAFvalidation$Show,
                  dnn = c("Predicted","Actural"))
c50_perf4
c50table4 = as.matrix(confusionMatrix(c50_perf4))
BDtest(c50table4,pr =0.5, conf.level = 0.95)

library(gbm)
gbm = train(Show~., data = MAFtrain, trControl=train_control,
            method="gbm", metric="Accuracy", verbose=FALSE)
gbm_pred = predict(gbm,MAFvalidation)
gbm_perf = table(gbm_pred,MAFvalidation$Show,
                 dnn = c("Predicted","Actural"))
gbm_perf
summary(gbm)
confusionMatrix(gbm_perf)
gbmtable = as.matrix(confusionMatrix(gbm_perf))
BDtest(gbmtable,pr =0.5, conf.level = 0.95)

gbm2 = train(Show~., data = MAFtrain, trControl=train_control,
             method="gbm", metric="Sens", verbose=FALSE)
gbm_pred2 = predict(gbm2,MAFvalidation)
gbm_perf2 = table(gbm_pred2,MAFvalidation$Show,
                  dnn = c("Predicted","Actural"))
gbm_perf2
confusionMatrix(gbm_perf2)
gbmtable2 = as.matrix(confusionMatrix(gbm_perf2))
BDtest(gbmtable2,pr =0.5, conf.level = 0.95)

gbm3 = train(Show~., data = MAFtrain, trControl=train_control2,
             method="gbm", metric="Accuracy", verbose=FALSE)
gbm_pred3 = predict(gbm3,MAFvalidation)
gbm_perf3 = table(gbm_pred3,MAFvalidation$Show,
                  dnn = c("Predicted","Actural"))
gbm_perf3
confusionMatrix(gbm_perf3)
gbmtable3 = as.matrix(confusionMatrix(gbm_perf3))
BDtest(gbmtable3,pr =0.5, conf.level = 0.95)

gbm4 = train(Show~., data = MAFtrain, trControl=train_control2,
             method="gbm", metric="Sens", verbose=FALSE)
gbm_pred4 = predict(gbm4,MAFvalidation)
gbm_perf4 = table(gbm_pred4,MAFvalidation$Show,
                  dnn = c("Predicted","Actural"))
gbm_perf4
confusionMatrix(gbm_perf4)
gbmtable4 = as.matrix(confusionMatrix(gbm_perf4))
BDtest(gbmtable4,pr =0.5, conf.level = 0.95)

########################################
# Experimental Result
# SVM
########################################

library(e1071)
set.seed(123)
svm1 = svm(Show~.,data=MAFtrain)
svm1
svm1.pred = predict(svm1,MAFvalidation)
svm1.perf = table(svm1.pred,MAFvalidation$Show,
                  dnn = c('Predicted','Actural'))
svm1.perf
svmtable1 = as.matrix(confusionMatrix(svm1.perf))
BDtest(svmtable1,pr =0.5, conf.level = 0.95)

set.seed(123)
tuned = tune.svm(Show~.,data=MAFtrain,
                 gamma = 10^(-6:10),
                 cost = 10^(-10:10))

svm5 = svm(Show~.,data=MAFtrain,cost=1)
svm5
svm5.pred = predict(svm5,MAFvalidation)
svm5.perf = table(svm5.pred,MAFvalidation$Show,
                  dnn = c('Predicted','Actural'))
svm5.perf
confusionMatrix(svm5.perf)
svmtable5 = as.matrix(confusionMatrix(svm5.perf))
BDtest(svmtable5,pr =0.5, conf.level = 0.95)

svm6 = svm(Show~.,data=MAFtrain,cost=0.1)
svm6
svm6.pred = predict(svm6,MAFvalidation)
svm6.perf = table(svm6.pred,MAFvalidation$Show,
                  dnn = c('Predicted','Actural'))
svm6.perf
confusionMatrix(svm6.perf)
svmtable6 = as.matrix(confusionMatrix(svm6.perf))
BDtest(svmtable1,pr =0.5, conf.level = 0.95)

svm7 = svm(Show~.,data=MAFtrain,cost=0.01)
svm7
svm7.pred = predict(svm7,MAFvalidation)
svm7.perf = table(svm7.pred,MAFvalidation$Show,
                  dnn = c('Predicted','Actural'))
svm7.perf
svmtable1 = as.matrix(confusionMatrix(svm7.perf))
BDtest(svmtable7,pr =0.5, conf.level = 0.95)



########################################
# Experimental Result
# Ensemble stacking
########################################
library(caretEnsemble)

algorithmlist = c("nb","glm","rpart","gbm","svmLinear","rf")
set.seed(123)
models = caretList(Show~., data = MAFtrain,trControl=train_control2,
                   methodList=algorithmlist)
models_pred = predict(models,MAFvalidation)
models_perf = table(models_pred,MAFvalidation$Show,
                    dnn = c("Predicted","Actural"))
models_perf
confusionMatrix(models_perf)
modelstable = as.matrix(confusionMatrix(models_perf))
BDtest(modelstable,pr =0.5, conf.level = 0.95)

set.seed(123)
models2 = caretList(Show~., data = MAFtrain,trControl=train_control2,
                    methodList=algorithmlist, metric="Sens")
models_pred2 = predict(models2,MAFvalidation)
models_perf2 = table(models_pred2,MAFvalidation$Show,
                     dnn = c("Predicted","Actural"))
models_perf2
confusionMatrix(models_perf2)
modelstable = as.matrix(confusionMatrix(models_perf))
BDtest(modelstable,pr =0.5, conf.level = 0.95)