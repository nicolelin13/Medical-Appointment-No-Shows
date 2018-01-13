######################################## 
# import data
# biref summary of data 
######################################## 
library(stringi)
library(lubridate)
MA= read.csv("KaggleV2-May-2016.csv", sep=",", header=T, na.strings=c("",".")) head(MA)
names(MA)
summary(MA)
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
# delete age=-1
MA = MA[MA$Age!=-1,] #delete 1 obs
# Realizing the age over 100 years old is not representative,
# delete the cases over 100 
MA = MA[MA$Age<=100,]
# Awaitday check
# 1 case await day is -6
# 4 cases await day is -1
MA = MA[MA$Awaitday!=-6,] #delete 1 obs 
MA = MA[MA$Awaitday!=-1,] #delete 4 obs 
# Awaiting time
# numeric variable -- Awaitday 
MA$Awaitday = as.integer(MA$Awaitday)
# Schedual time
# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year and the month
MA$Syear=as.numeric(as.POSIXlt(MA$SDate)$year+1900)
MA$Smonth=as.numeric(as.POSIXlt(MA$SDate)$mon+1)
# turn months into ordered facors to control the appearance/ordering in the presentation
MA$Smonthf=factor(MA$Smonth,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Ju n","Jul","Aug","Sep","Oct","Nov","Dec"))
as.data.frame(table(MA$SDate))
# only covered "Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
# Appointment time
# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year and the month
MA$Ayear=as.numeric(as.POSIXlt(MA$ADate)$year+1900) 
MA$Amonth=as.numeric(as.POSIXlt(MA$ADate)$mon+1)
# turn months into ordered facors to control the appearance/ordering in the presentation

MA$Amonthf=factor(MA$Amonth,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Ju n","Jul","Aug","Sep","Oct","Nov","Dec"))
as.data.frame(table(MA$AppointmentDay)) 
# only covered April, May, June
# Change the variable type
MA$Scholarship = as.factor(MA$Scholarship) 
MA$Hipertension = as.factor(MA$Hipertension) 
MA$Diabetes = as.factor(MA$Diabetes) 
MA$Alcoholism = as.factor(MA$Alcoholism) 
MA$Handcap = as.factor(MA$Handcap) 
MA$SMS_received = as.factor(MA$SMS_received)
# create a new variable Show-- change No.Show to Show 
MA$Show = NA
MA$Show[MA$No.show == "No"]="Yes" 
MA$Show[MA$No.show == "Yes"]="No"
MA$Show = as.factor(MA$Show)
######################################## 
# Dataset preparation 
########################################
keep = c("Gender","Age","Scholarship","Hipertension", "Diabetes","Alcoholism","Handcap","SMS_received", "Show","Awaitday","SWeekday","AWeekday", "Smonthf","Amonthf")
MAF = MA[,(names(MA) %in% keep)] # training vs. test dataset

set.seed(123)
ind = sample(3,nrow(MAF),replace = TRUE, prob = c(0.7,0.15,0.15)) 
MAFtrain = MAF[ind==1,]
MAFvalidation = MAF[ind==2, ]
MAFtest = MAF[ind==3,]
# change teh variable Age and Awaitday from integer to numeric MAFtrain[2] = lapply(MAFtrain[2], as.numeric)
MAFtrain[11] = lapply(MAFtrain[11], as.numeric)
MAFtest[2] = lapply(MAFtest[2], as.numeric) 
MAFtest[11] = lapply(MAFtest[11], as.numeric)
MAFvalidation[2] = lapply(MAFvalidation[2], as.numeric) 
MAFvalidation[11] = lapply(MAFvalidation[11], as.numeric)

######################################## 
# Bayesian Network 
######################################## 
install.packages("bnlearn")
library(bnlearn) 
source("http://bioconductor.org/biocLite.R") 
biocLite("Rgraphviz")
keep2 = c("Gender","Scholarship","Hipertension", "Diabetes","Alcoholism","Handcap","SMS_received", "Show","SWeekday","AWeekday", "Smonthf","Amonthf")
MAFtrain2 = MAFtrain[,(names(MAFtrain) %in% keep2)] 
## Comparison of plots and scores

## Constraint-Based Learning Algorithms
# Constraint-Based Learning Algorithms: Grow-Shrink
bn1 = gs(MAFtrain)
plot(bn1)
# Constraint-Based Learning Algorithms: Incremental Association
bn2 = iamb(MAFtrain)
plot(bn2)
# Constraint-Based Learning Algorithms: Fast Incremental Association
bn3 = fast.iamb(MAFtrain)
plot(bn3)
# Constraint-Based Learning Algorithms: Interleaved Incremental Association 
bn4 = inter.iamb(MAFtrain)
plot(bn4)
##Score-based Learning Algorithms
# Score-based Learning Algorithms: Hill-Climbing
bn5 = hc(MAFtrain)
print(bn5)
plot(bn5)
# Score-based Learning Algorithms: Tabu Search
bn6 = tabu(MAFtrain)
print(bn6)
plot(bn6)
## Hybrid Learning Algorithms
# Hybrid Learning Algorithms: Max-Min Hill-Climbing
bn7 = mmhc(MAFtrain)
plot(bn7)
# Hybrid Learning Algorithms: Restricted Maximization
bn8 = rsmax2(MAFtrain)
plot(bn8)
# loglik-cg

log_hc = score(bn5,MAFtrain,type = "loglik-cg") 
log_tabu = score(bn6,MAFtrain,type = "loglik-cg") 
# aic-cg
aic_hc = score(bn5,MAFtrain,type = "aic-cg") 
aic_tabu = score(bn6,MAFtrain,type = "aic-cg")
# bic-c
bic_hc = score(bn5,MAFtrain,type = "bic-cg") 
bic_tabu = score(bn6,MAFtrain,type = "bic-cg")
# Because there are only bn5 and bn6 including "Show", the 2 model are for final comparison. 
# Considering the comparisons of the performance of networks by different algorithms,
# a modified hill-climbing algorithm, tabu search is the best.
# modification will be provided on this network
# Because the model is to predict the target variable class,
# based on the initial finding, hc and tabu are more appropriate for this problem
# but with different score, the network can be very different,
# so for this issue, I used 3 scores for the 2 approaches -- 6 models for comparison. 
# use test set to compare the prediction power of the 6 models
bn9 = hc(MAFtrain,score='loglik-cg') 
plot(bn9)
bn10 = hc(MAFtrain,score='aic-cg') 
plot(bn10)
bn11 = hc(MAFtrain,score='bic-cg') 
plot(bn11)
bn12 = tabu(MAFtrain,score='loglik-cg') 
plot(bn12)
bn13 = tabu(MAFtrain,score='aic-cg') 
plot(bn13)
bn14 = tabu(MAFtrain,score='bic-cg')
plot(bn14)

install.packages("caret") 
install.packages("e1071") 
install.packages("bdpv") 
library(caret) 
library(e1071) 
library(bdpv)

fit9 = bn.fit(bn9, MAFtrain)
predict9 = predict(fit9, "Show", MAFtest) 
predict9_table = table(predict9,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predict9_table)
table9 = as.matrix(confusionMatrix(predict9_table)) 
BDtest(table9, pr=0.5, conf.level = 0.95)

fit10 = bn.fit(bn10, MAFtrain)
predict10 = predict(fit10, "Show", MAFtest) 
predict10_table = table(predict10,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predict10_table)
table10 = as.matrix(confusionMatrix(predict10_table)) 
BDtest(table10, pr=0.5, conf.level = 0.95)

fit11 = bn.fit(bn11, MAFtrain)
predict11 = predict(fit11, "Show", MAFtest) 
predict11_table = table(predict11,MAFtest$Show, dnn = c("Predict","Actual")) 
confusionMatrix(predict11_table)
table11= as.matrix(confusionMatrix(predict11_table))
BDtest(table11, pr=0.5, conf.level = 0.95)

fit12 = bn.fit(bn12, MAFtrain)
predict12= predict(fit12, "Show", MAFtest) 
predict12_table = table(predict12,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predict12_table)
table12= as.matrix(confusionMatrix(predict12_table)) 
BDtest(table12, pr=0.5, conf.level = 0.95)

fit13= bn.fit(bn13, MAFtrain)
predict13= predict(fit13, "Show", MAFtest) 
predict13_table = table(predict13,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predict13_table)
table13= as.matrix(confusionMatrix(predict13_table)) 
BDtest(table13, pr=0.5, conf.level = 0.95)

fit14= bn.fit(bn14, MAFtrain)
predict14= predict(fit14, "Show", MAFtest) 
predict14_table = table(predict14,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predict14_table)
table14= as.matrix(confusionMatrix(predict14_table)) 
BDtest(table14, pr=0.5, conf.level = 0.95)

# based on intial exploration, there is no difference among all the models
# no model could perform better than the majority accuracy
# boostrap based inferece
bootbn9 = boot.strength(data=MAFtrain,R=20, algorithm = "hc",algorithm.args = list(score="loglik-cg"))
avebootbn9 = averaged.network(bootbn9,threshold = 0.85) 
plot(avebootbn9)
fitbootbn9 = bn.fit(cextend(avebootbn9), MAFtrain) 
predictbootbn9 = predict(fitbootbn9, "Show", MAFtest) 
predictbootbn9_table = table(predictbootbn9,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predictbootbn9_table)
tablebootbn9= as.matrix(confusionMatrix(predictbootbn9_table)) 
BDtest(tablebootbn9, pr=0.5, conf.level = 0.95)
## confusion matrix a little difference!!!

bootbn10 = boot.strength(data=MAFtrain,R=20, algorithm = "hc",algorithm.args = list(score="aic-cg")) 
avebootbn10 = averaged.network(bootbn10,threshold = 0.85)
plot(avebootbn10)
fitbootbn10 = bn.fit(cextend(avebootbn10), MAFtrain)
predictbootbn10 = predict(fitbootbn10, "Show", MAFtest)
predictbootbn10_table = table(predictbootbn10,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predictbootbn10_table)
tablebootbn10= as.matrix(confusionMatrix(predictbootbn10_table)) 
BDtest(tablebootbn10, pr=0.5, conf.level = 0.95)
bootbn11 = boot.strength(data=MAFtrain,R=20, algorithm = "hc",algorithm.args = list(score="bic-cg")) 
avebootbn11 = averaged.network(bootbn11,threshold = 0.85)
plot(avebootbn11)

# only smonthf
fitbootbn11 = bn.fit(cextend(avebootbn11), MAFtrain) 
predictbootbn11 = predict(fitbootbn11, "Show", MAFtest) 
predictbootbn11_table = table(predictbootbn11,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predictbootbn11_table)

tablebootbn11= as.matrix(confusionMatrix(predictbootbn11_table)) 
BDtest(tablebootbn11, pr=0.5, conf.level = 0.95)
bootbn12 = boot.strength(data=MAFtrain,R=20, algorithm = "tabu",algorithm.args = list(score="loglik- cg"))
avebootbn12 = averaged.network(bootbn12,threshold = 0.85) 
plot(avebootbn12)
fitbootbn12 = bn.fit(cextend(avebootbn12), MAFtrain) 
predictbootbn12 = predict(fitbootbn12, "Show", MAFtest) 
predictbootbn12_table = table(predictbootbn12,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predictbootbn12_table)
tablebootbn12= as.matrix(confusionMatrix(predictbootbn12_table)) 
BDtest(tablebootbn12, pr=0.5, conf.level = 0.95)

#no consistent extension of avebootbn13 is possible.
bootbn13 = boot.strength(data=MAFtrain,R=20, algorithm = "tabu",algorithm.args = list(score="aic-cg")) 
avebootbn13 = averaged.network(bootbn13,threshold = 0.85)
plot(avebootbn13)
fitbootbn13 = bn.fit(cextend(avebootbn13), MAFtrain)
predictbootbn13 = predict(fitbootbn13, "Show", MAFtest)
predictbootbn13_table = table(predictbootbn13,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predictbootbn13_table)
tablebootbn13= as.matrix(confusionMatrix(predictbootbn13_table)) 
BDtest(tablebootbn13, pr=0.5, conf.level = 0.95)

bootbn14 = boot.strength(data=MAFtrain,R=20, algorithm = "tabu",algorithm.args = list(score="bic-cg")) 
avebootbn14 = averaged.network(bootbn14,threshold = 0.85)
plot(avebootbn14)

# only Smonthf
fitbootbn14 = bn.fit(cextend(avebootbn14), MAFtrain) 
predictbootbn14 = predict(fitbootbn14, "Show", MAFtest) 
predictbootbn14_table = table(predictbootbn14,MAFtest$Show,dnn = c("Predict","Actual")) 
confusionMatrix(predictbootbn14_table)
tablebootbn14= as.matrix(confusionMatrix(predictbootbn14_table)) 
BDtest(tablebootbn14, pr=0.5, conf.level = 0.95)

# 6 stacked plots
par(mfrow=c(2,3))
plot(avebootbn9, main="BN with Hill-climbing(loglik-cg)") 
plot(avebootbn10, main="BN with Hill-climbing(aic-cg)") 
plot(avebootbn11, main="BN with Hill-climbing(bic-cg)") 
plot(avebootbn12, main="BN with Tabu(loglik-cg)") 
plot(avebootbn13, main="BN with Tabu(aic-cg)") 
plot(avebootbn14, main="BN with Tabu(bic-cg)") 
par(mfrow=c(1,1))
plot(avebootbn9, main="BN with Hill-climbing(loglik-cg)") 
plot(avebootbn10, main="BN with Hill-climbing(aic-cg)") 
plot(avebootbn11, main="BN with Hill-climbing(bic-cg)") 
plot(avebootbn12, main="BN with Tabu(loglik-cg)") 
plot(avebootbn13, main="BN with Tabu(aic-cg)") 
plot(avebootbn14, main="BN with Tabu(bic-cg)")

############################################################# 
# Use all discrete variables 
############################################################ 
keep2 = c("Gender","Scholarship","Hipertension","Diabetes","Alcoholism","Handcap","SMS_received",
          "Show","SWeekday","AWeekday","Smonthf","Amonthf")
MAF2 = MA[,(names(MA) %in% keep2)]

# training vs. test dataset
set.seed(123)
ind2 = sample(3,nrow(MAF),replace = TRUE, prob = c(0.7,0.15,0.15)) 
MAFtrain2 = MAF2[ind2==1,]
MAFvalidation2 = MAF2[ind2==2, ]
MAFtest2 = MAF2[ind2==3,]

dboot1 = boot.strength(data=MAFtrain2,R=20, algorithm = "hc",algorithm.args = list(score="bde")) 
avedboot1 = averaged.network(dboot1,threshold = 0.85)
plot(avedboot1)
#no consistent extension of avedboot1 is possible.
fitdboot1 = bn.fit(cextend(avedboot1), MAFtrain2) 
predictdboot1 = predict(fitdboot1, "Show", MAFtest2) 
dboot1_table = table(predictdboot1,MAFtest2$Show,dnn = c("Predict","Actual")) 
confusionMatrix(dboot1_table)
tabledboot1= as.matrix(confusionMatrix(dboot1_table)) 
BDtest(tabledboot1, pr=0.5, conf.level = 0.95)

dboot2 = boot.strength(data=MAFtrain2,R=20, algorithm = "hc",algorithm.args = list(score="bds")) 
avedboot2 = averaged.network(dboot2,threshold = 0.85)
plot(avedboot2)
fitdboot2 = bn.fit(cextend(avedboot2), MAFtrain2)
predictdboot2 = predict(fitdboot2, "Show", MAFtest2) 
dboot2_table = table(predictdboot2,MAFtest2$Show,dnn = c("Predict","Actual"))
confusionMatrix(dboot2_table)
tabledboot2= as.matrix(confusionMatrix(dboot2_table)) 
BDtest(tabledboot2, pr=0.5, conf.level = 0.95)

dboot3 = boot.strength(data=MAFtrain2,R=20, algorithm = "hc",algorithm.args = list(score="mbde")) 
avedboot3 = averaged.network(dboot3,threshold = 0.85)
plot(avedboot3)
fitdboot3 = bn.fit(cextend(avedboot3), MAFtrain2)
predictdboot3 = predict(fitdboot3, "Show", MAFtest2) 
dboot3_table = table(predictdboot3,MAFtest2$Show,dnn = c("Predict","Actual")) 
confusionMatrix(dboot3_table)
tabledboot3= as.matrix(confusionMatrix(dboot3_table)) 
BDtest(tabledboot3, pr=0.5, conf.level = 0.95)
## a little different@!!

dboot4 = boot.strength(data=MAFtrain2,R=20, algorithm = "hc",algorithm.args = list(score="k2")) 
avedboot4 = averaged.network(dboot4,threshold = 0.85)
plot(avedboot4)
fitdboot4 = bn.fit(cextend(avedboot4), MAFtrain2)
predictdboot4 = predict(fitdboot4, "Show", MAFtest2)
dboot4_table = table(predictdboot4,MAFtest2$Show,
                     dnn = c("Predict","Actual")) 
confusionMatrix(dboot4_table)
tabledboot4= as.matrix(confusionMatrix(dboot4_table)) 
BDtest(tabledboot4, pr=0.5, conf.level = 0.95)

dboot5 = boot.strength(data=MAFtrain2,R=20, algorithm = "hc",algorithm.args = list(score="bdla")) 
avedboot5 = averaged.network(dboot5,threshold = 0.85)
plot(avedboot5)
fitdboot5 = bn.fit(cextend(avedboot5), MAFtrain2) 
predictdboot5 = predict(fitdboot5, "Show", MAFtest2) 
dboot5_table = table(predictdboot5,MAFtest2$Show,dnn = c("Predict","Actual")) 
confusionMatrix(dboot5_table)
tabledboot5= as.matrix(confusionMatrix(dboot5_table)) 
BDtest(tabledboot5, pr=0.5, conf.level = 0.95)

# no edges involveing show
dboot6 = boot.strength(data=MAFtrain2,R=20, algorithm = "mmhc") 
avedboot6 = averaged.network(dboot6,threshold = 0.85) plot(avedboot6)

# only hypertension
dboot7 = boot.strength(data=MAFtrain2,R=20, algorithm = "rsmax2") 
avedboot7 = averaged.network(dboot7,threshold = 0.85) 
plot(avedboot7)
fitdboot7 = bn.fit(cextend(avedboot7), MAFtrain2)
predictdboot7 = predict(fitdboot7, "Show", MAFtest2)
dboot7_table = table(predictdboot7,MAFtest2$Show,
                     dnn = c("Predict","Actual")) 
confusionMatrix(dboot7_table)
tabledboot7= as.matrix(confusionMatrix(dboot7_table)) 
BDtest(tabledboot7, pr=0.5, conf.level = 0.95)

# Delete the order inside of some factors
MAFtrain2$SWeekday = factor(MAFtrain2$SWeekday, ordered = FALSE) 
MAFtrain2$AWeekday = factor(MAFtrain2$AWeekday, ordered = FALSE) 
MAFtrain2$Smonthf = factor(MAFtrain2$Smonthf, ordered = FALSE) 
MAFtrain2$Amonthf = factor(MAFtrain2$Amonthf, ordered = FALSE)

# accuracy decrease and sensitivity increase nbn = naive.bayes(MAFtrain2, "Show") npred = predict(nbn, MAFtest2)
plot(nbn)
nbn_table = table(npred,MAFtest2$Show, dnn = c("Predict","Actual"))
confusionMatrix(nbn_table)
tablenbn= as.matrix(confusionMatrix(nbn_table)) 
BDtest(tablenbn, pr=0.5, conf.level = 0.95)
# accuracy decrease and sensitivity increase
# biggest change, but also biggest decrease on accuracy 
tbn = tree.bayes(MAFtrain2, "Show")
tpred = predict(tbn, MAFtest2)
plot(tbn)
tbn_table = table(tpred,MAFtest2$Show,
                  dnn = c("Predict","Actual")) 
confusionMatrix(tbn_table)
tabletbn= as.matrix(confusionMatrix(tbn_table)) 
BDtest(tabletbn, pr=0.5, conf.level = 0.95) 

plot(tbn, main="Original TBN")
plot(avedboot1, main="Discrete BN with Hill-climbing(bde)") 
plot(avedboot2, main="Discrete BN with Hill-climbing(bds)") 
plot(avedboot3, main="Discrete BN with Hill-climbing(mbde)") 
plot(avedboot4, main="Discrete BN with Hill-climbing(k2)") 
plot(avedboot5, main="Discrete BN with Hill-climbing(bdla)") 
plot(avedboot7, main="Discrete BN with rsmax2")

################################################################ 
# oversampling 
################################################################ 
install.packages("ROSE")
library("ROSE")
MAFtrain_over = ovun.sample(Show~.,data = MAFtrain,method = "over",seed = 1)$data 
table(MAFtrain_over$Show)

# boostrap based inferece
bootbn9_over = boot.strength(data=MAFtrain_over,R=20, algorithm = "hc",algorithm.args = list(score="loglik-cg"))
avebootbn9_over = averaged.network(bootbn9_over,threshold = 0.85) 
plot(avebootbn9_over)
fitbootbn9_over = bn.fit(cextend(avebootbn9_over), MAFtrain_over) 
predictions9 = predict(fitbootbn9_over, "Show", MAFtest) 
confusionMatrix(predictions9, MAFtest$Show)
## confusion matrix a little difference!!!
## but in this case, there is no edge pointing to "Show"
bootbn10_over = boot.strength(data=MAFtrain_over,R=20, algorithm = "hc",algorithm.args = list(score="aic-cg"))
avebootbn10_over = averaged.network(bootbn10_over,threshold = 0.85) 
plot(avebootbn10_over)
fitbootbn10_over = bn.fit(cextend(avebootbn10_over), MAFtrain_over) 
predictions10 = predict(fitbootbn10_over, "Show", MAFtest) 
confusionMatrix(predictions10, MAFtest$Show)

bootbn11_over = boot.strength(data=MAFtrain_over,R=10, algorithm = "hc",algorithm.args = list(score="bic-cg"))
avebootbn11_over = averaged.network(bootbn11_over,threshold = 0.85) 
plot(avebootbn11_over)
fitbootbn11_over = bn.fit(cextend(avebootbn11_over), MAFtrain_over) 
predictions11 = predict(fitbootbn11_over, "Show", MAFtest) 
confusionMatrix(predictions11, MAFtest$Show)

bootbn12_over = boot.strength(data=MAFtrain_over,R=20, algorithm = "tabu",algorithm.args = list(score="loglik-cg"))
avebootbn12_over = averaged.network(bootbn12_over,threshold = 0.85) 
plot(avebootbn12_over)
fitbootbn12_over = bn.fit(cextend(avebootbn12_over), MAFtrain_over) 
predictions12 = predict(fitbootbn12_over, "Show", MAFtest) 
confusionMatrix(predictions12, MAFtest$Show)

bootbn13_over = boot.strength(data=MAFtrain_over,R=20, algorithm = "tabu",algorithm.args = list(score="aic-cg"))
avebootbn13_over = averaged.network(bootbn13_over,threshold = 0.85) 
plot(avebootbn13_over)
fitbootbn13_over = bn.fit(cextend(avebootbn13_over), MAFtrain_over) 
predictions13 = predict(fitbootbn13_over, "Show", MAFtest) 
confusionMatrix(predictions13, MAFtest$Show) 
plot(avebootbn13_over,main="Oversampled BN with Tabu(aic-cg)")

bootbn14_over = boot.strength(data=MAFtrain_over,R=20, algorithm = "tabu",algorithm.args = list(score="bic-cg"))
avebootbn14_over = averaged.network(bootbn14_over,threshold = 0.85) plot(avebootbn14_over)

# only Smonthf
fitbootbn14_over = bn.fit(cextend(avebootbn14_over), MAFtrain_over) predictions14 = predict(fitbootbn14_over, "Show", MAFtest) confusionMatrix(predictions14, MAFtest$Show)

# 6 stacked plots
plot(avebootbn9_over, main="Oversampled BN with Hill-climbing(loglik-cg)")
plot(avebootbn10_over, main="Oversampled BN with Hill-climbing(aic-cg)") 
plot(avebootbn11_over, main="Oversampled BN with Hill-climbing(bic-cg)") 
plot(avebootbn12_over, main="Oversampled BN with Tabu(loglik-cg)") 
plot(avebootbn13_over, main="Oversampled BN with Tabu(aic-cg)") 
plot(avebootbn14_over, main="Oversampled BN with Tabu(bic-cg)")

keep2 = c("Gender","Scholarship","Hipertension", "Diabetes","Alcoholism","Handcap","SMS_received", "Show","SWeekday","AWeekday", "Smonthf","Amonthf")
MAFtrain_over2 = MAFtrain_over[,(names(MAFtrain_over) %in% keep2)] 
MAFtest2 = MAFtest[,(names(MAFtest) %in% keep2)]

nbover = naive.bayes(MAFtrain_over2,"Show") 
pred_nbover = predict(nbover,MAFtest2) 
confusionMatrix(pred_nbover, MAFtest2$Show) 
plot(nbover)

tanover = tree.bayes(MAFtrain_over2,"Show")
fitted = bn.fit(tanover,MAFtrain_over2,method = "bayes") 
pred_tanover = predict(fitted,MAFtest2) 
confusionMatrix(pred_tanover,MAFtest2$Show) 
plot(tanover)
plot(tanover, main="Oversampled TBN")
## a little different@!!

dboot4_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "hc",algorithm.args = list(score="k2"))
avedboot4_over = averaged.network(dboot4_over,threshold = 0.85) 
plot(avedboot4_over)
fitdboot4_over = bn.fit(cextend(avedboot4_over), MAFtrain_over2)
pred_tanover = predict(fitdboot4_over,"Show", MAFtest2) 
confusionMatrix(pred_tanover,MAFtest2$Show)

dboot1_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "hc",algorithm.args = list(score="bde"))
avedboot1_over = averaged.network(dboot1_over,threshold = 0.85) 
plot(avedboot1_over)

#no consistent extension of avedboot1 is possible.
fitdboot1_over = bn.fit(cextend(avedboot1_over), MAFtrain_over2) 
predictdboot1_over = predict(fitdboot1_over, "Show", MAFtest2) 
confusionMatrix(predictdboot1_over,MAFtest2$Show)

dboot2_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "hc",algorithm.args = list(score="bds"))
avedboot2_over = averaged.network(dboot2_over,threshold = 0.85) 
plot(avedboot2_over)

fitdboot2_over = bn.fit(cextend(avedboot2_over), MAFtrain_over2) 
predictdboot2_over = predict(fitdboot2_over, "Show", MAFtest2) 
confusionMatrix(predictdboot2_over,MAFtest2$Show)

dboot3_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "hc",algorithm.args = list(score="mbde"))
avedboot3_over = averaged.network(dboot3_over,threshold = 0.85) 
plot(avedboot3_over)

fitdboot3_over = bn.fit(cextend(avedboot3_over), MAFtrain2) 
predictdboot3_over = predict(fitdboot3_over, "Show", MAFtest2)

dboot5_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "hc",algorithm.args = list(score="bdla"))
avedboot5_over = averaged.network(dboot5_over,threshold = 0.85) 
plot(avedboot5_over)

fitdboot5 = bn.fit(cextend(avedboot5), MAFtrain_over2)
predictdboot5_over = predict(fitdboot5, "Show", MAFtest2) 
confusionMatrix(predictdboot5_over,MAFtest2$Show)

# no edges involveing show
dboot6_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "mmhc") 
avedboot6_over = averaged.network(dboot6_over,threshold = 0.85) 
plot(avedboot6_over)

fitdboot6_over = bn.fit(cextend(avedboot6_over), MAFtrain_over2) 
predictdboot6_over = predict(fitdboot6_over, "Show", MAFtest2) 
confusionMatrix(predictdboot6_over,MAFtest2$Show)

# only hypertension
dboot7_over = boot.strength(data=MAFtrain_over2,R=20, algorithm = "rsmax2") 
avedboot7_over = averaged.network(dboot7_over,threshold = 0.85) 
plot(avedboot7_over)

fitdboot7_over = bn.fit(cextend(avedboot7_over), MAFtrain_over2) 
predictdboot7_over = predict(fitdboot7_over, "Show", MAFtest2) 
confusionMatrix(predictdboot7_over,MAFtest2$Show)