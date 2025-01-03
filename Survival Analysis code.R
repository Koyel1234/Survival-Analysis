rm(list = ls())
options("install.lock"=FALSE)
data<-read.csv("heart_failure_clinical_records_dataset.csv")
data_all_numeric <- data # for fitting cox-PH model we need all columns as numeric, for other cases (descriptive, km plt..etc, we will use categorically seperated data)
head(data)
dim(data)
summary(data)
colnames(data)
#continuous variables (age,creatinine_phosphokinase,ejection_fraction,platelets,serum_creatinine,serum_sodium)
#categorical variables (anaemia,diabetes,high_blood_pressure,sex,smoking)

data$time <- round(data$time / 30, 3)  # assuming a month is consists of 30 days
data_all_numeric$time <- round(data$time / 30, 3)

# changing categorical columns to factor type
data$anaemia <- as.factor(data$anaemia)
levels(data$anaemia) <- c("no", "yes")

data$diabetes <- as.factor(data$diabetes)
levels(data$diabetes) <- c("yes", "no")

data$high_blood_pressure <- as.factor(data$high_blood_pressure)
levels(data$high_blood_pressure) <- c("yes", "no")

data$sex <- as.factor(data$sex)
levels(data$sex) <- c("female", "male")

data$smoking <- as.factor(data$smoking)
levels(data$smoking) <- c("no", "yes")


# creating groups for continuous variables (category by <Q1, [Q1,Q2),[Q2-Q3),>=Q3)
##age
for (i in 1:nrow(data)){
  if(data$age[i]<51){data$age[i]<-'age<51'}
  else if(data$age[i]<60 && data$age[i]>=51){data$age[i]<-'51<=age<60'}
  else if(data$age[i]<70 && data$age[i]>=60){data$age[i]<-'60<=age<70'}
  else{data$age[i]<-'age>=70'}
}

#creatinine_phosphokinase
for (i in 1:nrow(data)){
  if(data$creatinine_phosphokinase[i]<116.5){data$creatinine_phosphokinase[i]<-'cp<116.5'}
  else if(data$creatinine_phosphokinase[i]<250 && data$creatinine_phosphokinase[i]>=116.5){data$creatinine_phosphokinase[i]<-'116.5<=cp<250'}
  else if(data$creatinine_phosphokinase[i]<582 && data$creatinine_phosphokinase[i]>=250){data$creatinine_phosphokinase[i]<-'250<=cp<582'}
  else{data$creatinine_phosphokinase[i]<-'cp>=582'}
}

#ejection_fraction
for (i in 1:nrow(data)){
  if(data$ejection_fraction[i]<30){data$ejection_fraction[i]<-'ef<30'}
  else if(data$ejection_fraction[i]<38 && data$ejection_fraction[i]>=30){data$ejection_fraction[i]<-'30<=ef<38'}
  else if(data$ejection_fraction[i]<45 && data$ejection_fraction[i]>=38){data$ejection_fraction[i]<-'38<=ef<45'}
  else{data$ejection_fraction[i]<-'ef>=45'}
}

#platelets
for (i in 1:nrow(data)){
  if(data$platelets[i]<212500){data$platelets[i]<-'pl<212500'}
  else if(data$platelets[i]<262000 && data$platelets[i]>=212500){data$platelets[i]<-'212500<=pl<262000'}
  else if(data$platelets[i]<303500 && data$platelets[i]>=262000){data$platelets[i]<-'262000<=pl<303500'}
  else{data$platelets[i]<-'pl>=303500'}
}


#serum_creatinine
for (i in 1:nrow(data)){
  if(data$serum_creatinine[i]<0.900){data$serum_creatinine[i]<-'sc<0.900'}
  else if(data$serum_creatinine[i]<1.100 && data$serum_creatinine[i]>=0.900){data$serum_creatinine[i]<-'0.900<=sc<1.100'}
  else if(data$serum_creatinine[i]<1.400 && data$serum_creatinine[i]>=1.100){data$serum_creatinine[i]<-'1.100<=sc<1.400'}
  else{data$serum_creatinine[i]<-'sc>=1.400'}
}

#serum_sodium
for (i in 1:nrow(data)){
  if(data$serum_sodium[i]<134){data$serum_sodium[i]<-'ss<134'}
  else if(data$serum_sodium[i]<137 && data$serum_sodium[i]>=134){data$serum_sodium[i]<-'134<=ss<137'}
  else if(data$serum_sodium[i]<140 && data$serum_sodium[i]>=137){data$serum_sodium[i]<-'137<=ss<140'}
  else{data$serum_sodium[i]<-'ss>=140'}
}


summary(data)


### descriptive statistics

# table 5 of report
unique(data$DEATH_EVENT)
#install.packages("tidyverse")
library(tidyverse)
#total
s<-data$DEATH_EVENT %>% as.tibble() %>% count(value)
s$value
s$n
round((s$n/sum(s$n))*100,2) # 0:censoring, 1:death



# table 4 of report
#total

for(j in 1:(ncol(data)-2)){
  s<- data[[j]] %>% as.tibble() %>% count(value)
  print(colnames(data)[j])
  print(s$value)
  print(s$n)
  print(round((s$n/sum(s$n))*100,2))
  cat("\n")
}


#censored
data_censored <- subset(data, DEATH_EVENT==0)

for(j in 1:(ncol(data_censored)-2)){
  s<- data_censored[[j]] %>% as.tibble() %>% count(value)
  print(colnames(data_censored)[j])
  print('Unique Values')
  print(s$value)
  print("Total of Each Unique Value")
  print(s$n)
  print("Percentage of Each Unique Value")
  print(round((s$n/sum(s$n))*100,2))
  cat("\n")
}

#death
data_death <- subset(data, DEATH_EVENT==1)

for(j in 1:(ncol(data_death)-2)){
  s<- data_death[[j]] %>% as.tibble() %>% count(value)
  print(colnames(data_death)[j])
  print(s$value)
  print(s$n)
  print(round((s$n/sum(s$n))*100,2))
  cat("\n")
}



#install.packages("survival")
library(survival)

###main KM model
km.model_overall = survfit(Surv(`time`,`DEATH_EVENT`) ~ 1,type='kaplan-meier', data = data)
summary(km.model_overall)
plot(km.model_overall,conf.int = F,xlab = 'Time (in months)',ylab = 'Estimated Survival Function')


###KM Models based on categorical variables (anaemia,diabetes,high_blood_pressure,sex,smoking)
# anaemia
km.model_anaemia = survfit(Surv(`time`,`DEATH_EVENT`) ~ anaemia,type='kaplan-meier', data = data)
summary(km.model_anaemia)
plot(km.model_anaemia, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_anaemia$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')
survdiff(Surv(`time`,`DEATH_EVENT`) ~ anaemia, data = data)
#diabetes
km.model_diabetes = survfit(Surv(`time`,`DEATH_EVENT`) ~ diabetes,type='kaplan-meier', data = data)
summary(km.model_diabetes)
plot(km.model_diabetes, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_diabetes$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')

survdiff(Surv(`time`,`DEATH_EVENT`) ~ diabetes, data = data)
#high_blood_pressure
km.model_high_blood_pressure = survfit(Surv(`time`,`DEATH_EVENT`) ~ high_blood_pressure,type='kaplan-meier', data = data)
summary(km.model_high_blood_pressure)
plot(km.model_high_blood_pressure, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_high_blood_pressure$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')

survdiff(Surv(`time`,`DEATH_EVENT`) ~ high_blood_pressure, data = data)
#sex
km.model_sex = survfit(Surv(`time`,`DEATH_EVENT`) ~ sex,type='kaplan-meier', data = data)
summary(km.model_sex)
plot(km.model_sex, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_sex$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')
survdiff(Surv(`time`,`DEATH_EVENT`) ~ sex, data = data)
#smoking
km.model_smoking = survfit(Surv(`time`,`DEATH_EVENT`) ~ smoking,type='kaplan-meier', data = data)
summary(km.model_smoking)
plot(km.model_smoking, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_smoking$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')

survdiff(Surv(`time`,`DEATH_EVENT`) ~ smoking, data = data)
###KM Models based on continuous variables (age,creatinine_phosphokinase,ejection_fraction,platelets,serum_creatinine,serum_sodium)
# age
km.model_age = survfit(Surv(`time`,`DEATH_EVENT`) ~ age,type = 'kaplan-meier', data = data)
summary(km.model_age)
plot(km.model_age, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_age$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')

survdiff(Surv(`time`,`DEATH_EVENT`) ~ age, data = data)
#creatinine_phosphokinase
km.model_creatinine_phosphokinase = survfit(Surv(`time`,`DEATH_EVENT`) ~ creatinine_phosphokinase,type = 'kaplan-meier', data = data)
summary(km.model_creatinine_phosphokinase)
plot(km.model_creatinine_phosphokinase, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_creatinine_phosphokinase$strata))  ## legend labels


legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n',cex = 0.75)

survdiff(Surv(`time`,`DEATH_EVENT`) ~ creatinine_phosphokinase, data = data)
#ejection_fraction
km.model_ejection_fraction = survfit(Surv(`time`,`DEATH_EVENT`) ~ ejection_fraction,type = 'kaplan-meier', data = data)
summary(km.model_ejection_fraction)
plot(km.model_ejection_fraction, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_ejection_fraction$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')

survdiff(Surv(`time`,`DEATH_EVENT`) ~ ejection_fraction, data = data)
#platelets
km.model_platelets = survfit(Surv(`time`,`DEATH_EVENT`) ~ platelets,type = 'kaplan-meier', data = data)
summary(km.model_platelets)
plot(km.model_platelets, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_platelets$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')
survdiff(Surv(`time`,`DEATH_EVENT`) ~ platelets, data = data)
#serum_creatinine
km.model_serum_creatinine = survfit(Surv(`time`,`DEATH_EVENT`) ~ serum_creatinine,type = 'kaplan-meier', data = data)
summary(km.model_serum_creatinine)
plot(km.model_serum_creatinine, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_serum_creatinine$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')
survdiff(Surv(`time`,`DEATH_EVENT`) ~ serum_creatinine, data = data)
#serum_sodium
km.model_serum_sodium = survfit(Surv(`time`,`DEATH_EVENT`) ~ serum_sodium,type = 'kaplan-meier', data = data)
summary(km.model_serum_sodium)
plot(km.model_serum_sodium, lty = 2:3,col=c('red','blue','green','yellow'), xlab='Time (in months)', ylab='Survival')
lLab <- gsub("x=","",names(km.model_serum_sodium$strata))  ## legend labels
legend(
  "bottomleft",
  legend=lLab,
  col=c('red','blue','green','yellow'),
  lty=1:1,
  horiz=FALSE,
  bty='n')



survdiff(Surv(`time`,`DEATH_EVENT`) ~ serum_sodium, data = data)
## checking the assumptions of cox-PH model
#install.packages("survminer")
library("survminer")

#Computing a Cox model
res.cox <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia+ creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine+serum_sodium + sex + smoking , data =  data_all_numeric)
res.cox
#Testing linearity assumption
plot(predict(res.cox),residuals(res.cox,type='martingale'),xlab='fitted values',ylab='Martingale residuals',main='Residual plot',las=1)
abline(h=0)
lines(smooth.spline(predict(res.cox),residuals(res.cox,type='martingale')),col='red')
#Testing proportional Hazards assumption
test.ph <- cox.zph(res.cox)
print(test.ph)
# p-value for ejection_fraction <0.05, so Cox-PH model assumption was not valid for the our data set. 
#Furthermore, by looking for a global test, the Cox-PH assumption fails because the test 
#result was signifcant.

#As it can be shown in Table  1, the assumption of the Cox-PH model was not valid 
#for the HF data set; in this case, parametric AFT models were used for the HF data set

##fitting of parametric AFT model using INLA method
library("INLA")


# preparing data for fitting AFT models
sinla.data <- inla.surv(data$time, data$DEATH_EVENT)


# exponential model
exp.data <- inla(sinla.data ~ 1+age + anaemia+ creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, data = data, family = "exponential.surv", control.compute = list(dic=TRUE,waic=TRUE))
# Log-Normal
lognormal.data <- inla(sinla.data ~ 1+age + anaemia+ creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + sex +serum_sodium+ smoking, data = data, family = "lognormal.surv", control.compute = list(dic=TRUE,waic=TRUE))
#Weibull
weibull.data <- inla(sinla.data ~ 1+age + anaemia+serum_sodium +creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + sex + smoking, data = data, family = "weibull.surv", control.compute = list(dic=TRUE,waic=TRUE))
#Log-logistic
loglogistic.data <- inla(sinla.data ~ 1+age +serum_sodium+ anaemia+ creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + sex + smoking, data = data, family = "loglogistic.surv", control.compute = list(dic=TRUE,waic=TRUE))

## listing all distributions along with their DIC and WAIC values respectively
distns <-list('exp','lognormal','weibull','loglogistic')
dic_values<-list(exp.data$dic[[1]],lognormal.data$dic[[1]],weibull.data$dic[[1]],loglogistic.data$dic[[1]])
waic_values<-list(exp.data$waic[[1]],lognormal.data$waic[[1]],weibull.data$waic[[1]],loglogistic.data$waic[[1]])
distns
dic_values
waic_values



## loglogistic has minimum DIC, WAIC
## to indicate the results for Bayesian preferred (from table -2 pd,dic,waic results) AFT model using INLA method
summary(loglogistic.data)
