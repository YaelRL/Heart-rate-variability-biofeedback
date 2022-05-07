library(ggplot2)
library(summarytools)
library(dplyr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(interactions)
library(sjPlot)
library("readxl")
#install.packages("summarytools")

data=read_xlsx(path='F:/My Documents/MPH/Biostatistics_projects/3 HRV biofeedback/pr3_stud.xlsx', 
                sheet="pr3_stud", guess_max = 5000, na=c("-99", "NA"))


#data set exploration
names(data)
length(unique(data$id))
#5753 observations from 983 participants

#renaming
names(data)[names(data)=='avg_pain']<-'pain'
names(data)[names(data)=='avg_stress']<-'stress'
names(data)[names(data)=='weight_in_kg']<-'weight'
names(data)[names(data)=='height_in_cm']<-'height'
names(data)[names(data)=='age_in_years']<-'age'
names(data)[names(data)=='days_training_in_week']<-'days_per_week'
names(data)[names(data)=='no_stress_duration_in_min']<-'no_stress_duration'
names(data)[names(data)=='stress_duration_in_min']<-'stress_duration'
names(data)[names(data)=='total_duration_in_min']<-'duration'

#formatting categorical variable
data$gender<-factor(data$gender, levels=c("Male", "Female"))
label(data$gender)<-"Gender"

#outcome variables- checking for missing data
#visualizing missing data for outcome variables
library(naniar)
mdf<-data.frame(data[,c("pain", "stress")])
vis_miss(mdf)+ggtitle("Missing values for outcome variables")

#checking for correlations of our outcome variables pain and stress
#with other variables to see if we can impute the missing data based 
#on existing data
odata<-data
library(corrplot)
M <- cor(odata[,c(1:3, 5:10)], use="pairwise.complete.obs", method="pearson")
corrplot.mixed(M, upper="ellipse",order = "hclust", addrect = 2)
#pain and stress are not correlated to other variables

 
#eliminating observations with missing data for pain or stress
data<-data%>% filter(!is.na(data$pain))#%>%filter(!is.na(data$stress)) 
#length(unique(data$id))

#eliminating participants with a single observation
data<-data %>% group_by(id) %>% filter(n()>1)
#length(unique(data$id))
#777 participants, 2,959 observations

#adding a variable of number of reported weeks for each participant
weeks_num<-data %>% group_by(id) %>% summarise(count=n())
data<-merge(data, weeks_num, by=c("id"))

#ordering each participant's observations by chronological order
data<-arrange(data, id, week_index)

#creating time variable for each participant (1st observation is time=1)
#creating a df with id and 1st observation for each participant
week0<-data[!duplicated(data$id),] %>% select(id, week_index)
data<-merge(data, week0, by=c("id"))
data$time<-data$week_index.x-data$week_index.y+1
names(data)[names(data)=='week_index.x']<-'week_index'
data<-data%>% select(-week_index.y)

#last observation per participant
last<-data %>% select(id, week_index)%>% group_by(id) %>% filter(row_number() == n())
summary(last$week_index+1)
hist(last$week_index+1)

data<-merge(data, last, by=c("id"))
names(data)[names(data)=='week_index.x']<-'week_index'
data$last<-data$week_index.y+1

#%missing observations per participant
data$miss<-1-data$count/data$last
summary(data$miss)
#analyzing missing data
#number of missing observations per participant (out of 12)
data$missing_weeks<-12-data$count

#descriptive statistics for missing data
t1<-data[!duplicated(data$id),]
ggplot(t1, aes(x=count))+geom_histogram(binwidth = 1)
ggplot(t1, aes(x=100*miss))+geom_histogram(binwidth = 5)
summary(t1$count)
summary(t1$miss)
summary(t1$last)

table(t1$count, t1$last)
ggplot(t1, aes(x=miss*10))+geom_histogram(binwidth = 1)


#Dropout analysis (time 1-8: dropout, time 9-12: completer)
data$dropout<-ifelse(data$time_l<9, 1,0)
#looking for correlation between missing data and participant characteristics
#level 2 variables
table1(~ engagement+count+miss+gender+age+age_group+ BMI+ pain_group+ stress+ success_group| dropout, data=t1, 
       caption= "Missing data analysis",
       overall=F, extra.col=list(`P-value`=pvalue))
#dropout yes is correlated to engagement no, lower count

#level 1 variables
table1(~ pain+ stress+ success_group+days_per_week+no_stress_duration+duration| dropout,
       data=data, 
       caption= "Missing data analysis",
       overall=F, extra.col=list(`P-value`=pvalue))
#people who dropped out had lower training duration and lower stress free duration 
#(but same success proportion)
#higher pain levels and lower stress levels--> there is an association between
#pain and stress levels and missing data
#this is a possible missing data mechanism

#report analysis
data$report<-ifelse(data$count<4, 0,1)
data$report<-factor(data$report, labels=c("2-3", "4+"))
#looking for correlation between missing data and participant characteristics
#level 2 variables
table1(~ miss+last+gender+age+age_group+ BMI+ pain_group+ stress+ success_group| report, data=t1, 
       caption= "Missing data analysis",
       overall=F, extra.col=list(`P-value`=pvalue))
#those who reported 4+ entries had more weeks, more %miss, more males
#older, more 50+

#level 1 variables
table1(~ pain+ stress+ success_group+days_per_week+no_stress_duration+duration| report,
       data=data, 
       caption= "Missing data analysis",
       overall=F, extra.col=list(`P-value`=pvalue))
#those who reported 4+ entries had lower pain, higher stress, more training days per week,
#longer reduced stress, and total duration

#checking which variable describes better the amount of reports per participant
summary(lmer(count~dropout+(1|id), data=data, REML = T))
summary(lmer(pain~ count*gender+(1|id) ,data=data, REML = T))

summary(lm(count~report, data=t1))
summary(lm(count~dropout, data=t1))
summary(lm(count~miss, data=t1))
m<-glm(dropout~report+miss, data=t1, family=binomial)
tab_model(m)

m1<-glm(report~dropout+miss, data=t1, family=binomial)
m2<-glm(report~dropout+miss+age, data=t1, family=binomial)
tab_model(m1, m2, show.aic=TRUE)


sum(t1$last-t1$count)

cor(t1$miss, t1$age)

#counting how many observations per week
table(as.factor(data$week_index.x+1))

#creating  baseline pain variable
bsdf<-data[!duplicated(data$id),] %>% select(id, pain)
data<-merge(data, bsdf, by=c("id"))
data$pain_group[data$pain.y>6]<-3
data$pain_group[data$pain.y<7 & data$pain.y>3]<-2
data$pain_group[data$pain.y<4]<-1
data$pain_group<-factor(data$pain_group, levels=c(1,2,3), labels=c("1-3/10", "4-6/10", "7-10/10"))
label(data$pain_group)<-"Baseline pain level group"

names(data)[names(data)=='pain.x']<-'pain'
names(data)[names(data)=='pain.y']<-'Baseline pain level'
                

#checking for outliers and out-of-bound values, missing values
#height
#summary(data$height)
#data$height[data$height>200]
#data$height[data$height<150]
#ggplot(data, aes(x=height))+geom_histogram(binwidth = 2)
#fdf<-subset(data, data$gender=="Female")
#ggplot(fdf, aes(x=height))+geom_histogram(binwidth = 2)
#summary(subset(data, data$gender=="Female"))
#outliers- one female with height 210 and weight 130, replacing with upper value for women= 185
#fdf$height[fdf$height>180] #185
#fdf$height[fdf$height>180]
data$height[data$height==210]<-185

#weight
#summary(data$weight)
#data$weight[data$weight>140]
#ggplot(data, aes(x=weight))+geom_histogram(binwidth = 2)

#summary(data$age)
data$age[data$age<0]
#one participant with age out of bound, 3 observations, assigning median value
data$age[data$age<0]<-40
ggplot(data, aes(x=age))+geom_histogram(binwidth = 2)
summary(data$age)

#summary(data$gender)

#ggplot(data, aes(x=pain))+geom_histogram(binwidth = 1)
#ggplot(data, aes(x=stress))+geom_histogram(binwidth = 1)
#ggplot(data, aes(x=days_per_week))+geom_histogram(binwidth = 1)

#eliminating outliers
sum(data$stress_duration>1600)
ggplot(data, aes(x=no_stress_duration))+geom_histogram(binwidth = 10)+
  geom_vline(xintercept=4300)

sum(data$no_stress_duration>4300)
ggplot(data, aes(x=stress_duration))+geom_histogram(binwidth = 10)+
  geom_vline(xintercept=1600)

data$no_stress_duration[data$no_stress_duration>4300]<-4300
data$stress_duration[data$stress_duration>1600]<-1600

ggplot(data, aes(x=duration))+geom_histogram(binwidth = 10) +geom_vline(xintercept=5100)
data$duration[data$duration>5150]<-5100

#checking data consistency
incons<-data$id[data$duration!=(data$no_stress_duration+data$stress_duration)]
unique(incons)
#35 observations, 18 participants had inconsistent no_stress+stress and duration
#differences=1, probably due to rounding. 
data$duration=data$no_stress_duration+data$stress_duration

#creating variables
#BMI
data$BMI<-data$weight/(data$height/100)^2
summary(data$BMI)
hist(data$BMI)

#log(no-stress/stress) ratio
#data$stress_duration[data$stress_duration==0]<-1
#data$ratio<-log(data$no_stress_duration/data$stress_duration)
#summary(data$ratio)
#hist(data$ratio)

#proportion of training time in which there was no stress
data$success<-data$no_stress_duration/data$duration*10
summary(data$success)
label(data$success)<-"Proportion of reduced stress"

#success group- percent of reduced stress during training
data$success_group<-ifelse(data$success>7.5, 1,0)
data$success_group<-factor(data$success_group, levels=c(0,1), labels = c("No", "Yes"))
label(data$success_group)<-"Success in reducing stress"

#age groups
data$age_group[data$age<30]<-1
data$age_group[data$age>29 & data$age<50]<-2
data$age_group[data$age>49]<-3
data$age_group<-factor(data$age_group, levels=c(1,2,3), labels=c("Under 30", "30-49", "50+"))
label(data$age_group)<-"Age group"

# descriptive statistics 
ggplot(data, aes(x=weight))+geom_histogram(binwidth = 5)

#exploring correlations between variables
#condf<-data[, c("time", "age","BMI", "pain", "stress", "duration", "success")]
#catdf<-data[,c("obese", "age_group", "gender")]


#1. A. explore pain, stress and duration over time-----------------------

## Trajectory of pain over time--------------------------------
ggplot(data, aes(x=time, y=pain))+geom_jitter()+
  xlab("Pain level over time")+geom_smooth()
#downward trend in the first 3 months and then a plateau

ggplot(data, aes(x=time, y=pain, color=gender))+#geom_jitter()+
  xlab("Pain  level over time, by gender")+geom_smooth()
#slight difference between females and males, CI overlap considerably

#plotting individual trends, by pain groups
ggplot(data, aes(y=pain, x=time, group=id)) + facet_grid(.~pain_group)+ 
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time (months)")+ ylab("Pain")
#seems like people with high pain had a greater decrease in pain, people
#with low pain had pain increases

ggplot(data, aes(y=pain, x=time, group=id)) + facet_grid(.~gender)+ 
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time (months)")+ ylab("Pain")
#seems a bit like females had more decrease

ggplot(data, aes(y=pain, x=time, group=id)) + facet_grid(.~success_group)+ 
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time (months)")+ ylab("Pain")


#
#https://www.r-bloggers.com/2012/08/r-for-ecologists-putting-together-a-piecewise-regression/
#Piecewise model- creating dummy variables: 
#creating dummy variables to section the model with a 3-month cut-off
#section 1 is for time=1-3, section 2 is for time=4-12
data$t1_3<-ifelse(data$time<3, data$time, 3)
data$t4_12<-ifelse(data$time>2, data$time-2, 1)
#hist(data$t4_12)
#summary(data$t1_3)

#testing pain over time with a piecewise mixed model
m<-lmer(pain~t1_3+t4_12+(1|id), data=data)
summary(m)
tab_model(m, show.ci = FALSE, show.re.var= FALSE, show.r2=TRUE, show.icc=TRUE)
#ICC=0.33

#fluctuations of pain over time
m<-lmer(pain~time+ (1|id) ,data=data, 
    control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
tab_model(m, show.ci = FALSE, show.re.var= FALSE, show.r2=TRUE, show.icc=TRUE)

#testing pain over time with possibel moderators
#non significant interactions
summary(lmer(pain~ (t1_3+t4_12)*gender+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*obese+ (1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*BMI+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*duration+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*success+(1|id) ,data=data, REML = T))

#significant interactions
m<-lmer(pain~ (t1_3+t4_12)*stress+(1|id) ,data=data, REML = T)
#t1_3: the estimate for stress is 0.07 (SE=0.02, p-value<0.001)
#t4_12: the estimate for stress is -0.02 (SE=0.006, p-value=0.003)

#plotting the interaction
p1_3<-interact_plot(m, pred = t1_3, modx = stress, interval = T,modx.values="plus-minus",
                   int.width = 0.8,x.label="Time (weeks 1-3)",y.label="Pain level (1-10)",
                 legend.main="Stress",plot.points = TRUE)

p4_12<-interact_plot(m, pred = t4_12, modx = stress, interval = T,modx.values="plus-minus",
                 int.width = 0.8,x.label="Time (weeks 4-12)",y.label="Pain level (1-10)",
                 legend.main="Stress",plot.points = TRUE)



#pain over time, by baseline pain group
ggplot(data, aes(y=pain, x=time, group=pain_group)) + 
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time (months)")+ ylab("Pain")
#pain of 7-10/10 at baseline means decrease in pain
#lower pain at baseline-increase in pain

ggplot(data, aes(y=pain, x=time, group=id)) + facet_grid(.~pain_group)+
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time (weeks)")+ ylab("Pain")

m<-lmer(pain~(t1_3+t4_12)*pain_group+(1|id), data=data)
summary(m)
#significant interaction with t1-3


#https://stats.idre.ucla.edu/r/seminars/interactions-r/#s4
library(emmeans)
emtrends(m, pairwise ~ pain_group, var = "t1_3")
#1-3/10 has an increase in pain est=0.836
#4-6/10 has a small decrease in pain est=-0.272
#7-10/10 has a larger decrease in pain, est=-0.838

#plotting the interaction
mylist <- list(t1_3=seq(1,3,by=1),pain_group=c("1-3/10","4-6/10", "7-10/10"))
emmip(m, pain_group ~t1_3, at=mylist,CIs=TRUE)+xlab("Time (weeks 1-3)")+
  ylab("Pain level (1-10)")+ggtitle("Pain level over weeks 1-3, by baseline pain")

emtrends(m, pairwise ~ pain_group, var = "t4_12")
#interactions are all non-significant


## Trajectory of stress over time-----------------------------
ggplot(data, aes(x=time, y=stress))+geom_jitter()+
  xlab("Pain level over time")+geom_smooth()
#downward trend in the first 3 months and then a plateau

ggplot(data, aes(x=time, y=stress, color=gender))+#geom_jitter()+
  xlab("Pain  level over time, by gender")+geom_smooth()

m <- lmer(stress~ t1_3*gender+t4_12*gender+ (1 |id) ,data=data, REML = T)
summary(m)
tab_model(m, show.ci = FALSE, show.re.var= FALSE, show.r2=TRUE, show.icc=TRUE)
#gender*t1_3 interaction is significant, the other is not

emtrends(m, pairwise ~ gender, var = "t1_3")
mylist <- list(t1_3=seq(1,3,by=1),gender=c("Male", "Female"))
emmip(m, gender ~t1_3, at=mylist,CIs=TRUE)+xlab("Time (weeks 1-3)")+
  ylab("Pain level (1-10)")+ggtitle("Stress level over weeks 1-3, by gender")


#1. B. test the effect of training on pain, stress and duration over time
#training-related variables: success, success group, total duration

#creating lag variable for success
library(DataCombine)
data <- slide(data = data, Var = c('success'), GroupVar = c('id'),
            NewVar = c('successl'), slideBy = c(-1), keepInvalid = TRUE) 


#plotting un-nested association to pain, without time
ggplot(data, aes(x=duration, y=pain))+geom_jitter()+
  ggtitle("Pain level by success")+geom_smooth()

ggplot(data, aes(x=success, y=pain))+geom_jitter()+
  ggtitle("Pain level by success")+geom_smooth()
#both graphs show no association between pain and training metrics

ggplot(data, aes(x=success, y=as.factor(pain)))+geom_boxplot()+facet_grid(.~pain_group)+
  ggtitle("Pain level by success")#+geom_smooth()

#by id
ggplot(data, aes(y=success, x=time, group=id)) +facet_grid(.~success_group)+ 
  geom_point(size = 1.2, alpha = .8, position = "jitter")+
  theme_minimal()+  geom_smooth(method = lm, se= FALSE, size = .5, alpha  = .8)+
  xlab("Time (months)")+ ylab("Success")

#summary(lmer(pain~ time+success+(1|id) ,data=data, REML = T))
#summary(lmer(pain~ time*pain_group+success+gender+(1|id) ,data=data, REML = T))
#summary(lmer(pain~ time*success+(1|id) ,data=data, REML = T))
#time and success are both significant, but their interaction is not

summary(lmer(pain~ (t1_3+t4_12)*success_group+(1|id) ,data=data, REML = T))
#non significant

summary(lmer(pain~ time+duration+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*duration+ (1|id) ,data=data, REML = T))
#time and duration are both significant, but the interaction is not
#success had a much larger estimate

summary(lmer(pain~ (t1_3+t4_12)+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)+success+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*pain_group+success+(1|id) ,data=data, REML = T))
summary(lmer(pain~ (t1_3+t4_12)*pain_group+successl+(1|id) ,data=data, REML = T))
#after controlling for baseline pain over time, success is no longer significant

m1<-lmer(pain~ (t1_3+t4_12)+I(duration/100)+(1|id) ,data=data, REML = T)
m3<-lmer(pain~ (t1_3+t4_12)+durationl+(1|id) ,data=data, REML = T)

ggplot(data, aes(x=success, y=stress))+geom_jitter()+
  ggtitle("Pain level by success")+geom_smooth()
#slight positive trend for success >7.5

summary(lmer(stress~ t1_3+t4_12+success+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+success_group+(1|id) ,data=data, REML = T))
#higher estimates, t4_12 not significant

summary(lmer(duration~ time+success+(1|id) ,data=data, REML = T))
summary(lmer(duration~ time*success+(1|id) ,data=data, REML = T))
#1 unit increase in success (1-10) is associated with a 39 minute increase in weekly training duration
summary(lmer(duration~ t1_3+t4_12+success+(1|id) ,data=data, REML = T))

#2.------- test mediation model duration>>stress>>pain. ------------------------------

#direct model: pain~duration
summary(lmer(pain~ t1_3+t4_12+duration+(1|id) ,data=data, REML = T))
#duration is significant

#model a: stress~duration
summary(lmer(stress~ t1_3+t4_12+duration+(1|id) ,data=data, REML = T))
#duration is significant

#model b: pain~stress+duration
summary(lmer(pain~ t1_3+t4_12+duration+stress+(1|id) ,data=data, REML = T))
#both are significant

#---bootstrapping CI for ACME and ADE
#library(lme4)
library(mediation)

# m~x
model_a <- lme4::lmer(stress~ t1_3+t4_12+durations+(1|id) ,data=data, REML = T)
#summary(model_a)
#Y~x+m
model_b<-lme4::lmer(pain~ t1_3+t4_12+durations+stress+(1|id) ,data=data, REML = T)
#summary(model_b)
med.run <- mediate(model_a , model_b, treat = "durations", mediator = "stress", sims = 500, dropobs = TRUE)
summary(med.run)

#---------2B------------moderators


#test moderators weight, height, age and gender in the mediation model
#direct model pain~duration
summary(lmer(pain~ t1_3+t4_12+durations*gender+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+durations*age+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+durations*age_group+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+durations*BMI+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+duration*weight+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+duration*height+(1|id) ,data=data, REML = T))
#summary(lmer(pain~ t1_3+t4_12+duration*obese+(1|id) ,data=data, REML = T))
#none of the interactions is significant, these variables are not moderators

#model a: stress~duration
summary(lmer(stress~ t1_3+t4_12+duration*gender+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+duration*age+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+duration*age_group+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+duration*BMI+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+duration*weight+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+duration*height+(1|id) ,data=data, REML = T))
summary(lmer(stress~ t1_3+t4_12+duration*obese+(1|id) ,data=data, REML = T))
#none of the interactions is significant, these variables are not moderators


#model b
summary(lmer(pain~ t1_3+t4_12+stress+durations*gender+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+stress+durations*age+(1|id) ,data=data, REML = T))
#summary(lmer(stress~ t1_3+t4_12+duration*age_group+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+stress+durations*BMI+(1|id) ,data=data, REML = T))
#none of the interactions is significant, these variables are not moderators

summary(lmer(pain~ t1_3+t4_12+stress*age+durations+(1|id) ,data=data, REML = T))
#summary(lmer(stress~ t1_3+t4_12+duration*age_group+(1|id) ,data=data, REML = T))
summary(lmer(pain~ t1_3+t4_12+stress*BMI+durations+(1|id) ,data=data, REML = T))

m<-lmer(pain~ t1_3+t4_12+stress*gender+durations+(1|id) ,data=data, REML = T)
#the interation between stress and gender is significant

interact_plot(m, pred=stress, modx=gender, interval = FALSE, 
              x.label="Stress",
              y.label = "Pain level (0-10)",
              main.title ="Pain level and stress level by gender", 
              legend.main = "Gender",
              data=data)
#--------------------------------Joint Modeling
#https://rpsychologist.com/lmm-slope-missingness
#https://www.rdocumentation.org/packages/survival/versions/2.9-15/topics/Surv
data$dropout<-ifelse(data$last<9, 1,0)
data$dropout<-as.numeric(data$dropout)
t1$dropout<-as.numeric(t1$dropout)

library(JM)
library(survival)
library(nlme)
#library(segmented)
#trajectory of pain over time
data<-data%>%filter(!is.na(pain)) %>%arrange(id)
d_c<-data%>% dplyr::select(id, pain, t1_3, t4_12)%>% as.matrix()
#Linear Mixed Model
#fit<-lmer(pain~ time+(1|id) ,data=data, REML = T)
fit_m<-lme(pain~t1_3+t4_12+(1|id), data=data)

fit <- lme(pain ~ time,  random = ~ time| id, data = data)
#fit <- lm(y ~ x, data=df)
fits <- segmented.default(fit,  ~time, psi=3)

o<-lme(y~x+z, random=~1|g, data=dati)
os<-segmented.default(o, ~x+z, npsi=list(x=2, z=1))
#summarizing results (note the '.coef' argument)
slope(os, .coef=fixef(os))
plot.segmented(os, "x", .coef=fixef(os), conf.level=.95)
confint.segmented(os, "x", .coef=fixef(os))


#d_miss <- data %>%
 #group_by(id, time) %>%
#  summarise(time = max(time),
 #           age=age, miss=miss,count=count,
            #time = ifelse(time < 9, time + 1, time),
            #dropout = ifelse(time < 9, 1, 0)) %>%  arrange(id)

d_miss<-t1 %>% dplyr::select(id, time_l, dropout, count, miss, age)
names(d_miss)[names(d_miss)=='time_l']<-'time'

#Survival model
#fit_surv <- coxph(Surv(time, dropout) ~ 1 + count+ cluster(id),
 # data = d_miss,  x = TRUE, model = TRUE, na.action=na.exclude)

fit_surv<-coxph(Surv(time, dropout) ~ 1+count+miss+age, data = d_miss, x=TRUE)

# slope derivatives
dForm <- list(
  fixed = ~count,
  random = ~1,
  indFixed = c(3, 4),
  indRandom = c(2)
)
# Fit the joint model
fit_JM <- jointModel(
  fit_lme, 
  fit_surv,
  timeVar = "t1_3")
  #method = "piecewise-PH-aGH",
  #parameterization = "slope",
  #derivForm = dForm,
  #interFact = list(slope = ~count,
   #                data = d_miss))
summary(fit_JM)


summary(lmer(pain~time+(1|id), data=data))



#modeling missing data with survival models
#Cox model examples
#http://www.sthda.com/english/wiki/cox-proportional-hazards-model#:~:text=The%20Cox%20proportional%2Dhazards%20model,one%20or%20more%20predictor%20variables.&text=the%20construction%20of%20Kaplan%2DMeier%20survival%20curves%20for%20different%20patient%20groups

#evaluating survival models with train and test sets and C-index
#https://cran.r-project.org/web/packages/SurvMetrics/vignettes/SurvMetrics-vignette.html

#https://stats.oarc.ucla.edu/r/examples/alda/r-applied-longitudinal-data-analysis-ch-14/

fit_surv <- coxph(Surv(time, dropout) ~ 1 + count+ cluster(id),
                  data = d_miss,  x = TRUE, model = TRUE, na.action=na.exclude)

fit_surv <- coxph(Surv(time_l, dropout) ~ 1, data = t1)
summary(coxph(Surv(time_l, dropout) ~ 1+count, data = t1))
#c-index=0.66
summary(coxph(Surv(time_l, dropout) ~ 1+engagement, data = t1))
#c-index=0.61
summary(coxph(Surv(time_l, dropout) ~ 1+miss, data = t1))
#c-index=0.81
summary(coxph(Surv(time_l, dropout) ~ 1+count+engagement+miss+gender, data = t1))
#c-index=0.982
summary(coxph(Surv(time_l, dropout) ~ 1+count+miss, data = t1))
#c-index=0.981

#best model
summary(coxph(Surv(time_l, dropout) ~ 1+count+miss+age, data = t1))
#c-index=0.983

#missingness
data$t1_2<-ifelse(data$week_index<3,0,1)


#numbering and cross-referencing in bookdown
#https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html#cross-ref

#study limitations 
#https://towardsdatascience.com/endogeneity-the-reason-why-we-should-know-about-data-part-i-80ec33df66ae