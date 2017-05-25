# reading the data
setwd("E:/Aegis/Capstone Project/Data Sets/datachallenge-cods2016/datachallenge_cods2016/datachallenge_cods2016/")
a=read.csv("traincsv.csv")
b=read.csv("testcsv.csv")
ncol(a)
ncol(b)
c=rbind(a,b)

# exploration 
summary(c)
new_a=c 
summary(new_a)
# View(new_a)
str(new_a)

# functions
minmax=function(x){
  nnew=(x-min(x))/(max(x)-min(x))
}

outlier.replcement.with.median=function(a){
    v=summary(a)
    rightlimit=v[5]+1.5*IQR(v)
    leftlimit=v[2]-1.5*IQR(v)
    
    for(i in 1:length(a)){
      if(a[i]>rightlimit || a[i]<leftlimit){
        a[i]=median(a)
      }
    }
  return(a)
}

reversing_one_hot_encoding=function(z){
  final=c()
  temp=c()
  for(i in 1:nrow(z)){
    temp = c()
    for(j in 1:ncol(z)){
      temp=c(temp,z[i,j])
    }
    f=max(temp)
    
    if(f == -1){
      final=c(final,NA)
    }
    else{
      final=c(final,f)
    }
  }
  return(final)
}


# for X
new_a$X=NULL

# for ID
new_a$ID=NULL

# for salary
typeof(new_a$Salary)
# unique(new_a$Salary)
new_a$Salary=as.numeric(new_a$Salary)
new_a$Salary=log(new_a$Salary)
plot(density(new_a$Salary,na.rm = T))

# for DOJ
# typeof(new_a$DOJ)
# unique(new_a$DOJ)
# 
# for(i in 1:nrow(new_a)){
#   new_a$new_DOJ[i]=strsplit(new_a$DOJ[i]," ")[[1]][1]#spliting the DOJ and extract just date 
#   }
# new_a$new_DOJ=as.Date(new_a$new_DOJ,format = "%d-%m-%y")
# unique(new_a$new_DOJ)
# typeof(new_a$new_DOJ)
# 
# #for DOL 
# typeof(new_a$DOL)
# unique(new_a$DOL)
# today=format(Sys.time(), "%d-%m-%y")
# typeof(today)
# 
# for(i in 1:nrow(new_a)){
#   new_a$new_DOL[i]=strsplit(new_a$DOL[i]," ")[[1]][1]
#   if(new_a$new_DOL[i]=="present"){
#     new_a$new_DOL[i]=today
#   }
# }
# 
# new_a$new_DOL=as.Date(new_a$new_DOL,format = "%d-%m-%y")
# unique(new_a$new_DOL)
# typeof(new_a$new_DOL)
# 
# #for Designatiom
# unique(new_a$Designation)
# 
# for JobCity
# unique(new_a$JobCity)

# for Gender
unique(new_a$Gender)

# for DOB 
# unique(new_a$DOB)
new_a$DOB=as.character(new_a$DOB)

for(i in 1:nrow(new_a)){
  new_a$new_DOB[i]=strsplit(new_a$DOB[i]," ")[[1]][1]
}

new_a$new_DOB=as.Date(new_a$new_DOB,format = "%d-%m-%y")

library(eeptools)
for(i in 1:nrow(new_a)){
new_a$age[i]=floor(age_calc(new_a$new_DOB[i], units = "years"))
}

unique(new_a$age)
new_a$age=as.numeric(new_a$age)
# View(new_a)

# for 10percentage
typeof(new_a$X10percentage)
# unique(new_a$X10percentage)
new_a$X10percentage=minmax(new_a$X10percentage)
boxplot(new_a$X10percentage)
plot(density(new_a$X10percentage))
new_a$X10percentage=outlier.replcement.with.median(new_a$X10percentage)

# for 10board
# unique(new_a$X10board)

# for 12graduation
# unique(new_a$X12graduation)
typeof(new_a$X12graduation)
new_a$X12graduation=as.factor(new_a$X12graduation)

# for 12percentage
# unique(new_a$X12percentage)
typeof(new_a$X12percentage)
new_a$X12percentage=minmax(new_a$X12percentage)
boxplot(new_a$X12percentage)
plot(density(new_a$X12percentage))
new_a$X12percentage=outlier.replcement.with.median(new_a$X12percentage)

# for 12board  
# unique(new_a$X12board)

# for new_a$CollegeID
# unique(new_a$CollegeID)
typeof(new_a$CollegeID)
new_a$CollegeID=as.factor(new_a$CollegeID)

# for CollegeTier
# unique(new_a$CollegeTier)
new_a$CollegeTier=as.factor(new_a$CollegeTier)

# for Degree
# unique(new_a$Degree)
typeof(new_a$Degree)
new_a$Degree=as.factor(new_a$Degree)

# for Specialization
# unique(new_a$Specialization)
table(new_a$Specialization)
new_a$Specialization=as.factor(new_a$Specialization)
# new_a$Specialization=as.character(new_a$Specialization)
# new_a$Specialization[which(new_a$Specialization=="computer science")]="computer science & engineering"
# new_a$Specialization[which(new_a$Specialization=="electronics")]="electronics & telecommunications"
# new_a$Specialization[which(new_a$Specialization=="information science")]="information technology"
# new_a$Specialization[which(new_a$Specialization=="computer and communication engineering")]="computer science & engineering"

# for collegeGPA
# unique(new_a$collegeGPA)
new_a$collegeGPA[which(new_a$collegeGPA<10)]=10*new_a$collegeGPA[which(new_a$collegeGPA<10)]
new_a$collegeGPA=minmax(new_a$collegeGPA)
boxplot(new_a$collegeGPA)
plot(density(new_a$collegeGPA))
new_a$collegeGPA=outlier.replcement.with.median(new_a$collegeGPA)


# for CollegeCityID
# unique(new_a$CollegeCityID)

# for CollegeCityTier
# unique(new_a$CollegeCityTier)

# for CollegeState
# unique(new_a$CollegeState)

# for GraduationYear
# unique(new_a$GraduationYear)
table(new_a$GraduationYear)
new_a$GraduationYear[which(new_a$GraduationYear==0)]=2014
new_a$GraduationYear=as.factor(new_a$GraduationYear)

# for English
# unique(new_a$English)
new_a$English=minmax(new_a$English)
boxplot(new_a$English)
plot(density(new_a$English))
new_a$English=outlier.replcement.with.median(new_a$English)

# for Logical
# unique(new_a$Logical)
new_a$Logical=minmax(new_a$Logical)
boxplot(new_a$Logical)
plot(density(new_a$Logical))
new_a$Logical=outlier.replcement.with.median(new_a$Logical)

# for Quant 
# unique(new_a$Quant)
new_a$Quant=minmax(new_a$Quant)
boxplot(new_a$Quant)
plot(density(new_a$Quant))
new_a$Quant=outlier.replcement.with.median(new_a$Quant)

# for Domain 
# unique(new_a$Domain)
table(new_a$Domain)
new_a$new_Domain=ifelse(new_a$Domain==-1,0,new_a$Domain)
unique(new_a$new_Domain)
new_a$new_Domain=minmax(new_a$new_Domain)
boxplot(na.omit(new_a$new_Domain))
plot(density(new_a$new_Domain))
new_a$new_Domain=outlier.replcement.with.median(new_a$new_Domain)

# for ComputerProgramming
# unique(new_a$ComputerProgramming)
table(new_a$ComputerProgramming)
new_a$new_ComputerProgramming=ifelse(new_a$ComputerProgramming==-1,0,new_a$ComputerProgramming)
unique(new_a$new_ComputerProgramming)
new_a$new_ComputerProgramming=minmax(new_a$new_ComputerProgramming)
boxplot(new_a$new_ComputerProgramming)
plot(density(new_a$new_ComputerProgramming))
new_a$new_ComputerProgramming=outlier.replcement.with.median(new_a$new_ComputerProgramming)


# for ElectronicsAndSemicon
# unique(new_a$ElectronicsAndSemicon)
table(new_a$ElectronicsAndSemicon)
new_a$new_ElectronicsAndSemicon=ifelse(new_a$ElectronicsAndSemicon==-1,0,new_a$ElectronicsAndSemicon)
unique(new_a$new_ElectronicsAndSemicon)
new_a$new_ElectronicsAndSemicon=minmax(new_a$new_ElectronicsAndSemicon)
boxplot(new_a$new_ElectronicsAndSemicon)
plot(density(new_a$new_ElectronicsAndSemicon))
new_a$new_ElectronicsAndSemicon=outlier.replcement.with.median(new_a$new_ElectronicsAndSemicon)


# for ComputerScience 
# unique(new_a$ComputerScience)
table(new_a$ComputerScience)
new_a$new_ComputerScience=ifelse(new_a$ComputerScience==-1,0,new_a$ComputerScience)
unique(new_a$new_ComputerScience)
new_a$new_ComputerScience=minmax(new_a$new_ComputerScience)
boxplot(new_a$new_ComputerScience)
plot(density(new_a$new_ComputerScience))
new_a$new_ComputerScience=outlier.replcement.with.median(new_a$new_ComputerScience)


# for MechanicalEngg
# unique(new_a$MechanicalEngg)
table(new_a$MechanicalEngg)
new_a$new_MechanicalEngg=ifelse(new_a$MechanicalEngg==-1,0,new_a$MechanicalEngg)
unique(new_a$new_MechanicalEngg)
new_a$new_MechanicalEngg=minmax(new_a$new_MechanicalEngg)
boxplot(new_a$new_MechanicalEngg)
plot(density(new_a$new_MechanicalEngg))
new_a$new_MechanicalEngg=outlier.replcement.with.median(new_a$new_MechanicalEngg)


# for ElectricalEngg
# unique(new_a$ElectricalEngg)
table(new_a$ElectricalEngg)
new_a$new_ElectricalEngg=ifelse(new_a$ElectricalEngg==-1,0,new_a$ElectricalEngg)
unique(new_a$new_ElectricalEngg)
new_a$new_ElectricalEngg=minmax(new_a$new_ElectricalEngg)
boxplot(new_a$new_ElectricalEngg)
plot(density(new_a$new_ElectricalEngg))
new_a$new_ElectricalEngg=outlier.replcement.with.median(new_a$new_ElectricalEngg)


# for TelecomEngg 
# unique(new_a$TelecomEngg)
table(new_a$TelecomEngg)
new_a$new_TelecomEngg=ifelse(new_a$TelecomEngg==-1,0,new_a$TelecomEngg)
unique(new_a$new_TelecomEngg)
new_a$new_TelecomEngg=minmax(new_a$new_TelecomEngg)
boxplot(new_a$new_TelecomEngg)
plot(density(new_a$new_TelecomEngg))
new_a$new_TelecomEngg=outlier.replcement.with.median(new_a$new_TelecomEngg)


# for CivilEngg 
# unique(new_a$CivilEngg)
table(new_a$CivilEngg)
new_a$new_CivilEngg=ifelse(new_a$CivilEngg==-1,0,new_a$CivilEngg)
unique(new_a$new_CivilEngg)
new_a$new_CivilEngg=minmax(new_a$new_CivilEngg)
boxplot(new_a$new_CivilEngg)
plot(density(new_a$new_CivilEngg))
new_a$new_CivilEngg=outlier.replcement.with.median(new_a$new_CivilEngg)

# for conscientiousness
# unique(new_a$conscientiousness)
plot(density(new_a$conscientiousness))
new_a$conscientiousness=minmax(new_a$conscientiousness)
boxplot(new_a$conscientiousness)
plot(density(new_a$conscientiousness))
new_a$conscientiousness=outlier.replcement.with.median(new_a$conscientiousness)

# for agreeableness
# unique(new_a$agreeableness)
plot(density(new_a$agreeableness))
new_a$agreeableness=minmax(new_a$agreeableness)
boxplot(new_a$agreeableness)
plot(density(new_a$agreeableness))
new_a$agreeableness=outlier.replcement.with.median(new_a$agreeableness)


# for extraversion
# unique(new_a$extraversion)
plot(density(new_a$extraversion))
new_a$extraversion=minmax(new_a$extraversion)
boxplot(new_a$extraversion)
plot(density(new_a$extraversion))
new_a$extraversion=outlier.replcement.with.median(new_a$extraversion)


# for nueroticism
# unique(new_a$nueroticism)
plot(density(new_a$nueroticism))
new_a$nueroticism=minmax(new_a$nueroticism)
boxplot(new_a$nueroticism)
plot(density(new_a$nueroticism))
new_a$nueroticism=outlier.replcement.with.median(new_a$nueroticism)


# for openess_to_experience
# unique(new_a$openess_to_experience)
plot(density(new_a$openess_to_experience))
new_a$openess_to_experience=minmax(new_a$openess_to_experience)
boxplot(new_a$openess_to_experience)
plot(density(new_a$openess_to_experience))
new_a$openess_to_experience=outlier.replcement.with.median(new_a$openess_to_experience)

# reversing one hot encoding
# kk=new_a[,c(26:32)]
# View(kk)
# new_score=reversing_one_hot_encoding(kk)
# View(cbind(new_score,kk))

# Correlation
# new_a=cbind(new_a,new_score)
# View(new_a)
str(new_a)
colnames(new_a)
corr_df= new_a[,c(1,8,11,17,22:24,33:37,40:47)]
corr_df_na.rm=na.omit(corr_df)
str(corr_df)
corrplot::corrplot(cor(corr_df_na.rm))

#boxplot
boxplot(corr_df_na.rm)

# Sal_pre
# View(new_a)
colnames(new_a)
Sal_pre=new_a[,c(1,6,8,11,14,15,17,19,20,21,22:25,33:37,39,40:47)]
colnames(Sal_pre)
# View(Sal_pre)
str(Sal_pre)
dim(Sal_pre)

# Imputation
# which(is.na(Sal_pre),T)
# library(DMwR)
# Sal_pre_1=knnImputation(Sal_pre,k=10)
# Sal_pre_1$new_score=minmax(Sal_pre_1$new_score)
# View(Sal_pre_1)

# train_test_validate
# train=Sal_pre[1:3500,]
# test=Sal_pre[3501:3998,]
# validate=Sal_pre[3999:5498,]
# View(train)

#KNN regression

Sal_pre_knn=Sal_pre[,c(1,3,4,7,8,11:ncol(Sal_pre))]

trainknn=Sal_pre_knn[1:3500,]
testknn=Sal_pre_knn[3501:3998,]
validateknn=Sal_pre_knn[3999:5498,]

library(FNN)
R2=c()
RMSE1=c()
for(i in 1:20){
  Model=knn.reg(trainknn,testknn,trainknn$Salary,k=i)
  Model1=knn.reg(trainknn,y=trainknn$Salary,k=i)
  R2=c(R2,Model1$R2Pred)
  RMSE1=c(RMSE1,sqrt(mean((Model$pred-testknn$Salary)^2)))
}

cbind(c(1:20),R2,RMSE1)

FinalModel=knn.reg(trainknn,testknn,trainknn$Salary,k=5)
FinalModel1=knn.reg(trainknn,y=trainknn$Salary,k=5)
FinalModel1$R2Pred
sqrt(mean((FinalModel$pred-testknn$Salary)^2))

# Linear Regression
# fit_linear = lm(Salary ~ ., data = train)
# summary(fit_linear)
# step(fit_linear)
# 
# fit_linear_2=lm(formula = Salary ~ Gender + X10percentage + X12percentage + 
#                   CollegeTier + Degree + collegeGPA + CollegeState + GraduationYear + 
#                   English + Logical + Quant + conscientiousness + extraversion + 
#                   nueroticism + openess_to_experience + new_Domain + new_ComputerProgramming + 
#                   new_ElectronicsAndSemicon, data = train)
# 
# 
# out_l=predict(fit_linear_2,test)
# 
# RMSE_l=sqrt(mean((out_l-test$Salary)^2))
# RMSE_l
# 
# # Random Forest model
# library(randomForest)
# fit=randomForest(Salary ~ ., train, ntree=500)
# 
# out=predict(fit,test)
# 
# RMSE=sqrt(mean((out-test$Salary)^2))
# RMSE

# corr_mat=data.frame(new_a$`10percentage`,new_a$`12percentage`,new_a$collegeGPA,new_a$English,new_a$Logical,new_a$Quant,new_a$Domain,
#                     new_a$ComputerProgramming,new_a$ElectronicsAndSemicon,new_a$ComputerScience,new_a$MechanicalEngg,new_a$ElectricalEngg,
#                     new_a$TelecomEngg,new_a$CivilEngg,new_a$conscientiousness,new_a$agreeableness,new_a$extraversion,new_a$nueroticism
#                     ,new_a$openess_to_experience)
# cor(corr_mat)
# corrplot::corrplot(cor(corr_mat))#ok
# 
# GKmat=GoodmanKruskal::GKtauDataframe(new_a[,11:20])
# plot(GKmat, diagSize = 0.8)

#
# traincsv=subset(new_a,new_a$V1=="train")
# testcsv=subset(new_a,new_a$V1=="test")
# 
# samp=sample(nrow(traincsv),round(0.85*nrow(traincsv)))
# 
# lasttrain=traincsv[samp,]
# lastvalid=traincsv[-samp,]
# 
# fit=lm(Salary~as.factor(Gender)+as.factor(new_DOB)+`10percentage`+as.factor(`10board`)+as.factor(`12graduation`)+
#          `12percentage`+as.factor(`12board`)+as.factor(CollegeID)+as.factor(CollegeTier)+as.factor(Degree)+as.factor(Specialization)
#        +collegeGPA+as.factor(CollegeCityID)+as.factor(CollegeCityTier)+as.factor(CollegeState)+as.factor(GraduationYear)+English
#        +Logical+Quant+new_Domain+new_ComputerProgramming+new_ElectronicsAndSemicon+new_ComputerScience+new_MechanicalEngg+
#          new_ElectricalEngg+new_TelecomEngg+new_CivilEngg+conscientiousness+agreeableness+extraversion+nueroticism+
#          openess_to_experience,lasttrain)
# 
# summary(fit)                 
# 
# fit1=lm(Salary~as.factor(Gender)+as.factor(new_DOB)+`10percentage`+as.factor(`10board`)+as.factor(`12graduation`)+
#          `12percentage`+as.factor(`12board`)+as.factor(CollegeID)+as.factor(CollegeTier)+as.factor(Degree)+as.factor(Specialization)
#        +collegeGPA+as.factor(CollegeCityID)+as.factor(CollegeCityTier)+as.factor(CollegeState)+as.factor(GraduationYear)+English
#        +Logical+Quant+new_Domain+new_ComputerProgramming+new_ElectronicsAndSemicon+new_ComputerScience+new_MechanicalEngg+
#          new_ElectricalEngg+new_TelecomEngg+new_CivilEngg+conscientiousness+agreeableness+extraversion+nueroticism+
#          openess_to_experience,lasttrain)
# summary(fit1)
# 
# 
# 
# 
# ################################################################################
# unique(new_a[JobCity %like% "Ban" ][,"JobCity"])
# 
# unique(new_a$JobCity[grep('Ban',new_a$JobCity)])






