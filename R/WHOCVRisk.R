WHOCVRisk=function(region,age,gender,smoke,bmi,sbp)
{


Ref=read.csv(file=system.file("extdata", "CVDRisk.cvs", package = "WHOCVRisk"))
df=data.frame(region,age,gender,smoke,bmi,sbp)


#Age
df$age2=NA
df$age2[df$age>=40 & df$age<=44]<-"40-44"
df$age2[df$age>=45 & df$age<=49]<-"45-49"
df$age2[df$age>=50 & df$age<=54]<-"50-54"
df$age2[df$age>=55 & df$age<=59]<-"55-59"
df$age2[df$age>=60 & df$age<=64]<-"60-64"
df$age2[df$age>=65 & df$age<=69]<-"65-69"
df$age2[df$age>=70 & df$age<=74]<-"70-74"

#BMI
df$bmi2=NA
df$bmi2[df$bmi<20]<-"<20"
df$bmi2[df$bmi>=20 & df$bmi<=24]<-"20-24"
df$bmi2[df$bmi>=25 & df$bmi<=29]<-"25-29"
df$bmi2[df$bmi>=30 & df$bmi<=34]<-"30-35"
df$bmi2[df$bmi>=35]<-"35"

#SBP

df$sbp2=NA
df$sbp2[df$sbp<120]<-"<120"
df$sbp2[df$sbp>=120 & df$sbp<=139]<-"120-139"
df$sbp2[df$sbp>=140 & df$sbp<=159]<-"140-159"
df$sbp2[df$sbp>=160 & df$sbp<=179]<-"160-179"
df$sbp2[df$sbp>=180]<-">=180"

Var1=c("region","age2","gender","smoke","bmi2","sbp2")
df=df[,Var1]

Region<-Ref$Region[match(df$region,Ref$Region)]
Age<-Ref$Age[match(df$age2,Ref$Age)]
Gender<-Ref$Gender[match(df$gender,Ref$Gender)]
Smoke<-Ref$Smoke[match(df$smoke,Ref$Smoke)]
BMI<-Ref$BMI[match(df$bmi2,Ref$BMI)]
SBP<-Ref$SBP[match(df$sbp2,Ref$SBP)]

#Create a new variable called luv (look up value)
  df$fin<-paste(df$region,df$age2,df$gender,df$smoke,df$bmi2,df$sbp2,sep="")

#Create a new variable called refv (reference value)
  Ref$fin<-paste(Ref$Region,Ref$Age,Ref$Gender,Ref$Smoke,Ref$BMI,Ref$SBP,sep="")

df$Risk<-Ref$Risk[match(df$fin,Ref$fin)]
    return(df$Risk)

}
