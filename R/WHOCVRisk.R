#' WHOCVRisk: Calculating cardiovascular disease risk based on WHO cardiovascular disease risk charts: revised models to estimate risk in 21 global regions.
#'
#' Calculating cardiovascular disease risk based on WHO cardiovascular disease risk charts.
#'
#' @param region Region of residence including Andean Latin America, Australasia, Caribbean, Central Asia, Central Europe,
#'   Central Latin America, Central Sub-Saharan Africa, East Asia, Eastern Europe, Eastern Sub-Saharan Africa,
#'   High-Income Asia Pacific, High-Income North America, North Afrika and Middle East, Oceania, South Asia,
#'   Southeast Asia, Southern Latin America, Southern Sub-Saharan Africa, Tropical Latin America,
#'   Western Europe, Western Sub-Saharan Africa.
#' @param age Age from 40 to 74.
#' @param gender Gender including "M" Male and "F" Female.
#' @param smoke Smoke including "Smoker" and "Non-smoker".
#' @param sbp SBP in (mmHg).
#' @param bmi BMI (Only when lab data is not available). BMI= Weight in kilograms divided by height in meters squared.
#' @param dm  Diabetes (Only when lab data is available) including "Yes" Diabetic and "No" Non-Diabetic.
#' @param chol Total cholesterol in (mmol/l) (Only when lab data is available).
#'
#' @return Output is the World Health Organization cardiovascular disease risk revised estimates based on 21 global regions.
#'
#'
#' @examples
#' Example when lab data is not available 
#'   
#'   WHOCVRisk(region="High-Income Asia Pacific",
#'          age=50,gender="F",smoke="Smoker",
#'          bmi=27,sbp=160) 
#'
#'Example when lab data is available
#'   
#'   WHOCVRisk(region="High-Income Asia Pacific",
#'          age=50,gender="F",smoke="Smoker",
#'          sbp=160,dm="Yes",chol=4.5) 
#'
#' @export



WHOCVRisk=function(region,age,gender,smoke,sbp,bmi=NULL,dm=NULL,chol=NULL)
{
  
  if(is.null(dm) & is.null(chol) & !is.null(bmi))
  {
    Ref=CVDRisk
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
    return(paste0(df$Risk,"%"))
    
  }else if(is.null(bmi)){
    
    Ref=CVDRiskwlab
    df=data.frame(region,age,gender,smoke,sbp,dm,chol)
    
    #Age
    df$age2=NA
    df$age2[df$age>=40 & df$age<=44]<-"40-44"
    df$age2[df$age>=45 & df$age<=49]<-"45-49"
    df$age2[df$age>=50 & df$age<=54]<-"50-54"
    df$age2[df$age>=55 & df$age<=59]<-"55-59"
    df$age2[df$age>=60 & df$age<=64]<-"60-64"
    df$age2[df$age>=65 & df$age<=69]<-"65-69"
    df$age2[df$age>=70 & df$age<=74]<-"70-74"
    
    #SBP
    df$sbp2=NA
    df$sbp2[df$sbp<120]<-"<120"
    df$sbp2[df$sbp>=120 & df$sbp<=139]<-"120-139"
    df$sbp2[df$sbp>=140 & df$sbp<=159]<-"140-159"
    df$sbp2[df$sbp>=160 & df$sbp<=179]<-"160-179"
    df$sbp2[df$sbp>=180]<-">=180"
    
    
    df$chol2=NA
    df$chol2[df$chol<4]<-"<4"
    df$chol2[df$chol>=4 & df$chol<=4.9]<-"4-4.9"
    df$chol2[df$chol>=5 & df$chol<=5.9]<-"5-5.9"
    df$chol2[df$chol>=6 & df$chol<=6.9]<-"6-6.9"
    df$chol2[df$chol>=7]<-">=7"
    
    Var1=c("region","age2","gender","smoke","sbp2","dm","chol2")
    df=df[,Var1]
    
    
    Region<-Ref$Region[match(df$region,Ref$Region)]
    Age<-Ref$Age[match(df$age2,Ref$Age)]
    Gender<-Ref$Gender[match(df$gender,Ref$Gender)]
    Smoke<-Ref$Smoke[match(df$smoke,Ref$Smoke)]
    SBP<-Ref$SBP[match(df$sbp2,Ref$SBP)]
    DM<-Ref$DM[match(df$dm,Ref$DM)]
    CHOL<-Ref$CHOL[match(df$chol2,Ref$CHOL)]
    
    #Create a new variable called luv (look up value)
    df$fin<-paste(df$region,df$age2,df$gender,df$smoke,df$bmi2,df$sbp2,df$dm,df$chol2,sep="")
    
    #Create a new variable called refv (reference value)
    Ref$fin<-paste(Ref$Region,Ref$Age,Ref$Gender,Ref$Smoke,Ref$BMI,Ref$SBP,Ref$DM,Ref$CHOL,sep="")
    
    df$Risk<-Ref$Risk[match(df$fin,Ref$fin)]
    return(paste0(df$Risk,"%"))
  }
  
}

