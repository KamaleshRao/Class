####Declare Libraries, Set Sources, Define Directories#############

source("rSettings.R")
library(glmnet)
library(Matrix)
library(plotly)
library(fastDummies)
library(broom)


projDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/"
codeDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/Code/"
dataDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/Data/"
setwd(codeDir)

####Helper Functions###################

###define growth calc functions
lagGrowthCalc<-function(tempVec, tempLag){
  return((tempVec/Lag(tempVec, tempLag)[,1])-1)
}


lagGrowthCalcAndAppend<-function(tempData, colNames, tempLag=12, tempName="YrYr"){
  #temp_df<-reg_df
  for (i in colNames){
    if(class(tempData)[1]=="data.table"){
      tempData<-tempData[,temp:=lagGrowthCalc(tempData[,i, with=F], tempLag)]
      setnames(tempData,"temp",paste(i, tempName, sep="_"))
    }
    else{
      tempData$temp<-lagGrowthCalc(tempData[,i], tempLag)
      colnames(tempData)[match("temp", colnames(tempData))]<-paste(i, tempName, sep="_")
    }
  }
  return(tempData)
}

### Basic Review Function

ReviewDF<-function(tempDF, n=5){
    print(paste("Dim:",dim(tempDF)))
    print("Col Names:")
    print(colnames(tempDF))
    print("---head---")
    print(head(tempDF),n)
    print("---tail---")
    print(tail(tempDF),n)
}


####Data Prep##############################


###Pull in CNA Data Sets
filePreFix<-"AnnualbyProvince-"

dataList<-c()
for (fileName in list.files(dataDir)){
  print(fileName)
  dataList<-c(dataList, gsub(filePreFix,"",fileName))
}


yName<-"GrossRegionalProduct"
xNames<-c("ElectricityConsumption","FixedInvestment","FreightTraffic","HouseholdConsumption",
          "ImportsExports","RetailSales")
dateColName<-"Year"

regMeltDF<-data.frame()
clusterMeltDF<-data.frame()


###Melt Data Set of Economic Time-Series
for (fileName in dataList){
  fieldName<-gsub(".csv", "", fileName)
  print(fieldName)
  ###TO figure out demographics/regressors
  #print(paste(min(tempMeltDF$Year), max(tempMeltDF$Year), length(unique(tempMeltDF$Year)), max(tempMeltDF$Year)- min(tempMeltDF$Year)+1))
  tempDF<-read.csv(paste(dataDir,filePreFix,fileName, sep=""), skip=3, header=TRUE, nrows=31)
  tempMeltDF<-melt(tempDF,id=c("Region"))
  colnames(tempMeltDF)<-c("Region", "Year","Value")
  tempMeltDF$Year<-as.numeric(gsub("X","",tempMeltDF$Year))
  tempMeltDF<-tempMeltDF[complete.cases(tempMeltDF),]
  tempMeltDF$Series<-fieldName
  if (fieldName %in% c(yName, xNames)){
     regMeltDF<-rbind(regMeltDF, tempMeltDF)
    }
  else{
    clusterMeltDF<-rbind(clusterMeltDF,tempMeltDF[tempMeltDF$Year==max(tempMeltDF$Year),])
  }
}
clusterMeltDF$Year<-NULL

regionGroupingsDF<-read.csv(paste(dataDir,"RegionGroupings.csv", sep=""))

###Melt Data Set of Demographics Etc
#tempSummDF<-data.frame()
for (colName in colnames(regionGroupingsDF)[2:length(colnames(regionGroupingsDF))]){
  tempDF<-regionGroupingsDF[,c("Region", colName)]
  colnames(tempDF)<-c("Region","Value")
  tempDF$Series<-colName
  tempDF<-tempDF[,colnames(clusterMeltDF)]
  clusterMeltDF<-rbind(clusterMeltDF,tempDF)
}

yName<-"GrossRegionalProduct"
xNameList<-unique(regMeltDF$Series)
xNameList<-xNameList[!xNameList %in% yName]
regionsList<-unique(regMeltDF$Region)
clusterFieldList<-unique(clusterMeltDF$Series)
geoClusterFieldList<-c("RegionGroup1","EconomicRegion1", "EconomicRegion2")

##########LASSO######################


###UNNECCESSARY
formatErrorMat<-function(tempErrorMat, matType="All"){
  dimnames(tempErrorMat) = list(alphaVec, c(1:100))
  tempErrorDF<-melt(tempErrorMat)
  colnames(tempErrorDF)<-c("alpha", "lambda", "error")
  tempErrorDF$Sector<-yName
  tempErrorDF$Type<-matType
  return(data.table(tempErrorDF))}

###Setup

xRegNames<-xNameList

maxLag<-1

firstEndTrainDate<-2009
firstTrainDate<-2001

startTrainDateNum<-which(trainDateSeries == firstEndTrainDate)[[1]]
trainDateSeries<-c(firstTrainDate:max(regMeltDF$Year))
errorWgtVec<-rep(1,length(trainDateSeries))
errorWgtVec[(length(trainDateSeries)-9):length(trainDateSeries)]<-2
errorWgtVec[(length(trainDateSeries)-6):length(trainDateSeries)]<-4
errorWgtVec[(length(trainDateSeries)-3):length(trainDateSeries)]<-16

alphaVec<-seq(0,1, .1)
lambdaCount<-100
startTrainDateNum<-5
#length(trainDateSeries)-15

forecastSummary<-NULL
allSeriesForecastDF<-NULL
errorDT<-data.table()

startTime<-Sys.time()
print(startTime)
i<-1
tempRegion<-regionNames[i]

###Model Loop
coefNames<-paste(c(yName,xRegNames), "_YrYr",sep="")
coefNames<-c(coefNames,paste(coefNames, "_Lag_1",sep=""))
coefNames<-c(coefNames, "(Intercept)")
factorCountVec<-rep(0, length(coefNames))
names(factorCountVec)<-coefNames
for (tempRegion in regionNames[c(1:25,27:31)]){
  print(paste("Region Name:",tempRegion, ":: Target Name:",yName), sep="")
  tempRegionRegMeltDF<-regMeltDF[regMeltDF$Region==tempRegion,c("Year","Series", "Value")]
  
  tempRegionRegDF<-dcast(data = tempRegionRegMeltDF,formula = Year~Series,fun.aggregate = sum,value.var = "Value")
  tempRegionRegDF<-tempRegionRegDF[order(tempRegionRegDF$Year),]
  tempRegionRegDF<-lagGrowthCalcAndAppend(tempRegionRegDF, colNames=c(yName,xRegNames), tempLag=1)
  tempRegionRegDF<-tempRegionRegDF[,!colnames(tempRegionRegDF) %in% c(yName,xRegNames)]
  tempRegionRegDF<-tempRegionRegDF[complete.cases(tempRegionRegDF),]
  newRegColNames<-colnames(tempRegionRegDF)[2:length(colnames(tempRegionRegDF))]  

  for (colName in newRegColNames){
    for (lagNum in c(1:maxLag)){
      tempRegionRegDF<-cbind(tempRegionRegDF, Lag(tempRegionRegDF[,colName],k=lagNum))
      colnames(tempRegionRegDF)[length(colnames(tempRegionRegDF))]<-paste(colName,"Lag",lagNum, sep="_")
    }
  }
  tempRegionRegDF<-tempRegionRegDF[complete.cases(tempRegionRegDF),]
  newRegColNames<-colnames(tempRegionRegDF)[2:length(colnames(tempRegionRegDF))]
  newYName<-newRegColNames[1]
  newXColNames<-newRegColNames[2:length(newRegColNames)]
  if(dim(tempRegionRegDF)[1]!=17){
    print("Error in Dimension Count")
  }
  
  errorVec<-list()
  for (i in c(1:100)){
    errorVec[[i]]<-c(0)
  }
  for (endTrainDateNum in c(startTrainDateNum:(length(trainDateSeries)-1))){
    tempRegDF<-tempRegionRegDF[tempRegionRegDF[,dateColName]<=trainDateSeries[endTrainDateNum],]
    tempErrWgtVec<-errorWgtVec[tempRegionRegDF[,dateColName]<=trainDateSeries[endTrainDateNum]]
    tempForecastXRegMat<-matrix(sapply(tempRegionRegDF[tempRegionRegDF[,dateColName]==trainDateSeries[endTrainDateNum+1],newXColNames], as.numeric))
    tempForecastY<-tempRegionRegDF[tempRegionRegDF[,dateColName]==trainDateSeries[endTrainDateNum+1],newYName]
    
    tempy<-tempRegDF[complete.cases(tempRegDF),newYName]
    tempErrWgtVec<-tempErrWgtVec[complete.cases(tempRegDF)]
    tempXReg<-tempRegDF[complete.cases(tempRegDF),
                        !(colnames(tempRegDF) %in% c(newYName,dateColName))]
    
    tempFit<-glmnet(x=as.matrix(tempXReg), y=tempy, weights=tempErrWgtVec, nlambda = 100)
    tempForecast<-predict(tempFit, newx=t(tempForecastXRegMat))
    for (i in (1:length(tempForecast))){
      errorVec[[i]]<-c(errorVec[[i]],(tempForecastY-tempForecast[i])^2)
    }
  }
  avgError<-c()
  for (i in c(1:100)){
    avgError<-c(avgError,mean(errorVec[[i]][2:length(errorVec[[i]])]))
  }
  optLambda<-min(which.min(avgError[1:90])-5,length(tempFit$lambda)-5)
  coefDF<-data.frame(as.matrix(coef(tempFit,tempFit$lambda[optLambda])))
  for (coefName in row.names(coefDF)[coefDF$X1!=0]){
    #print(coefName)
    factorCountVec[coefName]<-factorCountVec[[coefName]]+1
  }
  #print(t(row.names(coefDF)[coefDF$X1!=0]))
}

modelXNames<-c("ElectricityConsumption_YrYr","ImportsExports_YrYr","RetailSales_YrYr","HouseholdConsumption_YrYr")
modelYName<-"GrossRegionalProduct_YrYr"

######Clustering

clusterValsDF<-dcast(clusterMeltDF[!clusterMeltDF$Series %in% geoClusterFieldList,], Region~Series, value.var="Value")

clusterSS<-c()
for (k in c(1:10)){
  clusterSS<-c(clusterSS,kmeans(clusterValsDF[,colnames(clusterValsDF)!="Region"],k)$tot.withinss)
}

plot(clusterSS)

k<-3

regionClusterDF<-data.frame(Region=clusterValsDF$Region,
                       DemoCluster=kmeans(clusterValsDF[,colnames(clusterValsDF)!="Region"],
                                          k)$cluster)
regionGroupingsDF<-merge(regionGroupingsDF, regionClusterDF, by="Region")
clusterList<-unique(regionClusterDF$DemoCluster)

#######Master Data Frame

dummy_cols(regionClusterDF$DemoCluster)

regionDataDF<-regionClusterDF

regionDataDF<-cbind(regionDataDF,
                    dummy_cols(regionClusterDF$DemoCluster)[,2:length(colnames(dummy_cols(regionClusterDF$DemoCluster)))])
colnames(regionDataDF)[3:length(colnames(regionDataDF))]<-c("cluster1","cluster2","cluster3")
regionDataDF<-cbind(regionDataDF,
                    dummy_cols(regionGroupingsDF$RegionGroup1)[,2:length(colnames(dummy_cols(regionGroupingsDF$RegionGroup1)))])
colnames(regionDataDF)<-gsub("\\.data_","RegGroup1_",colnames(regionDataDF) )
regionDataDF<-cbind(regionDataDF,
                    dummy_cols(regionGroupingsDF$EconomicRegion1)[,2:length(colnames(dummy_cols(regionGroupingsDF$EconomicRegion1)))])
colnames(regionDataDF)<-gsub("\\.data_","EcoReg1_",colnames(regionDataDF) )
regionDataDF<-cbind(regionDataDF,
                    dummy_cols(regionGroupingsDF$EconomicRegion2)[,2:length(colnames(dummy_cols(regionGroupingsDF$EconomicRegion2)))])
colnames(regionDataDF)<-gsub("\\.data_","EcoReg2_",colnames(regionDataDF) )
colnames(regionDataDF)[1]<-"Region"

regionRegDataDF<-dcast(regMeltDF, Region+Year~Series, value.var="Value")
regionRegDataDF<-lagGrowthCalcAndAppend(regionRegDataDF, colNames=c(yName,xRegNames), tempLag=1)
regionRegDataDF<-regionRegDataDF[,c(modelYName, "Year", "Region", modelXNames)]
regionRegDataDF<-merge(regionRegDataDF, regionDataDF,by="Region", all.x=TRUE)
regionRegDataDF<-merge(regionRegDataDF, regionGroupingsDF,by="Region", all.x=TRUE)
regionRegDataDF<-regionRegDataDF[complete.cases(regionRegDataDF),]

###Format BUGS data take 2

regionRegDataOPENBUGSDF<-regionRegDataDF[,c("Region","GrossRegionalProduct_YrYr","Year",                       
                                            "ElectricityConsumption_YrYr","ImportsExports_YrYr",
                                            "RetailSales_YrYr", "HouseholdConsumption_YrYr", 
                                            "DemoCluster","EconomicRegion1","RegionGroup1")]

regionRegDataOPENBUGSDF$Region<-as.numeric(factor(regionRegDataOPENBUGSDF$Region))
regionRegDataOPENBUGSDF$EconomicRegion1<-as.numeric(factor(regionRegDataOPENBUGSDF$EconomicRegion1))
regionRegDataOPENBUGSDF$RegionGroup1<-as.numeric(factor(regionRegDataOPENBUGSDF$RegionGroup1))

colnames(regionRegDataOPENBUGSDF)<-c("Region", "GDPYY", "Year", "ElecYY", "ImpExpYY", "RetSalYY", "ConsYY", "DCluster", "EcoReg1", "RegGr1")
colnames(regionRegDataOPENBUGSDF)<-paste(colnames(regionRegDataOPENBUGSDF),"[]", sep="")
write.table(regionRegDataOPENBUGSDF,file="regionRegData.txt", row.names = FALSE, quote=FALSE)

###Format BUGS data take 2

regionGroupField<-regionRegDataDF$EconomicRegion1
n <- dim(regionRegDataDF)[1]
y <- regionRegDataDF$GrossRegionalProduct_YrYr
x1 <- regionRegDataDF$ElectricityConsumption_YrYr
x2 <- regionRegDataDF$ImportsExports_YrYr
x3 <- regionRegDataDF$RetailSales_YrYr
x4 <- regionRegDataDF$HouseholdConsumption_YrYr
J <- length(unique(regionGroupField))
regionGroup<-as.numeric(factor(regionGroupField))
## Call Bugs from R (remember to save the radon.1.bug file before)
regionData.bugs <- list ("n", "J", "x1","x2","x3","x4", "y", "regionGroup")
regionGroupNames<-levels(factor(regionGroupField))

###Basic Regression#########################

regionLM0<-lm(GrossRegionalProduct_YrYr~., data=regionRegDataDF[,c(modelYName, modelXNames)])
tidy(regionLM0)
plot1<-ggplot(data.frame("residuals"=regionLM0$residuals, 
                         "Year"=as.factor(regionRegDataDF$Year)))+
      geom_boxplot(aes(x=Year, y=residuals, fill=Year))+theme2+
      theme(axis.text.x = element_text(size=12, angle=90))
print(plot1)

#Basic Regression by Region
regionLM0aResults<-data.frame()
regionLM0aResids<-data.frame()
for (region in regionsList){
  regionLM0a<-lm(GrossRegionalProduct_YrYr~., 
                data=regionRegDataDF[regionRegDataDF$Region==region,
                                     c(modelYName, modelXNames)])
  tempDF<-data.frame(t(tidy(regionLM0a)$estimate))
  tempDF$Measure<-"coef"
  tempDF2<-data.frame(t(tidy(regionLM0a)$p.value))
  tempDF2$Measure<-"pval"
  tempDF<-rbind(tempDF,tempDF2)
  tempDF$Region<-region
  regionLM0aResults<-rbind(regionLM0aResults,tempDF)
  tempDF3<-data.frame(regionLM0a$residuals)
  tempDF3$Region<-region
  tempDF3$Year<-regionRegDataDF[regionRegDataDF$Region==region,
                                "Year"]
  regionLM0aResids<-rbind(regionLM0aResids,tempDF3)
}
colnames(regionLM0aResults)[1:5]<-tidy(regionLM0a)$term
colnames(regionLM0aResids)<-c("residuals", "Region", "Year")

meltRegionLM0aResults<-melt(regionLM0aResults[regionLM0aResults$Measure=="coef",], id=c("Region"))

meltRegionLM0aResults<-meltRegionLM0aResults[!(meltRegionLM0aResults$variable %in% c("(Intercept)", "Measure")),]

plot2<-ggplot(data=meltRegionLM0aResults,aes(x=as.numeric(value), fill=variable)) + geom_density(alpha=0.5)+
  theme1+theme(axis.text.x = element_text(size=12, angle=90))+labs(x ="Coef Value")
print(plot2)

#NEEDS CLEANING UP
plot3<-ggplot(data=regionLM0aResids,
              aes(x=Year,y=residuals, 
              color=Region, shape=Region))+
              geom_line()+geom_point()+
              scale_colour_manual(name = "Region",
                      labels = regionsList,
                      values = rep_len(hcl(h = seq(15, 375, length = 11),
                                           l = 65, c = 100),
                                       length(regionsList))) +   
              scale_shape_manual(name = "Region",
                     labels = regionsList,
                     values = rep_len(c(1:5),
                                      length(regionsList)))+
              theme1
print(plot3)

tempRegionsList<-regionsList[26:33]

plot3a<-ggplot(data=regionLM0aResids[regionLM0aResids$Region %in% tempRegionsList,],
              aes(x=Year,y=residuals, 
                  color=Region, shape=Region))+
  geom_line()+geom_point()+
  scale_colour_manual(name = "Region",
                      labels = tempRegionsList,
                      values = rep_len(hcl(h = seq(15, 375, length = 5),
                                           l = 65, c = 100),
                                       length(tempRegionsList))) +   
  scale_shape_manual(name = "Region",
                     labels = tempRegionsList,
                     values = rep_len(c(1:5),
                                      length(tempRegionsList)))+
  theme1
print(plot3a)

#Group Coef Results by Region Definitions

regionGroupLM0Results<-merge(regionLM0aResults, 
                             regionGroupingsDF,
                          all.x=TRUE,
                         by="Region")

meltClusterLM0Results<-melt(regionGroupLM0Results[regionGroupLM0Results$Measure=="coef",], id=c("DemoCluster"))

plot4<-ggplot(data=meltClusterLM0Results[meltClusterLM0Results$variable %in% modelXNames,],
              aes(x=variable, y=as.numeric(value), fill=as.factor(DemoCluster)))+
  geom_boxplot()+theme1+
  theme(axis.text.x = element_text(size=9, angle=15))+labs(x ="Coef", y="Coef Value", fill="Cluster")
print(plot4)

meltRG1rLM0Results<-melt(regionGroupLM0Results[regionGroupLM0Results$Measure=="coef",], id=c("RegionGroup1"))

plot5<-ggplot(data=meltRG1rLM0Results[meltRG1rLM0Results$variable %in% modelXNames,],
              aes(x=variable, y=as.numeric(value), fill=as.factor(RegionGroup1)))+
  geom_boxplot()+theme1+
  theme(axis.text.x = element_text(size=9, angle=15))+labs(x ="Coef", y="Coef Value", fill="Region Group 1")
print(plot5)

meltER1rLM0Results<-melt(regionGroupLM0Results[regionGroupLM0Results$Measure=="coef",], id=c("EconomicRegion1"))

plot6<-ggplot(data=meltER1rLM0Results[meltER1rLM0Results$variable %in% modelXNames,],
              aes(x=variable, y=as.numeric(value), fill=as.factor(EconomicRegion1)))+
  geom_boxplot()+theme1+
  theme(axis.text.x = element_text(size=9, angle=15))+labs(x ="Coef", y="Coef Value", fill="Economic Region 1")
print(plot6)


regionGroupLM0Resids<-merge(regionLM0aResids, 
                             regionGroupingsDF,
                             all.x=TRUE,
                             by="Region")

meltClusterLM0Resids<-melt(regionGroupLM0Resids, id=c("DemoCluster"))

plot8<-ggplot(data=meltClusterLM0Resids[meltClusterLM0Resids$variable=="residuals",],
              aes(x=variable, y=as.numeric(value), fill=as.factor(DemoCluster)))+
  geom_boxplot()+theme1+
  theme(axis.text.x = element_blank())+labs(x ="Cluster", y="Residuals", fill="Cluster")
print(plot8)

meltRG1rLM0Resids<-melt(regionGroupLM0Resids, id=c("RegionGroup1"))

plot9<-ggplot(data=meltRG1rLM0Resids[meltRG1rLM0Resids$variable=="residuals",],
              aes(x=variable, y=as.numeric(value), fill=as.factor(RegionGroup1)))+
  geom_boxplot()+theme1+
  theme(axis.text.x = element_blank())+labs(x ="Coef", y="Coef Value", fill="Region Group 1")
print(plot9)

meltER1rLM0Resids<-melt(regionGroupLM0Resids, id=c("EconomicRegion1"))

plot10<-ggplot(data=meltER1rLM0Resids[meltER1rLM0Resids$variable=="residuals",],
              aes(x=variable, y=as.numeric(value), fill=as.factor(EconomicRegion1)))+
  geom_boxplot()+theme1+
  theme(axis.text.x = element_blank())+labs(y="Coef Value", fill="Economic Region 1")
print(plot10)

###BUGS Code########

regionMod.inits <- function (){
  list (a=rnorm(J), b1=rnorm(1),b2=rnorm(1),b3=rnorm(1),
        b4=rnorm(1),mu.a=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
region.parameters <- c ("a", "b1","b2", "b3", "b4", "mu.a", "sigma.y", "sigma.a")

regionM1.bugs.1 <- bugs (regionData.bugs, regionMod.inits, region.parameters, "regionMod1.txt", n.iter=1000,
                      working.directory=NULL, clearWD=TRUE, debug=TRUE )

plot(regionM1.bugs.1, display.parallel = FALSE)

str(regionM1.bugs.1)

alphaSim<-regionM1.bugs.1$sims.list$a
colnames(alphaSim)<-paste("a", regionGroupNames, sep="_")
meltAlphaSim<-melt(alphaSim)[,c(2:3)]

colnames(meltAlphaSim)<-c("Variable", "Value")
plot11<-ggplot(data=meltAlphaSim,aes(x=as.numeric(Value), fill=Variable)) + geom_density(alpha=0.5)+theme1
print(plot11)

###Model 2: Group Alpha, Group Beta

regionMod.inits <- function (){
  list (a=rnorm(J), b1=rnorm(1),b2=rnorm(1),b3=rnorm(1),
        b4=rnorm(1),mu.a=rnorm(1),mu.b1=rnorm(1),mu.b2=rnorm(1),
        mu.b3=rnorm(1),mu.b4=rnorm(1),
        sigma.y=runif(1), sigma.a=runif(1))
}
region.parameters <- c ("a", "b1","b2", "b3", "b4", "mu.a", "sigma.y", "sigma.a")

regionM2.bugs.2 <- bugs (regionData.bugs, regionMod.inits, region.parameters, "regionMod2.txt", n.iter=1000,
                         working.directory=NULL, clearWD=TRUE, debug=TRUE )

plot(regionM2.bugs.2, display.parallel = FALSE)

alphaSim<-regionM2.bugs.2$sims.list$a
colnames(alphaSim)<-paste("a", regionGroupNames, sep="_")
meltAlphaSim<-melt(alphaSim)[,c(2:3)]

colnames(meltAlphaSim)<-c("Variable", "Value")
plot12<-ggplot(data=meltAlphaSim,aes(x=as.numeric(Value), fill=Variable)) + geom_density(alpha=0.5)+theme1
print(plot12)

betaSim1<-regionM2.bugs.2$sims.list$b1
colnames(betaSim1)<-paste("b1", regionGroupNames, sep="_")
betaSim2<-regionM2.bugs.2$sims.list$b2
colnames(betaSim2)<-paste("b2", regionGroupNames, sep="_")
betaSim3<-regionM2.bugs.2$sims.list$b3
colnames(betaSim3)<-paste("b3", regionGroupNames, sep="_")
betaSim4<-regionM2.bugs.2$sims.list$b4
colnames(betaSim4)<-paste("b4", regionGroupNames, sep="_")

betaSim<-cbind(betaSim1, betaSim2, betaSim3, betaSim4)

meltBetaSim<-melt(betaSim)[,c(2:3)]

colnames(meltBetaSim)<-c("Variable", "Value")
plot13a<-ggplot(data=meltBetaSim[meltBetaSim$Variable %in% colnames(betaSim1),],aes(x=as.numeric(Value), fill=Variable)) + geom_density(alpha=0.5)+theme1
print(plot13a)

colnames(meltBetaSim)<-c("Variable", "Value")
plot13b<-ggplot(data=meltBetaSim[meltBetaSim$Variable %in% colnames(betaSim2),],aes(x=as.numeric(Value), fill=Variable)) + geom_density(alpha=0.5)+theme1
print(plot13b)

colnames(meltBetaSim)<-c("Variable", "Value")
plot13c<-ggplot(data=meltBetaSim[meltBetaSim$Variable %in% colnames(betaSim3),],aes(x=as.numeric(Value), fill=Variable)) + geom_density(alpha=0.5)+theme1
print(plot13c)

colnames(meltBetaSim)<-c("Variable", "Value")
plot13d<-ggplot(data=meltBetaSim[meltBetaSim$Variable %in% colnames(betaSim4),],aes(x=as.numeric(Value), fill=Variable)) + geom_density(alpha=0.5)+theme1
print(plot13d)