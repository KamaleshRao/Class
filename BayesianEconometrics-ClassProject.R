source("rSettings.R")
library(glmnet)
library(Matrix)
library(plotly)

projDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/"
codeDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/Code/"
dataDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/Data/"
setwd(codeDir)

####Data Prep###################

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

ReviewDF<-function(tempDF, n=5){
    print(paste("Dim:",dim(tempDF)))
    print("Col Names:")
    print(colnames(tempDF))
    print("---head---")
    print(head(tempDF),n)
    print("---tail---")
    print(tail(tempDF),n)
}

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


yName<-"GrossRegionalProduct"
xNameList<-unique(regMeltDF$Series)
xNameList<-xNameList[!xNameList %in% yName]
regionsList<-unique(regMeltDF$Region)
clusterFieldList<-unique(clusterMeltDF$Series)

##########LASSO######################

formatErrorMat<-function(tempErrorMat, matType="All"){
  dimnames(tempErrorMat) = list(alphaVec, c(1:100))
  tempErrorDF<-melt(tempErrorMat)
  colnames(tempErrorDF)<-c("alpha", "lambda", "error")
  tempErrorDF$Sector<-yName
  tempErrorDF$Type<-matType
  return(data.table(tempErrorDF))}

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


######Clustering

