library(reshape2)

projDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/"
codeDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/Code/"
dataDir<-"C:/My Crap/Lurn/Class/Bayesian Econometrics(JHU)/Project/Data/"
setwd(codeDir)

ReviewDF<-function(tempDF, n=5){
    print(paste("Dim:",dim(tempDF)))
    print(paste("Col Names:",colnames(tempDF)))
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
  if (fieldName %in% c(yName, xNames)){
     regMeltDF<-rbind(regMeltDF, tempMeltDF)
    }
  else{
    clusterMeltDF<-rbind(clusterMeltDF,tempMeltDF[tempMeltDF$Year==max(tempMeltDF$Year),])
  }
}

            