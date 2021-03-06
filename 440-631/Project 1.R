library(vars)
workdir<-"C:/My Crap/Lurn/Class/Finance and the Macroeconomy(JHU)/Project 1"
setwd(workdir)
alldata<-read.csv("alldata.csv",colClasses=c("character", rep("numeric",6), "character"),na.strings='#N/A')
alldata$Date<-as.Date(alldata$Date, format="%m/%d/%Y")
alldata<-subset(alldata,select=-c(X))
alldata$LOGGDPH<-log(alldata$GDPH)
alldata$LOGDGDP<-log(alldata$DGDP)
alldata$LOGPZRJOC<-log(alldata$PZRJOC)
alldata$LOGPFABWC<-log(alldata$FABWC)
spreadData<-read.csv("SpreadData.csv")
colnames(spreadData)[1]<-"Date"
spreadData$Date<-as.Date(spreadData$Date, format="%m/%d/%Y")


  
break1<-as.Date("6/30/1983", format="%m/%d/%Y")
break2<-as.Date("6/30/1990", format="%m/%d/%Y")
break3<-as.Date("9/30/2010", format="%m/%d/%Y")
break4<-as.Date("6/30/2002", format="%m/%d/%Y")

allVars<-c("FFED","FTCIL","LOGGDPH","LOGDGDP","LOGPZRJOC","LOGPFABWC")

data1<-alldata[alldata$Date<=break1,]
data2<-alldata[alldata$Date>=break2 & alldata$Date<break3,]
data3<-alldata[alldata$Date<=break1 | (alldata$Date>=break2 & alldata$Date<=break4),]


var0a<-VAR(data1[,allVars], p=4)
var0b<-VAR(data2[,allVars], p=4)
summary(var0a$varresult$FTCIL)
summary(var0b$varresult$FTCIL)
summary(var0a$varresult$LOGGDP)
summary(var0b$varresult$LOGGDP)
summary(var0a$varresult$LOGPFABWC)
summary(var0b$varresult$LOGPFABWC)

mod1Vars<-c("FFED","LOGGDPH","LOGPFABWC","LOGDGDP","LOGPZRJOC")
var1a<-VAR(data1[,mod1Vars], p=4)
var1b<-VAR(data2[,mod1Vars], p=4)
var1c<-VAR(data3[,mod1Vars], p=4)
summary(var1a$varresult$FTCIL)
summary(var1b$varresult$FTCIL)
summary(var1c$varresult$FTCIL)
summary(var1a$varresult$LOGGDP)
summary(var1b$varresult$LOGGDP)
summary(var1c$varresult$LOGGDP)
summary(var1a$varresult$LOGPFABWC)
summary(var1b$varresult$LOGPFABWC)
summary(var1c$varresult$LOGPFABWC)

mod2Vars<-c("FFED","FTCIL","LOGGDPH","LOGPFABWC","LOGDGDP","LOGPZRJOC")
var2a<-VAR(data1[,mod2Vars], p=4)
var2b<-VAR(data2[,mod2Vars], p=4)
var2c<-VAR(data3[,mod2Vars], p=4)
summary(var2a$varresult$FTCIL)
summary(var2b$varresult$FTCIL)
summary(var2c$varresult$FTCIL)
summary(var2a$varresult$LOGGDP)
summary(var2b$varresult$LOGGDP)
summary(var2c$varresult$LOGGDP)
summary(var2a$varresult$LOGPFABWC)
summary(var2b$varresult$LOGPFABWC)
summary(var2c$varresult$LOGPFABWC)

mod3Vars<-c("FFED","FTCIL","LOGGDPH","LOGDGDP","LOGPZRJOC")

var3a<-VAR(data1[,mod3Vars], p=4)
var3b<-VAR(data2[,mod3Vars], p=4)
var3c<-VAR(data3[,mod3Vars], p=4)
summary(var3a$varresult$FTCIL)
summary(var3b$varresult$FTCIL)
summary(var3c$varresult$FTCIL)
summary(var3a$varresult$LOGGDP)
summary(var3b$varresult$LOGGDP)
summary(var3c$varresult$LOGGDP)
summary(var3a$varresult$LOGPFABWC)
summary(var3b$varresult$LOGPFABWC)
summary(var3c$varresult$LOGPFABWC)

modData<-merge(alldata, spreadData, by="Date", all.x=TRUE)
moddata2<-modData[modData$Date>=break2 & modData$Date<break3,]

mod4Vars<-c("FFED","FTCIL","LOGGDPH","LOGDGDP","LOGPZRJOC")

var4a<-VAR(moddata2[,c("FTCIL","LOGPFABWC","Spread")], p=4)

var4b<-VAR(moddata2[,c("FFED","FTCIL","LOGGDPH","LOGPFABWC","LOGDGDP","LOGPZRJOC", "Spread")], p=4)

summary(var4b$varresult$FTCIL)
summary(var4b$varresult$LOGGDP)
summary(var4b$varresult$LOGPFABWC)
