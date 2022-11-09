install.packages("dplyr")
install.packages("plyr")
install.packages("Hmisc")
install.packages("reshape2")
library(Hmisc)
library (plyr)
library(reshape2)
library(dplyr)
destfile<- "D:/徐欣然 研究生/研一/R project/price_index_Feb20201.csv"
download.file(fileUrl,destfile="D:/徐欣然 研究生/研一/R project/price_index_Feb20201.csv",method = "curl")

priceData<-read.csv("D:/徐欣然 研究生/研一/R project/price_index_Feb20201.csv")


############Summarizing data###############
head(priceData,n=3)
tail(priceData)
names(priceData)
summary(priceData)
str(priceData)

quantile(priceData$councilDistrict, na.rm=TRUE)
quantile(priceData$councilDistrict, probs=c(0.5,0.75,0.9))

table(priceData$TIME_REF,useNA="ifany")

sum(is.na (priceData$councilDistrict))
any(is.na(priceData$councilDistrict))
all(priceData$DATA_VAL>0)

colSums(is.na(priceData))
all(colSums(is.na(priceData))==0)
#############Ordering with plyr2##################

arrange(priceData$TIME_REF,desc(priceData$DATA_VAL))

############Creating new variables#############
s1<- seq(1,1000,by=20);s1
priceData$high_VAL = ifelse(priceData$DATA_VAL<1200, TRUE, FALSE) 
table(priceData$high_VAL, priceData$DATA_VAL < 1200)

priceData$VALGroups = cut(priceData$DATA_VAL, breaks=quantile(priceData$DATA_VAL)) 
table(priceData$VALGroups)
table(priceData$VALGroups, priceData$DATA_VAL)
#######cutting with Hmisc labrary########

priceData$VALGroups = cut2 (priceData$DATA_VAL, g=5)
table(priceData$VALGroups)

#######reshape data with reshape2 library###


head(priceData$Series_title_3)
priceData$Serie3name<-rownames(priceData$Series_title_3)
serie3Melt<-melt(priceData$Series_title_3,id=c("Serie3name","Group","Subject"),measure.vars=c("mpg","hp"),na.rm = T)
head(serie3Melt)

#######Use the factor function for column "Series_title_1" and 
#get the average using the price values in column "Data_value" by sapply function. ######

priceData$zcf <- factor(priceData$Series_title_1) 
priceData$zcf[1:2407]
class(priceData$zcf)
sapply(priceData$DATA_VAL, mean)
######################################