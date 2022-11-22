
library(tidyverse)
library(readxl)

## Set the working directory

Sys.setenv(LANG = "en")
setwd("C:/Users/boris/Desktop/data hospital")

# We read the dataset   year 2017
data_2017  <- read.table("BFS-Datensatz 2017.dat",sep = "\t",header=FALSE,quote="\"")




# Omit two first rows are fake patients
data2017<-data_2017 %>% mutate(year=2017)

data_2017a <- data_2017[-c(1,2),]


num<-sort(rep(seq(1,1503,1),3))
idpat<-paste("pt",num,sep="_")
data_2017a$idpat<-idpat

# year 2018

data_2018 <- read_excel("BFS-Datensatz 2018.xlsx",col_names = FALSE)
data2018<-data_2018%>%mutate(year=2018)
data_2018a <- data_2018[-c(1,2),]
num<-sort(rep(seq(1,1613,1),3))
idpat<-paste("pt",num,sep="_")
data_2018a$idpat<-idpat


#year 2019
data_2019 <- read_excel("BFS-Datensatz_2019.xlsx",col_names = FALSE)
data2019<-data_2019%>%mutate(year=2019)

data_2019a <- data_2019[-c(1,2),]

num<-sort(rep(seq(1,1884,1),3))
idpat<-paste("pt",num,sep="_")
data_2019a$idpat<-idpat


## year 2020
data_2020 <- read_excel("BFS-Datensatz 2020.xlsx",col_names = FALSE)
data2020<-data_2020%>%mutate(year=2020)

data_2020a <- data_2020[-c(1,2),]

num<-sort(rep(seq(1,1566,1),3))
idpat<-paste("pt",num,sep="_")
data_2020a$idpat<-idpat


colnames(data_2018a)<-colnames(data_2017a)
colnames(data_2019a)<-colnames(data_2017a)
colnames(data_2020a)<-colnames(data_2017a)

### We generate separated datasets
MB17<-data_2017a%>%filter(V1=="MB")
MD17<-data_2017a%>%filter(V1=="MD")
MK17<-data_2017a%>%filter(V1=="MK")


MB17a<-MB17 %>% discard(~all(is.na(.) | . ==""))
MD17a<-MD17 %>% discard(~all(is.na(.) | . ==""))
MK17a<-MK17 %>% discard(~all(is.na(.) | . ==""))

MB18<-data_2018a%>%filter(V1=="MB")
MD18<-data_2018a%>%filter(V1=="MD")
MK18<-data_2018a%>%filter(V1=="MK")


MB18a<-MB18 %>% discard(~all(is.na(.) | . ==""))
MD18a<-MD18 %>% discard(~all(is.na(.) | . ==""))
MK18a<-MK18 %>% discard(~all(is.na(.) | . ==""))


MB19<-data_2019a%>%filter(V1=="MB")
MD19<-data_2019a%>%filter(V1=="MD")
MK19<-data_2019a%>%filter(V1=="MK")


MB19a<-MB19 %>% discard(~all(is.na(.) | . ==""))
MD19a<-MD19 %>% discard(~all(is.na(.) | . ==""))
MK19a<-MK19 %>% discard(~all(is.na(.) | . ==""))



MB20<-data_2020a%>%filter(V1=="MB")
MD20<-data_2020a%>%filter(V1=="MD")
MK20<-data_2020a%>%filter(V1=="MK")


MB20a<-MB20 %>% discard(~all(is.na(.) | . ==""))
MD20a<-MD20 %>% discard(~all(is.na(.) | . ==""))
MK20a<-MK20 %>% discard(~all(is.na(.) | . ==""))

cbind(MK17,MK18,MK19,MK20)
## LECTURA DE COSTOS

# Cost data



cost2017<-read.table("BIC3_SwissDRG_Datensatz 2017.dat",sep = "|",header=FALSE,quote="\"")
##consider _norm cost 2018
cost2018<-read_excel("19-03-29 Abstimmung ITAR_K_SwissDRG_Datensatz.xlsx",sheet="SwissDRG Datensatz_norm")

cost2019<-read.table("MK0000012_ohne_ÜL.txt",sep = "|")
cost2020<-read.table("MK0000018.txt",sep = "|")

dim(cost2017)
dim(cost2018)
dim(cost2019)
cost2017
ifelse(is.na(cost2017),0,cost2017)
cost2017<-apply(cost2017,MARGIN=2,FUN=function(x)ifelse(is.na(x),0,x))
time_cost %>% select(id) %>% table()%>%data.frame() %>% arrange(-Freq)
time_cost %>% filter(id=="5211529")

sum(cost2017[,76])
sum(cost2018[,76])
sum(cost2019[,76])
sum(cost2020[,76])


names_costs<-colnames(cost2018[,1:75])

c2017<-cost2017[,1:75]
c2018<-cost2018[,1:75]
c2019<-cost2019[,1:75]
c2020<-cost2020[,1:75]

colnames(c2017)<-names_costs
colnames(c2018)<-names_costs
colnames(c2019)<-names_costs
colnames(c2020)<-names_costs


codes %>% filter(code1=="6035777")
costs_dat %>% filter(FID==costs_dat)

costs_dat<-rbind(c2017,c2018,c2019,c2020)
colnames(costs_dat)
costs_dat %>% filter(FID==6036398) 

## generation base MB

#diagnoses
v30<-paste("V",seq(30,39,1),sep="")
#treatments
v40<-paste("V",seq(40,50,1),sep="")
#I select interest variables and create year to know from which table comes every row.
mb17<-MB17a%>%select(V11,V12,V13,V14,V16,V22,V25,v30,v40)%>%mutate(id=MD17a$V663,year=2017)
mb18<-MB18a%>%select(V11,V12,V13,V14,V16,V22,V25,v30,v40)%>%mutate(id=MD18a$V663,year=2018)
mb19<-MB19a%>%select(V11,V12,V13,V14,V16,V22,V25,v30,v40)%>%mutate(id=MD19a$V663,year=2019)
mb20<-MB20a%>%select(V11,V12,V13,V14,V16,V22,V25,v30,v40)%>%mutate(id=MD20a$V663,year=2020)

mb<-bind_rows(mb17,mb18,mb19,mb20)


library(icd.data)
ICD<-icd10cm2016 %>% select(code,three_digit,short_desc,chapter)



colnames(mb)
