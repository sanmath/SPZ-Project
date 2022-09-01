
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


## LECTURA DE COSTOS

# Cost data

cost2017<-read.table("BIC3_SwissDRG_Datensatz 2017.dat",sep = "|",header=FALSE,quote="\"")
##consider _norm cost 2018
cost2018<-read_excel("19-03-29 Abstimmung ITAR_K_SwissDRG_Datensatz.xlsx",sheet="SwissDRG Datensatz_norm")

cost2019<-read.table("MK0000012_ohne_ÜL.txt",sep = "|")
cost2020<-read.table("MK0000018.txt",sep = "|")
