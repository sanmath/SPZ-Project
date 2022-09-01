## New approach
library(readxl)
library(dplyr)
library(ggplot2)
library(gt)

setwd("C:/Users/boris/Desktop/data hospital")
# SCIM analysis -----------------------------------------------------------

#Codigos correspondientes acute -rehabilitation
codes <- read_excel("codes_2phases.xlsx")
getwd()
# leemos el conjunto de datos que contiene los valores SCIM 2017-2020
SCIM_17_20 <- read_excel("Auswertung_SCIM_2017-20201.xlsx")
#Transformamos el Codigo del paciente 
SCIM_17_20<-SCIM_17_20 %>% mutate(FID=as.character(FID))

# Notar que se desea mostrar SCIM en el tiempo al menos 4 valores
# 2 de acute y 2 de rehabilitacion
# Por lo tanto solo necesitamos considerar los codigos de la tabla codes
SCIM_17_20 %>% filter(FID=="6006542") %>% View()
SCIM_17_20 %>% filter(format(SCIM_17_20$DATUM,format = "%Y")=="2017")%>% select(FID) %>% table() %>% data.frame()

data.frame(table(SCIM_17_20$FID))
# Here I consider only codes that appear twice, entry-ac/rh-exit
SCIM_17_20 %>% filter(FID=="6010875")%>% View()
dobles <- SCIM_17_20 %>% select(FID) %>% 
  table() %>% as.data.frame()%>%filter(Freq==2)%>%select(FID)

singles <- SCIM_17_20 %>% select(FID) %>% 
  table() %>% as.data.frame()%>%filter(Freq==1)%>%select(FID)

# Selecciono solo los datos en dobles y ordeno por codigo y fecha
# Asi se ordenan los valores de SCIM en orden (fecha)
A1 <- SCIM_17_20 %>% filter(FID %in% dobles$FID) %>% 
  select(FID,DATUM,SCIM_TOTAL)  %>% 
  arrange(FID,DATUM)
SCIM_17_20 %>% select(FID) %>% table() %>% data.frame() %>% arrange(-Freq) %>% head()


# Agrupo por cada codigo, y muestro fecha1 y fecha2
# con respectivo SCIM1 y SCIM2
A1 <- A1%>%group_by(FID)%>%
  summarize(date1=first(DATUM),date2=last(DATUM),
            scim1=first(SCIM_TOTAL),scim2=last(SCIM_TOTAL))


# Genero codigo pat en codes (el unico que siempre necesitaba)
codes<-codes%>%mutate(indpat=paste("pat",1:nrow(codes),sep=""))

# Separo codigos ac_phase y rh_phase
ac<-codes%>%select(code1,indpat)
rh<-codes%>%select(code2,indpat)
ac1<-ac%>% select(code1)%>% mutate(phase="acute")
rh1<-rh%>% select(code2) %>% mutate(phase="rehab")
colnames(ac1)<-colnames(rh1)
cds<-rbind(ac1,rh1)
writexl::write_xlsx(x = cds,path = "cds.xlsx")
#emparejo datos para tener id unico pat en la tabla SCIM y saber como
#evoluciona cada paciente

scim_ac <-SCIM_17_20%>% merge(ac,by.x="FID",by.y="code1")

scim_rh <- SCIM_17_20 %>% merge(rh,by.x="FID",by.y="code2")

scim<-bind_rows(scim_ac,scim_rh)

SCIM <- scim %>%arrange(indpat,DATUM,FID) %>% select(indpat,FID, DATUM, SCIM_TOTAL)


#DONE!!
# Here we have a table where every patient is repeated 4 times with dates of
# SCIM measures and SCIM VALUE

# Info patient-diags- treats ----------------------------------------------


#diagnoses
v30<-paste("V",seq(30,39,1),sep="")
#treatments
v40<-paste("V",seq(40,50,1),sep="")

#I select interest variables and create year to know
#from which table comes every row.
source("lectura_MDMKMB.R")



# Additional information for repeated measures dataset --------------------


cpth2017 <- read_excel("cpth2017.xlsx")
cpth2018 <- read_excel("cpth2018.xlsx")
cpth2019 <- read_excel("cpth2019.xlsx")
cpth2020 <- read_excel("cpth2020.xlsx")


cpth2017<-cpth2017%>%mutate(year=2017)
cpth2018<-cpth2018%>%mutate(year=2018)
cpth2019<-cpth2019%>%mutate(year=2019)
cpth2020<-cpth2020%>%mutate(year=2020)


cpth<-rbind(cpth2017,cpth2018,cpth2019,cpth2020)%>%select(V1,clipath_code,year)

data.frame(table(cpth$V1))%>% arrange(-Freq)

SCIM$SCIM_TOTAL<-as.numeric(SCIM$SCIM_TOTAL)

cpth$V1<-as.character(cpth$V1)
colnames(cpth)
#Indications
ind_cpth <- read_excel("ind_cpth.xlsx")


SCIM_prin<-SCIM %>% merge(cpth,by.x="FID",by.y="V1") %>%
  merge(ind_cpth,by.x="clipath_code",by.y="Indication")

indpat4<-SCIM_prin%>%arrange(indpat,DATUM) %>% 
  group_by(indpat)%>%summarize(n=n())%>%filter(n==4)%>%select(indpat)

SCIM_def<-SCIM_prin%>%filter(indpat %in% indpat4$indpat)

dtf<-SCIM_def%>%arrange(indpat,DATUM)%>%
  mutate(variable=factor(rep(seq(1,4,1),424),ordered=TRUE)
)
head(dtf)
#dtf  dataset with all the scim values, 4 times each patient
colnames(dtf)
table(dtf$indpat)
#saveRDS(dtf,"SCIM4.rds")

dtf%>%ggplot(aes(variable, SCIM_TOTAL, color = Clinical.Pathway, group = indpat))+
  geom_point(size = 1.5) +
  geom_line()+facet_wrap(~Clinical.Pathway)

dictionary_vars %>% select(FID) %>% table() %>% data.frame() %>% arrange(-Freq) %>%

dictionary_vars <- read_excel("dictionary_vars.xlsx", 
                              sheet = "Sheet2")


PID<- dictionary_vars
PID%>%select(PID)%>%table()%>%data.frame()%>%arrange(-Freq)

colnames(PID)
nrow(mb_def)
mb_def  %>%merge(PID,by.x="id",by.y="FID",all.x=TRUE)%>%
  arrange(PID) %>% select(PID, id, V11,V12,V30,V31) 


mb_def2 %>% select(id) %>% unique()

fq<-PID%>%select(PID)%>%table()%>%data.frame()%>%arrange(-Freq)%>%select(Freq)
hist(fq$Freq, main="Frequency")
unique(PID$PID)
str(dtf)

mb_def %>% select(id) %>% table()%>% data.frame() %>% arrange(-Freq)
## Costos

head(cost2017)
head(cost2018)
head(cost2019)
head(cost2020)


cost2017<-cost2017%>%select(-V81)
colnames(cost2018)
cost2018<-cost2018%>%select(-KK_Total)

costo2017b<-apply(cost2017,MARGIN=2,FUN=function(x)ifelse(is.na(x),0,x))
costo2018b<-apply(cost2018,MARGIN=2,FUN=function(x)ifelse(is.na(x),0,x))
costo2019b<-apply(cost2019,MARGIN=2,FUN=function(x)ifelse(is.na(x),0,x))
costo2020b<-apply(cost2020,MARGIN=2,FUN=function(x)ifelse(is.na(x),0,x))

costo<-data.frame(rbind(costo2017b,costo2018b,costo2019b,costo2020b))

costo2<-costo%>%mutate(id=as.character(V2))%>%select(-V2)

total_cost<-costo%>%select(V3:V80)%>%rowSums()

data_cost<-data.frame(id=as.character(costo2$id),year=costo2$V1,total_cost=as.numeric(total_cost))



colnames(data_cost)
duplicados_costos<-data_cost%>% select(id)%>%table()%>%data.frame()%>%filter(Freq>1)%>%select(id)

#Hay duplicados en year 2017 y 2018
data_cost%>%filter(id%in%duplicados_costos$id)%>%arrange(id)%>%select(year)%>%table()

#hagamos un merge sin considerar los duplicados 
data_cost2<-data_cost%>%filter(!id%in%duplicados_costos$id)%>%arrange(id)
# data_cost2 tiene datos sin duplicados


mb_def2<-mb_def  %>%merge(PID,by.x="id",by.y="FID")%>%
  merge(data_cost2,by.x="id",by.y="id")


mb_def2 %>% group_by(PID) %>% 
  summarize(tcost=sum(total_cost))%>%select(tcost)

mb_def2$V16<-substring(mb_def2$V16,first = 1,last = 8 )
mb_def2$V26<-substring(mb_def2$V26,first = 1,last = 8 )
mb_def2$V26<-as.Date(mb_def2$V26,format = "%Y %m %d")
mb_def2$V16<-as.Date(mb_def2$V16,format = "%Y %m %d")

mb_def2<-mb_def2 %>% mutate(ndays=as.numeric(V26-V16))
saveRDS(mb_def2,"mb_def2.rds")

library(writexl)
write_xlsx()
write_xlsx(mb_def2,"MB.xlsx")
mbmb<-readxl::read_excel("MB.xlsx")
mbmb%>% filter(PID=="0000633")%>% View()



colnames(mbmb)
write_xlsx(dtf,"SCIM_values.xlsx")
write_xlsx(treat_time,"treat_time.xlsx")
mb_def2 %>% colnames()

readRDS("cnames.RDS")
# We define SCIM variation
var_scim<-dtf %>% group_by(FID) %>% summarize(var_SCIM=max(SCIM_TOTAL)-min(SCIM_TOTAL))
head(dtf)

mb_def3<-mb_def2 %>%merge(var_scim,by.x="id",by.y="FID") 
#######REGRESSIONS
mb_def3 %>% group_by(PID) %>% summarize(Vscim=max(var_SCIM))
colnames(treat_time)

testing<-treat_time %>% group_by(id) %>% summarize(manas=mean(Anästhesie),mphys=mean(Physiotherapie))
acu<-codes %>% select(id=code1) %>% mutate(col="acute")
reh<-codes %>% select(id=code2) %>% mutate(col="rehab")
cds<-rbind(acu,reh)
testing %>% merge(cds,by="id")%>% head(n=100)
codes
head(testing)
table(treat_time$id)%>%data.frame()%>% arrange(-Freq)

hist(treat_time$Bildgebende_Verfahren,breaks = 200,xlim = c(0,10000))

ggplot(plot,aes(x=`Physiotherapie`))+geom_histogram()+facet_wrap(~col)

mb_def2$ndays
table(mb_def2$PID)
## Normalization data units of variables in treat_time and see patterns
## total_sum_minutes/tax_points/CHF/ divided to n_days(length_of_stay)
## Description average, sd, var, max ,min... check presentation ppt
mb_def$V26
mb_def %>% filter(is.na(V26))
plot<-treat_time%>%merge(cds,by="id")
pidfid %>% filter(is.na(FID) )
mb_def %>% filter(is.na(id))
dictionary_vars 
plot<-table(mb_def2$id)%>%data.frame() %>% arrange(-Freq)
dim(mb_def3)

mb_def2<-readRDS("mb_def2.rds")
cnms<-readRDS("cnames.RDS")
colnames(mb_def2)<-colnames(cnms)
save.RDS(mb_def2)
writexl::write_xlsx(mb_def2,"mb_def2.xlsx")
getwd()
head(mb_def2)

library(zipcodeR)
w
library(lme4)
lmer()
colnames(dtf)
lmer(SCIM_TOTAL ~ variable + sex + (1 | id),
     data = sample_data)