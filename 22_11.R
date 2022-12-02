serv_cons_phase$id
Tab1<-mb %>% group_by(id)%>% summarize(lesion=first(Lesion),length_stay=first(length_stay))

Servs<-serv_cons_phase  %>% merge(Tab1,by="id")

unique(Servs$lesion)
Servs$les1<-substr(Servs$lesion,1,1)

new_servs<-Servs %>% group_by(les1) %>% summarize(nurse=sum(nurs),phy=sum(physt),n=n(),sum_length=sum(length_stay,na.rm=TRUE))

new_servs %>% mutate(les1=les1,value1=nurse/sum_length,value2=phy/sum_length)

## COnsider all the equivalent measures.
serv_cons_phase
codes
val1<-Servs %>% group_by(uniq) %>% summarize(nurse=sum(nurs),phy=sum(physt),n=n(),sum_length=sum(length_stay,na.rm=TRUE))%>%
  mutate(value1=nurse/sum_length,value2=phy/sum_length)
hist(val1$value1,breaks = 200,xlim=c(0,1000))
colnames(Servs)

table_gender<-mb %>% select(id,gender,length_stay) %>% group_by(id) %>%summarize(id=id,gender=first(gender),patient_stay=sum(length_stay))
Servs<-serv_cons_phase  %>% merge(table_gender,by="id")
colnames(Servs)
servs_gender<-Servs %>% group_by(id) %>% summarize(phase,nurse=sum(nurs),gender=first(gender),phy=sum(physt),n=n(),sum_length=as.numeric(sum(patient_stay,na.rm=TRUE)))%>%
  mutate(value1=nurse/sum_length,value2=phy/sum_length)
library(ggplot2)
ggplot(servs_gender,aes(x=log(value2),fill=gender))+
  geom_histogram()+xlim(0, 8)

colnames(servs_gender)
entry<-substr(mb$entry_date,1,8)
leave<-substr(mb$leave_date,1,8)
leave<-as.Date(leave,"%Y%m%d")
entry<-as.Date(entry,"%Y%m%d")

length_stay<-difftime(leave,entry,units=c("days"))
mb$length_stay<-length_stay
