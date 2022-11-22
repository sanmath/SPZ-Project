serv_cons_phase$id
Tab1<-mb %>% group_by(id)%>% summarize(lesion=first(Lesion),length_stay=first(length_stay))

Servs<-serv_cons_phase  %>% merge(Tab1,by="id")

unique(Servs$lesion)
Servs$les1<-substr(Servs$lesion,1,1)

new_servs<-Servs %>% group_by(les1) %>% summarize(nurse=sum(nurs),phy=sum(physt),n=n(),sum_length=sum(length_stay,na.rm=TRUE))

new_servs %>% mutate(les1=les1,value1=nurse/sum_length,value2=phy/sum_length)


colnames(Servs)
entry<-substr(mb$entry_date,1,8)
leave<-substr(mb$leave_date,1,8)
leave<-as.Date(leave,"%Y%m%d")
entry<-as.Date(entry,"%Y%m%d")

length_stay<-difftime(leave,entry,units=c("days"))
mb$length_stay<-length_stay
