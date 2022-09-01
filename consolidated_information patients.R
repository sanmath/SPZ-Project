
#diagnoses
v30<-paste("V",seq(30,39,1),sep="")
#treatments
v40<-paste("V",seq(40,50,1),sep="")

#I select interest variables and create year to know from which table comes every row.
mb17<-MB17a%>%select(V11,V12,V13,V14,V22,V25,v30,v40)%>%mutate(id=MD17a$V663,year=2017)
mb18<-MB18a%>%select(V11,V12,V13,V14,V22,V25,v30,v40)%>%mutate(id=MD18a$V663,year=2018)
mb19<-MB19a%>%select(V11,V12,V13,V14,V22,V25,v30,v40)%>%mutate(id=MD19a$V663,year=2019)
mb20<-MB20a%>%select(V11,V12,V13,V14,V22,V25,v30,v40)%>%mutate(id=MD20a$V663,year=2020)

mb<-bind_rows(mb17,mb18,mb19,mb20)


library(icd.data)
ICD<-icd10cm2016 %>% select(code,three_digit,short_desc,chapter)

mb%>%merge(ICD,by.x="V30",by.y="code",all.x=TRUE)

colnames(mb)
data.frame(mb%>%select(id)%>%table())%>%select(Freq)%>%table)()
