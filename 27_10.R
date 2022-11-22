library(ggplot2)
library(dplyr)
time_cost %>% select(id) %>% table() %>% data.frame() %>% arrange(-Freq)
head(time_cost)
fid_pid

time_cost %>% filter(id=="6025255")
time_cost %>%merge(fid_pid,by.x="id",by.y="FID") %>% arrange(PID) %>% 
  merge(mb4%>%select(id,phase), by.x="id",by.y="id")%>% arrange(PID,year)

codes2<-rbind(data.frame(id=codes$code1,idf=codes$uniq),data.frame(id=codes$code2,idf=codes$uniq))

AAA<-time_cost %>%merge(fid_pid,by.x="id",by.y="FID") %>% arrange(PID) %>% merge(codes2,by.x="id",by.y="id")%>% 
  merge(mb4%>%select(id,phase), by.x="id",by.y="id")%>% merge(cpth,by.x="id",by.y="V1") %>%  arrange(PID)

# Aggregation by code uniq take care of duplicates by merge by id and add all the data single cases.



AAA %>%ggplot(aes(x=clipath_code,y=non_med_ter ))+geom_boxplot()
colnames(AAA)
###########33

table(time_cost$year)
