serv_cons_phase %>% select(phase) %>% table()
colnames(serv_cons_phase)
ifss<- serv_cons_phase |> filter(PID=="0152021") %>% select(c("id"))
serv_cons_phase %>% select(PID) %>% table() %>% data.frame() %>% arrange(-Freq)


SCIM_17_20 |> filter(FID %in% ifss$id) %>% select(FID) %>% unique()
SCIM_17_20 |> filter(FID %in% ifss$id) %>% select(FID,SCIM_TOTAL,FALL_NR,DATUM) %>% select(FID)
##

serv_cons %>% merge(fid_pid,by.x="id",by.y="FID")

serv_cons %>% select(id) %>% table() %>% data.frame() %>% arrange(Freq) %>% head()

serv_cons_phase %>% select(PID) %>% table() %>% data.frame() %>% arrange(-Freq) %>% head()
gh<-serv_cons_phase %>% filter(PID=="0000374") %>% select(id)
gh$id
SCIM_17_20 %>% filter(FID%in%gh$id) %>% select(FID,SCIM_TOTAL,DATUM,FALL_NR)

serv_cons_phase
library(dplyr)
head(data.frame(table(time_cost$id))|> arrange(-Freq),n=410)
head(data.frame(table(serv_cons$id))|> arrange(-Freq),n=10)
