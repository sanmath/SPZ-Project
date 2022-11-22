## Consolidation data hospital

serv_cons$id

A<-serv_cons%>% merge(codes%>% select(code1,uniq),by.x="id",by.="code1") %>%
  merge(fid_pid,by.x="id",by.y="FID") %>% mutate(phase="acu")

A1<-serv_cons%>% merge(codes%>% select(code2,uniq),by.x="id",by.="code2") %>%
  merge(fid_pid,by.x="id",by.y="FID") %>% mutate(phase="rehab")

PA<-rbind(A,A1)
colnames(PA)
PA %>% select(PID) %>% table() %>% data.frame() %>% head() %>% arrange(-Freq)
 PA  %>% filter(PID=="0000374")

fid_pid
head(A)

B<-A%>% merge(codes%>% select(code2,uniq),by.x="id",by.y="code2")



table(A$PID) %>% data.frame() %>% arrange(-Freq)
C<-rbind(A,B)
colnames(C)
C %>% group_by(uniq) %>% summarize(op_room=sum(op_room))
colnames(fid_pid)


table(SCIM_17_20$FID) %>% head()

