#We include more costs
setwd("C:/Users/boris/Dropbox/PC/Desktop/data hospital")
stuchrechnung <- read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                            skip = 21)


Xcols <-  read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                     sheet = "2017", range = "A22:U23")

colnames(Xcols)
X2017 <- read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                    skip = 22,sheet="2017")


X2018 <- read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                    skip = 22,sheet="2018")

X2019 <- read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                    skip = 22,sheet="2019")

X2020 <- read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                    skip = 22,sheet="2020")

X2021 <- read_excel("20211112 Leistungen pro Fall für MEST_Snapshot1.xlsx", 
                    sheet = "T2_2021", skip = 21)

colnames(X2017)<-colnames(Xcols)

colnames(X2018)<-colnames(Xcols)
colnames(X2019)<-colnames(Xcols)
colnames(X2020)<-colnames(Xcols)
colnames(X2021)<-colnames(Xcols)
colnames(stuchrechnung)<-colnames(Xcols)

vbs<-c("Liste geladen (1502 Zeilen)","20 OP-Saal",
       "23 Anästhesie",
       "24 Intensivpflege (IPS)",
       "26 Bildgebende Verfahren",
       "29 Labor",
       "32 Physiotherapie",
       "33 Ergotherapie",
       "34 Logopädie",
       "35 Nichtärztliche Therapien und Beratungen",
       "39 Pflege",
       "40 Psychologie")




X2017<-X2017 %>% select(vbs)%>% mutate(year=2017) 
X2018<-X2018 %>% select(vbs)%>% mutate(year=2018)
X2019<-X2019 %>% select(vbs)%>% mutate(year=2019)
X2020<-X2020 %>% select(vbs)%>% mutate(year=2020)
X2021<-X2021 %>% select(vbs)%>% mutate(year=2021)

vbs1<-c("id","OP-Saal",
       "Anästhesie",
       "Intensivpflege (IPS)",
       "Bildgebende_Verfahren",
       "Labor",
       "Physiotherapie",
       "Ergotherapie",
       "Logopädie",
       "Nichtärztliche Therapien und Beratungen",
       "Pflege",
       "Psychologie")

X2021$`20 OP-Saal`<-as.numeric(X2021$`20 OP-Saal`)

Xjoined<-bind_rows(X2017,X2018,X2019,X2020)
colnames(Xjoined)<-vbs1
saveRDS(Xjoined,"Xjoined.rds")

writexl::write_xlsx(Xjoined,"cost_time.xlsx")
str(Xjoined)

writexl::write_xlsx(treat_time,"cost_time.xlsx")

treat_time<-readRDS("Xjoined.RDS")
saveRDS(treat_time,"treat_time.rds")
time_cost<-readRDS("treat_time.rds")
names_costs<-c("id","op_room","anaest","int_care","imag",
  "lab","physt","ergo","logo","non_med_ter","nurs","psyc","year")

unts<-c("min","min","min","chf","chf","min","min","min","tp","min","tp")
time_cost
colnames(time_cost)<-names_costs
saveRDS(time_cost,"time_cost.rds")
View(time_cost)
colnams<-treat_time%>% colnames()
colnames(treat_time)<-c(colnams[1:12],"year")


table(paste(treat_time$id,treat_time$year,sep="_"))%>% data.frame()%>% arrange(-Freq)
              dtf%>%nrow()         
colnames(dtf)              
mb_def2%>%select(PID) %>% unique()
mhg %>% filter(PID=="0089695")%>% View()

