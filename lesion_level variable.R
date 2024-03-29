
library(readxl)

mb$les_lev
mb221222<-mb250123
mbnames<-read_excel("mbnames.xlsx")
colnames(mb2212)<-mbnames$description
mb2212
length(colnames(mb221222))
length(mbnames$description)

mb2212
##This code helps to include lesion level in the MB dataset
colnames(mb)
cols1<-data.frame(col1=colnames(mb))
#Diagnosis main and secondary
mbnames<-data.frame(mbnames)
col_names<-cols1|>merge(mbnames,by.x="col1",by.y="col",all.x = TRUE,sort = FALSE)|>select(description)
colnames(mb)<-col_names$description
colnames(mb)<-cols1$col1

colnames(mb)


test<-mb%>%select("main_diag",grep(colnames(mb),pattern="add_diag"))
colnames(mb)
#We create a matrix with TRUE and FALSE to check which diagnoses from the levels table is 
matriz<-apply(test,MARGIN=2,FUN=function(x)grepl(
  "G8260|G8261|G8262|G8263|G8264|G8265|G8266|G8267|G8269|S1470|S1471|S1472|S1473|S1474|S1475|S1476|S1477|S1478|S2470|S2471|S2472|S2473|S2474|S2475|S2476|S2477|S347|S3471|S3472|S3473|S3474|S3475|S3476|S3477",x))

#We sum each row to have an indicator of the resulting number of diagnoses related to the lesion level 
ind_col<-rowSums(matriz)
#new matriz has the diagnosis and the last column with the number of diagnoses that had a lesion level
new_matriz<-cbind(test,ind_col)


new_matriz%>%filter(ind_col==1)

# patients that have two lesion levels special case
new_matriz %>% filter(ind_col==0)
test
exmp<-mapply(1:5,FUN=function(x)test[x,][matriz[x,]][1])
exmp

lapply(1:10,FUN=function(x)exmp[[x]][1])
exmp[[20]]



set1<-which(new_matriz$ind_col==0)
set2<-which(new_matriz$ind_col>=1)

v1<-vector(mode = "character",length=nrow(mb))



for(i in set1){
  v1[i]<-"no_lesion_level"
}

for(i in set2){
  v1[i]<-test[i,][matriz[i,]][1]
}

les_lev<-(unlist(v1))


mb$les_lev<-les_lev

mb

table(mb$les_lev)

lesion_level<-readxl::read_excel("lesion_level.xlsx")
mb<-merge(mb,lesion_level,by.x="les_lev",by.y="ICD")
colnames(mb)
table(mb$Lesion.x==mb$Lesion.y)
mb|>select(Lesion.x)
library(ggplot2)
library(forcats)

ggplot(mb,aes(x=fct_infreq(Lesion.x)))+geom_bar()+theme(axis.text.x = element_text(angle = 90))
mb$les<-substr(mb$Lesion,1,1)
saveRDS(mb,"mb.rds")
## Paraplegia and tetraplegia it is possible to identify by level of lesion
# It could be any relation between diagnoses and level of lesion
# It is possible to get a lower amount of groups by grouping similiar lesion levels



colnames(mb)
table(mb$les_lev)
