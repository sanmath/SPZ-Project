
##This code helps to include lesion level in the MB dataset


#Diagnosis main and secondary
test<-mb%>%select("main_diag",grep(colnames(mb),pattern="add_diag"))
dim(test)
#We create a matrix with TRUE and FALSE to check which diagnoses from the levels table is 
matriz<-apply(test,MARGIN=2,FUN=function(x)grepl(
  "G8260|G8261|G8262|G8263|G8264|G8265|G8266|G8267|G8269|S1470|S1471|S1472|S1473|S1474|S1475|S1476|S1477|S1478|S2470|S2471|S2472|S2473|S2474|S2475|S2476|S2477|S347|S3471|S3472|S3473|S3474|S3475|S3476|S3477",x))

#We sum each row to have an indicator of the resulting number of diagnoses related to the lesion level 
ind_col<-rowSums(matriz)
#new matriz has the diagnosis and the last column with the number of diagnoses that had a lesion level
new_matriz<-cbind(test,ind_col)

new_matriz%>%filter(ind_col==2)

# patients that have two lesion levels special case
new_matriz %>% filter(ind_col==0)

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

6537+29

table(mb$les_lev)

lesion_level<-readxl::read_excel("lesion_level.xlsx")
mb<-merge(mb,lesion_level,by.x="les_lev",by.y="ICD")
mb_final$Clinical.Pathway
colnames(mb_final)
mb_final$Lesion1<-substr(x = mb_final$Lesion,1,1)
library(ggplot2)
library(forcats)
ggplot(mb_final,aes(x=fct_infreq(Lesion1),fill=Lesion1))+geom_bar()+scale_fill_manual(values=colores)
       