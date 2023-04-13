tab_varnew$length_stay.x
colnames(tab_varnew)
data_regression<-tab_varnew|>select(length_stay.x,logo,ergo,physt,int_care,group1,ins_class,age,ins_type,gender2,
                   age_group,SCIM_TOTAL,stay_aft_leav,main_cost_center,asia1.x,etiology)
colnames(tab_varnew)
tab_varnew$SCIM_TOTAL<-as.numeric(tab_varnew$SCIM)


data_regression<-tab_varnew|>
  group_by(id,group1)|>
  summarize(mean_logo=mean(logo),
            mean_psyc=mean(psyc),
            mean_imag=mean(imag),
            mean_lab=mean(lab),
            mean_nurs=mean(nurs),
            mean_anaest=mean(anaest),
            mean_int_care=mean(int_care),
            mean_ergo=mean(ergo),
            mean_physt=mean(physt),
            mean_non_med=mean(non_med),
            mean_los=mean(length_stay.x),
            N=n(),
            sex=first(gender.x),
            etiology=first(etiology),
            age_group=first(age_group),
            injury_group=first(group1),
            stay_aft_leav=first(stay_aft_leav2),
            main_cost=first(main_cost_center),
            group_level=first(group_level),
            ins_class=first(ins_class),
            ins_type=first(ins_type),
            SCIM=mean(SCIM_TOTAL,na.rm=TRUE),
            age=first(age))


library(tidyr)
library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
data_regression

#Models for each service------------------------------------------------------------

modelo_physt<-lm(data=data_regression,
             mean_los~sex+age_group+group_level+
               group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)
summary(modelo_physt)
modelo_ergo<-lm(data=data_regression,
                 mean_los~age_group+ group1+ etiology+ stay_aft_leav+ SCIM)

modelo_logo<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+main_cost+stay_aft_leav+ins_class+SCIM)

modelo_psyc<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)

modelo_non_med<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)

modelo_nursing<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)

modelo_int_care<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)

modelo_anaest<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)


modelo_imag<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)

modelo_lab<-lm(data=data_regression,
 mean_los~sex+age_group+group_level+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM)

data_regression$main_cost<-as.factor(data_regression$main_cost)
###modelling

data_regression$main_cost<-as.factor(data_regression$main_cost)

data_regression<-data_regression|>filter(group1!="ND")
modelnb<-MASS::glm.nb(mean_los~sex+age_group+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM,data=data_regression)
summary(modelnb)

m1<-vglm(mean_los~sex+age_group+group1+etiology+main_cost+stay_aft_leav+ins_class+SCIM,data=data_regression,family=posnegbinomial())
summary(m1)
output <- data.frame(resid = resid(m1)[, 1], fitted = fitted(m1))
ggplot(output, aes(fitted, resid)) + geom_jitter(position = position_jitter(width = 0.25), 
                                                 alpha = 0.5) + stat_smooth(method = "loess")

ggplot(output, aes(fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25), alpha=.5) +
  stat_quantile(method="rq")

output <- within(output, {
  broken <- cut(fitted, hist(fitted, plot=FALSE)$breaks)
})

ggplot(output, aes(broken, resid)) +
  geom_boxplot() +
  geom_jitter(alpha=.25)

library(betareg)
fitdistrplus::fitdistr(data_regression$mean_logo,distr = "dnormal")
