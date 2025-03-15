setwd("C:/github/ufm") ## This is my WD in my local github desktop

library(tidyverse) ## Packages
library(readxl)
library(modelsummary)
library(fixest)
library(patchwork)


dat<-read_excel("data.xlsx",sheet="gabung")|>
  mutate(missing = if_else(if_any(everything(), is.na),1, 0))

dat|>filter(missing==1)|>print(n=300)

summary(dat$missing)

## generate some stuff

dat$profit<-dat$REVENUE-dat$EXPENDITURE
dat$hrpro<-dat$profit/2080
dat$lprofit<-log(dat$profit)
dat$hrpro<-log(dat$hrpro)
dat$LREVENUE<-log(dat$REVENUE)
dat$LEXPENDITURE<-log(dat$EXPENDITURE)
dat$one<-dat$SINGLE_IMSE/dat$TOTAL_IMSE*100
dat$lone<-log(dat$one)
dat$LUNPAID_WORKER<-log(dat$UNPAID_WORKER)
dat$LTOTAL_IMSE<-log(dat$TOTAL_IMSE)
dat$rat<-dat$SINGLE_IMSE/dat$TOTAL_IMSE
dat$lrat<-log(dat$rat)
dat$LSINGLE_IMSE<-log(dat$SINGLE_IMSE)
dat$LINFORMAL<-log(dat$INFORMAL)
dat$LWAGES<-log(dat$WAGES)
dat$LIDSD_INST	<-log(dat$	IDSD_INST		)
dat$LIDSD_INFRA	<-log(dat$	IDSD_INFRA		)
dat$LIDSD_ICT	<-log(dat$	IDSD_ICT		)
dat$LIDSD_MACRO	<-log(dat$	IDSD_MACRO		)
dat$LIDSD_HEALTH	<-log(dat$	IDSD_HEALTH		)
dat$LIDSD_SKILLS	<-log(dat$	IDSD_SKILLS		)
dat$LIDSD_PRODUCT	<-log(dat$	IDSD_PRODUCT		)
dat$LIDSD_LABOUR	<-log(dat$	IDSD_LABOUR		)
dat$LIDSD_FINANCIAL	<-log(dat$	IDSD_FINANCIAL		)
dat$LIDSD_MARKET	<-log(dat$	IDSD_MARKET		)
dat$LIDSD_BUSINESS	<-log(dat$	IDSD_BUSINESS		)
dat$LIDSD_INNOVATION	<-log(dat$	IDSD_INNOVATION		)
dat$LIDSD<-log(dat$IDSD)

dat<-dat|>mutate(y23=if_else(Year==2023,1,0))
dat<-dat|>mutate(j=case_when(Province=="DKI Jakarta"~1,
                               Province=="Jawa Barat"~1,
                               Province=="Jawa Tengah"~1,
                               Province=="Jawa Timur"~1,
                             .default=0))


## Summary stats of IDSD

idsd<-dat|>select(IDSD_INST,IDSD_INFRA,
                                        IDSD_ICT,
                                        IDSD_MACRO,
                                        IDSD_HEALTH,
                                        IDSD_SKILLS,
                                        IDSD_PRODUCT,
                                        IDSD_LABOUR,
                                        IDSD_FINANCIAL,
                                        IDSD_MARKET,
                                        IDSD_BUSINESS,
                                        IDSD_INNOVATION
)

idsd22<-dat|>filter(Year==2022)|>select(IDSD_INST,
                  IDSD_INFRA,
                  IDSD_ICT,
                  IDSD_MACRO,
                  IDSD_HEALTH,
                  IDSD_SKILLS,
                  IDSD_PRODUCT,
                  IDSD_LABOUR,
                  IDSD_FINANCIAL,
                  IDSD_MARKET,
                  IDSD_BUSINESS,
                  IDSD_INNOVATION
)

idsd23<-dat|>filter(Year==2023)|>select(IDSD_INST,
                                        IDSD_INFRA,
                                        IDSD_ICT,
                                        IDSD_MACRO,
                                        IDSD_HEALTH,
                                        IDSD_SKILLS,
                                        IDSD_PRODUCT,
                                        IDSD_LABOUR,
                                        IDSD_FINANCIAL,
                                        IDSD_MARKET,
                                        IDSD_BUSINESS,
                                        IDSD_INNOVATION
)

d233<-dat|>filter(Year==2023)

datasummary_correlation(idsd,output = 'tab/corr.html')
datasummary_correlation(idsd22,output = 'tab/corr22.html')
datasummary_correlation(idsd23,output = 'tab/corr23.html')

datasummary(IDSD_INST+IDSD_INFRA+IDSD_ICT+IDSD_MACRO+IDSD_HEALTH+IDSD_SKILLS+IDSD_PRODUCT+
              IDSD_LABOUR+IDSD_FINANCIAL+IDSD_MARKET+IDSD_BUSINESS+IDSD_INNOVATION~
              factor(Year)*(Mean+SD+N+Histogram),
            data=dat,output='tab/idsdsum.docx')

## Aggregate by province year

datp<-dat|>group_by(Province,Year)|>
  summarise_if(is.numeric,mean,na.rm=TRUE)

## summary stats per province

datasummary(IDSD_INST+IDSD_INFRA+IDSD_ICT+IDSD_MACRO+IDSD_HEALTH+IDSD_SKILLS+IDSD_PRODUCT+
              IDSD_LABOUR+IDSD_FINANCIAL+IDSD_MARKET+IDSD_BUSINESS+IDSD_INNOVATION~
              factor(Year)*(Mean+SD+N+Histogram),
            data=datp,output='tab/idsdsump.docx')

macro<-read_excel("macro.xlsx",sheet="gp")
datp<-datp|>full_join(macro,by=c("Province","Year"))
datp$gp<-datp$GRP/datp$pop

## graph

plt1<-datp|>ggplot(aes(x=GRP,y=IDSD,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  labs(x="Gross Regional Product (Billion USD)",y="IDSD")+
  theme_classic()
#ggsave("fig/fig1.png")

plt2<-datp|>ggplot(aes(x=pop,y=IDSD,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  labs(x="Population (1,000)",y="IDSD")+
  theme_classic()
#ggsave("fig/fig2.png")

plt3<-datp|>ggplot(aes(x=gp,y=IDSD,color=factor(Year)))+
  geom_point(size=1.2)+
  labs(x="Gross Regional Product per capita (Million USD)",y="IDSD")+
  theme_classic()
#ggsave("fig/fig3.png")
plt4<-datp|>ggplot(aes(x=budget,y=IDSD,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  labs(x="Planned provincial budget (1,000 IDR)",y="IDSD")+
  theme_classic()

plt<-plt1+plt2+plt3+plt4+plot_layout(guides = 'collect',axis_titles = 'collect')+
  plot_annotation(title="Figure 1: IDSD and Macro Variables",
                  caption="Source: Author's calculation based on BPS data")
ggsave("fig/idsd.png",plt,width=9,height=6)

## Regression
### Kab/Kota level
#### TOTAL_IMSE variable of increased no. of UMKM, reflects "success"
#### INFORMAL 
#### WAGES
#### profit

##### TOTAL IMSE IDSD
#invar<-'~LIDSD_INST+LIDSD_INFRA+LIDSD_ICT+LIDSD_MACRO+LIDSD_HEALTH+LIDSD_SKILLS+LIDSD_PRODUCT+LIDSD_LABOUR+LIDSD_FINANCIAL+LIDSD_MARKET+LIDSD_BUSINESS+LIDSD_INNOVATION'
#invar<-'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+LIDSD_MARKET'
invar<-'~LIDSD_INST+LIDSD_INFRA+LIDSD_ICT+LIDSD_SKILLS+LIDSD_LABOUR+LIDSD_MARKET+LIDSD_BUSINESS+LIDSD_INNOVATION'
var<-'TOTAL_IMSE'
lvar<-'LTOTAL_IMSE'

tot1<-feols(formula(paste(lvar,'~LIDSD')),dat)
tot2<-feols(formula(paste(lvar,'~LIDSD|Year+Province')),dat)
tot3<-fepois(formula(paste(var,'~LIDSD')),dat)
tot4<-fepois(formula(paste(var,'~LIDSD|Year+Province')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/totalidsd.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/totalidsd.xlsx")

##### TOTAL IMSE IDSD id

tot1<-feols(formula(paste(lvar,invar)),dat)
tot2<-feols(formula(paste(lvar,invar,'|Province+Year')),dat)
tot3<-fepois(formula(paste(var,invar)),dat)
tot4<-fepois(formula(paste(var,invar,'|Province+Year')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/total.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/total.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/total.docx")

##### INFORMAL

dat$info<-dat$INFORMAL
dat$linfo<-log(dat$info)
var<-'info'
lvar<-'linfo'

tot1<-feols(formula(paste(lvar,'~LIDSD')),dat)
tot2<-feols(formula(paste(lvar,'~LIDSD|Province+Year')),dat)
tot3<-fepois(formula(paste(var,'~LIDSD')),dat)
tot4<-fepois(formula(paste(var,'~LIDSD|Year+Province')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/informalidsd.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/informalidsd.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/informalidsd.docx")

##### TOTAL IMSE informal id

tot1<-feols(formula(paste(lvar,invar)),dat)
tot2<-feols(formula(paste(lvar,invar,'|Province+Year')),dat)
tot3<-fepois(formula(paste(var,invar)),dat)
tot4<-fepois(formula(paste(var,invar,'|Year+Province')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/informal.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/informal.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/informal.docx")

##### wages

var<-'WAGES'
lvar<-'LWAGES'

tot1<-feols(formula(paste(lvar,'~LIDSD')),dat)
tot2<-feols(formula(paste(lvar,'~LIDSD|Year+Province')),dat)
tot3<-fepois(formula(paste(var,'~LIDSD')),dat)
tot4<-fepois(formula(paste(var,'~LIDSD|Year+Province')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/wagesidsd.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/wagesidsd.xlsx")

##### TOTAL IMSE wages id

tot1<-feols(formula(paste(lvar,invar)),dat)
tot2<-feols(formula(paste(lvar,invar,'|Year+Province')),dat)
tot3<-fepois(formula(paste(var,invar)),dat)
tot4<-fepois(formula(paste(var,invar,'|Province+Year')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/wages.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/wages.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/wages.docx")

##### PROFIT

var<-'profit'
lvar<-'lprofit'

tot1<-feols(formula(paste(lvar,'~LIDSD')),dat)
tot2<-feols(formula(paste(lvar,'~LIDSD|Year+Province')),dat)
tot3<-fepois(formula(paste(var,'~LIDSD')),dat)
tot4<-fepois(formula(paste(var,'~LIDSD|Year+Province')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/profitidsd.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/profitidsd.xlsx")

##### TOTAL IMSE profit id

dat$informal<-dat$INFORMAL/dat$TOTAL_IMSE
dat$linformal<-log(dat$informal)
var<-'profit'
lvar<-'lprofit'

tot1<-feols(formula(paste(lvar,invar)),dat)
tot2<-feols(formula(paste(lvar,invar,'|Year+Province')),dat)
tot3<-fepois(formula(paste(var,invar)),dat)
tot4<-fepois(formula(paste(var,invar,'|Year+Province')),dat)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/profit.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/profit.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/profit.docx")

## Province level
dat$hrpro<-dat$profit/2080
datpp<-dat|>group_by(Province,Year)|>
  summarise_if(is.numeric,sum,na.rm=TRUE)
datp<-dat|>group_by(Province,Year)|>
  summarise_if(is.numeric,mean,na.rm=TRUE)
macro<-read_excel("macro.xlsx",sheet="gp")
macro$hrpay<-as.numeric(macro$hrpay)
datp$tot<-datpp$TOTAL_IMSE
datp$info<-datpp$INFORMAL
datp$sinf<-datp$info/datp$tot


datp<-datp|>full_join(macro,by=c("Province","Year"))

datp<-datp|>mutate(case_when(Province=="DKI Jakarta"~1,
                               Province=="Jawa Barat"~1,
                               Province=="Jawa Tengah"~1,
                               Province=="Jawa Timur"~1,
                               Province=="Banten"~1,
                               .default=0))


datp$gp<-datp$GRP/datp$pop
datp$lpop<-log(datp$pop)
datp$lGRP<-log(datp$GRP)
datp$lgp<-log(datp$gp)
datp$lhrpay<-log(datp$hrpay)
datp$ltot<-log(datp$tot)
datp$lbudget<-log(datp$budget)
datp$lsinf<-log(datp$sinf)
datp$linfo<-log(datp$linfo)


datp<-datp|>select(Year,Province,lpop,lGRP,lgp,lhrpay,lbudget,ltot,tot,j,info,
                   IDSD,IDSD_INST,IDSD_LABOUR,IDSD_BUSINESS,pop,GRP,sinf,lsinf,
                   LIDSD,LIDSD_INST,LIDSD_LABOUR,LIDSD_BUSINESS,hrpay,linfo,
                   profit,hrpro,INFORMAL,worker,umkm)

datp$lprofit<-log(datp$profit)
datp$lhrpro<-log(datp$hrpro)
datp$share<-log(datp$hrpro)
datp<-datp|>filter(tot>0)


datp$gap<-datp$worker-datp$umkm
datp$lgap<-log(datp$gap)

datasummary(IDSD_INST+IDSD_LABOUR+IDSD_BUSINESS+gap+tot+GRP+pop+
              profit+hrpay+INFORMAL+worker+umkm~
              factor(Year)*(Mean+SD+N+Histogram),
            data=datp,output='tab/province.docx')


## Plot

datp|>ggplot(aes(x=lprofit,y=lhrpay,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="log MSME profit",y="log hourly pay")+
  theme_classic()
ggsave("fig/prowage.png",width=9,height=6)

datp|>ggplot(aes(x=lhrpro,y=lhrpay,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="log MSME profit",y="log hourly pay")+
  theme_classic()
ggsave("fig/lhrprowage.png",width=9,height=6)

datp|>ggplot(aes(x=hrpro,y=hrpay,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="log MSME profit",y="log hourly pay")+
  theme_classic()
ggsave("fig/hrprowage.png",width=9,height=6)

datp|>ggplot(aes(x=umkm,y=worker,color=factor(Year)))+
  geom_point(size=1.2)+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  labs(x="self-employed return (1000 IDR/month)",y="employee wage (1000 IDR/month)")+
  theme_classic()
ggsave("fig/worker.png",width=9,height=6)

##### tot


var<-'tot'
lvar<-'ltot'

tot1<-feols(formula(paste(lvar,'~LIDSD+lGRP+lpop+lgp+lbudget+lhrpay')),datp)
tot2<-feols(formula(paste(lvar,'~LIDSD+lGRP+lpop+lgp+lbudget+lhrpay|Year+j')),datp)
tot3<-fepois(formula(paste(var,'~LIDSD+lGRP+lpop+lgp+lbudget+lhrpay')),datp)
tot4<-fepois(formula(paste(var,'~LIDSD+lGRP+lpop+lgp+lbudget+lhrpay|Year+j')),datp)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/totidsd.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/totidsd.xlsx")

##### TOTAL IMSE informal id

#var<-'sinf'
#lvar<-'lsinf'
var<-'info'
lvar<-'linfo'

tot1<-feols(formula(paste(lvar,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+lhrpay')),datp)
tot2<-feols(formula(paste(lvar,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+lhrpay|Year+j')),datp)
tot3<-fepois(formula(paste(var,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+lhrpay')),datp)
tot4<-fepois(formula(paste(var,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+lhrpay|Year+j')),datp)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/tot.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/tot.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/tot.docx")

##### hrpay

var<-'hrpay'
lvar<-'lhrpay'

tot1<-feols(formula(paste(lvar,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot2<-feols(formula(paste(lvar,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)
tot3<-fepois(formula(paste(var,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot4<-fepois(formula(paste(var,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/hridsd.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/hridsd.xlsx")

##### TOTAL IMSE informal id

#var<-'sinf'
#lvar<-'lsinf'
var<-'info'
lvar<-'linfo'

tot1<-feols(formula(paste(lvar,' ~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot2<-feols(formula(paste(lvar,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)
tot3<-fepois(formula(paste(var,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot4<-fepois(formula(paste(var,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/inf.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/inf.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/inf.docx")

##### gap

var<-'gap'
lvar<-'lgap'

tot1<-feols(formula(paste(lvar,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot2<-feols(formula(paste(lvar,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)
tot3<-fepois(formula(paste(var,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot4<-fepois(formula(paste(var,'~LIDSD+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/gapt.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/gapt.xlsx")

##### gapppppp

tot1<-feols(formula(paste(lvar,' ~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot2<-feols(formula(paste(lvar,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)
tot3<-fepois(formula(paste(var,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot')),datp)
tot4<-fepois(formula(paste(var,'~LIDSD_INST+LIDSD_LABOUR+LIDSD_BUSINESS+lGRP+lpop+lgp+lbudget+ltot|Year+j')),datp)

tot<-list(
  "OLS"=tot1,
  "FE OLS"=tot2,
  "POLS"=tot3,
  "FE POLS"=tot4
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/gap.html")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/gap.xlsx")
modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/gap.docx")
