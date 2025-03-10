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
dat$lprofit<-log(dat$profit)
dat$LREVENUE<-log(dat$REVENUE)
dat$LEXPENDITURE<-log(dat$EXPENDITURE)
dat$one<-dat$SINGLE_IMSE/dat$TOTAL_IMSE*100
dat$lone<-log(dat$one)
dat$LUNPAID_WORKER<-log(dat$UNPAID_WORKER)
dat$LTOTAL_IMSE<-log(dat$TOTAL_IMSE)
dat$rat<-dat$SINGLE_IMSE/dat$TOTAL_IMSE
dat$lrat<-log(dat$rat)
dat$LSINGLE_IMSE<-log(dat$SINGLE_IMSE)
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

#### profit

var<-'WAGES'
lvar<-'LWAGES'

tot1<-feols(formula(paste(var,'~IDSD')),d233)
tot2<-feols(formula(paste(lvar,'~LIDSD')),d233)
tot3<-fepois(formula(paste(var,'~LIDSD')),d233)

tot<-list(
  "OLS"=tot1,
  "Log OLS"=tot2,
  "POLS"=tot3
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/total.html")


tot1<-feols(formula(paste(var,'~IDSD_INST')),d233)
tot2<-feols(formula(paste(lvar,'~LIDSD_INST')),d233)
tot3<-fepois(formula(paste(var,'~LIDSD_INST')),d233)

tot<-list(
  "OLS"=tot1,
  "Log OLS"=tot2,
  "POLS"=tot3
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/inst.html")

tot1<-feols(formula(paste(var,'~IDSD_BUSINESS')),d233)
tot2<-feols(formula(paste(lvar,'~LIDSD_BUSINESS')),d233)
tot3<-fepois(formula(paste(var,'~LIDSD_BUSINESS')),d233)

tot<-list(
  "OLS"=tot1,
  "Log OLS"=tot2,
  "POLS"=tot3
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/bis.html")

tot1<-feols(formula(paste(var,'~IDSD_LABOUR')),d233)
tot2<-feols(formula(paste(lvar,'~LIDSD_LABOUR')),d233)
tot3<-fepois(formula(paste(var,'~LIDSD_LABOUR')),d233)

tot<-list(
  "OLS"=tot1,
  "Log OLS"=tot2,
  "POLS"=tot3
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/labor.html")

tot1<-feols(formula(paste(var,'~IDSD_FINANCIAL')),d233)
tot2<-feols(formula(paste(lvar,'~LIDSD_FINANCIAL')),d233)
tot3<-fepois(formula(paste(var,'~LIDSD_FINANCIAL')),d233)

tot<-list(
  "OLS"=tot1,
  "Log OLS"=tot2,
  "POLS"=tot3
)

modelsummary(tot,stars=T,gof_omit = 'FE|IC|RMSE|Std.|Adj.',output="reg/fin.html")