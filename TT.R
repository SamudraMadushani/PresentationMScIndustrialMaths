A<-DataDengueTEST %>%fabletools::aggregate_key(Province/Districts,Counts=sum(Counts))
DX<-A%>% features(Counts, feature_set(pkgs = "feasts"))
DX$Districts<-c("Kandy","Matale","NuwaraEliya","Central","Ampara","Batticaloa","Kalmune","Trincomalee",
                "Eastern","Anuradhapura","Polonnaruwa","NorthCentral","Kurunagala","Puttalam",    
                "NorthWestern","Jaffna","Kilinochchi","Mannar","Mullaitivu","Vavuniya","Northern",
                "Kegalle","Ratnapura","Sabaragamuwa","Galle","Hambanthota","Matara","Southern",
                "Badulla","Monaragala","Uva","Colombo","Gampaha","Kalutara","Western","Sri Lanka")

names(DX)[1] <- 'Level'
DX$Level<-c("District","District","District","Province","District","District","District","District",
            "Province","District","District","Province","District","District",    
            "Province","District","District","District","District","District","Province",
            "District","District","Province","District","District","District","Province",
            "District","District","Province","District","District","District","Province","Country")

pcsW <- DX %>%
  dplyr::select(-c(1,2,31,33,35,37)) %>%
  prcomp(scale = TRUE) %>%
  augment(DX)


FBVZW<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Level, label=Districts)) +
  geom_point(aes(size=trend_strength)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2")

FBVZW<-FBVZW+ theme(legend.position = "bottom",legend.box = "vertical")

# Change point analysis
##########################################################################################

par(mar=c(4,4,.3,.3)) 
set.seed(1)
# Change in mean example following EFK
x=1:500
z=c(rnorm(100,1,sd=0.5),rnorm(150,0,sd=0.5),rnorm(200,2,sd=0.5),rnorm(50,0.5,sd=0.5))
plot(x,z,type='l',xlab='',ylab='',col='green')
lines(x=1:100,y=rep(1,100),col='red',lwd=3)
lines(x=101:250,y=rep(0,150),col='red',lwd=3)
lines(x=251:450,y=rep(2,200),col='red',lwd=3)
lines(x=451:500,y=rep(0.5,50),col='red',lwd=3)
# Change in variance example following EFK
x=1:500
z=c(rnorm(100,0,sd=0.1),rnorm(150,0,sd=0.7),rnorm(200,0,sd=0.25),rnorm(50,0,sd=1))
plot(x,z,type='l',xlab='',ylab='')


# Spatial hierarchical forecasting workflow

DengueTEST<-tibble(Week,Date,Counts,Districts,Province)
DengueTRAIN<-DengueTEST%>%dplyr::filter(lubridate::year(Week)<2020)
DataDengueTRAIN<-DengueTRAIN%>% 
  as_tsibble(key=c(Province,Districts),index=Week)#<<

library(magrittr)
library(fabletools)
library(fable)
  # Spatial hierarchical forecasting workflow

HS_DataDengueTRAIN<-DataDengueTRAIN%>%
  fabletools::aggregate_key(Province/Districts,Counts=sum(Counts))#<<



HS_DataDengueTRAIN%>%
  model(arima =ARIMA(Counts),snaive=SNAIVE(Counts),naive=NAIVE(Counts),ets=ETS(Counts),avg = MEAN(Counts))%>% forecast(h =52)%>%print(n=5)


fit_TRAIN <- HS_DataDengueTRAIN%>%
  model(arima =ARIMA(Counts),snaive=SNAIVE(Counts),naive=NAIVE(Counts),ets=ETS(Counts),avg =     MEAN(Counts))%>%
  fabletools::reconcile(bu_arima = bottom_up(arima),#<<
                        top_arima=top_down(arima),#<<
                        ols_arima=min_trace(arima,method="ols"),#<<
                        MinT_arima=min_trace(arima,method="mint_shrink"),#<<
                        wls_var_arima = min_trace(arima,method="wls_var"),#<<
                        wls_struct_arima=min_trace(arima,method="wls_struct"),#<<
                        MinT_cov_arima=min_trace(arima,method="mint_cov"),#<<
                        bu_ets = bottom_up(ets),#<<
                        top_ets=top_down(ets),#<<
                        ols_ets=min_trace(ets,method="ols"),#<<
                        MinT_ets=min_trace(ets,method="mint_shrink"),#<<
                        wls_var_ets = min_trace(ets,method="wls_var"),#<<
                        wls_struct_ets=min_trace(ets,method="wls_struct"),#<<
                        MinT_cov_ets=min_trace(ets,method="mint_cov"))#<<

fc_TRAIN <- fit_TRAIN %>% forecast(h = 52)
fc_TRAIN

HS_DataDengueTEST<-DataDengueTEST%>%fabletools::aggregate_key(Province/Districts,Counts=sum(Counts))

ac_TRAIN<-accuracy(fc_TRAIN, HS_DataDengueTEST)
ac_TRAIN_MASE<-ac_TRAIN[,c(1,2,3,10)]
ac_TRAIN_MASE

# Compute all temporal aggregated time series

  D<-D%>%dplyr::filter(Week!=53)
  
  Dengue_Disease<-D[,c(5,6,7)]
  Dengue_Disease_tsbl<-Dengue_Disease%>%as_tsibble(key=Division,index=EndDate)
  Weekly_Dengue<-Dengue_Disease_tsbl%>%
    tsibble::index_by(WD=~yearweek(.))%>%
    dplyr::summarise(Dengue=sum(Dengue))
  Dengue_total_data<-c(Weekly_Dengue$Dengue)
 
  Dengue_total_data<- ts(Dengue_total_data, freq=52, start=c(2006,52))
  Dengue_train_data<- window(Dengue_total_data , start = c(2006,52), end = c(2019,51), frequency = 52)
  aggts_total = thief::tsaggregates(Dengue_total_data)#<<
  aggts_train = thief::tsaggregates(Dengue_train_data)#<<
  

  base <- list()
  for(i in seq_along(aggts_train))
    base[[i]] <- forecast(forecast::auto.arima(aggts_train[[i]]),
                          h=1*frequency(aggts_train[[i]]))


reconciled <- thief::reconcilethief(base,comb = c("bu"))
# comb = c("bu","ols","struc","shr")

  # Accuracy checking 
  
  #### Weekly series accuracy 
  

accuracy(reconciled[[1]],aggts_total[[1]])[,"MASE"]


# Visualization of model performance-Weekly series

```{r ,echo=FALSE}
library(readxl)
library(broom)
Weekly_20AD<- read_excel("C:/Users/USER/Desktop/Book1.xlsx")
Weekly_20AD<-Weekly_20AD[,-c(1)]

pcsWk20AD <- Weekly_20AD %>%
  dplyr::select(-c(1,2)) %>%
  prcomp(scale = TRUE) %>%
  augment(Weekly_20AD)

Method<-c("HS","AVG","HS","ETS","HS","SNAIVE",rep("AVG",3),rep("HS",3),"AVG",rep("HS",2),
          rep("AVG",3),rep("HS",2),rep("AVG",2),rep("HS",1),rep("AVG",2),rep("HS",2),
          rep("AVG",2),rep("HS",6),rep("AVG",1),rep("HS",5),"SNAIVE",rep("HS",2),"SNAIVE",rep("HS",3),rep("SNAIVE",3),rep("AVG",2),
          rep("HS",3),rep("AVG",1),rep("HS",3),rep("AVG",1),rep("HS",10),rep("AVG",1),"SNAIVE",
          rep("HS",2),"SNAIVE","HS","AVG",rep("HS",2),rep("AVG",2),rep("HS",5),"SNAIVE","AVG",
          "HS","NAIVE","HS","SNAIVE",rep("HS",8),"NAIVE",rep("HS",6),rep("HS",15),"SNAIVE",
          "AVG",rep("NAIVE",2),"HS","SNAIVE",rep("HS",3),"AVG",rep("HS",2),"ETS",rep("HS",8))

pcsWk20AD$Level<-Method

FBVZWW1MAD<-pcsWk20AD %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Level,label=Districts)) +
  geom_point(position=position_jitter(h=0.1, w=0.1),size=2) +
  theme(aspect.ratio = 1)+
  labs(x="PC1", y = "PC2")
FBVZWW1MAD<-FBVZWW1MAD+ theme(legend.position = "right",legend.box = "vertical")+
  labs(colour = NULL)+scale_color_manual(values = c("#e7298a","#e6ab02","#1b9e77","#762a83","#543005"))
FBVZWW1MAD








