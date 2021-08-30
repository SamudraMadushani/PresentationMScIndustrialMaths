library(tsfeatures)

load("H:/ProgressPresentation/ProgressPresentation/data/FinalData.Rda")
D<-Data_Counts
Week<-tsibble::yearweek(D$EndDate)
Districts<-D$Division
Counts<-as.integer(D$Dengue)
HM_data_districts<-tibble(Week,Districts,Counts)
HM_data_districts<-HM_data_districts%>% as_tsibble(key = Districts,index=Week)

HM2<-ggplot(HM_data_districts, aes(x=Week, y=Districts)) +
  geom_tile(aes(fill = Counts)) +
  scale_fill_viridis_c(option = "D", direction = -1,breaks = c(500,1500, 2500)) +
  labs(title = " ",y = "Districts") +
  theme_light()+ theme(legend.position = 'right')


##########################################################################

library(thief)
DA<-Data_Counts
DA<-DA%>%dplyr::filter(Week!=53)

Dengue_DiseaseA<-DA[,c(5,6,7)]
Dengue_Disease_tsblA<-Dengue_DiseaseA%>%as_tsibble(key=Division,index=EndDate)
Weekly_DengueA<-Dengue_Disease_tsblA%>%
  index_by(WD=~yearweek(.))%>%
  dplyr::summarise(Dengue=sum(Dengue))
Dengue_total_dataA<-c(Weekly_DengueA$Dengue)

Dengue_total_dataA<- ts(Dengue_total_dataA, freq=52, start=c(2006,52))

#############################################################################
# Construct all temporal aggregates
#############################################################################

#Aggregated time series (total and training session-FIRST SET)
aggts_total = thief::tsaggregates(Dengue_total_dataA)
W<-autoplot(aggts_total[[1]],col="#e7298a") + labs(y = " ",x=" ",title="Weekly")
BW<-autoplot(aggts_total[[2]],col="#1b9e77") + labs(y = " ",x=" ",title="Bi-weekly")
M<-autoplot(aggts_total[[3]],col="#d95f02") + labs(y = " ",x=" ",title="Monthly")
Q<-autoplot(aggts_total[[4]],col="#7570b3") + labs(y = " ",x=" ",title="Quarterly")
Y<-autoplot(aggts_total[[5]],col="#66a61e") + labs(y = "Counts",x="Week",title="Yearly")
#TEM<-(W/BW/M/Q/Y)
TEM<-(W|BW|M)/(Q|Y)
library(broom)
library(ggrepel)

Temporal_DengueFeatures<-tsfeatures(aggts_total)

Temporal_DengueFeatures[,1]<-c("Weekly","Bi-weekly","Monthly","Quarterly","Semi-annual","Annual")
names(Temporal_DengueFeatures)[1]<-'Series'

hwl_pca <- Temporal_DengueFeatures[,-c(1,10,11,12,20)] %>%
  na.omit() %>%
  prcomp(scale=TRUE)%>%
  augment(Temporal_DengueFeatures)

TEMFBV<-hwl_pca %>%
  ggplot(aes(x=.fittedPC1, y=.fittedPC2)) +
  geom_point(aes(col=Series,size=4))+guides(size = FALSE)+
  labs(x="PC1", y = "PC2",title = " ")

TEMFBV1<-hwl_pca %>%
  ggplot(aes(x=.fittedPC1, y=.fittedPC2,label=Series,col=trend)) +
  geom_point(aes(col=trend,size=trend))+guides(size = FALSE)+
  labs(x="PC1", y = "PC2",title = "Trend strength")+
  geom_text_repel(aes(label=Series), max.overlaps = Inf)

TEMFBV2<-hwl_pca %>%
  ggplot(aes(x=.fittedPC1, y=.fittedPC2,label=Series,col=entropy)) +
  geom_point(aes(col=entropy,size=entropy))+guides(size = FALSE)+
  labs(x="PC1", y = "PC2",title = "entropy")+
  geom_text_repel(aes(label=Series), max.overlaps = Inf)

TEMFBV3<-hwl_pca %>%
  ggplot(aes(x=.fittedPC1, y=.fittedPC2,label=Series,col=x_acf1))+
  geom_text_repel(aes(label=Series), max.overlaps = Inf)+
  geom_point(aes(col=x_acf1,size=x_acf1))+guides(size = FALSE)+
  labs(x="PC1", y = "PC2",title = "x_acf1")

TEMFBV4<-hwl_pca %>%
  ggplot(aes(x=.fittedPC1, y=.fittedPC2,label=Series,col=seasonal_strength))+
  geom_text_repel(aes(label=Series), max.overlaps = Inf)+
  geom_point(aes(col=seasonal_strength,size=seasonal_strength))+guides(size = FALSE)+
  labs(x="PC1", y = "PC2",title = "seasonal_strength")






