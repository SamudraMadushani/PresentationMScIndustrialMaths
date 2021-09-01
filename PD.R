library(tsfeatures)
library(viridisLite)
library(viridis)

load("H:/ProgressPresentation/ProgressPresentation/data/FinalData.Rda")
D<-Data_Counts
Week<-tsibble::yearweek(D$EndDate)
Districts<-D$Division
Counts<-as.integer(D$Dengue)
HM_data_districts<-tibble(Week,Districts,Counts)
HM_data_districts<-HM_data_districts%>% as_tsibble(key = Districts,index=Week)

HM2<-ggplot(HM_data_districts, aes(x=Week, y=Districts)) +
  geom_tile(aes(fill = Counts)) +
  scale_fill_viridis(option="magma",breaks = c(500,1500, 2500)) +
  labs(title = "Global visualization",y = "Districts") +
  theme_light()+ theme(legend.position = 'right')+
   scale_y_discrete(limits=c("Colombo","Gampaha","Kandy","Ratnapura","Kegalle","Kalutara","Batticaloa","Matara","Jaffna","Galle","Puttalam","Matale","Hambanthota","Anuradhapura","Badulla","Vavuniya","Ampara","Monaragala","Trincomalee","Polonnaruwa","NuwaraEliya","Mannar","Mullaitivu","Kilinochchi"))

#NewHTDATA<-HM_data_districts%>%dplyr::filter(lubridate::year(Week)<2019)
#NewHTDATA<-NewHTDATA%>%dplyr::filter(lubridate::year(Week)>2017)


################################################################
Week<-tsibble::yearweek(D$EndDate)
Date<-D$EndDate
Province<-rep(c(rep("Western",3),rep("Central",3),rep("Southern",3),rep("Nothern",5),
                rep("Eastern",3),rep("NorthWestern",2),rep("NorthCentral",2),
                rep("Uva",2),rep("Sabaragamuwa",2),rep("Eastern",1)),730)
Districts<-D$Division
Counts<-as.integer(D$Dengue)
DengueTEST<-tibble(Week,Date,Counts,Districts,Province)
DataDengueTEST<- DengueTEST%>% 
  as_tsibble(key=c(Province,Districts),index=Week)

DIS_DataDengueTEST<-DataDengueTEST %>%fabletools::aggregate_key(Province/Districts,Counts=sum(Counts)) #ARRANGE TEST SET
DIS_DataDengueTEST

Dengue_Districts<-DIS_DataDengueTEST %>%
  dplyr::filter(!is_aggregated(Districts)& !is_aggregated(Province))

Ampara<-Dengue_Districts%>% dplyr::filter(Districts=="Ampara")
Anuradhapura<-Dengue_Districts%>% dplyr::filter(Districts=="Anuradhapura")
Badulla<-Dengue_Districts%>% dplyr::filter(Districts=="Badulla")
Batticaloa<-Dengue_Districts%>% dplyr::filter(Districts=="Batticaloa")
Colombo<-Dengue_Districts%>% dplyr::filter(Districts=="Colombo")
Galle <-Dengue_Districts%>% dplyr::filter(Districts=="Galle")
Gampaha<-Dengue_Districts%>% dplyr::filter(Districts=="Gampaha")
Hambanthota<-Dengue_Districts%>% dplyr::filter(Districts=="Hambanthota")
Jaffna<-Dengue_Districts%>% dplyr::filter(Districts=="Jaffna")
Kalmune<-Dengue_Districts%>% dplyr::filter(Districts=="Kalmune")
Kalutara<-Dengue_Districts%>% dplyr::filter(Districts=="Kalutara")
Kandy<-Dengue_Districts%>% dplyr::filter(Districts=="Kandy")
Kegalle<-Dengue_Districts%>% dplyr::filter(Districts=="Kegalle")
Kilinochchi<-Dengue_Districts%>% dplyr::filter(Districts=="Kilinochchi")
Kurunagala<-Dengue_Districts%>% dplyr::filter(Districts=="Kurunagala")
Mannar<-Dengue_Districts%>% dplyr::filter(Districts=="Mannar")
Matale<-Dengue_Districts%>% dplyr::filter(Districts=="Matale")
Matara<-Dengue_Districts%>% dplyr::filter(Districts=="Matara")
Monaragala<-Dengue_Districts%>% dplyr::filter(Districts=="Monaragala")
Mullaitivu<-Dengue_Districts%>% dplyr::filter(Districts=="Mullaitivu")
NuwaraEliya<-Dengue_Districts%>% dplyr::filter(Districts=="NuwaraEliya")
Polonnaruwa<-Dengue_Districts%>% dplyr::filter(Districts=="Polonnaruwa")
Puttalam<-Dengue_Districts%>% dplyr::filter(Districts=="Puttalam")
Ratnapura<-Dengue_Districts%>% dplyr::filter(Districts=="Ratnapura")
Trincomalee<-Dengue_Districts%>% dplyr::filter(Districts=="Trincomalee")
Vavuniya<-Dengue_Districts%>% dplyr::filter(Districts=="Vavuniya")
Dis_Data<-data.frame(Ampara$Week,Ampara$Counts,Anuradhapura$Counts,Badulla$Counts,
                     Batticaloa$Counts,Colombo$Counts,Galle$Counts,Gampaha$Counts,
                     Hambanthota$Counts,Jaffna$Counts,Kalmune$Counts,Kalutara$Counts,
                     Kandy$Counts,Kegalle$Counts,Kilinochchi$Counts,Kurunagala$Counts,
                     Mannar$Counts,Matale$Counts,Matara$Counts,Monaragala$Counts,
                     Mullaitivu$Counts,NuwaraEliya$Counts,Polonnaruwa$Counts,
                     Puttalam$Counts,Ratnapura$Counts,Trincomalee$Counts,Vavuniya$Counts)
Dis_Data_Original<-Dis_Data
Dis_Data
Min_Max_Transformation<-function(Dis_Data)
{
  for (i in 2:27){
    rng<-range(Dis_Data[,i],na.rm=TRUE)
    Dis_Data[,i]<-(Dis_Data[,i]-rng[1])/(rng[2]-rng[1])  
  }
  return(Dis_Data);
} 
Transformed_Dis_Data<-Min_Max_Transformation(Dis_Data)  
Week<-rep(Transformed_Dis_Data[,1],26)
Counts<-c(Transformed_Dis_Data[,2],Transformed_Dis_Data[,3],
          Transformed_Dis_Data[,4],Transformed_Dis_Data[,5],
          Transformed_Dis_Data[,6],Transformed_Dis_Data[,7],
          Transformed_Dis_Data[,8],Transformed_Dis_Data[,9],
          Transformed_Dis_Data[,10],Transformed_Dis_Data[,11],
          Transformed_Dis_Data[,12],Transformed_Dis_Data[,13],
          Transformed_Dis_Data[,14],Transformed_Dis_Data[,15],
          Transformed_Dis_Data[,16],Transformed_Dis_Data[,17],
          Transformed_Dis_Data[,18],Transformed_Dis_Data[,19],
          Transformed_Dis_Data[,20],Transformed_Dis_Data[,21],
          Transformed_Dis_Data[,22],Transformed_Dis_Data[,23],
          Transformed_Dis_Data[,24],Transformed_Dis_Data[,25],
          Transformed_Dis_Data[,26],Transformed_Dis_Data[,27])
Districts<-c(rep("Ampara",730),rep("Anuradhapura",730),rep("Badulla",730),rep("Batticaloa",730),
             rep("Colombo",730),rep("Galle",730),rep("Gampaha",730),rep("Hambanthota",730),
             rep("Jaffna",730),rep("Kalmune",730),rep("Kalutara",730),rep("Kandy",730),
             rep("Kegalle",730),rep("Kilinochchi",730),rep("Kurunagala",730),rep("Mannar",730),
             rep("Matale",730),rep("Matara",730),rep("Monaragala",730),rep("Mullaitivu",730),
             rep("NuwaraEliya",730),rep("Polonnaruwa",730),rep("Puttalam",730),rep("Ratnapura",730),
             rep("Trincomalee",730),rep("Vavuniya",730))

Province<-c(rep("Eastern",730),rep("North Central",730),rep("Uva",730),rep("Eastern",730),
            rep("Western",730),rep("Southern",730),rep("Western",730),rep("Southern",730),
            rep("Nothern",730),rep("Eastern",730),rep("Western",730),rep("Central",730),
            rep("Sabaragamuwa",730),rep("Nothern",730),rep("North Western",730),rep("Nothern",730),
            rep("Central",730),rep("Southern",730),rep("Uva",730),rep("Nothern",730),
            rep("Central",730),rep("North Central",730),rep("North Western",730),rep("Sabaragamuwa",730),
            rep("Eastern",730),rep("Nothern",730))

Trans_Dengue<-data.frame(Week,Counts,Districts,Province)
Trans_Dengue
Trans_DengueTRAIN<-Trans_Dengue%>% 
  as_tsibble(key=c(Province,Districts),index=Week)

Trans_DengueMOD<-data.frame(Week,Counts,Districts)
Trans_DengueMOD
Trans_DengueTRAINMOD<-Trans_DengueMOD%>% 
  as_tsibble(key=c(Districts),index=Week)

HMT2<-ggplot(Trans_DengueTRAINMOD, aes(x=Week, y=Districts)) +
  geom_tile(aes(fill = Counts)) +
  scale_fill_viridis(option="magma") +
  labs(title = "Local visualization",y = "Districts") +
  theme_light()+ theme(legend.position = 'right')+
  scale_y_discrete(limits=c("Colombo","Gampaha","Kandy","Ratnapura","Kegalle","Kalutara","Batticaloa","Matara","Jaffna","Galle","Puttalam","Matale","Hambanthota","Anuradhapura","Badulla","Vavuniya","Ampara","Monaragala","Trincomalee","Polonnaruwa","NuwaraEliya","Mannar","Mullaitivu","Kilinochchi"))

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

########################################################################################

library(broom)
library(ggrepel)
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
  geom_point() +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2")

FBVZW<-FBVZW+ theme(legend.position = "bottom",legend.box = "vertical")+scale_color_manual(values = c("#e41a1c","#6a3d9a","#ff7f00"))

FBVZW1<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = trend_strength, label=Districts)) +
  geom_point(aes(size=trend_strength,col = trend_strength)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Trend strength")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW1<-FBVZW1+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

#(FBVZW|FBVZW1)

#Wt<-Dengue_Districts%>%dplyr::filter(Province=="Western")%>%autoplot(Counts)


