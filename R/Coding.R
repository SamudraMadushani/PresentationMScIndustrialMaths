HACF<-DataDengueTEST%>%dplyr::filter(Districts=="Colombo")%>%autoplot(Counts,col="blue")+labs(x="Week",title = "Colombo-The Highest acf1")

LACF<-DataDengueTEST%>%dplyr::filter(Districts=="Mullaitivu")%>%autoplot(Counts,col="blue")+labs(x="Week",title = "Mullaitivu-The Lowest acf1")


FBVZW2<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = seasonal_strength_year, label=Districts)) +
  geom_point(aes(size=seasonal_strength_year)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Seasonal strength")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW2<-FBVZW2+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

FBVZW3<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = spectral_entropy, label=Districts)) +
  geom_point(aes(size=spectral_entropy)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Entropy")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW3<-FBVZW3+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)


FBVZW2<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = seasonal_strength_year, label=Districts)) +
  geom_point(aes(size=seasonal_strength_year)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Seasonal strength")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW2<-FBVZW2+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

FBVZW3<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = spectral_entropy, label=Districts)) +
  geom_point(aes(size=spectral_entropy)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "Entropy")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW3<-FBVZW3+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)


FBVZW4<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = acf1, label=Districts)) +
  geom_point(aes(size=acf1)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "acf1")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW4<-FBVZW4+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)



FBVZW5<-pcsW %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = n_crossing_points, label=Districts)) +
  geom_point(aes(size=n_crossing_points)) +
  theme(aspect.ratio = 1)+
  geom_text_repel(aes(label=Districts), max.overlaps = Inf)+
  labs(x="PC1", y = "PC2",title = "crossing_points")+scale_color_viridis(option = "A")+
  ggeasy::easy_center_title()+guides(size = FALSE)

FBVZW5<-FBVZW5+ theme(legend.position = "bottom",legend.box = "vertical")+
  labs(colour = NULL)

LCP<-DIS_DataDengueTEST %>%
  dplyr::filter(is_aggregated(Districts)& is_aggregated(Province))%>%autoplot(Counts,col="blue")+labs(x="Week",title = "Sri Lanka-The lowest number of crossing points")

HCP<-DataDengueTEST%>%dplyr::filter(Districts=="Ampara")%>%autoplot(Counts,col="blue")+labs(x="Week",title = "Ampara-The highest number of crossing points")

HCP<-HCP+ geom_hline(yintercept=1,color = "red", size=1)
LCP<-LCP+ geom_hline(yintercept=454.5,color = "red", size=1)

(HCP/LCP)

library(sf)
library(maps)
library(oz)
library(ozmaps)
sf_sl <- read_sf("H:/FINALANALYSISPROJECT/DENGUE/mapdata/lka_admbnda_adm2_slsd_20200305.shp") %>%
  dplyr::select(ADM2_EN, geometry)

sf_sl <- sf_sl %>% st_transform(5235)
sf_sl<-sf_sl%>%dplyr::mutate(ADM2_EN  = case_when(ADM2_EN  ==  "[unknown]" ~"KALMUNE",TRUE ~ ADM2_EN))
sf_sl <- sf_sl %>%
  mutate(DISTRICT = str_to_upper(ADM2_EN)) %>% select(-ADM2_EN)
ACF<- read_excel("H:/FINALANALYSISPROJECT/DENGUE/FMethod/TRIALMOD.xlsx")
districts_sl_yearly <- left_join(sf_sl, ACF ,by = c("DISTRICT"))

LEGEND<-ggplot(districts_sl_yearly) +
  geom_sf(aes(fill = Method))+ggtitle("acf1 by districts")+theme(axis.ticks.x = element_blank(),
                                                           axis.text.x = element_blank(),
                                                           axis.ticks.y = element_blank(),
                                                           axis.text.y = element_blank())

LEGEND<- LEGEND+ scale_fill_manual(breaks = c("low", "high"),
                                   values=c("#40004b","#e08214")) +
  theme(legend.position = "none")

Ym<-autoplot(aggts_total[[5]],col="#66a61e") + labs(y = "Counts",x="Week",title="Yearly")+geom_vline(xintercept = c(2019,2020))
Wm<-autoplot(aggts_total[[1]],col="#e7298a") + labs(y = " ",x=" ",title="Weekly")+geom_vline(xintercept = 2019)

(Wm/Ym)



