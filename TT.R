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
plot(x,z,type='l',xlab='',ylab='')
lines(x=1:100,y=rep(1,100),col='red',lwd=3)
lines(x=101:250,y=rep(0,150),col='red',lwd=3)
lines(x=251:450,y=rep(2,200),col='red',lwd=3)
lines(x=451:500,y=rep(0.5,50),col='red',lwd=3)
# Change in variance example following EFK
x=1:500
z=c(rnorm(100,0,sd=0.1),rnorm(150,0,sd=0.7),rnorm(200,0,sd=0.25),rnorm(50,0,sd=1))
plot(x,z,type='l',xlab='',ylab='')






