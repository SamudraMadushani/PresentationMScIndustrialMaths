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
