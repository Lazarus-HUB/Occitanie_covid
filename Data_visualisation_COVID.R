
# Uploading the intensive care values 
Saturation_HP <- read.csv("D:/Fichiers R/stats de la région/Nombres_lit_dipo_lit.csv",T,sep=";")

# changing the WD for uploading the files
setwd("D:/Fichiers R/stats de la région/Graph/graphiques_regions")
file.remove(dir())

Coloration <-c("grey", brewer.pal(n=12, name = "Set3"))

# Loop for representing every county 

for (j in levels(Full_Data$Département)){
  b<-paste(j,Sys.Date())
  HP_PO<-Full_Data[Full_Data$Département==j,]
  Sat<-Saturation_HP$DR[Saturation_HP$Département==j]
  HP_PO<-HP_PO[HP_PO$Patients%in%c("Hospitalisation en cours","Dont réanimation"),]
  COL<-Coloration[c(7,13)]
  
  # Barplot visualisation
  
  t<-ggplot(HP_PO,aes(x=Dates, y = Nombre, fill=Patients))+
    geom_bar(stat="identity", position=position_dodge())+
    scale_x_date(name=NULL,date_labels = ("%d/%m"),breaks = "2 day")+
    scale_y_continuous(name= "Nombre de patients",limits = c(0,max(HP_PO[,length(Full_Data)])))+
    scale_fill_manual(values = Coloration[c(7,13)])+
    geom_hline(yintercept=Sat, linetype="dashed", color = Coloration[7])+
    annotate("text",x=as.Date("20/03/2020",format="%d/%m/%Y"), y =(Sat+0.2*Sat), label = "Saturation réa", color=Coloration[7])+
    labs(title = paste("Détail de l'évolution du nombre de patients Hospitalisés ou décédés dans le département", as.character(HP_PO$Département[1])),
         subtitle = paste("du 2020-03-20 au", Sys.Date()),
         caption = "Data source: ARS occitanie; https://mapthenews.maps.arcgis.com/apps/opsdashboard/index.html#/5e09dff7cb434fb194e22261689e2887")+
    theme(axis.text.x = element_text(angle=20))+    
    # Add the confinement date as an arrow
    # geom_segment(aes(x = as.Date("17/03/2020",format="%d/%m/%Y"), y = 400,xend=as.Date("17/03/2020",format="%d/%m/%Y"), yend = 320), 
    # arrow = arrow(length = unit(0.4, "cm"),type = "closed")) +
    theme_bw()
  print(t)
  dev.print(device = png, file = paste0(b,".png"), width = 800)
  
  # curve plot
  T_1<-ggplot(HP_PO,aes(x=Dates, y = Nombre, color=Patients))+
    geom_point()+
    geom_line()+
    scale_x_date(name=NULL,date_labels = ("%d/%m"),breaks = "2 day")+
    scale_y_continuous(name= "Nombre de patients",limits = c(0,max(HP_PO[,length(Full_Data)])))+
    scale_color_manual(values = Coloration[c(7,13)])+
    geom_hline(yintercept=Sat, linetype="dashed", color = Coloration[7])+
    annotate("text",x=as.Date("20/03/2020",format="%d/%m/%Y"), y =(Sat+0.2*Sat), label = "Saturation réa", color=Coloration[7])+
    labs(title = paste("Détail de l'évolution du nombre de patients Hospitalisés ou décédés dans le département", as.character(HP_PO$Département[1])),
         subtitle = paste("du 2020-03-20 au", Sys.Date()),
         caption = "Data source: ARS occitanie; https://mapthenews.maps.arcgis.com/apps/opsdashboard/index.html#/5e09dff7cb434fb194e22261689e2887")+
    theme(axis.text.x = element_text(angle=20))+
    
    # geom_segment(aes(x = as.Date("17/03/2020",format="%d/%m/%Y"), y = 400,xend=as.Date("17/03/2020",format="%d/%m/%Y"), yend = 320),
    # arrow = arrow(length = unit(0.4, "cm"),type = "closed")) +
    theme_bw()
  print(T_1)
  dev.print(device = png, file = paste0(b,"line",".png"), width = 800)
}

########################### Mapping ######################################
# New WD
setwd(dir = "D:/Fichiers R/stats de la région/Graph/Gif_map")
map <- getData('GADM', country='FRA', level=2)
Occitanie <- subset(map, NAME_1=="Occitanie")
couleurs <- colorRampPalette(c('white', 'red'))

# inverse %in% function creation
'%out%' <- function (x, table) match(x, table, nomatch = 0L) == 0L

# Use of Full_data_0 for the maping loop
Full_Data_0$Département <- as.factor(Full_Data_0$Département)
levels(Full_Data_0$Département) <- Liste_departement
Full_Data_0$Patients <- as.factor(Full_Data_0$Patients)

#Chargement des données de population par région
POP <- read.csv("D:/Fichiers R/stats de la région/POP_region.csv",T,sep=",")
POP$Département <- as.factor(POP$Département)

# selection of hopitalisation data only
Hosp_data <- Full_Data_0 %>% 
  filter(Département %out% "OCCITANIE",Patients %in% "Hospitalisation en cours") %>% 
  left_join(POP,by="Département") %>% # Message d'erreur que je ne comprends pas, ça fonctionne.
  replace(is.na(.), 0)

# Index for colorisation
idx <- match(Occitanie$NAME_2, Hosp_data$Département)

file.remove(dir())
for(i in 3:(length(Hosp_data)-1)){
  Date_jour<-as.Date(names(Hosp_data)[i],format = "%Y-%m-%d") 
  b<-paste0("MapGiF_",Date_jour)
  Occitanie$X26_03_20<- (Hosp_data[,i]*10000/Hosp_data$Population)[idx,]
  spplot(Occitanie, names(Occitanie)[length(Occitanie)+1],col.regions=couleurs(45), at = seq(0,3.8,.10), main="Hospitalisation en cours pour 10000 habitants, Occitanie", sub=as.character(Date_jour)) %>% 
    print()
  dev.print(device = png, file = paste0(b,".png"), width = 600)
}
