# Occitanie_covid
A short R program to upload data from ARS PDF and visualise data
# uploading data from the website
# get URL from ARS website 

base_url <- "https://www.occitanie.ars.sante.fr" # main website
Url_text<-GET(paste0(base_url,"/coronavirus-dernier-point-de-situation-en-occitanie-0")) %>% 
  content("text")
Liste_liens <- unlist(strsplit(Url_text, "<a href="))
Liste_adresses <- Liste_liens[grepl("BulletinInfo.+?\\.pdf", Liste_liens)] %>%  #extract link to the PDF
  str_sub(2 ,str_locate(.,".pdf")[,"end"]) %>% 
  str_remove(base_url) # make sure that the format is the same for all

# the uploading loop
Destination<-"D:/Fichiers R/stats de la région/Bulletin_occitanie_Covid"
setwd(dir = Destination)
# optional clear directory
#file.remove(dir())

# Downloading of the PDF files
for (i in 1:length(Liste_adresses)) {
  Nom_bulletin <- str_extract(Liste_adresses[i], "BulletinInfo.+")
  if (TRUE %in% str_detect(Liste_adresses[i], list.files())) {
    #Vérification que le fichier n'existe pas dans la destination
    print(paste("fichier", Nom_bulletin, "est déjà téléchargé"))
  } else{
    #Téléchargement du fichier
    download.file(
      paste0(base_url, Liste_adresses[i]),
      destfile = paste0(Destination, "/", Nom_bulletin),
      quiet = FALSE,
      mode = "wb"
    )
  }
}

########################## Data extraction ################################ 

Destination <-
  "D:/Fichiers R/stats de la région/Bulletin_occitanie_Covid"
setwd(Destination)

# Date from filenames
Dates <- as.Date(str_extract(list.files(), "(?<=_)[[:digit:]]{8}"), format = "%Y%m%d")


B<-list()
for (i in 1:length(list.files())) {
  if (Dates[i] > as.Date("20200319", "%Y%m%d")) { # unified PDF format from 23/03/2020 
    print(Dates[i])
    covid_table <- extract_tables(
      list.files()[i],
      output = "data.frame",
      area = list(c(154, 13, 354, 346)), # this area is related to the PDF format
      guess = FALSE
    ) %>%
      as.data.frame()
    NRct <- nrow(covid_table)
    NCct <- ncol(covid_table) 
    
    if (Dates[i] > as.Date("20200321", "%Y%m%d")) { # before the 23/03/2020, the variable "return from hospitalisation" didn't exist 
      Cnames <- c("Département","Hospitalisation en cours","Dont réanimation","Total retour", "Total décès") 
    }else{
      Cnames <- c("Département","Hospitalisation en cours","Dont réanimation","Total décès")
    }
    
    B[[i]] <- covid_table %>% 
      as_tibble() %>%
      slice(c(1:NRct)[str_detect(covid_table[,1],".ri.+ge")]:NRct) %>% # Trouver la colone qui commence et trouver la colonne qui finit
      mutate_at(2:NCct, as.numeric) %>%
      set_colnames(Cnames) %>% 
      pivot_longer(2:NCct,
                   names_to = "Patients",
                   values_to = as.character(Dates[i]))
  }else{
    print("Date à pas compatibles")
  }
}

# deleting element == NULL in the list

B[sapply(B, is.null)]<- NULL

# Creation of a simple data.frame (maybe more efficient with apply and a function)

Full_Data_0<-B[[3]][,1:2] # Selection of the fist two column for the left_join 

for (i in 1:length(B)){
  Full_Data_0<-left_join(Full_Data_0,B[[i]], by=c("Département","Patients"))
}

# shaping for ggplot
Liste_departement<-c("Ariège","Aude","Aveyron","Gard","Gers","Hérault","Haute-Garonne","Hautes-Pyrénées","Lot","Lozère","OCCITANIE","Pyrénées-Orientales","Tarn-et-Garonne","Tarn")

Full_Data <- pivot_longer(Full_Data_0,3:length(Full_Data_0),names_to="Dates",values_to="Nombre")

Full_Data$Département <- as.factor(Full_Data$Département)
levels(Full_Data$Département) <- Liste_departement
Full_Data$Patients <- as.factor(Full_Data$Patients)
Full_Data$Dates <- as.Date(Full_Data$Dates)
