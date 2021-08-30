Reading_EpidemiologicalReports <- function(path, startdate, year, week, Cname, DeletingRows,
                                           TotalColmns, PercentageColmns, DeleteColmns){
  x<-list.files(path = ".", pattern = NULL, all.files = FALSE,
                full.names = FALSE, recursive = FALSE,
                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  results<-list(1:length(x))
  j=1;
  
  for (i in 1:length(x)) {
    print(x[i])
    texdocument <- pdftools::pdf_text(x[i])
    head(texdocument)
    length(texdocument)
    sector_texdocument <- texdocument[1]
    sector_texdocument <- stringr::str_split(sector_texdocument, "\n\n |\n | \n|\n\n\n")
    sector_texdocument <- sector_texdocument[[1]]
    head(sector_texdocument)
    sector_texdocument <- trimws(sector_texdocument)
    sector_texdocument<-sector_texdocument[-DeletingRows]
    sector_texdocument
    sector_texdocument <- stringr::str_split_fixed(sector_texdocument, " {1,}", TotalColmns)
    sector_texdocument <- data.frame(sector_texdocument, stringsAsFactors = FALSE)
    sector_texdocument <- sector_texdocument[ , -PercentageColmns]
    names(sector_texdocument) <- Cname
    DWeek<-tsibble::as_tibble(sector_texdocument)
    DWeek<-DWeek[ , -DeleteColmns]
    
    DWeek$Year<-year[i]
    DWeek$Week<-week[i]
    DWeek$StartDate<-as.Date(startdate[j])
    DWeek$EndDate<-as.Date(startdate[j+1])
    DWeek$Division<-c("Colombo","Gampaha","Kalutara","Kandy","Matale","NuwaraEliya","Galle","Hambanthota","Matara","Jaffna","Kilinochchi","Mannar","Vavuniya","Mullaitivu","Batticaloa","Ampara","Trincomalee","Kurunagala","Puttalam","Anuradhapura","Polonnaruwa","Badulla","Monaragala","Ratnapura","Kegalle","Kalmune")
    results[[i]] <- DWeek
    j=j+2;
  }
  
  for (i in 1:length(x))
  {
    setwd(here::here("dataraw"))
    write.csv(results[[i]],file=paste(year[i],week[i],"week.csv",sep="_"))
  }
  
  return(results)
}

path <- setwd(here::here("pdffiles","2020"))
startdate <- c("2019-12-21","2019-12-27","2019-12-28","2020-01-03","2020-01-04","2020-01-10","2020-01-11","2020-01-17","2020-01-18","2020-01-24","2020-01-25","2020-01-31","2020-02-01","2020-02-07","2020-02-08","2020-02-14","2020-02-15","2020-02-21","2020-02-22","2020-02-28","2020-02-29","2020-03-06","2020-03-07","2020-03-13","2020-03-14","2020-03-20","2020-03-21","2020-03-27","2020-03-28","2020-04-03","2020-04-04","2020-04-10","2020-04-11","2020-04-17","2020-04-18","2020-04-24","2020-04-25","2020-05-01","2020-05-02","2020-05-08","2020-05-09","2020-05-15","2020-05-16","2020-05-22","2020-05-23","2020-05-29","2020-05-30","2020-06-05","2020-06-06","2020-06-12","2020-06-13","2020-06-19","2020-06-20","2020-06-26","2020-06-27","2020-07-03","2020-07-04","2020-07-10","2020-07-11","2020-07-17","2020-07-18","2020-07-24","2020-07-25","2020-07-31","2020-08-01","2020-08-07","2020-08-08","2020-08-14","2020-08-15","2020-08-21","2020-08-22","2020-08-28","2020-08-29","2020-09-04","2020-09-05","2020-09-11","2020-09-12","2020-09-18","2020-09-19","2020-09-25","2020-09-26","2020-10-02","2020-10-03","2020-10-09","2020-10-10","2020-10-16","2020-10-17","2020-10-23","2020-10-24","2020-10-30","2020-10-31","2020-11-06","2020-11-07","2020-11-13","2020-11-14","2020-11-20","2020-11-21","2020-11-27","2020-11-28","2020-12-04","2020-12-05","2020-12-11","2020-12-12","2020-12-18")
year <- c(2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020)
week <- c(52,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51);
Cname <- c("Division",
        "Dengue",
        "Dengue B",
        "Dysentery",
        "Dysentery B",
        "Encephalitis",
       "Encephalitis B",
       "Enteric Fever",
        "Enteric Fever B",
        "Food Poisioning",
       "Food Poisioning B",
        "Leptospirosis",
        "Leptospirosis B",
        "Typhus Fever",
        "Typhus Fever B",
       "Viral Hepatistis",
       "Viral Hepatistis B",
        "Human Rabies",
        "Human Rabies B",
        "Chickenpox",
        "Chickenpox B",
       "Meningitis",
       "Meningitis B",
       "Leishmaniasis",
        "Leishmaniasis B")

 DeletingRows <- c(1,2,3,30,31,32,33,34)
TotalColmns <- 27
PercentageColmns <- c(26,27)
DeleteColmns <- c(3,5,7,9,11,13,15,17,19,21,23,25)
Weeks_2020 <- Reading_EpidemiologicalReports(path,startdate,year,week,Cname,DeletingRows,TotalColmns,PercentageColmns,DeleteColmns)
