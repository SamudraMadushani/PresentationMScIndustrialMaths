Processed_Data<-function(processed_data_path)
{
  Disease_Counts_csv <- list.files(path = ".", pattern = NULL, all.files = FALSE,
                                   full.names = FALSE, recursive = FALSE,
                                   ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  Data_Counts <- tsibble::as_tibble (data.table::rbindlist (lapply (Disease_Counts_csv , data.table::fread),fill = T))
  Data_Counts <- dplyr::relocate(Data_Counts, Year,Week,StartDate,EndDate,.before=Division)
  Data_Counts <- dplyr::arrange(Data_Counts, StartDate)
  setwd(here::here("data"))
  save (Data_Counts, file = 'FinalData.Rda')
  return(Data_Counts)
}

processed_data_path <- setwd(here::here("dataraw"))
Processed_Data(processed_data_path)
