#' Download bulk data from the faostat website
#' http://www.fao.org/faostat/en/#data
#' 
#' Note the files called "normalized" are in long format 
#' with a year column instead of one column for each year.
#' The long format is preferable for data analysis. 
#' @param url_bulk character url of the faostat bulk zip file to download
#' @param data_folder character path of the local folder where to download the data
#' @author Paul Rougieux
#' @examples 
#' \dontrun{
#' # Load global forestry data in long format
#' url_forestry <- "http://fenixservices.fao.org/faostat/static/bulkdownloads/Forestry_E_All_Data_(Normalized).zip"
#' download_faostat_bulk(url_forestry)
#' }
#' @export
download_faostat_bulk <- function(url_bulk, data_folder="data_raw"){
    file_name <- basename(url_bulk)
    download.file(url_bulk, file.path(data_folder, file_name))
}

#' Function to read zipped files and return a data frame
#' Reads the main csv file within the archive.
#' The main file has the same name as the name of the archive. 
#' Note: the zip archive might also contain metadata files about Flags and Symboles.
#' @param zip_file_name character name of the zip file to read
#' @return data frame of FAOSTAT data 
#' @examples 
#' \dontrun{
#' # Read a file then assign it to a data frame and save it as rds
#' forestry_e_all_data <- read_faostat_bulk("data_raw/Forestry_E_All_Data_(Normalized).zip")
#' saveRDS(forestry_e_all_data,"data_raw/forestry_e_all_data.rds")
#' }
#' @export
read_faostat_bulk <- function(zip_file_name){
    # The main csv file shares the name of the archive
    csv_file_name <- gsub(".zip$",".csv", basename(zip_file_name))
    # Read the csv file within the zip file
    df <- read.csv(unz(zip_file_name, csv_file_name), stringsAsFactors = FALSE)
    return(df)
}
