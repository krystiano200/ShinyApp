library(rgugik)
library(sf)
library(stars)
library(dplyr)
library(stringr)

krajkowo = read_sf("shp/krajkowo.shp")


req_df = DEM_request(krajkowo)


req_df_zip = req_df %>% filter(str_sub(URL, start = -4, end = -1) == ".zip") %>%
  filter(product %in% c("DTM","DSM"))


req_df_zip_fil = req_df_zip %>% filter(seriesID == 72586)




tile_download(req_df_zip_fil)#, outdir = "./DANE")


filenames = req_df_zip_fil$filename
filenames
filenames_trim = str_sub(filenames , start = 14)
filenames_trim




files = dir("./")
files


pattern = c(paste0(filenames_trim, collapse="|")) 

index = grep(pattern,files)


dem_filenames = files[index]
dem_filenames






