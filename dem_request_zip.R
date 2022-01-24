library(rgugik)
library(sf)
library(stars)
library(dplyr)
library(stringr)

krajkowo = read_sf("krajkowo.shp")


req_df = DEM_request(krajkowo)


req_df_zip = req_df %>% filter(str_sub(URL, start = -4, end = -1) == ".zip") %>%
  filter(product %in% c("DTM","DSM"))


req_df_zip_fil = req_df_zip %>% filter(seriesID == 72586)



#pobieramy do swojego folderu
tile_download(req_df_zip_fil, outdir = "./ZIP")


sheetnames = req_df_zip_fil$sheetID
sheetnames

files = dir("ZIP/")
files


pattern = c(paste0(sheetnames, collapse="|")) 

index = grep(pattern,files)

dem_filenames = files[index]
dem_filenames


#przy otwieraniu jednego z plików jest bład.
img = read_stars("ZIP/N-33-142-B-d-4-2-4_o.asc")











