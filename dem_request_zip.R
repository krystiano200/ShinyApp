library(rgugik)
library(sf)
library(stars)
library(dplyr)
library(stringr)

#setwd("C:/Users/Krystian/Desktop/Aplikacja")
krajkowo = read_sf("shp/krajkowo.shp")


req_df = DEM_request(krajkowo)


req_df_zip = req_df %>% filter(str_sub(URL, start = -4, end = -1) == ".zip") %>%
  filter(product %in% c("DTM","DSM"))


req_df_zip_fil = req_df_zip %>% filter(seriesID == 72586)


#TEMP
my_dir = tempdir()

tile_download(req_df_zip_fil, outdir = my_dir)


sheetnames = req_df_zip_fil$sheetID
sheetnames




files = dir(my_dir)
files


pattern = c(paste0(sheetnames, collapse="|")) 

index = grep(pattern,files)


dem_filenames = files[index]

###
sheetnames_p = c()
for(i in dem_filenames){
  if(str_sub(i, start = -5) == 'p.asc'){
    print(i)
    sheetnames_p = c(sheetnames_p,i)
  }
}

sheetnames_p
setwd(my_dir)

types = rep("numeric", 3)

read_asc = function(x){
  types = rep("numeric", 3)
  read.table(x,colClasses = types , col.names = c("X", "Y", "Z") )
}

img_dem_zip = lapply(sheetnames_p, read_asc)


my_st_as_stars = function(x){
  st_as_stars(x , coords = c("X", "Y"), crs = "epsg:2180" )
}


img_dem_zip_stars = lapply(img_dem_zip, my_st_as_stars)

plot(img_dem_zip_stars[[1]], col = terrain.colors(99, alpha = NULL))




  
#tu mamy blad

img_join = do.call(st_mosaic, img_dem_zip_stars)









