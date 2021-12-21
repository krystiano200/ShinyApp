library(mapedit)
library(sf)
library(rgugik)
library(mapview)
library(stars)

morasko = st_point(c(16.942, 52.464))
morasko = st_sfc(morasko, crs = 4326)

what_we_created <- mapview(morasko) %>%
  editMap()

my_draw = what_we_created$finished

plot(my_draw)
req_df = ortho_request(my_draw)

req_df = req_df[req_df$composition == "CIR",]
req_df = req_df[req_df$year == 2020,]

tile_download(req_df)

img = read_stars("73885_960650_6.179.12.21.3.3.tif")
plot(img)

calc_ndvi = function(img) {(img[1] - img[2]) / (img[1] + img[2])}
ndvi_map = st_apply(img , MARGIN = c("x", "y"), FUN = calc_ndvi)
plot(ndvi_map, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))

