#SHINY APP SIERPIEN

library(shiny)
library(shinythemes)
library(rgugik)
library(sf)
library(ggplot2)
library(stars)
library(mapview)
library(mapedit)
library(leaflet)


morasko = st_point(c(16.895, 52.489))
morasko = st_sfc(morasko, crs = 4326)



m = mapview(morasko)@map
testsf = NULL






#USER INTERFACE




ui <-   navbarPage("Rgugik", theme = shinytheme("yeti"),
                   navbarMenu("Granice",
                   tabPanel("Wojewodztwo",
                   sidebarPanel(
                   selectInput(inputId = "Dataset",
                   label = "Wybierz wojewodztwo",
                   choices = voivodeship_names$NAME_PL),
                   downloadButton("download", "Pobierz")),
                   mainPanel(
                   plotOutput(outputId = "distplot",width = "100%"  , height = "700px"))),
                              
                   tabPanel("Powiat",
                   sidebarPanel(
                   selectInput(inputId = "Dataset2",
                   label = "Wybierz powiat",
                   choices = county_names$NAME),
                   downloadButton("download2","Pobierz")),
                   mainPanel(
                   plotOutput(outputId = "plot_powiat",width = "100%"  , height = "700px")),),
                   
                   tabPanel("Gmina",
                   sidebarPanel(
                   selectInput(inputId = "Dataset3",
                   label = "Wybierz gmine",
                   choices = commune_names$NAME),
                   downloadButton("download3", "Pobierz")),
                   mainPanel(
                   plotOutput(outputId = "plot_gmina", width = "100%"  , height = "700px" )),)),
                              
                   navbarMenu("Ortofotomapa",
                   tabPanel("SHAPEFILE",
                   sidebarPanel(
                   fileInput("filemap" , label = "Wybierz plik shp",  accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE)),
                   mainPanel(
                   radioButtons(inputId = "composition",
                                label = "Select composition:",
                                choices = c("CIR", "RGB" , "NDVI")),
                                
                   splitLayout(cellWidths = c("50%", "50%"),
                   plotOutput(outputId = "myshapefile")),
                   plotOutput("myshapefile_ndvi"))),
                            
                   tabPanel("Z MAPY",
                   sidebarPanel(
                   tagList(h2("Draw"),
                   editModUI("test-edit"),
                   leafletOutput("edited"))),
                   mainPanel(radioButtons(inputId = "composition2",
                                          label = "Select composition:",
                                          choices = c("CIR", "NDVI"),
                                          selected = "CIR"),
                  plotOutput(
                  outputId = "mapedit_finished")))),
                  
                  tabPanel("NMT",
                  sidebarPanel(
                  fileInput("filemap2" , label = "Wybierz plik shp",  accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE)),
                  mainPanel(
                  radioButtons(inputId = "composition3",
                               label = "Select composition:",
                               choices = c("DEM", "TREE"),
                               selected = "DEM"),
                               plotOutput(outputId = "myshapefile2"))),
)


server <- function(input,output){
  
  
  dane <- reactive({borders_get(voivodeship = input$Dataset)})
  dane2 <- reactive({borders_get(county = input$Dataset2)})
  dane3 <- reactive({borders_get(commune = input$Dataset3)})
  
  output$distplot <- renderPlot({
  plot(dane(), main = paste0(input$Dataset))
  })
  
  output$download <- downloadHandler(
    filename <- function() {
      paste0(input$Dataset,".zip")
      
    },
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, paste0(input$Dataset))
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(dane(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }  
  )
  
  output$plot_powiat <- renderPlot({
    plot(dane2() , main = paste0(input$Dataset2 ))
  })
  
  output$download2 <- downloadHandler(
    filename <- function() {
      paste0(input$Dataset2,".zip")
      
    },
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, paste0(input$Dataset2))
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(dane2(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })
    }  
  )
  
  output$plot_gmina <- renderPlot({
    plot(dane3(), main = paste0(input$Dataset3))
  })
  
  output$download3 <- downloadHandler(
    filename <- function() {
      paste0(input$Dataset3,".zip")
      
    },
    content = function(file) {
      withProgress(message = "Exporting Data", {
        
        incProgress(0.5)
        tmp.path <- dirname(file)
        
        name.base <- file.path(tmp.path, paste0(input$Dataset3))
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        sf::st_write(dane3(), dsn = name.shp, ## layer = "shpExport",
                     driver = "ESRI Shapefile", quiet = TRUE)
        
        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        req(file.copy(name.zip, file))
        
        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        
        incProgress(0.5)
      })  
    }        
  )
  
  
  map <- reactive({
    req(input$filemap)
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    map <- read_sf(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/"
                         
                         
    ))
    
    
    
    
  })
  
  my_raster = reactive({
    st_set_crs(map(),2180)
    req_df = ortho_request(map())
    if(input$composition == "CIR" | input$composition == "NDVI"){
    req_df = req_df[req_df$composition == "CIR", ]}
    if(input$composition == "RGB"){
    req_df = req_df[req_df$composition == "RGB", ]
    }
    req_df = req_df[order(-req_df$year), ]
    last_year =  req_df$year[1]
    req_df = req_df[req_df$year == last_year, ]
    tile_download(req_df, method = "wget")
    
    filenames = paste0(req_df$filename,".tif")
    if (length(filenames) > 1){
      img = lapply(filenames, read_stars)
      img = do.call(st_mosaic, img)}
    if(length(filenames) == 1){img = read_stars(filenames)}  
    st_crs(img) = 2180
    img = st_crop(img, map())
    #img
    
    
})
  
  
output$myshapefile <- renderPlot({
  
    #calc_ndvi = function(img) {(img[1] - img[2]) / (img[1] + img[2])}
    #ndvi = st_apply(my_raster(), MARGIN = c("x", "y"), FUN = calc_ndvi)
    if(input$composition == "RGB" | input$composition == "CIR"){
    plot(my_raster(), rgb = c(1, 2, 3), main = input$composition)}
    
    if(input$composition == "NDVI"){
    calc_ndvi = function(img) {(img[1] - img[2]) / (img[1] + img[2])}
    ndvi = st_apply(my_raster(), MARGIN = c("x", "y"), FUN = calc_ndvi)
    plot(ndvi, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))
      
    }
      
      
    
  
    #plot(ndvi, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))
    
    
    
    
    #if(input$composition == "CIR"){
      #plot(my_raster(), rgb = c(1, 2, 3), main = "CIR") +
        #plot(ndvi, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))
    #  }
      
  #  if(input$composition == "NDVI"){
      #plot(ndvi, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))
    #}
   
    
  })

# output$myshapefile_ndvi <- renderPlot({
#   if(input$composition == "NDVI"){
#   calc_ndvi = function(img) {(img[1] - img[2]) / (img[1] + img[2])}
#   ndvi = st_apply(my_raster(), MARGIN = c("x", "y"), FUN = calc_ndvi)
#   plot(ndvi, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))}})
  
  
  
  # map2 <- reactive({
  #   req(input$filemap2)
  #   shpdf2 <- input$filemap2
  #   tempdirname2 <- dirname(shpdf2$datapath[1])
  #   
  #   for (i in 1:nrow(shpdf2)) {
  #     file.rename(
  #       shpdf2$datapath[i],
  #       paste0(tempdirname2, "/", shpdf2$name[i])
  #     )
  #   }
  #   
  #   map2 <- read_sf(paste(tempdirname2,
  #                        shpdf2$name[grep(pattern = "*.shp$", shpdf2$name)],
  #                        sep = "/"
  #                        
  #                        
  #   ))
  #   
  #   
  #   
  #   
  # })
  # 
  # 
  # my_raster2 = reactive({
  #   st_set_crs(map2(),2180)
  #   dem_df = DEM_request(map2())
  #   dem_df = dem_df[dem_df$composition == "CIR", ]
  #   dem_df = dem_df[order(-dem_df$year), ]
  #   last_year2 =  dem_df$year[1]
  #   dem_df = dem_df[dem_df$year == last_year2, ]
  #   tile_download(dem_df , method = 'wget')
  #   
  #   
  #   filenames2 = paste0(dem_df$filename,".tif")
  #   if (length(filenames2) > 1){
  #     img2 = lapply(filenames2, read_stars)
  #     img2 = do.call(st_mosaic, img2)}
  #   if(length(filenames2) == 1){img2 = read_stars(filenames2)}  
  #   img2 = st_transform(img2,2180) = #2180
  #   img2 = st_crop(img2, map())
    #img
    
    
    
    
    
    
    
 # })
  
  
  crud <- callModule(editMod, "test-edit", m, "breweries")
  my_polygon <- reactive({req(crud()$finished)})
  
  
  my_raster_map <- reactive({
    #st_set_crs(my_polygon(),2180)
    req_df_map = ortho_request(my_polygon())
    req_df_map = req_df_map[req_df_map$composition == "CIR", ]
    req_df_map = req_df_map[order(-req_df_map$year), ]
    last_year_map =  req_df_map$year[1]
    req_df_map = req_df_map[req_df_map$year == last_year_map, ]
    tile_download(req_df_map )#, method = 'wget')
    
    
    filenames3 = paste0(req_df_map$filename,".tif")
    if (length(filenames3) > 1){
      img_map = lapply(filenames3, read_stars)
      img_map = do.call(st_mosaic, img_map)}
    if(length(filenames3) == 1){img_map = read_stars(filenames3)

   
    st_crs(my_polygon()) = st_crs(img_map)
    st_crs(img_map) = st_crs(img_map)
    img_map = st_crop(img_map, my_polygon())
   # st_crop potrzebuje obiektow z tym samym epsg 
    
    
    
    
    }   
    
    
  })
#
   output$mapedit_finished <- renderPlot({
     
     
  

     calc_ndvi_map = function(img_map) (img_map[1] - img_map[2]) / (img_map[1] + img_map[2])
     ndvi_map = st_apply(my_raster_map(), MARGIN = c("x", "y"), FUN = calc_ndvi_map)


     if(input$composition2 == "CIR"){
       plot(my_raster_map(), rgb = c(1, 2, 3), main = "CIR") }
     if(input$composition2 == "NDVI"){
       plot(ndvi_map, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))
     }
   })

  
  
  
  
  
  
  
  
  
  
  
}

shinyApp(server = server, ui = ui)








