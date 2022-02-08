#SHINY APP SIERPIEN

library(shiny)
library(shinythemes)
library(rgugik)
library(sf)
library(stars)
library(mapview)
library(mapedit)
library(leaflet)
library(dplyr)
library(stringr)





morasko = st_point(c(16.942, 52.464))
morasko = st_sfc(morasko, crs = 4326)
'%nin%' = Negate('%in%')
files = dir("./")
m = mapview(morasko)@map
#n = mapview(morasko)@map
#testsf = NULL



#USER INTERFACE

ui <-   navbarPage(
  "RGUGIK",
  theme = shinytheme("yeti"),
  navbarMenu(
    "Granice",
    tabPanel("Wojewodztwo",
             sidebarPanel(
               selectInput(
                 inputId = "Dataset",
                 label = "Wybierz wojewodztwo",
                 choices = voivodeship_names$NAME_PL
               ),
               downloadButton("download", "Pobierz")
             ),
             mainPanel(
               plotOutput(
                 outputId = "distplot",
                 width = "100%"  , 
                 height = "700px"
               )
             )
    ),
    
    tabPanel(
      "Powiat",
      sidebarPanel(
        selectInput(
          inputId = "Dataset2",
          label = "Wybierz powiat",
          choices = county_names$NAME
        ),
        downloadButton("download2","Pobierz")
      ),
      mainPanel(
        plotOutput(
          outputId = "plot_powiat",
          width = "100%",
          height = "700px"
        )
      ),
    ),
    
    tabPanel(
      "Gmina",
      sidebarPanel(
        selectInput(
          inputId = "Dataset3",
          label = "Wybierz gmine",
          choices = commune_names$NAME
        ),
        downloadButton("download3", "Pobierz")
      ),
      mainPanel(
        plotOutput(
          outputId = "plot_gmina",
          width = "100%", 
          height = "700px" 
        )
      ),
    )
  ),
  
  navbarMenu(
    "Ortofotomapa",
    tabPanel(
      "SHAPEFILE",
      sidebarPanel(
        fileInput(
          "filemap",
          label = "Wybierz plik shp",
          accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),    
          multiple=TRUE
        )
      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Table" ,DT::dataTableOutput("tableortho")), 
          tabPanel(
            "Plot",
            splitLayout(cellWidths = c("50%","50%"),
                        plotOutput(
                          outputId = "myshapefile"),
                        plotOutput(
                          outputId = "myshapefile_ndvi"
                        )
            ))
        )
        
        
        
      )
    ),
    
    tabPanel(
      "Z MAPY",
      sidebarPanel(
        tagList(h2("Draw"),
                editModUI("test-edit"),
                leafletOutput("edited"))),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Table", DT::dataTableOutput("tableorthomap")),
          tabPanel(
            "Plot",
            splitLayout(cellWidths = c("50%","50%"),
                        plotOutput(
                          outputId = "mapedit_finished"),
                        plotOutput(
                          outputId = "mapedit_ndvi" )
                        
                        
            )
          ))
      ))
  ),
  navbarMenu("NMT",
             tabPanel(
               "SHAPEFILE",
               sidebarPanel(
                 fileInput(
                   "filemap2",
                 label = "Wybierz plik shp",
                 accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),
                 multiple=TRUE
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Table", DT::dataTableOutput("tabledem")),
                   tabPanel(
                     "Plot",
                plotOutput(
                  outputId = "myshapefile2")))
                 )),
              tabPanel(
               "Z Mapy",
               sidebarPanel(
                 tagList(h2("Draw"),
                         editModUI("test-edit"),
                         leafletOutput("edited"))),
              mainPanel(
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Table", DT::dataTableOutput("tabledemmap")),
                   tabPanel(
                     "Plot"
                   )),
                 plotOutput(
                   outputId = "mapedit_dem")
                 )
              )
             )
      )






#SERVER

server <- function(input,output){
  
  dane <- reactive({
    borders_get(voivodeship = input$Dataset)
  })
  dane2 <- reactive({
    borders_get(county = input$Dataset2)
  })
  dane3 <- reactive({
    borders_get(commune = input$Dataset3)
  })
  
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
        
        if (length(Sys.glob(name.glob)) > 0) 
          file.remove(Sys.glob(name.glob))
        sf::st_write(dane(),
                     dsn = name.shp,
                     driver = "ESRI Shapefile", 
                     quiet = TRUE)
        
        zip::zipr(zipfile = name.zip,
                  files = Sys.glob(name.glob))
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
  
  
  map_ortho <- reactive({
    req(input$filemap)
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    map_ortho <- read_sf(paste(tempdirname,
                               shpdf$name[grep(pattern = "*.shp$",shpdf$name)],
                               sep = "/"
                               
                               
    ))
    
    
  })
  
  my_tableortho <-  reactive({
    ortho_request(map_ortho()) %>% arrange(desc(year)) %>% 
      select(year,resolution,composition,
             CRS,URL,filename,seriesID)
  })
  
  
  output$tableortho <- DT::renderDataTable({
    my_tableortho() 
  })
  my_rows_sel = reactive({input$tableortho_rows_selected})
  table_sel = reactive({my_tableortho()[my_rows_sel(),]})
  real_sel = reactive({filter(my_tableortho(), seriesID == table_sel()$seriesID)})
  
  my_raster <- reactive({
    # my_rows_sel = input$tableortho_rows_selected
    # table_sel = my_tableortho()[my_rows_sel,]
    # real_sel = filter(my_tableortho(), seriesID == table_sel$seriesID)
    if(paste0(real_sel()$filename,".tif") %nin%  files){
      tile_download(real_sel())}
    filenames = paste0(real_sel()$filename,".tif")
    if (length(filenames) > 1){
      img = lapply(filenames, read_stars)
      img = do.call(st_mosaic, img)}
    else if(length(filenames) == 1){img = read_stars(filenames)}
    ortho_temp = map_ortho()
    st_crs(img) = 2180
    if(st_crs(ortho_temp) != st_crs(img)){
      ortho_temp = st_transform(ortho_temp, st_crs(img))
    }
    img = st_crop(img , ortho_temp)
    
    
    
  })
  
  output$myshapefile <- renderPlot({
    plot(my_raster(), rgb = c(1, 2, 3), main ="")})
  
  output$myshapefile_ndvi <- renderPlot({
    if(real_sel()$composition == "CIR"){
      calc_ndvi = function(img) {(img[1] - img[2]) / (img[1] + img[2])}
      ndvi = st_apply(my_raster(), MARGIN = c("x", "y"), FUN = calc_ndvi)
      plot(ndvi, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))}
  }) 
  
  
  
  
  
  map_dem <- reactive({
    req(input$filemap2)
    shpdf_dem <- input$filemap2
    tempdirname_dem <- dirname(shpdf_dem$datapath[1])
    
    for (i in 1:nrow(shpdf_dem)) {
      file.rename(
        shpdf_dem$datapath[i],
        paste0(tempdirname_dem, "/", shpdf_dem$name[i])
      )
    }
    
    map_dem <- read_sf(paste(tempdirname_dem,
                             shpdf_dem$name[grep(pattern = "*.shp$",shpdf_dem$name)],
                             sep = "/"
                             
                             
    ))
    
    
    
    
    
  })
  
  
  
  
  
  my_table <-  reactive({
    DEM_request(map_dem()) %>%
      select(year,format,resolution,
             CRS,filename,product,seriesID, URL) %>%
      filter(product %in% c("DTM", "DSM")) %>% 
      arrange(desc(year))
    
  })
  
  
  
  
  
  output$tabledem <- DT::renderDataTable({
    my_table() 
  })
  
  
  
  
  nmt_img <- reactive({
    
    files = dir("./")
    my_rows = input$tabledem_rows_selected
    table_selected = my_table()[my_rows,]
    real_selected = filter(my_table(), seriesID == table_selected$seriesID)
    
    if(str_sub(real_selected$URL, start = -4, end = -1)[1] == ".zip"){
      print("---------------------------------------------------")
      my_dir = tempdir()
      tile_download(real_selected, outdir = my_dir)
      files_zip = dir(my_dir)
      sheetnames = real_selected$sheetID
      
      pattern = c(paste0(sheetnames, collapse="|")) 
      index = grep(pattern, files_zip)
      dem_filenames = files_zip[index]
      sheetnames_p = c()
      for(i in dem_filenames){
        if(str_sub(i, start = -5) == 'p.asc'){
          sheetnames_p = c(sheetnames_p,i)
        }
      }
      setwd(my_dir)
      
      read_asc = function(x){
        types = rep("numeric", 3)
        read.table(x, colClasses = types ,col.names = c("X", "Y", "Z") )
      }
      
      my_rast = function(x){
        y = terra::rast(x, crs = "EPSG:2180")
        y
      }
      
      # img_dem_zip = lapply(sheetnames_p, read_asc)
      # img_dem_zip_rast = lapply(img_dem_zip, my_rast)
      # terra::plot(img_dem_zip_rast[[1]], col = terrain.colors(99, alpha = NULL))
      # img_dem_zip_rast2 = do.call(terra::mosaic, img_dem_zip_rast)
      # terra::plot(img_dem_zip_rast2)
      
      
      
      if (length(dem_filenames) > 1){
        
        img_dem = lapply(sheetnames_p, read_asc)
        img_dem = lapply(img_dem, my_rast)
        #img_dem = do.call(terra::mosaic, img_dem)
        img_dem = img_dem[[1]]
      }
      else if(length(dem_filenames) == 1){
        
        img_dem = read_asc(dem_filenames)
        img_dem = my_rast(img_dem)
      }
      
      dem_temp = map_dem()
      terra::crs(img_dem) = "EPSG:2180"
      crs_dem = terra::crs(img_dem)
      if(st_crs(dem_temp) != crs_dem){
        dem_temp = st_transform(dem_temp, crs_dem)
      }
      
      img_dem = terra::crop(img_dem,dem_temp)
    }
    
    
    else{
      if(paste0(real_selected$filename,".asc") %nin%  files){
        tile_download(real_selected)}
      dem_filenames = paste0(real_selected$filename,".asc")}
    
    if (length(dem_filenames) > 1){
      img_dem = lapply(dem_filenames, read_stars)
      img_dem = do.call(st_mosaic, img_dem)}
    else if(length(dem_filenames) == 1){img_dem = read_stars(dem_filenames)}
    dem_temp = map_dem()
    st_crs(img_dem) = 2180
    if(st_crs(dem_temp) != st_crs(img_dem)){
      dem_temp = st_transform(dem_temp, st_crs(img_dem))
    }
    img_dem = st_crop(img_dem , dem_temp)
    
  })
  
  
  
  
  
  output$myshapefile2 <- renderPlot({
    plot(nmt_img(),col = terrain.colors(99, alpha = NULL), main = "NMT")
    
  })
  
  
  
  
  
  
  
  crud <- callModule(editMod, "test-edit", m, "breweries")
  my_polygon <- reactive({
    req(crud()$finished)
  })
  
  my_tableorthomap <- reactive({
    ortho_request(my_polygon())
  })
  
  output$tableorthomap <- DT::renderDataTable({
    my_tableorthomap() %>% arrange(desc(year)) %>% 
      select(year,resolution,composition,
             CRS,URL,filename,seriesID)
    
  })
  my_select = reactive({input$tableorthomap_rows_selected})
  table_select = reactive({my_tableorthomap()[my_select(),]})
  real_select = reactive({filter(my_tableorthomap(), seriesID == table_select()$seriesID)})
  
  
  
  my_raster_map <- reactive({
    #my_select = input$tableorthomap_rows_selected
    #table_select = my_tableorthomap()[my_select,]
    #real_select = filter(my_tableorthomap(), seriesID == table_select$seriesID)
    
    if(paste0(real_select()$filename,".tif") %nin%  files){
      tile_download(real_select())
    }
    filenames_map = paste0(real_select()$filename,".tif")
    
    if (length(filenames_map) > 1){
      img_map = lapply(filenames_map, read_stars)
      img_map = do.call(st_mosaic, img_map)
    }
    else if(length(filenames_map) == 1){
      img_map = read_stars(filenames_map)
    }
    orthomap_temp = my_polygon()
    #st_crs(img_map) = 2180
    if(st_crs(orthomap_temp) != st_crs(img_map)){
      orthomap_temp = st_transform(orthomap_temp, st_crs(img_map))
    }
    img_map = st_crop(img_map,orthomap_temp)
    img_map
    
  })
  
  output$mapedit_finished <- renderPlot({
    plot(my_raster_map(), rgb = c(1, 2, 3), main = "aaa")
  })
  
  output$mapedit_ndvi <- renderPlot({
    if(real_select()$composition == "CIR"){
      calc_ndvi = function(img) {(img[1] - img[2]) / (img[1] + img[2])}
      ndvi_map = st_apply(my_raster_map(), MARGIN = c("x", "y"), FUN = calc_ndvi)
      plot(ndvi_map, main = "NDVI", col = hcl.colors(10, palette = "RdYlGn"))}
    
    
  }) 
  
}




crud <- callModule(editMod, "test-edit", m, breweries)
my_polygon2 <- reactive({
  req(crud$finished)
})

my_tabledem_map <- reactive({
  DEM_request(my_polygon2()) %>%
    select(year,format,resolution,
           CRS,filename,product,seriesID, URL) %>%
    filter(product %in% c("DTM", "DSM")) %>% 
    arrange(desc(year))
})

output$tabledemmap <- DT::renderDataTable({
  my_tabledem_map() 
})

my_select_map = reactive({input$tabledemmap_rows_selected})
table_select_map = reactive({my_tabledemmap()[my_select_map(),] })
real_select_map = reactive({filter(my_tabledem_map(), seriesID == table_select_map()$seriesID)})



my_dem_map <- reactive({
  
  
  if(paste0(real_select_map()$filename,".asc") %nin%  files){
    tile_download(real_select_map())
  }
  dem_filenames_map = paste0(real_select_map()$filename,".asc")
  
  if (length(dem_filenames_map) > 1){
    dem_map = lapply(dem_filenames_map, read_stars)
    dem_map = do.call(st_mosaic, dem_map)
  }
  else if(length(dem_filenames_map) == 1){
    dem_map = read_stars(dem_filenames_map)
  }
  temp_poly_dem = my_polygon2()
  #st_crs(img_map) = 2180
  if(st_crs(temp_poly_dem) != st_crs(dem_map)){
    temp_poly_dem = st_transform(temp_poly_dem, st_crs(dem_map))
  }
  dem_map = st_crop(dem_map,temp_poly_dem)
  dem_map
  
})

output$mapedit_dem <- renderPlot({
  plot(my_dem_map(),col = terrain.colors(99, alpha = NULL), main = "NMT")
})






shinyApp(server = server, ui = ui)









