library(shiny)
library(shinythemes)
library(mapview)
library(mapedit)
library(sf)

morasko = st_point(c(16.942, 52.464))
morasko = st_sfc(morasko, crs = 4326)
m = mapview(morasko)@map
testsf = NULL



ui <- navbarPage(
      
    "map",
    tabPanel(
      "first",
      sidebarPanel(
        tagList(h2("Draw"),
                editModUI("test-edit"),
                leafletOutput("edited"))),
        mainPanel(
          plotOutput(outputId = "plot1"))),
          
      tabPanel(
        "second",
        sidebarPanel(
          tagList(h2("Draw"),
                  editModUI("test-edit2"),
                  leafletOutput("edited2"))),
          mainPanel(
            plotOutput(outputId = "plot2")
          )        
        )
      )

server <- function(input,output){
  crud <- callModule(editMod, "test-edit", m, "breweries")
  my_polygon <- reactive({
    req(crud()$finished)
  })
  
  output$plot1 <- renderPlot({
    plot(my_polygon())
    
  })
  
  crud2 <- callModule(editMod, "test-edit2", m, "breweries")
  my_polygon2 <- reactive({
    req(crud2()$finished)
  })
  
  output$plot2 <- renderPlot({
    plot(my_polygon2())
    
  })
  
  
  
  
}

shinyApp(server = server, ui = ui)