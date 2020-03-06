title = tags$a(href="https://sydney.edu.au/agriculture/",
               tags$img(src="sia.jpg",height="100"),
               "NIR live demo.")

shinyUI(fluidPage(
  
  
  
  titlePanel(title=title),
  
  sidebarLayout(
    sidebarPanel(
      textInput("site_id", "Site ID:", value = "Site 1"),
      textInput("lat", "Latitude (dd):", value = "-33.896154"),
      textInput("long", "Longitude (dd):", value = "151.196253"),
      selectInput("scan_res", "Scan resolution (cm)", choices=c(5,10), selected=5),
      checkboxGroupInput("properties", "Properties:", 
                         choices=c("TC","pH","CEC","Clay","Sand"), selected=c("TC","pH","CEC","Clay","Sand"))#,
      #actionButton("goButton", "Go!")
    ),
    
    mainPanel(
      plotOutput("Plot")
    )
  )
))
