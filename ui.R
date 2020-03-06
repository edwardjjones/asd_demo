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
      numericInput("scan_res", "Scan resolution (cm)", value =5 ,min=5,max=10, step=5),
      checkboxInput("do_epo", "Apply EPO? (moist condition)", 
                         value = T)#,
      #actionButton("goButton", "Go!")
    ),
    
    mainPanel(
      plotOutput("Plot")
    )
  )
))
