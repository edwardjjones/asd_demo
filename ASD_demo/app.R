server = shinyServer(function(input, output, session) {
  nir<<-c()
  cols<<-c()
  predict_c<<-c()
  predict_clay<<-c()
  i<<-0
  
  setwd("..")
  dirs<<-paste0(getwd(),"/scans")
  #write(1, file = "Scans/data")
  ##identify if new file added to folder
  ##process files as they are inputted
  ##create pdf when scans finished 
  ##move files to new folder with outputted pdf
  
  # ============================================================
  # This part of the code monitors the file for changes once
  # every four seconds.
  
  pollData <- reactivePoll(100, session,
                           # This function returns the time that the logfile was last
                           # modified
                           checkFunc = function() {
                             list.files(dirs) 
                           },
                           # This function returns the content of the logfile
                           valueFunc = function() {
                             if(identical(list.files(dirs,full.names=T, pattern="asd$"), character(0))){
                               ""
                             }else{
                               as.numeric(get_spectra(tail(list.files(dirs,full.names=T, pattern="asd$"),1)))
                             }
                           }
  )
  
  
  output$Plot <- renderPlot({
    tot_scans=(100/input$scan_res+1)
    #input$goButton
    #isolate({
    pars = c('plt','usr')
    par(xpd=NA)
    par(cex=1)
    par(cex.axis=1.5)
    par(cex.lab=1.8)
    par(cex.main=2)
    par(cex.sub=1)
    par(mar=c(5.1,5.1,1,2.1))
    
    # layout(matrix(c(1,2,3,4,4,4), 2, 3, byrow = TRUE),widths=c(1,3,3,3),heights=c(5,5,5,1))
    layout(matrix(c(4,4,4,1,2,3), 2, 3, byrow = TRUE),widths=c(1,3,3,3),heights=c(5,5))
    
    plot(NA,xlab="Colour",ylab="Depth (cm)", xaxt="n", yaxt="n", ylim=c(1,11),xlim=c(0,1), yaxs="i",xaxs="i")
    axis(2,at=1:11, labels=rev(seq(0,100,10)), las=1)
    par1 <- c(list(mfg=c(1,1)), par(pars))
    
    plot(NA,col="light grey",ylab="Depth (cm)",  yaxt="n", ylim=c(1,11),xlim=c(0,6), yaxs="i",xaxs="i", xlab="TC (%)")
    axis(2,at=1:11, labels=rev(seq(0,100,10)), las=1)
    par3 <- c(list(mfg=c(1,2)), par(pars))
    
    plot(NA,col="light grey",ylab="Depth (cm)",  yaxt="n", ylim=c(1,11),xlim=c(0,100), yaxs="i",xaxs="i", xlab="Clay (%)")
    axis(2,at=1:11, labels=rev(seq(0,100,10)), las=1)
    par4 <- c(list(mfg=c(1,3)), par(pars))
    
    plot(NA,type="l", lty=1, ylim=c(0,1), xlim=c(0,2151), yaxs="i",xaxs="i", xlab="Wavelength (nm)", ylab="Reflectance",xaxt="n")
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "light grey")
    axis(1,at=seq(51,2151,350), labels=seq(400,2500,350))
    par2 <- c(list(mfg=c(2,1)), par(pars))
    
    
    
    ##obsolete equation to use pollData() so that renderPlot is reactive to pollData()
    if(is.numeric(pollData())){cat(i+1,"/ ",tot_scans,"\n")}else{
      cat(i,"/ ",tot_scans,"\n")
    }
    
    if(!identical(list.files(dirs,full.names=T, pattern="asd$"), character(0))){
      
      nir<<-rbind(nir,pollData())
      
      ###change this to predict TC and clay
      if(nrow(nir)>0){
        do_epo<<-input$do_epo
        source(paste0(getwd(),"/ASD_demo/read_only/R_code/predict_TC_clay.R"))
      }
      
      
      i<<-nrow(nir)
      
      
      
      ##predict colour
      waves=350:2500
      rgb_col=c(r = mean(nir[i,which(waves%in%600:690)]),
                g = mean(nir[i,which(waves%in%520:600)]),
                b = mean(nir[i,which(waves%in%420:520)]))
      rgb_col=rgb_col*3.5
      rgb_col[rgb_col>1]=1
      cols<<-c(cols, rgb(rgb_col[1],rgb_col[2],rgb_col[3]))
      
      ##plot coloured polygon
      par(par1)
      for(j in 1:nrow(nir)){
        
        mid_pt=11-(j-1)*input$scan_res/10
        
        polygon(x=c(0,0,1,1), y=c(mid_pt - input$scan_res/20, mid_pt + input$scan_res/20,
                                  mid_pt + input$scan_res/20, mid_pt - input$scan_res/20),col=cols[j])  
      }
      
      ##Plot TC 
      par(par3)
      points(predict_c[1:i], seq(11,0,-(input$scan_res/10))[1:i], col="blue", pch=13, cex=2)
      
      ##Plot clay 
      par(par4)
      points(predict_clay[1:i], seq(11,0,-(input$scan_res/10))[1:i], col="red", pch=13, cex=2)
      
      ##Plot coloured spectra
      par(par2)
      matplot(1:ncol(nir), t(nir),col=cols, add=T, type="l", lty=1)
      
      if(i < tot_scans){
        par(par1)
        points(0.5,11-(input$scan_res/10)*i, col="red", pch=16, cex=2)
        par(par2)
        mtext(paste("Scan core at", i*input$scan_res ,"cm"), outer=T, line = -3,  cex = 1.5)
      }else{
        mtext("Exporting data...", outer=T, line = -3,  cex = 1.5)
        cat("Exporting data...\n")
        
        
        ##Export plot area
        site_id_export <<- input$site_id
        scan_res_export <<- input$scan_res
        source(paste0(getwd(),"/ASD_demo/read_only/R_code/export_pdf.R"))
        
        ##Export NIR spectra
        colnames(nir)=350:2500
        temp=data.frame("ID"=rep(input$site_id,nrow(nir)),
                        "upper_depth"=seq(0,100,length.out=tot_scans),
                        "lower_depth"=seq(0,100,length.out=tot_scans))
        nir=cbind(temp,nir)
        
        write.csv(nir,paste0(getwd(),"/out/",input$site_id,"_spectra.csv"),row.names = F)
        
        ##Export Prediction
        temp=data.frame(temp,"TC"=predict_c,"clay"=predict_clay)
        write.csv(temp,paste0(getwd(),"/out/",input$site_id,"_predictions.csv"),row.names = F)
        
        ##Delete ASD files
        file.remove(list.files(paste0(getwd(),"/scans"),full.names=T))
        
        ##Close app
        # stopApp(returnValue = invisible())
        
        ##reset i and nir
        nir<<- NULL
        i<<-0
        predict_c<<-NULL
        predict_clay<<-NULL
        cols<<-NULL
      }
    }
    #})
  }, height = 600, width = 600 )
})

title = tags$a(href="https://sydney.edu.au/agriculture/",
               tags$img(src="sia.jpg",height="100"),
               "NIR live demo.")

ui = shinyUI(fluidPage(
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

shinyApp(ui = ui,server = server)
