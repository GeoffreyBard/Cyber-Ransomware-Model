#Introduction
required_packages <- c("data.table","gtools","ggplot2","shiny","shinyFiles","shinythemes","shinyWidgets","rmarkdown","shinycssloaders","RColorBrewer","gridExtra","deSolve")
lapply(required_packages,library,character.only=T)

path_home <- dirname(rstudioapi::getSourceEditorContext()$path) #Path

#File neccesary
valueadd <- paste0(path_home,"/DatabaseApres/valueadd.csv") 
ocde <- paste0(path_home,"/DatabaseApres/ocde.csv")
# Loading other scripts neccesary
source(paste0(path_home,"/dds.R"))

#INTRO-----------------------------------------------This file allows us tto create the dynamism of the different elements like the dynamism of a button-----

#FUNCTION DYNAMISM
shinyServer(function(input, output) {
  
  observeEvent(input$app1,{

    Listpays <- c(input$Listpays1,input$Listpays2)
    nsx <- c(input$ns1,input$ns2,input$ns3,input$ns4,input$ns5,input$ns6,input$ns7,input$ns8,input$ns9,input$ns10)
    f1 <- application1(input$vecSA,input$pop,input$beta,input$loigamma,input$times,input$variable,input$pf,Listpays,nsx)
    output$plot1 <- renderPlot({
      grid.arrange(f1[[1]],f1[[2]])
    })
    output$plot2 <- renderPlot({
      f1[[6]]
    })
    # output$plot3 <- renderPlot({
    #   grid.arrange(f1[[5]],f1[[6]])
    # })
  })

})#END OF SERVER
