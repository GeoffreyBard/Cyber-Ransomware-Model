#Introduction
required_packages <- c("data.table","gtools","ggplot2","shiny","shinyFiles","shinythemes","shinyWidgets","rmarkdown","shinycssloaders")
lapply(required_packages,library,character.only=T)

path_home <- dirname(rstudioapi::getSourceEditorContext()$path) #Path

#File neccesary
valueadd <- paste0(path_home,"/DatabaseApres/valueadd.csv") 
ocde <- paste0(path_home,"/DatabaseApres/ocde.csv")
# Loading other scripts neccesary
source(paste0(path_home,"/dds.R"))

#INTRO-----------------------------------------------This file allows us tto create the dynamism of the different elements like the dynamism of a button-----

#FUNCTION DYNAMISM
shinyServer(function(input, output,session) {
  eventReactive(input$app1, {
    print("NIQUE TA MERE")}
    )

})#END OF SERVER
