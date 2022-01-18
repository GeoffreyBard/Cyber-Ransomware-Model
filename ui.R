#Introduction
required_packages <- c("data.table","gtools","ggplot2","shiny","shinyFiles","shinythemes","shinyWidgets","rmarkdown","shinycssloaders")
lapply(required_packages,library,character.only=T)

shinyUI(fluidPage(theme = shinytheme("flatly"),
                  #title of the application
                  titlePanel("Reinsurance Counterparty Risk: STEC Calculation"),
                  #creation of the navigation bar with different tabs in it

)#END TITLEPANEL
)#END SHINYUI

