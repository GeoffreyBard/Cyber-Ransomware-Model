#Introduction
required_packages <- c("data.table","gtools","ggplot2","shiny","shinyFiles","shinythemes","shinyWidgets","rmarkdown","shinycssloaders")
lapply(required_packages,library,character.only=T)
#INTRO-----------------------------------------------This file allows us to place the visual elements of the application ----------------------------------------------------------------------------------------------------------------
#How does it work? 
#1/ Create a tab with tabPanel

#2/ fluidRow(Column()) allows to place elements of different size horizontally. 
#The Column function splits the application into 12 rows. So Column(6,) & Column(6,) allows to split the application in two.

#3/All elements like text, line breaks, images are often the result of an existing HTML/CSS command. 
#For example in HTML, the <p /p> tag is used to write text. In Rshiny, it will be p("").

#4/ Interactive elements, such as buttons, are to be visually created here, but their operation when clicked is derived from the server.R file

#5/Finally, there are plenty of commands, and Google remains your friend for that!

#AT THIS DAY WE HAVE 8 TABS, EACH ONE CORRESPONDS TO A TAB OF THE APPLICATION. /!\/!\/!\WARNING , the 7th tab is not available. It is only in comments /!\/!\/!\

#0CREATION---------------------------------------------------Creation of Interface. Each Tab will be separated by a big commentary-----------------------------------------------------#
#create the interface and set the theme


# A more complicated app with the flatly theme
shinyUI(fluidPage(
    theme = shinytheme("darkly"),
    navbarPage(id = "nav_tab","Applications : Modélisation de la propagation d'un ransomware",
    tabPanel("Multi-groupe SIR sur des portefeuilles",value = "home",
     sidebarLayout(
      sidebarPanel(
        h3("Paramètres épidémie"),
        checkboxGroupInput("vecSA", "Secteurs d'activité :",
                           c("Mines et carrières" = "D05T09",
                             "Fabrication" = "D10T33",
                             "Electricite, gaz, eaux et déchets" = "D35T39",
                             "Construction" = "D41T43",
                             "Commerce de gros et de détail" = "D45T47",
                             "Transport et stockage" = "D49T53",
                             "Hébergement et restauration" = "D55T56",
                             "Information et communication" = "D58T63",
                             "Activites immobilières" = "D68",
                             "Autres services" = "D69T82")),
        textInput("pop", "Nombre d'entreprises",value = "4064278"),  
        textInput("beta", "Valeur de Beta:",
                     value = 1.845*10^-5),
        selectInput("loigamma", "Loi de gamma :",
                    c("Exponentielle " = "exp",
                      "Pareto" = "pareto",
                      "Autre" = "autre")),
        sliderInput("times", "Duree de l'epidemie:",
                    value = 100, min = 0, max = 200),
        selectInput("variable", "Variable:",
                    c("Une infection (Aléatoire) " = "1",
                      "Une infection (Représentatif)" = "2",
                      "Lot d'embrassement (Aléatoire)" = "3",
                      "Lot d'embrassement (Représentatif) " = "4")),
      actionButton("app1", "Simulation"),
      width = 2),
      mainPanel(
        tabsetPanel(type = "tabs",
            tabPanel("Portfeuilles",
                     fluidRow(column(5,h2("3 types de portefeuilles pour un assureur : ")),column(2),column(5,sliderInput("pf", "Taille des portefeuilles : ", value = 10000, min = 5000, max = 50000))),
                     h4("Portefeuille A : "),
                     p("Le portefeuille A est un portefeuille uniformisé tant au niveau des pays, que des secteurs des activités. Il représenterait dans la réalité un échantillon des entreprises touchées par l'épidémie."),
                     h4("Portefeuille B : "),
                     p("Le portefeuille B est un portefeuille uniformisé au niveau des pays, mais complétement aléatoire pour les secteurs d'activités."),
                     h4("Portefeuille C : "),
                     p("Le portefeuille C est un portefeuille libre. Le paramètrage de ce dernier permet de le représenter de la façon que l'on veut."),
                     br(),
                     h3("Composition du portefeuille C :"),
                                  fluidRow(column(2,h4("Pays : ")),column(2,),column(4,h4("Secteur d'activités (en %) : "))),
                                  fluidRow(column(2,checkboxGroupInput("Listpays1", "",
                                                     c("Australia" = "Australia",
                                                       "Belgium" = "Belgium",
                                                       "Canada" = "Canada",
                                                       "Czech Republic" = "Czech Republic",
                                                       "France" = "France",
                                                       "Germany" = "Germany",
                                                       "Italy" = "Italy",
                                                       "Japan" = "Japan"))),
                                           column(2,checkboxGroupInput("Listpays2", "",
                                                                       c("Netherlands" = "Netherlands",
                                                                         "Poland" = "Poland",
                                                                         "Spain" = "Spain",
                                                                         "Sweden" = "Sweden",
                                                                         "Turkey" = "Turkey",
                                                                         "United Kingdom" = "United Kingdom",
                                                                         "United States" = "United States"))),
                                           column(2,textInput("ns1", "Mines et carrières",value = "0"),
                                           textInput("ns2", "Fabrication",value = "0"),  
                                           textInput("ns3", "Electricite, gaz, eaux et déchets",value = "0"),  
                                           textInput("ns4", "Construction",value = "0"),  
                                           textInput("ns5", "Commerce de gros et de détail",value = "0")),
                                           column(1,),
                                           column(2,textInput("ns6", "Transport et stockage",value = "0"),
                                                  textInput("ns7", "Hébergement et restauration",value = "0"),  
                                                  textInput("ns8", "Information et communication",value = "100"),  
                                                  textInput("ns9", "Activites immobilières",value = "0"),  
                                                  textInput("ns10", "Autres services",value = "0"))  
                                           
                                  ),
                     ),
          tabPanel("Graphique infection", verbatimTextOutput("summary")),
          tabPanel("Graphique Portefeuille", tableOutput("table"))
        ),width = 10
      )
    )
    ),
    tabPanel("Inputs Retreatments", value = "Inputs retreatments",
    )
   )
  )
)

# shinyUI(fluidPage(
#     
#   theme = shinytheme("darkly"),                #creation of the navigation bar with different tabs in it
#                   navbarPage(id = "nav_tab","Applications : Propagation d'un ransomware",
#                              #1-------------------------------------------------------------TAB HOME----------------------------------------------------------------------------------------------------------------
# 
#                              tabPanel("Multi-groupe SIR sur des portefeuilles",value = "home",
#                                       tags$style(HTML(".tabbable > .nav > li > a {background-color: black;  color:black}")),
#                                       tabsetPanel(
#                                         tabPanel("Parametrages",
#                                                  #Intra AXA id loading file (Help bouton), Output Folder find path, and choice of currency of entity
#                                           
#                                         ), #END OF TAB RO
#                                         
#                                         #NEW TAB UNDERWRITTING. IF U HAVE TO COMPLETE IT, Take inspiration from the code in the run off tab 
#                                         tabPanel("Graphique",
#                                         
#                                                  ),
#                                       ),
#                              ),
#                              
#                              #2-------------------------------------------------------------TAB INPUTS RETREATMENTS-----------------------------------------------------------------------------------------------------
#                              
#                              #Creation first tab "INPUTS RETRATMENTS" 
#                              tabPanel("Inputs Retreatments", value = "Inputs retreatments",
#                                       #Cretion title
#                                       fluidRow(
#                                         column(width=2),
#                                         column(
#                                           h4(p(strong("Inputs retreatment for the STEC calculation"),style="color:black;text-align:center")),
#                                           width=8,style="background-color:lavender;border-radius: 10px")
#                                       ),
#                                       br(),
#                                       #TEXT + FIG 1
#                                       fluidRow(
#                                         column(
#                                           h4(p(strong("Retreatment of the inputs",style="background-color:lavender;padding:5px;border-radius: 10px"))),
#                                           p('This tab is dedicated to the retreatment of your inputs : the exposures, the payment pattern, the term structure.'),
#                                           p('The paths you are supposed to fill is the raw extracts from IRIS.'),
#                                           p('On the fig.1, you have an illustration of the architecture you have to respect in order to make the retreatment work.'),
#                                           p('NB : It is mandatory to rename the exposure file and the payment pattern file with your entity name, as described in the architecture. You must save the file in the csv format (the separator and the decimal used do not matter).', style="color:red"),
#                                           width = 8
#                                         ),
#                                         column(4,
#                                                p(strong("Fig 1 : Illustration of the architecture")),
#                                                tags$img(src="path_retreatment.png",width="260px",height="181px")
#                                         )
#                                       ),
#                                       
#                                       hr(),
#                                       #COMMAND IN ORDER TO NAVIGATE BETWEEN UNDER TABS (RUN OFF & UNDERWRITTING)
#                                       tags$style(HTML(".tabbable > .nav > li[class=active]    > a {background-color: #BFF7BB; color:black}")),
#                                       
#                                       #Creation of First undertab RO
#                                       tabsetPanel(
#                                         tabPanel("Run-Off",
#                                                  #Intra AXA id loading file (Help bouton), Output Folder find path, and choice of currency of entity
#                                                  fluidRow(
#                                                    column(5,
#                                                           h4('Intra AXA Id'),
#                                                           fileInput("intra_id_rt_ro", "Select file for the intragroup Reinsurers (id_intra_axa.csv)",
#                                                                     multiple = FALSE,
#                                                                     width = '700px',
#                                                                     accept = c("text/csv",".xlsx",".xls", "text/comma-separated-values,text/plain",".csv")
#                                                           ),
#                                                           actionButton("ro_file_retreat_help", label = "Help")
#                                                    ),
#                                                    
#                                                    column(5,
#                                                           h4('Output folder'),
#                                                           fluidRow(
#                                                             column(2,
#                                                                    br(),
#                                                                    shinyDirButton("diroutput_file_retreat_ro", "  Browse...  ", "Upload",)
#                                                             ),
#                                                             column(10,
#                                                                    #to get a text input from the user. There is an id, a text to describe the entry and a style specification(optionnal)
#                                                                    textInput("output_file_retreat_ro","Enter Path for the output folder",width='700px'))
#                                                           )
#                                                    ),
#                                                    
#                                                    column(2,
#                                                           h4('Currency of entity'),
#                                                           selectInput("currency_list", "Select your currency", choices = c("CHF","CNY","EUR","GBP","HKD","JPY", "KRW","MXN","USD","TRY"))
#                                                    )
#                                                  ),
#                                                  #TITLE OF THE NEXT LINE
#                                                  hr(), 
#                                                  h4('Exposure folder'),
#                                                  
#                                                  #Select path Exposure folder and Button to run exposure retreatment
#                                                  fluidRow(
#                                                    column(1,
#                                                           br(),
#                                                           shinyDirButton("dirro_file_retreat", "  Browse...  ", "Upload",)
#                                                    ),
#                                                    column(6,
#                                                           #to get a text input from the user. There is an id, a text to describe the entry and a style specification(optionnal)
#                                                           textInput("ro_file_retreat_ro","Select folder with your entity exposures from IRIS",width='700px')
#                                                    ),
#                                                    column(3,
#                                                           br(),
#                                                           actionButton("Exposure_retreat", label = "Run Exposure retreatment")
#                                                    ),
#                                                    fluidRow(
#                                                      column(1,
#                                                             p(""),
#                                                             p("")
#                                                      ),
#                                                      column(8,
#                                                             div(withSpinner(uiOutput("retreat_expo_ro")),style="color:black;text-align:center;font-size:25px;")
#                                                      )
#                                                    ),
#                                                    
#                                                    
#                                                  ),
#                                                  #TITLE OF THE NEXT LINE
#                                                  hr(),
#                                                  h4('Payment Pattern file'),
#                                                  
#                                                  #Select path PP and Button to run PP
#                                                  fluidRow(
#                                                    column(1,
#                                                           br(),
#                                                           shinyDirButton("dirpp_file_retreat", "  Browse...  ", "Upload",)
#                                                    ),
#                                                    column(6,
#                                                           #to get a text input from the user. There is an id, a text to describe the entry and a style specification(optionnal)
#                                                           textInput("pp_file_retreat","Select folder with your entity payment pattern from IRIS",width='700px')
#                                                    ),
#                                                    column(3,
#                                                           br(),
#                                                           
#                                                           actionButton("pp_retreat", label = "Run Payment Pattern retreatment")
#                                                    ),
#                                                    fluidRow(
#                                                      column(1,
#                                                             p(""),
#                                                             p("")
#                                                      ),
#                                                      column(8,
#                                                             div(withSpinner(uiOutput("retreat_pp_ro")),style="color:black;text-align:center;font-size:25px;")
#                                                      )
#                                                    ),
#                                                    
#                                                  ),  
#                                                  
#                                                  #TITLE OF THE NEXT LINE
#                                                  hr(),
#                                                  h4('Term Structure file'),
#                                                  
#                                                  #Select path TS and Button to run TS
#                                                  fluidRow(
#                                                    column(6,
#                                                           #to get a text input from the user. There is an id, a text to describe the entry and a style specification(optionnal)
#                                                           fileInput("ts_file_retreat", "Select your excel file Market Consistent Economic Targets",
#                                                                     multiple = FALSE,
#                                                                     width = '700px',
#                                                                     accept = c("text/csv",".xlsx",".xls","text/comma-separated-values,text/plain",".csv")
#                                                           )
#                                                    ),
#                                                    column(6,
#                                                           br(),
#                                                           actionButton("ts_retreat", label = "Run Term Structure retreatment")
#                                                    ),
#                                                    fluidRow(
#                                                      column(1,
#                                                             p(""),
#                                                             p("")
#                                                      ),
#                                                      column(8,
#                                                             div(withSpinner(uiOutput("retreat_ts_ro")),style="color:black;text-align:center;font-size:25px;")
#                                                      )
#                                                    ),
#                                                  ),
#                                                  
#                                                  hr(),
#                                         ), #END OF TAB RO
#                                         
#                                         #NEW TAB UNDERWRITTING. IF U HAVE TO COMPLETE IT, Take inspiration from the code in the run off tab 
#                                         tabPanel("Underwritting",)
#                                       ), #END OF TWO TABS RO AND UNDERWRITTING
#                              ),#END OF TAB
#                              
# 
#                              #9-------------------------------------------------------------REST OF THE PARANTHESES----------------------------------------------------------------------------------------------------------------
#                              
#                   )#END NAVBARPAGE
# )#END TITLEPANEL
# )#END SHINYUI
