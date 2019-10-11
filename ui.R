borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")

shinyUI(fluidPage(theme = shinytheme('journal'),
navbarPage("Crashes",
    tabPanel("Introduction"
    
    ),
    tabPanel("Graphs",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "boroG",label = "Select Borough",
                               choices = borochoice,
                               selected = "Brooklyn"),
                selectizeInput(inputId = 'yearG',label = "Select Year",
                               choices = sort(unique(Scrash$YEAR)),
                               selected = '2019'),
                checkboxInput("pedinjG", label = "Pedestrian Injuries", value = F),
                checkboxInput("bikeinjG", label = "Cyclist Injuries", value = F)
            ),
            mainPanel(
                htmlOutput('maingraph')
            )
        )    
    ),
    tabPanel("Maps",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "boroM",label = "Select Borough",
                               choices = borochoice,
                               selected = "Brooklyn"),
                selectizeInput(inputId = 'yearM',label = "Select Year",
                               choices = sort(unique(Scrash$YEAR)),
                               selected = '2019'),
                checkboxGroupInput("accreasonM", label = h4("Accident Causes"), 
                                   choices = list("Human" = 'ATH', 
                                                  "Vehicular" = 'ATV', 
                                                  "Environmental" = 'ATE'),
                                   selected = c('ATH','ATE','ATV')),
                checkboxInput("bikelaneM", label = "Show Bikelanes", value = F),
                checkboxInput("bikePZM", label = "Show Bike Priority Zones", value = F),
                checkboxGroupInput("injtypeM", label = h4("Injuries and Deaths"), 
                                   choices = list("Pedestrian" = 'pedinj', 
                                                  "Cyclist" = 'cycinj'),
                                   selected = c())
            ),
            mainPanel(
                textOutput("acccount"),
                leafletOutput("mainmap")
            )
        )
    )
    
)
))
