borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")

shinyUI(fluidPage(theme = shinytheme('journal'),
navbarPage("NYC Vision Zero",
    tabPanel("Introduction",
        tabsetPanel(type = "pills",
            tabPanel("History",
                     #use h4 or smaller
                     ), 
            tabPanel("Summary",
                     
                     ),
            tabPanel("About the Author",
                     
                     )
        )
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
                checkboxGroupInput("injtypeG", label = h4("Injuries and Deaths"), 
                                   choices = list("Pedestrian" = 'pedinj', 
                                                  "Cyclist" = 'cycinj'),
                                   selected = c())
            ),
            mainPanel(
                htmlOutput('TimeLine'),
                htmlOutput('TimePercent'),
                h3(textOutput("TotalIncidents")),
                htmlOutput('AccidentTypes'),
                htmlOutput('MainGraph')
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
                                   choices = list("Human" = 'Human', 
                                                  "Vehicular" = 'Vehicular', 
                                                  "Environmental" = 'Environmental'),
                                   selected = c('Human','Vehicular','Environmental')),
                checkboxInput("bikelaneM", label = "Show Bikelanes", value = F),
                checkboxInput("bikePZM", label = "Show Bike Priority Zones", value = F),
                checkboxGroupInput("injtypeM", label = h4("Injuries and Deaths"), 
                                   choices = list("Pedestrian" = 'pedinj', 
                                                  "Cyclist" = 'cycinj'),
                                   selected = c())
            ),
            mainPanel(
                textOutput("acccount"),
                leafletOutput("MainMap")
            )
        )
    )
)
))
