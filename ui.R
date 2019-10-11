borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")

shinyUI(fluidPage(theme = shinytheme('journal'),
navbarPage("Crashes",
    tabPanel("Introduction"
    
    ),
    tabPanel("Tables",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "boro",label = "Select Borough",
                               choices = borochoice,
                               selected = "Brooklyn"),
                selectizeInput(inputId = 'year',label = "Select Year",
                               choices = sort(unique(Scrash$YEAR)),
                               selected = '2019'),
                checkboxInput("pedinjt", label = "Pedestrian Injuries", value = F),
                checkboxInput("bikeinjt", label = "Cyclist Injuries", value = F)
            ),
            mainPanel(
                plotOutput('graph')
            )
        )    
    ),
    tabPanel("Maps",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "boro",label = "Select Borough",
                               choices = borochoice,
                               selected = "Brooklyn"),
                selectizeInput(inputId = 'year',label = "Select Year",
                               choices = sort(unique(Scrash$YEAR)),
                               selected = '2019'),
                checkboxGroupInput("accreason", label = h4("Accident Causes"), 
                                   choices = list("Human" = 'ATH', 
                                                  "Vehicular" = 'ATV', 
                                                  "Environmental" = 'ATE'),
                                   selected = c('ATH','ATE','ATV')),
                checkboxInput("bikelane", label = "Show Bikelanes", value = F),
                checkboxInput("bikePZ", label = "Show Bike Priority Zones", value = F),
                checkboxGroupInput("injtype", label = h4("Injuries and Deaths"), 
                                   choices = list("Pedestrian" = 'pedinj', 
                                                  "Cyclist" = 'cycinj'),
                                   selected = c())
                # checkboxInput("pedinj", label = "Show Pedestrian Injuries", value = F),
                # checkboxInput("bikeinj", label = "Show Cyclist Injuries", value = T)
                
            ),
            mainPanel(
                textOutput("acccount"),
                leafletOutput("mainmap")
            )
        )
    )
    
)
))
