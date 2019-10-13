borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")
yearchoice = sort(unique(Scrash$YEAR))

shinyUI(fluidPage(#theme = shinytheme('journal'),
navbarPage("NYC Vision Zero",
    tabPanel("Introduction",
        tabsetPanel(type = "pills",
            tabPanel("History"
                     #use h5 or smaller
                     ),
            tabPanel("Summary",
                     selectizeInput(inputId = "boroS",label = h5("Select Borough"),
                                    choices = borochoice,
                                    selected = "Brooklyn"),
                     fluidRow(align = 'center',
                        h4(textOutput('AllCollisions'))
                     ),
                     htmlOutput('TimeLineA'),
                     htmlOutput('TimePercentA'), 
                     fluidRow(align = 'center',
                              h4(textOutput('IFCollisions'))
                     ),
                     htmlOutput('TimeLineIF'),
                     htmlOutput('TimePercentIF')
                     ),
            tabPanel("About the Author"

                     )
        )
    ),
    tabPanel("Graphs",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "boroG",label = h5("Select Borough"),
                               choices = borochoice,
                               selected = "Brooklyn"),
                selectizeInput(inputId = 'yearG',label = h5("Select Year"),
                               choices = yearchoice,
                               selected = '2019'),
                checkboxGroupInput("injtypeG", label = h5("Injuries and Deaths"), 
                                   choices = list("Pedestrian" = 'pedinj', 
                                                  "Cyclist" = 'cycinj'),
                                   selected = c())
            ),
            mainPanel(
                
                fluidRow(align = 'left',
                    h5(textOutput("TotalIncidents"))
                ),
                htmlOutput('AccidentTypes'),
                fluidRow(align = 'center',
                         h5(textOutput("LCTitle"))
                ),
                htmlOutput('LeadingCauses')
            )
        )
    ),
    tabPanel("Maps",
        sidebarLayout(
            sidebarPanel(
                selectizeInput(inputId = "boroM",label = h5("Select Borough"),
                               choices = borochoice,
                               selected = "Brooklyn"),
                selectizeInput(inputId = 'yearM',label = h5("Select Year"),
                               choices = yearchoice,
                               selected = '2019'),
                checkboxGroupInput("accreasonM", label = h5("Accident Causes"), 
                                   choices = list("Human" = 'Human', 
                                                  "Vehicular" = 'Vehicular', 
                                                  "Environmental" = 'Environmental'),
                                   selected = c('Human','Vehicular','Environmental')),
                checkboxInput("bikelaneM", label = "Show Bikelanes", value = F),
                checkboxInput("bikePZM", label = "Show Bike Priority Zones", value = F),
                checkboxGroupInput("injtypeM", label = h5("Injuries and Deaths"), 
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
