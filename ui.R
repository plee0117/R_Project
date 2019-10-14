borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")
yearchoice = sort(unique(Scrash$YEAR))

shinyUI(fluidPage(#theme = shinytheme('journal'),
navbarPage("NYC Vision Zero",
    tabPanel("Introduction",
        tabsetPanel(type = "pills",
            tabPanel("History", #use h5 or smaller
                     "Vision Zero is program implemented in 2014 aimed to reduce the 
                     number of traffic related fatalities New York City. "
                     ),
            tabPanel("Overview",
                     selectizeInput(inputId = "boroS",label = h5("Select Borough"),
                                    choices = borochoice,
                                    selected = "Brooklyn"),
                     fluidRow(align = 'center',
                              h4(textOutput('IFCollisions'))
                     ),
                     htmlOutput(align = 'center', 'TimeLinePIF'),
                     htmlOutput(align = 'center', 'TimePercentPIF'),
                     htmlOutput(align = 'center', 'TimeLineCIF'),
                     htmlOutput(align = 'center', 'TimePercentCIF'),
                     fluidRow(align = 'center',
                        h4(textOutput('AllCollisions'))
                     ),
                     htmlOutput(align = 'center', 'TimeLineA'),
                     htmlOutput(align = 'center', 'TimePercentA')


                     ),
            tabPanel("Author",
                     fluidRow(h2("")),
                     fluidRow(align = 'center', 
                              a(img(src = 'GitHub-Mark-120px-plus.png',
                                    width = '30'), 
                                href = 'https://github.com/plee0117')),
                     fluidRow(h2("")),
                     fluidRow(align = 'center', 
                              a(img(src = 'LI-In-Bug.png', width = '30'),
                                href = 'https://www.linkedin.com/in/paul-l-078408178/'))
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
    ),

    tabPanel("Causes",
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
                         h1(textOutput(""))
                ),
                htmlOutput('LeadingCauses')
            )
        )
    )
)
))
