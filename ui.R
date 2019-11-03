borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")
yearchoice = sort(unique(Scrash$YEAR))

shinyUI(fluidPage(#theme = shinytheme('journal'),
navbarPage("NYC Vision Zero",
    tabPanel("Introduction",
        tabsetPanel(type = "pills",
            tabPanel("History", 
                     h2(""),#use h5 or smaller
                     fluidRow("Vision Zero is a New York City program 
                              implemented in 2014 aimed at reducing the number 
                              of traffic related fatalities."), 
                     fluidRow("Its goal is to eliminate all deaths and injuries 
                              from the streets by 2024, through improved 
                              legislation, community education and engagement, 
                              law enforcement, and street design."), 
                     fluidRow("Use this app to navigate the different efforts 
                              and to explore the various neighborhoods affected.")
                     ),
            tabPanel("Overview",
                     selectizeInput(inputId = "boroS",label = h5("Select Borough"),
                                    choices = borochoice,
                                    selected = "Bronx"),
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
                     fluidRow(align = 'center',
                              img(src = 'PaulLee1.jpg', width = '100')),
                     fluidRow(align = 'center',
                              h5("Paul (Kwang) Lee is a Data Science Fellow at NYC Data Science Academy")),
                     fluidRow(h2("")),
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
    tabPanel("Boroughs",
             sidebarLayout(
                 sidebarPanel(
                     selectizeInput(inputId = "boroM",label = "Select Borough",
                                    choices = borochoice,
                                    selected = "Bronx"),
                     selectizeInput(inputId = 'yearM',label = "Select Year",
                                    choices = yearchoice,
                                    selected = '2019'),
                     checkboxGroupInput("accreasonM", label = "Accident Causes", 
                                        choices = list("Human" = 'Human', 
                                                       "Vehicular" = 'Vehicular', 
                                                       "Environmental" = 'Environmental'),
                                        selected = c('Human','Vehicular','Environmental')),
                     checkboxGroupInput("PZtypeM", label = "Priority Zones and Improvements", 
                                        choices = list("Enhanced Crossings" = 'EC',
                                                       "Bike Priority Zones" = 'BPZ', 
                                                       "Neighborhood Slow Zones"='NSZ',
                                                       "Safe Streets for Seniors" = 'SSS'),
                                        selected = c()),
                     checkboxGroupInput("injtypeM", label = "Injuries and Deaths", 
                                        choices = list("Pedestrian" = 'pedinj', 
                                                       "Cyclist" = 'cycinj'),
                                        selected = c('pedinj')),
                     helpText('Note: If no injury type is selected, all accidents will be displayed.')
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
                                   selected = c('pedinj')),
                helpText('Note: If no injury type is selected, all accidents will be displayed.')
            ),
            mainPanel(
                
                fluidRow(align = 'left',
                    h5(textOutput("TotalIncidents"))
                ),
                htmlOutput('AccidentTypes'),
                fluidRow(align = 'center',
                         h1(textOutput(""))
                ),
                htmlOutput('LeadingCausesD'),
                infoBoxOutput('LeadingCausesC')
            )
        )
    )
)
))
