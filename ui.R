borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Island")
yearchoice = sort(unique(Scrash$YEAR))

shinyUI(fluidPage(#theme = shinytheme('journal'),
navbarPage("NYC Vision Zero",
    tabPanel("Introduction",
        tabsetPanel(type = "pills",
            tabPanel("History", #use h5 or smaller
                     "Vision Zero is a program implemented in 2014 aimed at 
                     reduinge the number of traffic related fatalities New York 
                     City. Its goal to eliminate all deaths and injuries from the 
                     streets by 2024, through improved legislation, enforcement 
                     and street design. Use this app to navigate the different efforts
                     and to explore the various neighborhoods affected."
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
                     selectizeInput(inputId = "boroM",label = "Select Borough",
                                    choices = borochoice,
                                    selected = "Brooklyn"),
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
