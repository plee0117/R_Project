borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Islan")

shinyUI(fluidPage(

    titlePanel("Crashes"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "boro",label = "Select Borough",
                           choices = sort(unique(crashnona$BOROUGH)), selected = "BRONX"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        mainPanel(
            plotOutput("distPlot"),
            leafletOutput("mainmap")
        )
    )
))
