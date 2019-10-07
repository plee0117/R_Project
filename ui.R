borochoice = c('Bronx','Brooklyn','Manhattan','Queens',"Staten Islan")

shinyUI(fluidPage(

    titlePanel("Crashes"),

    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "boro",label = "Select Borough",
                           choices = sort(unique(crashnona$BOROUGH)),
                           selected = "BRONX"),
            selectizeInput(inputId = 'year',label = "Select Year",
                           choices = sort(unique(crashnona$YEAR)),
                           selected = '2019'),
            checkboxInput("bikelane", label = "Show Bikelanes", value = TRUE),
            #bikelane doesn't do anything yet
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(

            leafletOutput("mainmap")
        )
    )
))
