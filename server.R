
library(shiny)

shinyServer(function(input, output) {
    output$bikelane <- renderPrint({
        input$bikelane # doens't do anything yet
    })
    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    output$mainmap <- renderLeaflet({
        crashnona %>% filter(., BOROUGH == input$boro) %>% 
            filter(., YEAR == input$year) %>% 
            leaflet() %>% 
            addTiles() %>% addCircles(~LONGITUDE,~LATITUDE)
    })

})
