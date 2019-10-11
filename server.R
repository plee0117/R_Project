


shinyServer(function(input, output) {

    markthese <- reactive({
        switch (input$boro,
            'Bronx' = Xcrash,
            'Brooklyn' = Kcrash,
            'Manhattan' = Mcrash,
            'Queens' = Qcrash,
            'Staten Island' = Scrash) %>% 
            filter(., YEAR == input$year) -> temporary
        if (length(input$accreason) > 0) {
            rbind(
                if ('ATH' %in% input$accreason) {
                    temporary %>% filter(., ATH == 'Y')
                } else NULL,
                if ('ATV' %in% input$accreason) {
                    temporary %>% filter(., ATV == 'Y')
                } else NULL,
                if ('ATE' %in% input$accreason) {
                    temporary %>% filter(., ATE == 'Y')
                } else NULL) %>% 
            unique() -> temporary2
        } else temporary -> temporary2
        if (input$bikeinj) {
            temporary2 %>% filter(., NUMBER.OF.CYCLIST.INJURED > 0 |
                                      NUMBER.OF.CYCLIST.KILLED > 0) ->
                temporary3
        } else{
            temporary2 -> temporary3
        }
        if (input$pedinj) {
            temporary3 %>% filter(., NUMBER.OF.PEDESTRIANS.INJURED > 0 |
                                      NUMBER.OF.PEDESTRIANS.KILLED > 0) ->
                temporary4
        } else{
            temporary3 -> temporary4
        }
        temporary4
    })
    
    #testing elements from here
    boroname <- reactive({
        switch (input$boro,
                'Bronx' = Xcrash,
                'Brooklyn' = Kcrash,
                'Manhattan' = Mcrash,
                'Queens' = Qcrash,
                'Staten Island' = Scrash) 
    })
    output$acccount <- renderText({
        c(input$boro,input$year,input$pedinj,input$bikeinj,nrow(boroname()),
        markthese() %>% summarise(., n())[[1]][1]
        )
    })
    #testing elements up to here
    output$mainmap <- renderLeaflet({
            leaflet() %>% addTiles() %>%
             addCircles(data = markthese(),
                        weight = 1, radius = markthese()$NUMBER.OF.CYCLIST.INJURED*200 +
                            markthese()$NUMBER.OF.CYCLIST.KILLED*1000,
                        color = "Red"
                        ) 
    })
    observeEvent(input$bikePZ, {
        proxy <- leafletProxy('mainmap')
        if (input$bikePZ) {
            proxy %>%
                addPolygons(data = bikepriority, layerId = LETTERS[1:20],
                             weight = 1, color = 'blue')
        } else{
            proxy %>%
                removeShape(layerId = LETTERS[1:20])
        }
    })
    

    # observeEvent(input$bikelane, {
    #     proxy <- leafletProxy('mainmap')
    #     if (input$bikelane) {
    #         proxy %>%  
    #             addPolylines(data = bikepathgeo, layerId = 'bikelanelayer', 
    #                          weight = 1, color = 'blue')
    #     } else{
    #         proxy %>% 
    #             removeShape(layerId = 'bikelanelayer')
    #     }
    # })

})
#?addPolylines
