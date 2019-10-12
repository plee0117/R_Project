
shinyServer(function(input, output) {

    markthese <- reactive({
        
        # pick the dataframe to use by borough and year
        switch (input$boroM, 
            'Bronx' = Xcrash,
            'Brooklyn' = Kcrash,
            'Manhattan' = Mcrash,
            'Queens' = Qcrash,
            'Staten Island' = Scrash) %>% 
            filter(., YEAR == input$yearM) -> temporary
        
        # pick the type of accident causes
        if (length(input$accreasonM) > 0) {
            rbind(
                if ('ATH' %in% input$accreasonM) {
                    temporary %>% filter(., ATH == 'Y')
                } else NULL,
                if ('ATV' %in% input$accreasonM) {
                    temporary %>% filter(., ATV == 'Y')
                } else NULL,
                if ('ATE' %in% input$accreasonM) {
                    temporary %>% filter(., ATE == 'Y')
                } else NULL) %>% 
            unique() -> temporary2
        } else temporary -> temporary2
        
        # filter by the type of injuries and if nothing is selected -> all acc
        if ('pedinj' %in% input$injtypeM) {
            if ('cycinj' %in% input$injtypeM) {
                temporary2 %>% filter(., NUMBER.OF.CYCLIST.INJURED > 0 |
                                          NUMBER.OF.CYCLIST.KILLED > 0 |
                                          NUMBER.OF.PEDESTRIANS.INJURED > 0 |
                                          NUMBER.OF.PEDESTRIANS.KILLED > 0) -> 
                    temporary3
            } else{
                temporary2 %>% filter(., NUMBER.OF.PEDESTRIANS.INJURED > 0 |
                                          NUMBER.OF.PEDESTRIANS.KILLED > 0) ->
                    temporary3                
            }
        } else{
            if ('cycinj' %in% input$injtypeM) {
                temporary2 %>% filter(., NUMBER.OF.CYCLIST.INJURED > 0 |
                                          NUMBER.OF.CYCLIST.KILLED > 0) ->
                    temporary3
            }else{
                temporary2 -> temporary3
            }
        }
        temporary3
    })
    
    
    graphthese <- reactive({
        
        # pick the dataframe to use by borough and year
        switch (input$boroG,
                'Bronx' = Xcrash,
                'Brooklyn' = Kcrash,
                'Manhattan' = Mcrash,
                'Queens' = Qcrash,
                'Staten Island' = Scrash) %>% 
            filter(., YEAR == input$yearG) -> temporary4
        
        # filter by injury type to produce graphs
        if ('pedinj' %in% input$injtypeG) {
            if ('cycinj' %in% input$injtypeG) {
                temporary4 %>% filter(., NUMBER.OF.CYCLIST.INJURED > 0 |
                                          NUMBER.OF.CYCLIST.KILLED > 0 |
                                          NUMBER.OF.PEDESTRIANS.INJURED > 0 |
                                          NUMBER.OF.PEDESTRIANS.KILLED > 0) -> 
                    temporary5
            } else{
                temporary4 %>% filter(., NUMBER.OF.PEDESTRIANS.INJURED > 0 |
                                          NUMBER.OF.PEDESTRIANS.KILLED > 0) ->
                    temporary5
            }
        } else{
            if ('cycinj' %in% input$injtypeG) {
                temporary4 %>% filter(., NUMBER.OF.CYCLIST.INJURED > 0 |
                                          NUMBER.OF.CYCLIST.KILLED > 0) ->
                    temporary5
            }else{
                temporary4 -> temporary5
            }
        }
        
        # pick top 5 accident reasons
        temporary5 %>%   pivot_longer(., cols =  c(CONTRIBUTING.FACTOR.VEHICLE.1, 
                                                   CONTRIBUTING.FACTOR.VEHICLE.2,
                                                   CONTRIBUTING.FACTOR.VEHICLE.3, 
                                                   CONTRIBUTING.FACTOR.VEHICLE.4, 
                                                   CONTRIBUTING.FACTOR.VEHICLE.5), 
                                      names_to = 'AccFactor', values_to = 'AccFactorVal') %>% 
            filter(., AccFactorVal != '') %>% filter(., AccFactorVal != 'Unspecified') %>% 
            inner_join(., AccReason, by = c('AccFactorVal' = 'reason')) %>% 
            filter(., category == 'Human') %>% group_by(., AccFactorVal) %>% 
            summarise(., No_Acc = n()) %>% arrange(., desc(No_Acc)) %>% top_n(., 5) ->
            temporary6
        temporary6
    })
    
    output$mainmap <- renderLeaflet({
        leaflet() %>% addTiles() %>% addProviderTiles('OpenMapSurfer.Roads') %>%
             addCircles(data = markthese(),
                        weight = 1, radius = markthese()$NUMBER.OF.CYCLIST.INJURED*200 +
                            markthese()$NUMBER.OF.PEDESTRIANS.INJURED*250 +
                            markthese()$NUMBER.OF.CYCLIST.KILLED*1000 +
                            markthese()$NUMBER.OF.PEDESTRIANS.KILLED*1250,
                        color = "Red"
                        ) 
    })
    output$maingraph <- renderGvis({
        gvisColumnChart(
            data = graphthese(), xvar = graphthese()$AccFactorVal, yvar = No_Acc
        )
    })

    observeEvent({input$bikePZM
                   input$injtypeM}, {
        proxy <- leafletProxy('mainmap')
        if (input$bikePZM) {
            proxy %>%
                addPolygons(data = bikepriority, layerId = LETTERS[1:10],
                             weight = 1, color = 'blue')
        } else{
            proxy %>%
                removeShape(layerId = LETTERS[1:10])
        }
    })

    #testing elements from here
    boroname <- reactive({
        switch (input$boroM,
                'Bronx' = Xcrash,
                'Brooklyn' = Kcrash,
                'Manhattan' = Mcrash,
                'Queens' = Qcrash,
                'Staten Island' = Scrash) 
    })
    output$acccount <- renderText({
        c(input$boroM,input$yearM,input$pedinjM,input$bikeinjM,nrow(boroname()),
          markthese() %>% summarise(., n())[[1]][1]
        )
    })
    #testing elements up to here    

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
