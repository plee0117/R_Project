
shinyServer(function(input, output) {

    # Map Reactive ####
    markthese <- reactive({
        
        # pick the dataframe to use by borough and year
        switch (input$boroM, 
            'Bronx' = Xcrash,
            'Brooklyn' = Kcrash,
            'Manhattan' = Mcrash,
            'Queens' = Qcrash,
            'Staten Island' = Scrash) %>% 
            filter(., YEAR == input$yearM) %>% 
            pivot_longer(., cols =  c(CONTRIBUTING.FACTOR.VEHICLE.1, 
                                      CONTRIBUTING.FACTOR.VEHICLE.2,
                                      CONTRIBUTING.FACTOR.VEHICLE.3, 
                                      CONTRIBUTING.FACTOR.VEHICLE.4, 
                                      CONTRIBUTING.FACTOR.VEHICLE.5), 
                         names_to = 'AccFactor', values_to = 'AccFactorVal') %>% 
            filter(., AccFactorVal != '') %>% 
            filter(., AccFactorVal != 'Unspecified') %>% 
            inner_join(., AccReason, by = c('AccFactorVal' = 'reason'))-> 
            temporary
        
        # pick the type of accident causes
        if (length(input$accreasonM) > 0) {
            rbind(
                if ('Human' %in% input$accreasonM) {
                    temporary %>% filter(., category == 'Human')
                } else NULL,
                if ('Vehicular' %in% input$accreasonM) {
                    temporary %>% filter(., category == 'Vehicular')
                } else NULL,
                if ('Environmental' %in% input$accreasonM) {
                    temporary %>% filter(., category == 'Environmental')
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
    
    # Graph Reactive ####
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
        
        # pick top accident reasons
        temporary5 %>%  
            pivot_longer(., cols =  c(CONTRIBUTING.FACTOR.VEHICLE.1, 
                                      CONTRIBUTING.FACTOR.VEHICLE.2, 
                                      CONTRIBUTING.FACTOR.VEHICLE.3, 
                                      CONTRIBUTING.FACTOR.VEHICLE.4, 
                                      CONTRIBUTING.FACTOR.VEHICLE.5), 
                                      names_to = 'AccFactor', 
                                      values_to = 'AccFactorVal') %>% 
            filter(., AccFactorVal != '') %>% 
            filter(., AccFactorVal != 'Unspecified') %>% 
            inner_join(., AccReason, by = c('AccFactorVal' = 'reason')) ->
            temporary6
        temporary6
    })

    # Summary reactive ####
    # pick the dataframe to use by borough
    # select all incidents    
    SummaryTab <- reactive({
        switch (input$boroS, 
                'Bronx' = Xcrash,
                'Brooklyn' = Kcrash,
                'Manhattan' = Mcrash,
                'Queens' = Qcrash,
                'Staten Island' = Scrash) -> borocrash
        borocrash %>% 
            group_by(.,YEAR) %>% 
            summarise(., Total = n()) -> totalcrash
        borocrash  %>% filter(., NUMBER.OF.PEDESTRIANS.INJURED >0 |
                                  NUMBER.OF.PEDESTRIANS.INJURED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Pedestrians = n()) -> pedcrash
        borocrash  %>% filter(., NUMBER.OF.CYCLIST.INJURED >0 |
                                  NUMBER.OF.CYCLIST.INJURED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Cyclists = n()) -> cyccrash
        inner_join(totalcrash, pedcrash, by = 'YEAR') %>%
            inner_join(., cyccrash, by = 'YEAR') %>% 
            mutate(.,Year = as.character(YEAR))
    })    
    

    
    # Graph Tab outputs ####
    output$MainGraph <- renderGvis({
        graphthese()%>% group_by(., AccFactorVal) %>% 
            summarise(., No_Accidents = n()) %>% 
            arrange(., desc(No_Accidents)) %>% top_n(., 5) -> top5_acc
        gvisColumnChart(
            data = top5_acc, xvar = 'AccFactorVal', 
            yvar = 'No_Accidents', options = list(legend= 'none')
        )
    })
    output$TotalIncidents <- renderText({
        graphthese() %>% summarise(., sum(n())) -> showthis
        c("Total number of incidents",
           as.data.frame(showthis)[1,1]
           )
    })
    output$AccidentTypes <-renderGvis({
        graphthese() %>%group_by(category) %>% 
            summarise( total = sum(n())) -> piedata
        gvisPieChart(
            data = piedata, labelvar = "category", numvar = "total",
            options = list(legend = 'none')
        )
    })
    
    # SummaryTab outputs ####
    # All collisions ####
    output$AllCollisions <- renderText({
        "All Collisions"
    })
    output$TimeLineA <-renderGvis({
        gvisLineChart(
            data = SummaryTab(), xvar = "Year", 
            yvar = c("Total", "Pedestrians", "Cyclists"),
            options = list(legend = 'bottom', focusTarget = 'category')
        )
    })
    output$TimePercentA <-renderGvis({
        gvisColumnChart(
            data = SummaryTab(), xvar = "Year", 
            yvar = c("Total", "Pedestrians", "Cyclists"),
            options = list(legend = 'none', isStacked = 'percent', 
                           focusTarget = 'category')
        )
    })
    # Collisions involing injuries and fatalities ####
    output$IFCollisions <- renderText({
        "Collisions Resulting in Injuries and Fatalities"
    })
    output$TimeLineIF <-renderGvis({
        SummaryTab() %>% 
        gvisLineChart(
            data = SummaryTab(), xvar = "Year", 
            yvar = c("Total", "Pedestrians", "Cyclists"),
            options = list(legend = 'bottom', focusTarget = 'category')
        )
    })
    output$TimePercentIF <-renderGvis({
        gvisColumnChart(
            data = SummaryTab(), xvar = "Year", 
            yvar = c("Total", "Pedestrians", "Cyclists"),
            options = list(legend = 'none', isStacked = 'percent', 
                           focusTarget = 'category')
        )
    })
    
    # Map Tab outputs ####
    output$MainMap <- renderLeaflet({
        leaflet() %>% addTiles() %>% addProviderTiles('OpenMapSurfer.Roads') %>%
            addCircles(data = markthese(),
                       weight = 1, radius = markthese()$NUMBER.OF.CYCLIST.INJURED*200 +
                           markthese()$NUMBER.OF.PEDESTRIANS.INJURED*250 +
                           markthese()$NUMBER.OF.CYCLIST.KILLED*1000 +
                           markthese()$NUMBER.OF.PEDESTRIANS.KILLED*1250,
                       color = "Red"
            ) 
    })
    # bike priority zone ####
    observeEvent({input$bikePZM
                   input$injtypeM}, {
        proxy <- leafletProxy('MainMap')
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
        c(input$boroM,input$yearM,input$pedinjM,input$bikeinjM,nrow(boroname())#,
          #markthese() %>% summarise(., n())[[1]][1]
        )
    })
    #testing elements up to here    

    # observeEvent(input$bikelane, {
    #     proxy <- leafletProxy('MainMap')
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
