
biketimeline = data.frame(Year = 2012:2019, 
                      Events = c(NA,"Citi Bike","Vision Zero",rep(NA,times = 5)))
timeline = data.frame(Year = 2012:2019, 
                          Events = c(NA,NA,"Vision Zero",rep(NA,times = 5)))
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

    # Overview1 reactive ####
    # pick the dataframe to use by borough
    # select only incidents resulting in ped and cyc injuries and deaths  
    IFCollision <- reactive({
        switch (input$boroS, 
                'Bronx' = Xcrash,
                'Brooklyn' = Kcrash,
                'Manhattan' = Mcrash,
                'Queens' = Qcrash,
                'Staten Island' = Scrash) -> borocrash
        borocrash  %>% filter(., NUMBER.OF.PEDESTRIANS.INJURED >0 |
                                  NUMBER.OF.PEDESTRIANS.KILLED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Pedestrians = n()) -> pedcrash
        borocrash  %>% filter(., NUMBER.OF.CYCLIST.INJURED >0 |
                                  NUMBER.OF.CYCLIST.KILLED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Cyclists = n()) -> cyccrash
        borocrash  %>% filter(., NUMBER.OF.PEDESTRIANS.KILLED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Pedestrian_Fatalities = n()) -> peddeath
        borocrash  %>% filter(., NUMBER.OF.CYCLIST.KILLED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Cyclist_Fatalities = n()) -> cycdeath
        inner_join(pedcrash, peddeath, by = 'YEAR') %>%
            inner_join(., cyccrash, by = 'YEAR') %>% 
            inner_join(., cycdeath, by = 'YEAR') %>%
            rename(., Year = YEAR)
    })
    
    # Overview2 reactive ####
    # pick the dataframe to use by borough
    # select all incidents    
    AllCollision <- reactive({
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
                                  NUMBER.OF.PEDESTRIANS.KILLED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Pedestrians = n()) -> pedcrash
        borocrash  %>% filter(., NUMBER.OF.CYCLIST.INJURED >0 |
                                  NUMBER.OF.CYCLIST.KILLED > 0) %>% 
            group_by(., YEAR) %>% 
            summarise(., Cyclists = n()) -> cyccrash
        inner_join(totalcrash, pedcrash, by = 'YEAR') %>%
            inner_join(., cyccrash, by = 'YEAR') %>% 
            rename(., Year = YEAR)
    })

    # Overview Tab outputs ####
    # Collisions involing injuries and fatalities ####
    output$IFCollisions <- renderText({
        "Collisions Resulting in Injuries and Fatalities"
    })
    output$TimeLinePIF <-renderGvis({
        IFCollision() %>% inner_join(.,timeline, by = "Year") %>% 
            rename(., Pedestrians.annotation = Events) %>% 
            mutate(.,Year = as.character(Year)) %>% 
            select(., c("Year", "Pedestrians", "Pedestrians.annotation", "Pedestrian_Fatalities"))->
            IFC
        gvisLineChart(
            data = IFC, xvar = "Year", 
            yvar = c("Pedestrians", "Pedestrians.annotation", "Pedestrian_Fatalities"),
            options = list(legend = 'bottom', focusTarget = 'category',
                           title = "Pedestrian Injuries and Fatalities",
                           annotations = "{style:'line'}",
                           series = "[{targetAxisIndex:0, color:'red'},
                           {targetAxisIndex:1, color:'blue'}]",
                           vAxes ="[{title:'Injuries'}, {title:'Fatalities'}]")
        )
    })
    output$TimePercentPIF <-renderGvis({
        IFCollision() %>% 
            mutate(.,Year = as.character(Year), Fatality_Rate = Pedestrian_Fatalities/Pedestrians,
                   Fatality_Rate.tooltip = paste0(round(Fatality_Rate*100,2),'%')) ->IFC
        gvisColumnChart(
            data = IFC, xvar = "Year", 
            yvar = c("Fatality_Rate","Fatality_Rate.tooltip"),
            options = list(legend = 'none',
                           title = "Pedestrian Accident Fatalities Rate",
                           focusTarget = 'category',
                           vAxis = "{format:'#,###.#%'}")
        )
    })
    output$TimeLineCIF <-renderGvis({
        IFCollision() %>% inner_join(.,biketimeline, by = "Year") %>%
            rename(., Cyclist.annotation = Events) %>% 
            mutate(.,Year = as.character(Year))->
            IFC
        gvisLineChart(
            data = IFC, xvar = "Year", 
            yvar = c("Cyclists", "Cyclist.annotation", "Cyclist_Fatalities"),
            options = list(legend = 'bottom', focusTarget = 'category',
                           title = "Cyclist Injuries and Fatalities",
                           annotations = "{style:'line'}",
                           series = "[{targetAxisIndex:0, color:'red'},
                           {targetAxisIndex:1, color:'blue'}]",
                           vAxes ="[{title:'Injuries'}, {title:'Fatalities'}]")
        )
    })

    output$TimePercentCIF <-renderGvis({
        IFCollision() %>% 
            mutate(.,Year = as.character(Year), Fatality_Rate = Cyclist_Fatalities/Cyclists,
                   Fatality_Rate.tooltip = paste0(round(Fatality_Rate*100,2),'%')) ->IFC
        gvisColumnChart(
            data = IFC, xvar = "Year", 
            yvar = c("Fatality_Rate","Fatality_Rate.tooltip"),
            options = list(legend = 'none', 
                           title = "Cyclist Injuries and Fatalities Rate",
                           vAxis = "{format:'#,###.#%'}", 
                           focusTarget = 'category')
        )
    })
    
    # All collisions ####
    output$AllCollisions <- renderText({
        "All Collisions"
    })
    output$TimeLineA <-renderGvis({
        AllCollision() %>% inner_join(.,biketimeline, by = "Year") %>%
            rename(., Cyclist.annotation = Events) %>% 
            mutate(.,Year = as.character(Year))->
            IFC
        gvisLineChart(
            data = IFC, xvar = "Year", 
            yvar = c("Total", "Pedestrians", "Cyclists", "Cyclist.annotation"),
            options = list(legend = 'bottom', focusTarget = 'category',
                           annotations = "{style:'line'}",
                           title = "All Traffic Accidents")
        )
    })
    output$TimePercentA <-renderGvis({
        AllCollision() %>% 
            mutate(.,Year = as.character(Year), 
                   Pedestrian_Rate = Pedestrians/Total, 
                   Pedestrian_Rate.tooltip = paste0(round(Pedestrian_Rate*100,2),'%'),
                   Cyclist_Rate = Cyclists/Total,
                   Cyclist_Rate.tooltip = paste0(round(Cyclist_Rate*100,2),'%')) ->IFC
        gvisLineChart(
            data = IFC, xvar = "Year", 
            yvar = c("Pedestrian_Rate", "Pedestrian_Rate.tooltip", 
                     "Cyclist_Rate", "Cyclist_Rate.tooltip"),
            options = list(legend = 'bottom', 
                           focusTarget = 'category',
                           vAxis = "{format:'#,###.#%'}",
                           title = "Accident Rates Involving Pedestrians and Cyclists")
        )
    })
    # Map Tab outputs ####
    output$MainMap <- renderLeaflet({
        leaflet() %>% addTiles() %>% addProviderTiles('OpenMapSurfer.Roads') %>%
            addCircles(data = markthese(), stroke = 0.01, fillOpacity = 0.3,
                       weight = 0.1, radius = markthese()$NUMBER.OF.CYCLIST.INJURED*100 +
                           markthese()$NUMBER.OF.PEDESTRIANS.INJURED*150 +
                           markthese()$NUMBER.OF.CYCLIST.KILLED*500 +
                           markthese()$NUMBER.OF.PEDESTRIANS.KILLED*750,
                       color = "Red"
            ) 
    })

    # Improvement and priority zone ####
    observeEvent({input$PZtypeM
                   input$injtypeM}, {
        proxy <- leafletProxy('MainMap')
        if ('BPZ' %in%input$PZtypeM) {
            proxy %>%
                addPolygons(data = bikepriority, layerId = LETTERS[1:10],
                             weight = 1, color = 'blue')
        } else{
            proxy %>%
                removeShape(layerId = LETTERS[1:10])
        }
        if ('EC' %in%input$PZtypeM) {
            EnhancedCrossing %>% 
                filter(.,as.Date(Date_Imple,'%m/%d/%Y') < 
                           as.Date(paste0(input$yearM,'/01/01'))) -> 
                ECC
            proxy %>%
                addCircles(data = ECC, layerId = paste0("E",(1:184)),
                            weight = 5, color = 'green', opacity = 10)
        } else{
            proxy %>%
                removeShape(layerId = paste0("E",(1:184)))
        }
        if ('SSS' %in%input$PZtypeM) {
            proxy %>%
                addPolygons(data = senior, layerId = paste0("S",(1:41)),
                            weight = 1, color = 'blue', opacity = 0.5)
        } else{
            proxy %>%
                removeShape(layerId = paste0("S",(1:41)))
        }
        if ('NSZ' %in%input$PZtypeM) {
            proxy %>%
                addPolygons(data = neighborhood, layerId = paste0("N",(1:28)),
                            weight = 1, color = 'blue')
        } else{
            proxy %>%
                removeShape(layerId = paste0("N",(1:28)))
        }

    })
    # bike lanes ####
    observeEvent(input$bikelane, {
        proxy <- leafletProxy('MainMap')
        if (input$bikelane) {
            proxy %>%
                addPolylines(data = bikepathgeo, layerId = 'bikelanelayer',
                             weight = 1, color = 'blue')
        } else{
            proxy %>%
                removeShape(layerId = 'bikelanelayer')
        }
    })
    
    
    # Causes Tab outputs ####
    output$TotalIncidents <- renderText({
        graphthese() %>% summarise(., sum(n())) -> showthis
        c("Number of incidents:",
          as.data.frame(showthis)[1,1]
        )
    })
    output$AccidentTypes <-renderGvis({
        graphthese() %>%group_by(category) %>% 
            summarise( total = sum(n())) -> piedata
        gvisPieChart(
            data = piedata, labelvar = "category", numvar = "total",
            options = list(legend = 'labeled', pieSliceText = 'none',
                           tooltip = "{ignoreBounds:'true',isHtml:'true'}",
                           title = "Accident Factors")
        )
    })

    output$LeadingCauses <- renderGvis({
        graphthese()%>% group_by(., AccFactorVal) %>% 
            summarise(., No_Accidents = n()) %>% 
            arrange(., desc(No_Accidents)) %>% top_n(., 5) -> top5_acc
        gvisColumnChart(
            data = top5_acc, xvar = 'AccFactorVal', 
            yvar = 'No_Accidents', 
            options = list(legend= 'none', 
                           title = "Leading Causes of Accidents")
        )
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
    

})
#?addPolylines
