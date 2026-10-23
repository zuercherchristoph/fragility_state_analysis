output$fragility_exits_ui <- renderUI({
page_sidebar(

  sidebar = sidebar(width = 400, fillable_mobile = TRUE,
                    h3("Input",style = "background-color: #0d2b56;color: white !important; text-align: center;"),
                    selectInput(inputId="Data_input", label = "Input data", choices=datavariable), 
                    selectInput(inputId="Year_input", label="Reference period", choices =YearList$WGI), 
                    selectInput(inputId="PCA_input", label="Input indicators",choices=PCA_Varlist$WGI, multiple = TRUE, selected=default_indicators$WGI),
                    #hr(),
                    selectInput(inputId="Country_select_type", label="Country group", choices=geo_group),
                    selectInput(inputId="Country_select", label="Selected country", choices=Map_data$WGI$`Country.Territory`, selected =Map_data$WGI$`Country.Territory`[1] , width="100%"),

                   # hr(),
                    radioButtons(inputId="Percentile_type", label="Percentile type", choices=c("Fixed","Varying"), selected="Varying", inline = TRUE),
                    radioButtons(inputId="Index_type", label="Type of indicators", choices = c("Raw", "Composite"), selected="Composite", inline=TRUE),
                    selectInput(inputId="Output_indicator", label="Output indicator", choices=outputvariable, selected=default_output_indicator),
                    sliderInput(input="Percentile_Value", label="Sucess threshold (percentiles)", min=min(Yearlydata$Percent), max=max(Yearlydata$Percent), value=c(30,35), dragRange = TRUE, sep="",step=5, width="100%" ),
                    #hr(),
                    sliderInput(input="Criterium_Value", label="Success threshold (year)", min=1, max=length(YearList$WGI), value=5,  sep="",step = 5, width="100%" ),
                    
                    #hr(),
                    span(style="font-size:16px; font-weight:1000; color: white !important;", "Fragility Level (Map)"),
                    
                    sliderInput(input="Medium_Treshold", label="Very High Fragility", min=min(Yearlydata$Percent), max=max(Yearlydata$Percent), value=15,  sep="",step=5 , width="100%"),
                    sliderInput(input="High_Treshold", label="High Fragility", min=min(Yearlydata$Percent), max=max(Yearlydata$Percent), value=25,  sep="",step=5 , width="100%"),
                    sliderInput(input="Very_High_Treshold", label="Medium Fragility", min=min(Yearlydata$Percent), max=max(Yearlydata$Percent), value=35,  sep="",step=5, width="100%" ),
                    #hr()
                    
  ),

  fluidRow(
    h3("Output",style = "background-color: #0d2b56;color: white !important; text-align: center;"),
    column(6, uiOutput("Groupinfobox")), column(6, uiOutput("Mapbox")),
    column(6, uiOutput("PCAinfobox")),column(6, uiOutput("ExitTransBox")),
    h3("Table Output",style = "background-color: #0d2b56;color: white !important; text-align: center;"),
     column(12, uiOutput("SuccessTab"))
    #)
  )  
)
  
  
  
})




output$PCAinfobox <- renderUI({
  div(class = "graphclass",
      highchartOutput("PCAGraph"))
})
output$ExitTransBox <- renderUI({
  div(class = "graphclass",
      highchartOutput("TrendGraph"))
})

output$Mapbox <- renderUI({
  div(class = "graphclass", highchartOutput("GeoMap"))
})

output$Groupinfobox <- renderUI({
  div(class = "graphclass", highchartOutput("PieMap"))
})

output$SuccessTab <- renderUI({
  div(class = "graphclass",
      tabsetPanel(
        #tabPanel("Output Table",  dataTableOutput("Output_data")),
        tabPanel("Success Table", dataTableOutput("success_data")),
        tabPanel(
          "Fragility State Table",
          dataTableOutput("fragility_data")
        ),
        tabPanel("PCA Output Data", dataTableOutput("PCAoutputdata")),
        tabPanel("Percentile Data", dataTableOutput("Percoutputdata"))
      ))
  
})


indexdata <- reactive({
  temp <- Index_data[[input$Data_input]]
  
  return(temp)
})

mapdata <- reactive({
  temp <- Map_data[[input$Data_input]]
  return(temp)
})

InputYear <- reactive({
  temp <- YearList[[input$Data_input]]
  return(temp)
})

InputPACVar <- reactive({
  temp <- PCA_Varlist[[input$Data_input]]
  return(temp)
})

Percentile_type_reac <- reactive({
  if (input$Index_type == "Raw") {
    temp <- InputPACVar()
    temp[temp %in% input$PCA_input]
  } else{
    outputvariable
    
  }
})

observeEvent(input$PCA_input, {
  new_choices <- Percentile_type_reac()
  updateSelectInput(session = session,
                    inputId = "Output_indicator",
                    choices = new_choices,
                    selected = keep_or_default(input$Output_indicator, new_choices, new_choices[1]))
})



observeEvent(input$Data_input, {
  #Criterium_Value
  country <- sort(mapdata()[input$Country_select_type][, 1])

  #print(unlist(country))
  updateSelectInput(session = session,
                    inputId = "Year_input",
                    choices = InputYear())
  # Preferred default indicators for this source (falls back to its first 3
  # variables for any source without an explicit preference, e.g. FSI).
  preferred_selected <- default_indicators[[input$Data_input]]
  if (is.null(preferred_selected)) preferred_selected <- InputPACVar()[1:3]
  updateSelectInput(
    session = session,
    inputId = "PCA_input",
    choices = InputPACVar(),
    selected = preferred_selected
  )
  # Index_type (Raw/Composite) is intentionally left as-is here (not reset)
  # so a Composite/Varying setup persists across Input-data switches instead
  # of snapping back to Raw every time.
  new_output_choices <- if (input$Index_type == "Raw") InputPACVar() else outputvariable
  updateSelectInput(session = session,
                    inputId = "Output_indicator",
                    choices = new_output_choices,
                    selected = keep_or_default(input$Output_indicator, new_output_choices, new_output_choices[1]))
  updateSliderInput(
    session = session,
    inputId = "Criterium_Value",
    min = 1,
    max = length(InputYear())
  )
  updateSelectInput(session = session,
                    inputId = "Country_select",
                    choices = country)
  
  
})
observeEvent(input$Percentile_Value, {
  updateRadioButtons(
    session = session,
    inputId = "Percentile_success",
    choices = paste0(input$Percentile_Value, "th percentile"),
    inline = TRUE
  )
  
  
})

observeEvent(input$Medium_Treshold, {
  updateSliderInput(
    session = session,
    inputId = "High_Treshold",
    min = input$Medium_Treshold,
    max = max(Yearlydata$Percent)
  )
  
  
})

observeEvent(input$High_Treshold, {
  updateSliderInput(
    session = session,
    inputId = "Very_High_Treshold",
    min = input$High_Treshold,
    max = max(Yearlydata$Percent)
  )
  
  
})

observeEvent(input$Index_type, {
  new_choices <- Percentile_type_reac()
  updateSelectInput(session = session,
                    inputId = "Output_indicator",
                    choices = new_choices,
                    selected = keep_or_default(input$Output_indicator, new_choices, new_choices[1]))


})

observeEvent(input$Country_select_type, {
  country <- sort(mapdata()[input$Country_select_type][, 1])
  updateSelectInput(session = session,
                    inputId = "Country_select",
                    choices = country)
})

observeEvent(input$downdata,{
 
  showModal(DownloadModal(divid='downdata', csvid='CSV', xlsxid='XLSX'))
})

# output$CSV<-downloadHandler(
#   
#   filename = function(){
#     
#     if(input$downdata=="PCA"){
#       paste0("PCA-", Sys.Date(), ".csv")
#     }else{
#       if(input$downdata=="PERC"){
#         paste0("PERC-", Sys.Date(), ".csv")
#       }else{
#         if(input$downdata=="FRAG"){
#           paste0("FRAG-", Sys.Date(), ".csv")
#         }else{
#           if(input$downdata=="SUCC"){
#             paste0("SUCC-", Sys.Date(), ".csv")
#           }
#         }
#       } 
#     }
#     
#   },
#   content = function(file){
#     if(input$downdata=="PCA"){
#       data<-PCAoutputdata_func()
#     }else{
#       if(input$downdata=="PERC"){
#            data<-Percoutputdata_func()
#       }else{
#         if(input$downdata=="FRAG"){
#           data<-fragility_data_func()
#         }else{
#           if(input$downdata=="SUCC"){
#             data<-exitdrop_func()
#           }
#         }
#       }
#     }
#     
#     
#     write.csv(data,file, row.names = FALSE)
#   }
# )
# 
# 
# output$XLSX<-downloadHandler(
#   
#   filename = function(){
#     if(input$downdata=="PCA"){
#       paste0("PCA-", Sys.Date(), ".xlsx")
#     }else{
#       if(input$downdata=="PERC"){
#         paste0("PERC-", Sys.Date(), ".xlsx")
#       }else{
#         if(input$downdata=="FRAG"){
#           paste0("FRAG-", Sys.Date(), ".xlsx")
#         }else{
#           if(input$downdata=="SUCC"){
#             paste0("SUCC-", Sys.Date(), ".xlsx")
#           }
#         }
#       } 
#     }
#   },
#   content = function(file){
#     if(input$downdata=="PCA"){
#       data<-PCAoutputdata_func()
#     }else{
#       if(input$downdata=="PERC"){
#         print("toto")
#         data<-Percoutputdata_func()
#         print(data)
#       }else{
#         if(input$downdata=="FRAG"){
#           data<-fragility_data_func()
#         }else{
#           if(input$downdata=="SUCC"){
#             data<-exitdrop_func()
#           }
#         }
#       }
#     }
#     write.xlsx(data,file, colNames = TRUE)
#   }
# )


# ---- Helpers ----
data_to_download <- reactive({
  req(input$downdata)  # stoppe tant que c'est NULL
  switch(input$downdata,
         "PCA"  = PCAoutputdata_func(),
         "PERC" = Percoutputdata_func(),
         "FRAG" = fragility_data_func(),
         "SUCC" = exitdrop_func(),
         NULL   # valeur par défaut si inconnu
  )
})

file_prefix <- reactive({
  req(input$downdata)
  switch(input$downdata,
         "PCA" = "PCA", "PERC" = "PERC", "FRAG" = "FRAG", "SUCC" = "SUCC",
         "DATA"
  )
})

# ---- CSV ----
output$CSV <- downloadHandler(
  filename = function() sprintf("%s-%s.csv", file_prefix(), Sys.Date()),
  contentType = "text/csv",
  content = function(file) {
    df <- data_to_download()
    #print(df)
    shiny::validate(shiny::need(!is.null(df), "Aucune donnée à exporter."))
    utils::write.csv(df, file, row.names = FALSE)
    # ou readr::write_csv(df, file) si tu préfères
  }
)

# ---- XLSX ----
output$XLSX <- downloadHandler(
  filename = function() sprintf("%s-%s.xlsx", file_prefix(), Sys.Date()),
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  content = function(file) {
    df <- data_to_download()
    shiny::validate(shiny::need(!is.null(df), "Aucune donnée à exporter."))
    # openxlsx::write.xlsx ou writexl::write_xlsx (choisis un seul paquet)
    openxlsx::write.xlsx(df, file) 
    # writexl::write_xlsx(df, path = file)
  }
)



fragility_data_func<-reactive({
  
  varchar<-Percentile_type_reac()
  varcharT<-names(varchar)[varchar==input$Output_indicator]
  
  selectcol<-c("Country.Territory","Success_group","Indicator")
  renamecol<-c("Country name","Fraigility State",varcharT)
  #print(eval(renamecol))
  if(input$Country_select_type!="Country.Territory") {
    selectcol<-c("Country.Territory",input$Country_select_type,"Success_group","Indicator")
    namechsr<-names(geo_group)[geo_group==input$Country_select_type]
    renamecol<-c("Country name",namechsr, "Fraigility State", varcharT)
    
  }
  temp <- success_grouping() |>
    select(all_of(selectcol)) |>
    arrange(Success_group) |>
    rowwise() |>
    mutate(Success_group = names(class_group)[Success_group], Indicator = round(Indicator, 2)) |>
    arrange(Indicator) |>
    rename_with(~ all_of(renamecol), all_of(selectcol))
  
  return(temp)
})  

# observeEvent(input$Year_input,{
#   #selectcol<-c("Country.Territory", "Year", input$Output_indicator)
#   
#   temp<-Percoutputdata_func()
#   print(temp)
# })

Percoutputdata_func<-reactive({
  selectcol<-c("Country.Territory", "Year", input$Output_indicator)

  temp<-PCA_reactive_df()
  

  temp<-temp[["ind"]]|>as_tibble()|>
    select(all_of(selectcol))
  
  date_range<-tibble(Year=seq(from=min(temp$Year), to=max(temp$Year), by=1))
  if(input$Percentile_type=="Fixed"){
    per_df<-temp|>ungroup()|>
      summarise(Perfunc(get(input$Output_indicator)))
    res_df<-date_range|>group_by(Year)|>summarise(per_df)|>
      mutate(Indicator=input$Output_indicator)|>
      relocate(Indicator, Year)
  }
  
  # else{
  #   res_df<-temp|>group_by(Year)|>summarise(Perfunc(get(input$Output_indicator)))|>
  #     mutate(Indicator=input$Output_indicator)|>
  #     relocate(Indicator, Year)
  # }
  if(input$Percentile_type=="Varying"){
      res_df<-temp|>group_by(Year)|>summarise(Perfunc(get(input$Output_indicator)))|>
        mutate(Indicator=input$Output_indicator)|>
        relocate(Indicator, Year)
  }

  return(res_df|>
           ungroup())
})


PCAoutputdata_func<-reactive({
  temp<-PCA_reactive_df()
  
  selectcol<-c("Country.Territory", "Year", "Factor1", "Factor2", "NFactor1", "NFactor2")
  temp<-temp[["ind"]]|>as_tibble() |>
    select(all_of(selectcol))|>
    rename(`Country Name`=`Country.Territory`,
           `Loading Factor 1`=Factor1,
           `Loading Factor 2`=Factor2,
           `Loading Factor 1 Normalized`=NFactor1,
           `Loading Factor 2 Normalized`=NFactor2)
  return(temp)
}) 


# Group_list_func<-reactive({
#   mapdf<-
#   return(mapdf)
#
# })
PCA_reactive_df <- reactive({
  
  
  if (length(input$PCA_input) >= 3) {
    # print(head(indexdata()))
    # print(input$PCA_input)
    # print(InputYear())
    temp <- PCA_pooled_func(
      data = indexdata(),
      Yearrange = InputYear(),
      pcavar = input$PCA_input,
      countryvar = "Country.Territory"
    )
    
    return(temp)
  }
  
})

Percentile_func <- reactive({
  result <- list()
  temp <- PCA_reactive_df()
  temp <- temp[["ind"]] |> as_tibble() |>
    gather(key = "Index",
           value = "value",
           -`Country.Territory`,
           -Year) |>
    filter(Index == input$Output_indicator)
  
  
  
  
  
  percent <- as.numeric(input$Percentile_Value) / 100
  
  if (input$Percentile_type == "Fixed") {
    percent_df <- temp |> reframe(quantile_func(value, probs = percent), .groups = "drop")
    percent_df <- tibble(Year = InputYear()) |> group_by(Year) |> reframe(percent_df, .groups = "drop")
  }
  if (input$Percentile_type == "Varying") {
    percent_df <- temp |> group_by(Year) |> reframe(quantile_func(value, probs =
                                                                    percent), .groups = "drop")
    
  }
  result$source_df <- temp
  result$percent_df <- percent_df
  return(result)
})



success_grouping <- reactive({
  mapdf <- mapdata()
  temp <- PCA_reactive_df()
  temp <- temp[["ind"]] |> as_tibble() |>
    gather(key = "Index",
           value = "value",
           -`Country.Territory`,
           -Year) |>
    filter(Index == input$Output_indicator)
  
  
  
  MT <- Yearlydata |>
    dplyr::filter(Percent == as.numeric(input$Medium_Treshold)) |>
    pull(Inter)

  HT <- Yearlydata |>
    dplyr::filter(Percent == as.numeric(input$High_Treshold)) |>
    pull(Inter)
  

  
  VHT <- Yearlydata |>
    dplyr::filter(Percent == as.numeric(input$Very_High_Treshold)) |>
    pull(Inter)
  

  
  
  #print(MT); print(HT);print(VHT)
  percent <- seq(5, 95, by = 5) / 100
  percent_df <- temp |> 
    group_by(Year) |> 
    reframe(quantile_func(value, probs = percent), .groups = "drop")
  
  per_temp <- inner_join(temp, percent_df, 
                         by = "Year",relationship = "many-to-many") |>
    dplyr::filter(Year == input$Year_input) |> group_by(`Country.Territory`) |>
    summarise(
      proba = mean((value - Percent_value) > 0, na.rm = TRUE),
      Indicator = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  per_temp <- per_temp |>
    mutate(Success_group = ifelse(proba > VHT, 4,
                                  ifelse(proba > HT, 3, 
                                         ifelse(proba > MT, 2, 1)))) |>
    select(c(`Country.Territory`, Success_group, Indicator))
  
  tempmap <- left_join(per_temp, mapdf, by = "Country.Territory")
  return(tempmap)
  # }
  
  
  
})
#---------------------------- Graphics ---------------------------

output$PCAGraph <- renderHighchart({
  tryCatch({
    pca_list <- InputPACVar()
    
    #print( pca_list)
    temp <- PCA_reactive_df()
    
    #ffff<<-temp
    temp_eig <- temp[["eig"]] |> as_tibble()
    
    
    temp_eig <- temp_eig |>
      filter(Year == as.numeric(input$Year_input)) |> select(all_of(starts_with("eig")))
    
    
    temp_var <- temp[["var"]] |> as_tibble() |>
      filter(Year == as.numeric(input$Year_input)) |>
      ungroup() |> select(c(Factor1, Factor2, CFactor1, CFactor2, PCAVAR))
    
    temp_PCA <- temp_var |> select(c(Factor1, Factor2, PCAVAR)) |> mutate(VarLabel =
                                                                            names(pca_list)[pca_list %in% PCAVAR])
    
    temp_PCA <- bind_rows(temp_PCA, temp_PCA |> mutate(Factor1 = 0, Factor2 =
                                                         0)) |>
      mutate(Factor1 = as.numeric(Factor1),
             Factor2 = as.numeric(Factor2))
    
    # print(temp_PCA)
    
    p <- highchart()|>
      #hc_size(width = 100, height = 600)|>
      hc_chart(
               # marginTop = 60,
               #   marginRight = 10,
               #   marginBottom = 60,
               #   marginLeft = 10,
                 backgroundColor = "#ffffff",
                 shadow = TRUE) |>
      hc_xAxis(
        plotLines = list(
          list(
            label = list(text = ""),
            color = "black",
            width = 2,
            value = 0,
            dashStyle = 'ShortDot'
          )
        ),
        max = 1,
        min = -1,
        title = list(text = paste0(
          "Axe 1", " (", round(unlist(temp_eig$eig1), 1), "%", ")"
        ))
      ) |>
      hc_yAxis(
        plotLines = list(
          list(
            label = list(text = ""),
            color = "black",
            width = 2,
            value = 0,
            dashStyle = 'ShortDot'
          )
        ),
        max = 1,
        min = -1,
        title = list(text = paste0(
          "Axe 2", " (", round(unlist(temp_eig$eig2), 1), "%", ")"
        ))
      ) |>
      
      hc_plotOptions(series = list(
        borderColor = "black",
        marker = list(enabled = FALSE, states = list(hover = list (enabled =
                                                                     F)))
      )) |> hc_legend(enabled = FALSE) |>
      hc_title(text = "Relation between Indicators (PCA)", align = "center") |>
      hc_add_theme(hc_theme_elementary()) |>
      hc_exporting(enabled = T) |>
      hc_credits(enabled = F) |>
      # hc_add_series(
      #   data = circ,
      #   type = "polygon",
      #   hcaes(x = x, y = y),
      #   color = "rgba(222,235,255,.8)",
      #   name = ""
      # ) |>
      hc_add_series(
        data = temp_PCA,
        type = "line",
        tooltip = list(pointFormat = "Axe 1: {point.Factor1: ,.2f} <br> Axe 2: {point.Factor2:,.2f}"),
        hcaes(
          x = Factor1,
          y = Factor2,
          group = VarLabel,
          name = VarLabel
        )
      ) |>
      hc_tooltip(crosshairs = FALSE)
  }, error = function(e) {
  })
  
})

output$TrendGraph <- renderHighchart({
  tryCatch({
    nameLabel <- Percentile_type_reac()
    result <- Percentile_func()
    
    
    geo_variable <- c("Code", "Country.Territory")
    
    if (input$Country_select_type != "Country.Territory")
      geo_variable <- c("Code", "Country.Territory", input$Country_select_type)
    
    
    mapdf <- mapdata() |> select(all_of(geo_variable))
    
    source_df <- result$source_df
    
    source_df <- left_join(source_df, mapdf, by = "Country.Territory") |>
      group_by(across(all_of(
        c("Year", input$Country_select_type)
      )
      )) |>
      summarise(value = mean(value, na.rm = TRUE),
                .groups = "drop") |>
      filter(get(input$Country_select_type) == input$Country_select) |>
      ungroup()
    

    
    percent_df <- result$percent_df |> ungroup()
    
    p <- highchart()|>
      hc_chart(
               # marginTop = 60,
               # marginRight = 10,
               # marginBottom = 60,
               # marginLeft = 10,
               backgroundColor = "#ffffff",
               shadow = TRUE)|>
      hc_yAxis(title = list(text = "Fragility indicator")) |>
      hc_xAxis(title = list(text = "Reference period (year)")) |>
      hc_plotOptions(series = list(
        borderColor = "black",
        marker = list(enabled = FALSE, states = list(hover = list (enabled =
                                                                     F)))
      )) |>
      hc_title(
        text = paste("Trend in fragility:", input$Country_select),
        align = "center"
      ) |>
      hc_add_theme(hc_theme_elementary()) |>
      hc_exporting(enabled = T) |>
      hc_credits(enabled = F) |>
      hc_add_series(
        data = source_df,
        name = names(nameLabel)[nameLabel == input$Output_indicator],
        type = "line",
        tooltip = list(pointFormat = "Value: {point.value:,.2f}"),
        hcaes(x = Year, y = value),
        color = "blue"
      ) |>
      hc_add_series(
        data = percent_df,
        type = "line",
        dashStyle = 'ShortDot',
        tooltip = list(pointFormat = "Value: {point.Percent_value:,.2f}"),
        hcaes(
          x = Year,
          y = Percent_value,
          group = Percentile,
          name = Percentile
        ),
        color = c("red", "green")
      )
    
    p
  }, error = function(e) {
  })
  
  
})

output$GeoMap <- renderHighchart({
  tryCatch({
    temp <- success_grouping()#|>filter(Year==as.numeric(input$Year_input))
    
    
    
    
    highchart() |>
      hc_chart(
        # marginTop = 60,
        # marginRight = 10,
        # marginBottom = 60,
        # marginLeft = 10,
        backgroundColor = "#ffffff",
        shadow = TRUE)|> 
        hc_add_series_map(
        map =  map,
        df = temp,
        name = "Fragility Characteristic",
        value = "Success_group",
        joinBy = c("iso-a3", "Code"),  # Ensure this column exists
        dataLabels = list(enabled = FALSE)
      ) |>
      hc_colorAxis(dataClassColor = "category", dataClasses = listcolor1) |>
      hc_legend(
        align = 'right',
        verticalAlign = 'bottom',
        x = 0,
        y = 0,
        floating = TRUE,
        borderWidth = NULL,
        backgroundColor = 'white'
      ) |>
      hc_mapNavigation(enabled = TRUE, enableButtons = FALSE) |>
      hc_plotOptions(series = list(stacking = FALSE)) |>
      hc_title(text = "Levels of Fragility: Map", align = "center") |>
      hc_subtitle(#text = "Source: Our computations",
        verticalAlign = 'bottom',
        align = 'left') |>
      hc_credits(enabled = F) |>
      hc_exporting(enabled = T)
  }, error = function(e) {
  })
  
  
})

output$PieMap <- renderHighchart({
  tryCatch({
    name_variable <- Percentile_type_reac()
    
    canvasClickFunctionPie <- JS(
      "function(event) {Shiny.onInputChange('canvasClickedPie', [this.name, event.point.name]);}"
    )
    
    geo_variable <- c("Char_group", "Country.Territory")
    if (input$Country_select_type != "Country.Territory")
      geo_variable <- c("Char_group", input$Country_select_type)
    
    temp <- success_grouping() |>
      rowwise() |>
      mutate(Char_group = names(class_group)[Success_group]) |>
      group_by(across(all_of(geo_variable))) |>
      summarise(Indicator = mean(Indicator, na.rm = T),
                nchild = n(),
                .groups = "drop") |> group_by(Char_group) |> mutate(prop = nchild / sum(nchild, na.rm =
                                                                                          T) * 100) |> ungroup()
    
    #print(temp)
    temp_parent <- temp |> group_by(Char_group) |>
      summarise(
        Indicator = mean(Indicator, na.rm = T),
        nparent = sum(nchild, na.rm = T),
        .groups = "drop"
      ) |>
      ungroup() |> mutate(
        prop = nparent / sum(nparent, na.rm = T) * 100,
        lcolor = ifelse(
          Char_group == "Very high",
          "#FF0000",
          ifelse(
            Char_group == "High",
            "#FF5E00",
            ifelse(Char_group == "Medium", "#FFFF00", "#009d00")
          )
        )
      )
    #print(temp_parent)
    if (input$Country_select_type != "Country.Territory") {
      df_drilldown <- temp |>
        group_nest(Char_group) |>
        mutate(
          id = Char_group,
          type = "pie",
          innerSize = '20%',
          height = '60%',
          data = map(
            data,
            mutate,
            name = get(input$Country_select_type),
            y = prop,
            nchild = nchild,
            indicator = Indicator
          ),
          data = map(data, list_parse),
          allowPointSelect = TRUE
        )
      
      texttool <- paste(
        names(name_variable)[name_variable == input$Output_indicator],
        ': {point.indicator:,.1f}<br>
                   Country Count: {point.nchild:,.0f}<br> Proportion : {point.prop:,.1f}%'
      )
      
    } else{
      df_drilldown <- temp |>
        group_nest(Char_group) |>
        mutate(
          id = Char_group,
          type = "column",
          
          data = map(
            data,
            mutate,
            name = get(input$Country_select_type),
            y = Indicator,
            nchild = nchild,
            indicator = Indicator
          ),
          data = map(data, list_parse),
          allowPointSelect = TRUE
        )
      
      texttool <- paste(
        names(name_variable)[name_variable == input$Output_indicator],
        ': {point.indicator:,.1f}<br>
                    Proportion : {point.prop:,.1f}%'
      )
    }
    
    
    
    p <- highchart() |>
      hc_title(text = "Levels of Fragility: Distribution",
               style = gtitlestyle,
               align = "center") |>
      hc_subtitle(text = "(Distribution by State)",
                  style = gsubtitlestyle,
                  align = "center") |>
      hc_chart(
        # marginTop = 60,
        # marginRight = 10,
        # marginBottom = 60,
        # marginLeft = 10,
        backgroundColor = "#ffffff",
        shadow = TRUE
      ) |>
      hc_add_series(
        temp_parent,
        type = "pie",
        hcaes(
          y = prop,
          name = Char_group,
          color = lcolor,
          drilldown = Char_group
        ),
        names = names(geo_group)[geo_group == input$Country_select_type],
        # dataLabels=list(enabled=T),
        # events = list(click =  canvasClickFunctionPie),
        #colour=temp_parent$lcolor,
        innerSize = '40%',
        height = "100%",
        colorByPoint = TRUE,
        tooltip = list(
          pointFormat = 'Country Count: {point.nparent:,.0f}<br>
                    Proportion :{point.prop:,.1f}%',
          useHTML = TRUE
        )
      ) |>
      hc_plotOptions(pie = list(
        events = list(click =  canvasClickFunctionPie),
        dataLabels = list(enabled = TRUE, align = 'center')
      )) |>
      
      hc_drilldown(
        allowPointDrilldown = TRUE,
        
        series = list_parse(df_drilldown),
        drillUpButton = list(relativeTo = 'spacingBox', position = list(x =
                                                                          0, y = 30))
        
      ) |> hc_legend(enabled = FALSE) |>
      hc_tooltip(pointFormat = texttool, useHTML = TRUE) |>
      hc_add_theme(hc_theme_elementary()) |>
      hc_yAxis(title = "") |>
      hc_xAxis(title = "") |>
      hc_exporting(enabled = T)
    
    p
  }, error = function(e) {
  })
  
  
})

exitdrop_func <- reactive({
  selectcol <- c("Country.Territory", "Year", input$Output_indicator)
  renamecol <- c(
    Country = "Country.Territory",
    Year = "Year",
    key = input$Output_indicator
  )
  temp <- PCA_reactive_df()
  temp <- temp[["ind"]] |> as_tibble() |>
    select(all_of(selectcol)) |>
    rename(all_of(renamecol))
  
  
  #|>
  # filter(Year<=as.numeric(input$Year_input))
  
  if (input$Percentile_type == "Fixed") {
    temp <- temp |> mutate(
      Lower = quantile(key, probs = as.numeric(input$Percentile_Value[1]) / 100),
      Upper = quantile(key, probs = as.numeric(input$Percentile_Value[2]) /
                         100),
      below_start_p = ifelse(key <= Lower, 1, 0),
      below_exit_p = ifelse(key < Upper, 1, 0),
      above_exit_p = ifelse(key > Upper, 1, 0)
    ) |>
      group_by(Country) |> arrange(Year, .by_group = T) |>
      mutate(period = 1:n())
  }#else{
  if (input$Percentile_type == "Varying") {
    temp <- temp |> group_by(Year) |> mutate(
      Lower = quantile(key, probs = as.numeric(input$Percentile_Value[1]) / 100),
      Upper = quantile(key, probs = as.numeric(input$Percentile_Value[2]) /
                         100),
      below_start_p = ifelse(key <= Lower, 1, 0),
      below_exit_p = ifelse(key < Upper, 1, 0),
      above_exit_p = ifelse(key > Upper, 1, 0)
    ) |>
      group_by(Country) |> arrange(Year, .by_group = T) |>
      mutate(period = 1:n())
  }
  #}
  
  temp<-temp |> rename(year = Year)|>ungroup()
  

  ttres<<-as.numeric(input$Criterium_Value)
  resf <- drop_exit_func(
    df = temp ,
    TIME_THRESHOLD = as.numeric(input$Criterium_Value)
  )
  
  # Lcountry<-unique(temp$Country)
  # cdf<-NULL
  # for(cc in Lcountry ){
  #   restemp<-temp|>ungroup()|>
  #     filter(Country==cc)|> summarise(drop_exit_func(
  #                                                    below_start_p=below_start_p,
  #                                                    below_exit_p=below_exit_p,
  #                                                    above_exit_p=above_exit_p,
  #                                                    period=period,
  #                                                    nperiod=as.numeric(input$Criterium_Value),
  #                                                    year=Year))
  #   cdf<-bind_rows(cdf, bind_cols(Country=cc,restemp))
  # }
  # resf<-cdf|>filter(!(Exit_year==9999 & Drop_year==9999 & Above_ext_Per=="Never exit") )
  
  return(resf)
})

output$success_data <- renderDataTable({
  tryCatch({
    resf <- exitdrop_func()|>
      relocate(Country, Year, Type)
    
    
    
    renamecol <- c(
      "Country"="Country",
      "Event Year" = "Year",
      "Fragility Event" = "Type"
      
    )
    datatable(
      resf,
      colnames = renamecol ,
      rownames = FALSE,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scrollY = 250,
        scroller = TRUE,
        buttons = list(
          "copy",
          list(
            extend = 'collection',
            text="Download data",
            action=DT::JS("function(e,dt,node,config){
                            Shiny.setInputValue('downdata','SUCC',{priority: 'event'});
              }")
            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            # exportOptions = list(modifier = list(page = "all"))
            
          )
        )
        
        
      )
    )
  }, error = function(e) {
  })
  
})


output$fragility_data <- renderDataTable({
  tryCatch({
    # varchar <- Percentile_type_reac()
    # varcharT <- names(varchar)[varchar == input$Output_indicator]
    # 
    # selectcol <- c("Country.Territory", "Success_group", "Indicator")
    # renamecol <- c("Country name", "Fraigility State", varcharT)
    # #print(eval(renamecol))
    # if (input$Country_select_type != "Country.Territory") {
    #   selectcol <- c(
    #     "Country.Territory",
    #     input$Country_select_type,
    #     "Success_group",
    #     "Indicator"
    #   )
    #   namechsr <- names(geo_group)[geo_group == input$Country_select_type]
    #   renamecol <- c("Country name", namechsr, "Fraigility State", varcharT)
    #   
    # }
    # temp <- success_grouping() |> select(all_of(selectcol)) |>
    #   arrange(`Success_group`) |>
    #   rowwise() |>
    #   mutate(Success_group = names(class_group)[Success_group],
    #          Indicator = round(Indicator, 2)) |>
    #   rename_with( ~ all_of(renamecol), all_of(selectcol))
    temp<-fragility_data_func()
    
    #print(temp)
    
    datatable(
      temp ,
      rownames = FALSE,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scrollY = 250,
        scroller = TRUE,
        buttons = list(
          "copy",
          list(
            extend = 'collection',
            text="Download data",
            action=DT::JS("function(e,dt,node,config){
                            Shiny.setInputValue('downdata','FRAG',{priority: 'event'});
              }")
            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            # exportOptions = list(modifier = list(page = "all"))
            
          )
        )
        
        
      )
    ) |> formatStyle("Fraigility State",
                     target = "row",
                     backgroundColor = styleEqual(
                       c("Very high", "High", "Medium", "Low"),
                       c("#FF0000", "#FF5E00", "#FFFF00", "#009d00")
                     ))
  }, error = function(e) {
  })
  
})

output$PCAoutputdata <- renderDataTable({
  tryCatch({
    # temp <- PCA_reactive_df()
    # 
    # temp <- temp[["ind"]] |> as_tibble() |>
    #   select(`Country.Territory`,
    #          Year,
    #          Factor1,
    #          Factor2,
    #          NFactor1,
    #          NFactor2) |>
    #   rename(
    #     `Country Name` = `Country.Territory`,
    #     `Loading Factor 1` = Factor1,
    #     `Loading Factor 2` = Factor2,
    #     `Loading Factor 1 Normalized` = NFactor1,
    #     `Loading Factor 2 Normalized` = NFactor2
    #   )
    temp<-PCAoutputdata_func()
    # dataaa<<-temp
    datatable(
      temp ,
      rownames = FALSE,
      extensions = 'Buttons',
      #c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scrollY = 250,
        scroller = TRUE,
        buttons = list(
          "copy",
          list(
            extend = 'collection',
            text="Download data",
            action=DT::JS("function(e,dt,node,config){
                            Shiny.setInputValue('downdata','PCA',{priority: 'event'});
              }")
            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            # exportOptions = list(modifier = list(page = "all"))
            
          )
        )
        
        
      )
    ) |>
      formatRound(colnames(temp)[-c(1:2)], 3)
    
  }, error = function(e) {
  })
})


output$Percoutputdata <- renderDataTable({
  tryCatch({
    selectcol <- c("Country.Territory", "Year", input$Output_indicator)
    temp <- PCA_reactive_df()
    
    temp <- temp[["ind"]] |> as_tibble() |>
      select(all_of(selectcol))
    
    date_range <- tibble(Year = seq(
      from = min(temp$Year),
      to = max(temp$Year),
      by = 1
    ))
    if (input$Percentile_type == "Fixed") {
      per_df <- temp |> ungroup() |>
        summarise(Perfunc(get(input$Output_indicator)))
      res_df <- date_range |> group_by(Year) |> summarise(per_df) |>
        mutate(Indicator = input$Output_indicator) |>
        relocate(Indicator, Year)
    }
    if (input$Percentile_type == "Varying") {
      res_df <- temp |> group_by(Year) |> summarise(Perfunc(get(input$Output_indicator))) |>
        mutate(Indicator = input$Output_indicator) |>
        relocate(Indicator, Year)
    }
    
    datatable(
      res_df ,
      rownames = FALSE,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        deferRender = TRUE,
        scrollY = 250,
        scroller = TRUE,
        buttons = list(
          "copy",
          list(
            extend = 'collection',
            text="Download data",
            action=DT::JS("function(e,dt,node,config){
                            Shiny.setInputValue('downdata','PERC',{priority: 'event'});
              }")
            # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            # exportOptions = list(modifier = list(page = "all"))
            
          )
        )
        
        
      )
    ) |>
      formatRound(colnames(res_df)[-c(1, 2)], 3)
  }, error = function(e) {
  })
})
