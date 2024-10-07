source("R/Helpers.R")
source("R/functions.R")
load("datareadyforshiny.rds")
source("R/tab_01_main.R")


ui<- fluidPage(id="top",
               # Call Javasript and CSS Code from file
               singleton(tags$head(
                 includeScript("iframeResizer.contentWindow.min.js"),
                 includeScript("script.js"),
               )),
               tags$html(class = "no-js", lang = "en"),
               tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
               tags$head(
                 tags$style(HTML(paste(readLines("style.css"), collapse = ""))),
                 tags$style(HTML(
                   ".box, .irs-bar-edge, .irs-bar, .irs.min, .irs.max, .irs-single, .logo, .navbar, .tabbable > .nav > li > a, .tabbable > .nav > li[class=active] > a, .marker-cluster {
            # ... [consolidate all your CSS rules here]
          }"
                 )),
                 useShinyjs(),
                 mobileDetect('isMobile')
               ),
               tabItems(erstelleTab_front())
               
)

# Server Function
server <- function(input, output, session) {
  #   
  createTextDisplay.time.series <- function(id, textContent.time.series) {
    uiOutput(outputId = paste0("interpretation.", id))
  }
  
  serverTextDisplay.time.series <- function(id, textContent.time.series) {
    textVisible <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("toggleText.", id)]], {
      textVisible(!textVisible())
    })
    
    output[[paste0("interpretation.", id)]] <- renderUI({
      if (textVisible()) {
        h6(HTML(textContent.time.series))
      }
    })
  }
  
  createTextDisplay.wave1 <- function(id, textContent.wave1) {
    uiOutput(outputId = paste0("interpretation.", id))
  }
  
  serverTextDisplay.wave1 <- function(id, textContent.wave1) {
    textVisible <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("toggleText.", id)]], {
      textVisible(!textVisible())
    })
    
    output[[paste0("interpretation.", id)]] <- renderUI({
      if (textVisible()) {
        h6(HTML(textContent.wave1))
      }
    })
  }  
  
  createTextDisplay.wave2 <- function(id, textContent.wave2) {
    uiOutput(outputId = paste0("interpretation.", id))
  }
  
  serverTextDisplay.wave2 <- function(id, textContent.wave2) {
    textVisible <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("toggleText.", id)]], {
      textVisible(!textVisible())
    })
    
    output[[paste0("interpretation.", id)]] <- renderUI({
      if (textVisible()) {
        h6(HTML(textContent.wave2))
      }
    })
  }
  
  
  createTextDisplay.wave3 <- function(id, textContent.wave3) {
    uiOutput(outputId = paste0("interpretation.", id))
  }
  
  serverTextDisplay.wave3 <- function(id, textContent.wave3) {
    textVisible <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("toggleText.", id)]], {
      textVisible(!textVisible())
    })
    
    output[[paste0("interpretation.", id)]] <- renderUI({
      if (textVisible()) {
        h6(HTML(textContent.wave3))
      }
    })
  }
  
  createTextDisplay.wave4 <- function(id, textContent.wave4) {
    uiOutput(outputId = paste0("interpretation.", id))
  }
  
  serverTextDisplay.wave4 <- function(id, textContent.wave4) {
    textVisible <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("toggleText.", id)]], {
      textVisible(!textVisible())
    })
    
    output[[paste0("interpretation.", id)]] <- renderUI({
      if (textVisible()) {
        h6(HTML(textContent.wave4))
      }
    })
  }
  
  createTextDisplay.wave5 <- function(id, textContent.wave5) {
    uiOutput(outputId = paste0("interpretation.", id))
  }
  
  serverTextDisplay.wave5 <- function(id, textContent.wave5) {
    textVisible <- reactiveVal(FALSE)
    
    observeEvent(input[[paste0("toggleText.", id)]], {
      textVisible(!textVisible())
    })
    
    output[[paste0("interpretation.", id)]] <- renderUI({
      if (textVisible()) {
        h6(HTML(textContent.wave5))
      }
    })
  }
  
  
  
  observe({
    shinyjs::click("btn_current_state")
    shinyjs::click("btn_law_favor_tech")
    shinyjs::click("btn_current")
    shinyjs::click("btn_current_w1")
    shinyjs::click("btn_policymaking_US")
  })
  
  
  ###########################################################################################################################################
  # 1st two plots of Trend Analysis: Comprehensiveness of digital privacy laws
  ###########################################################################################################################################
  # Q12 law_breadth
  # Do you think current digital privacy laws in [the U.S. / the EU / country] cover more areas than needed, fewer areas than needed, or all areas needed?
  # ●	More areas than needed
  # ●	All areas needed
  # ●	Fewer areas than needed
  # 
  output$law_breadth_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_breadth_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "More areas than needed", data = law_breadth_Europe$Value[law_breadth_Europe$Area == "More areas than needed"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "All areas needed", data = law_breadth_Europe$Value[law_breadth_Europe$Area == "All areas needed"], color = '#6C8ACC',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Fewer areas than needed", data = law_breadth_Europe$Value[law_breadth_Europe$Area == "Fewer areas than needed"], color = '#BCCDE6',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  output$law_breadth_chart_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_breadth_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "More areas than needed", data = law_breadth_USA$Value[law_breadth_USA$Area == "More areas than needed"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "All areas needed", data = law_breadth_USA$Value[law_breadth_USA$Area == "All areas needed"], color = '#6C8ACC',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Fewer areas than needed", data = law_breadth_USA$Value[law_breadth_USA$Area == "Fewer areas than needed"], color = '#BCCDE6',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.time.series("law_breadth_sample", textContent.time.series)
  
  
  ###########################################################################################################################################
  # 2nd plots of Trend Analysis: Enforcement of digital privacy practices
  ###########################################################################################################################################
  ################################# Q15
  # Q15 law_enforcement
  # Are the digital privacy practices required by [U.S. / EU / country’s] law enforced …
  # ●	completely
  # ●	mostly
  # ●	somewhat
  # ●	a little
  # ●	not at all
  
  output$Enforcement_chart15_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_enforcement_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Mostly", data = law_enforcement_Europe$Value[law_enforcement_Europe$Rating == "Mostly"], color = '#681E13',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Somewhat", data = law_enforcement_Europe$Value[law_enforcement_Europe$Rating == "Somewhat"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "A little", data = law_enforcement_Europe$Value[law_enforcement_Europe$Rating == "A little"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Not at all", data = law_enforcement_Europe$Value[law_enforcement_Europe$Rating == "Not at all"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  output$Enforcement_chart15_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_enforcement_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Mostly", data = law_enforcement_USA$Value[law_enforcement_USA$Rating == "Mostly"], color = '#681E13',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Somewhat", data = law_enforcement_USA$Value[law_enforcement_USA$Rating == "Somewhat"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "A little", data = law_enforcement_USA$Value[law_enforcement_USA$Rating == "A little"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Not at all", data = law_enforcement_USA$Value[law_enforcement_USA$Rating == "Not at all"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.time.series("law_enforcement_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 3rd plots of Trend Analysis: Stakeholder ratings of organizations’ privacy protection performance
  ###########################################################################################################################################
  # Q22 orgs_rating_priv
  # 
  # a) How would you rate the performance of these firms in protecting people’s digital privacy?
  #   [Rows]
  # ●	[org_apple] Apple
  # ●	[org_google] Google 
  # ●	[org_meta] Meta (including Facebook, Instagram, Whatsapp, Oculus)
  # ●	[org_amazon] Amazon 
  # ●	[org_microsoft] Microsoft 
  # ●	[org_visa] Visa
  # ●	[org_mastercard] Mastercard  
  # 
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  
  # Q23 orgs_rating_pub
  # 
  # b) How would you rate the performance of these organizations in protecting people’s digital privacy?
  #   [Rows]
  # ●	[org_census] U.S. Census Bureau
  # ●	[org_irs] U.S. Internal Revenue Service
  # ●	[org_ssa] U.S. Social Security Administration
  # 
  # [Rows]
  # ●	[org_cstatistics] [country]’s statistical agency
  # ●	[org_ctaxes] [country]’s tax authority
  # ●	[org_csocial] [country]’s social insurance authority
  # 
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  
  
  questionL2_Europe <- reactive({
    req(input$Organisations.stacked.bar.chart)
    filter(orgs_rating_priv_update_Europe, Organisation %in% input$Organisations.stacked.bar.chart)
  })
  
  questionL2_USA <- reactive({
    req(input$Organisations.stacked.bar.chart)
    filter(orgs_rating_priv_update_USA, Organisation %in% input$Organisations.stacked.bar.chart)
  })
  
  output$Organisations1_chart_Europe <- renderHighchart({
    req(input$Organisations.stacked.bar.chart)
    
    Organisations1.chart_data <- questionL2_Europe()
    selected_organisation <- input$Organisations.stacked.bar.chart
    subtitle_text <- paste0("<i>Percentage of stakeholders considering ", selected_organisation, "'s protection of people's privacy as good or excellent</i>")
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 100) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = subtitle_text, align = "left",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(type = "category", categories = unique(Organisations1.chart_data$wave),
               labels = list(style = list(fontSize = "14px", color = "black"))) %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_series(name = "wave",
                    data = Organisations1.chart_data$Value, color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"))) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>Good/Excellent</b>: {point.y:.0f}%") %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE)
  })
  
  output$Organisations1_chart_USA <- renderHighchart({
    req(input$Organisations.stacked.bar.chart)
    
    Organisations1.chart_data <- questionL2_USA()
    selected_organisation <- input$Organisations.stacked.bar.chart
    subtitle_text <- paste0("<i>Percentage of stakeholders considering ", selected_organisation, "'s protection of people's privacy as good or excellent</i>")
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 100) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = subtitle_text, align = "left",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(type = "category", categories = unique(Organisations1.chart_data$wave),
               labels = list(style = list(fontSize = "14px", color = "black"))) %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_legend(enabled = FALSE) %>%
      hc_add_series(name = "wave",
                    data = Organisations1.chart_data$Value, color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"))) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>Good/Excellent</b>: {point.y:.0f}%") %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE)
  })
  
  
  serverTextDisplay.time.series("organisations_sample", textContent.time.series)
  
  
  ###########################################################################################################################################
  # 4th plots of Trend Analysis: Most important approach in protecting people‘s digital privacy: Adapting privacy laws to technological developments
  ###########################################################################################################################################
  # Q: priorities
  # How would you rank the importance of the following approaches to protecting people’s digital privacy?
  # 
  ## [priorities_control] ●	Giving individuals control over their data
  ## [priorities_enforcement] ●	Enforcing rules about the circumstances under which particular kinds of data can be processed 
  # ●	Providing restitution for harms individuals suffer if their privacy is violated
  ## [priorities_adaptlaw] ●	Adapting privacy laws to respond to current technological developments
  # ●	Developing technical solutions to ensure individuals cannot be identified
  
  # First approach Europe: Adapting privacy laws to respond to current technological developments
  output$priorities_chart_approach_1_EU <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 50) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_approach_1_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_approach_1_Europe$Value[priorities_approach_1_Europe$ranks == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  # First approach USA: Adapting privacy laws to respond to current technological developments
  output$priorities_chart_approach_1_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 50) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_approach_1_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_approach_1_USA$Value[priorities_approach_1_USA$ranks == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.time.series("priorities_sample1", textContent.time.series)
  
  ###########################################################################################################################################
  # 5th plots of Trend Analysis: Most important approach in protecting people‘s digital privacy: Giving individuals control over their data
  ###########################################################################################################################################
  
  
  # Second approach for Europe: Giving individuals control over their data
  output$priorities_chart_approach_2_EU <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 50) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_approach_2_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_approach_2_Europe$Value[priorities_approach_2_Europe$ranks == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  # Second approach for USA: Giving individuals control over their data
  output$priorities_chart_approach_2_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 50) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_approach_2_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_approach_2_USA$Value[priorities_approach_2_USA$ranks == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.time.series("priorities_sample2", textContent.time.series)
  
  ###########################################################################################################################################
  # 6th two plots of Trend Analysis: Balance of interest in digital privacy laws
  ###########################################################################################################################################
  # Q:  law_favor_tech
  # Do you think digital privacy laws and regulations in [the U.S. / the EU / country] more strongly favor the rights and needs of businesses or of individual users? 
  # ●	Strongly favor businesses
  # ●	Somewhat favor businesses
  # ●	Favor neither businesses nor individual users
  # ●	Somewhat favor individual users
  # ●	Strongly favor individual users
  
  output$law_favor_tech_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_favor_tech_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Strongly) favor businesses", data = law_favor_tech_Europe$Value[law_favor_tech_Europe$Rating == "(Somewhat/Strongly) favor businesses"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Favor neither businesses nor individual users", data = law_favor_tech_Europe$Value[law_favor_tech_Europe$Rating == "Favor neither businesses nor individual users"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Strongly/Somewhat) favor individual users", data = law_favor_tech_Europe$Value[law_favor_tech_Europe$Rating == "(Strongly/Somewhat) favor individual users"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  output$law_favor_tech_chart_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_favor_tech_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Strongly) favor businesses", data = law_favor_tech_USA$Value[law_favor_tech_USA$Rating == "(Somewhat/Strongly) favor businesses"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Favor neither businesses nor individual users", data = law_favor_tech_USA$Value[law_favor_tech_USA$Rating == "Favor neither businesses nor individual users"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Strongly/Somewhat) favor individual users", data = law_favor_tech_USA$Value[law_favor_tech_USA$Rating == "(Strongly/Somewhat) favor individual users"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.time.series("law_favor_tech_trend_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 7th two plots of Trend Analysis: Influence of laws on development of privacy-preserving practices and technologies
  ###########################################################################################################################################
  # Q:  law_innovation
  #Do you think digital privacy laws and regulations in [the U.S. / the EU / country] encourage or discourage innovation and development of privacy-preserving practices and technologies in organizations?
  #●	Strongly encourage innovation and development
  #●	Somewhat encourage innovation and development
  #●	Neither encourage nor discourage innovation and development
  #●	Somewhat discourage innovation and development
  #●	Strongly discourage innovation and development
  
  output$law_innovation_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_innovation_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Strongly/Somewhat) discourage innovation and development", data = law_innovation_Europe$Value[law_innovation_Europe$Rating == "(Strongly/Somewhat) discourage innovation and development"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neutral", data = law_innovation_Europe$Value[law_innovation_Europe$Rating == "Neutral"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Somewhat/Strongly) encourage innovation and development", data = law_innovation_Europe$Value[law_innovation_Europe$Rating == "(Somewhat/Strongly) encourage innovation and development"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  output$law_innovation_chart_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(law_innovation_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Strongly/Somewhat) discourage innovation and development", data = law_innovation_USA$Value[law_innovation_USA$Rating == "(Strongly/Somewhat) discourage innovation and development"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neutral", data = law_innovation_USA$Value[law_innovation_USA$Rating == "Neutral"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Somewhat/Strongly) encourage innovation and development", data = law_innovation_USA$Value[law_innovation_USA$Rating == "(Somewhat/Strongly) encourage innovation and development"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.time.series("law_innovation_trend_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 8th two plots of Trend Analysis:  Current and future outlook of digital privacy laws (Europe)
  ###########################################################################################################################################
  # Q: state_current_law and Q: state_outlook_law
  # Overall, how would you rate digital privacy laws and regulations in [the U.S. / the EU / country] today?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  # In the next few years, how optimistic or pessimistic are you that
  # digital privacy laws and regulations in your region [of expertise] will move in the
  # direction you prefer?
  # ●	Very optimistic
  # ●	Somewhat optimistic
  # ●	Neither
  # ●	Somewhat pessimistic
  # ●	Very pessimistic
  
  # Europe
  output$StatelawCurrent_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate digital privacy laws
                and regulations in the EU today?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_current_law_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = state_current_law_Europe$Value[state_current_law_Europe$Rating == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = state_current_law_Europe$Value[state_current_law_Europe$Rating == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  output$StatelawFuture_Europe <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that
                digital privacy laws and regulations in the EU will move in the
                direction you prefer?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_outlook_law_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = state_outlook_law_Europe$Value[state_outlook_law_Europe$Rating == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = state_outlook_law_Europe$Value[state_outlook_law_Europe$Rating == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = state_outlook_law_Europe$Value[state_outlook_law_Europe$Rating == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  serverTextDisplay.time.series("StatelawCombined_Europe_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 9th two plots of Trend Analysis:  Current and future outlook of digital privacy laws (USA)
  ###########################################################################################################################################
  # Q: state_current_law and Q: state_outlook_law
  # Overall, how would you rate digital privacy laws and regulations in [the U.S. / the EU / country] today?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  # In the next few years, how optimistic or pessimistic are you that
  # digital privacy laws and regulations in your region [of expertise] will move in the
  # direction you prefer?
  # ●	Very optimistic
  # ●	Somewhat optimistic
  # ●	Neither
  # ●	Somewhat pessimistic
  # ●	Very pessimistic
  
  # USA
  output$StatelawCurrent_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate digital privacy laws
                and regulations in the US today?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_current_law_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = state_current_law_USA$Value[state_current_law_USA$Rating == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = state_current_law_USA$Value[state_current_law_USA$Rating == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  output$StatelawFuture_USA <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that
                digital privacy laws and regulations in the US will move in the
                direction you prefer?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_outlook_law_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = state_outlook_law_USA$Value[state_outlook_law_USA$Rating == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = state_outlook_law_USA$Value[state_outlook_law_USA$Rating == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = state_outlook_law_USA$Value[state_outlook_law_USA$Rating == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  
  serverTextDisplay.time.series("StatelawCombined_USA_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 10th plots of Trend Analysis: Policymaking approaches in digital privacy protection
  ###########################################################################################################################################
  # Q:  policymaking_US
  # Do you think that digital privacy policy in the US should be made at the …
  # ●	federal level
  # ●	state level
  # ●	Both [PRG: Anchor]
  # Q:  policymaking_EU
  # Do you think that digital privacy policy in the EU should be made at the …
  # ●	EU-level
  # ●	EU member-state level
  # ●	Both [PRG: Anchor]
  
  # Render the selected plot for Europe
  output$policymaking_trend_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(policymaking_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "EU Level", data = policymaking_Europe$Value[policymaking_Europe$Area == "Europe Level"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "EU Member-State Level", data = policymaking_Europe$Value[policymaking_Europe$Area == "EU Member-State Level"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Both", data = policymaking_Europe$Value[policymaking_Europe$Area == "Both"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  # Render the selected plot for USA
  output$policymaking_trend_chart_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = policymaking_USA$category_n,
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Federal Level", data = policymaking_USA$Value[policymaking_USA$Area == "Federal Level"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "State Level", data = policymaking_USA$Value[policymaking_USA$Area == "State Level"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Both", data = policymaking_USA$Value[policymaking_USA$Area == "Both"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.time.series("policymakingCombined_trend_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 11th plots of Trend Analysis: Current and future outlook of organizational digital privacy practices (Europe)
  ###########################################################################################################################################
  # Q: state_current_pra
  # Overall, how would you rate organizations’ digital privacy policies and practices in [the U.S. / the EU / country] today?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  # Q: state_outlook_practice
  # In the next few years, how optimistic or pessimistic are you that organizations’ digital privacy policies and practices in [the U.S. / the EU / country] will move in the direction you prefer?
  # ●	Very optimistic
  # ●	Somewhat optimistic
  # ●	Neither optimistic nor pessimistic
  # ●	Somewhat pessimistic
  # ●	Very pessimistic
  
  # Europe
  output$state_current_pra_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate organizations' digital privacy policies
                and practices in the EU today?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_current_pra_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = state_current_pra_Europe$Value[state_current_pra_Europe$Rating == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = state_current_pra_Europe$Value[state_current_pra_Europe$Rating == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  output$state_outlook_pra_chart_Europe <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that
                organizations' digital privacy policies and practices in the US will move in the direction you prefer?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_outlook_pra_Europe$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = state_outlook_pra_Europe$Value[state_outlook_pra_Europe$Rating == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = state_outlook_pra_Europe$Value[state_outlook_pra_Europe$Rating == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = state_outlook_pra_Europe$Value[state_outlook_pra_Europe$Rating == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  serverTextDisplay.time.series("StatePraCombined_EU_sample", textContent.time.series)
  
  
  ###########################################################################################################################################
  # 11th plots of Trend Analysis: Current and future outlook of organizational digital privacy practices (USA)
  ###########################################################################################################################################
  # Q: state_current_pra
  # Overall, how would you rate organizations’ digital privacy policies and practices in [the U.S. / the EU / country] today?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  # Q: state_outlook_practice
  # In the next few years, how optimistic or pessimistic are you that organizations’ digital privacy policies and practices in [the U.S. / the EU / country] will move in the direction you prefer?
  # ●	Very optimistic
  # ●	Somewhat optimistic
  # ●	Neither optimistic nor pessimistic
  # ●	Somewhat pessimistic
  # ●	Very pessimistic
  
  # Europe
  output$state_current_pra_chart_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate organizations' digital privacy policies
                and practices in the US today?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_current_pra_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = state_current_pra_USA$Value[state_current_pra_USA$Rating == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = state_current_pra_USA$Value[state_current_pra_USA$Rating == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  output$state_outlook_pra_chart_USA <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that
                digital privacy laws and regulations in the EU will move in the
                direction you prefer?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_outlook_pra_USA$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = state_outlook_pra_USA$Value[state_outlook_pra_USA$Rating == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = state_outlook_pra_USA$Value[state_outlook_pra_USA$Rating == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = state_outlook_pra_USA$Value[state_outlook_pra_USA$Rating == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  serverTextDisplay.time.series("StatePraCombined_USA_sample", textContent.time.series)
  
  ###########################################################################################################################################
  # 1st Plot of Wave 3: Generative AI’s influence on stakeholder views of privacy policy
  ###########################################################################################################################################
  #Has the rise of generative AI changed the way you think about privacy policy?
  #○	Yes
  #○	No
  
  output$gen_AI_chart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 80) %>%
      hc_xAxis(categories = unique(gen_AI$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Yes", data = gen_AI$Value[gen_AI$Area == "Yes"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave3("gen_AI_sample", textContent.wave3)
  
  ###########################################################################################################################################
  # 2nd Plot of Wave 3: Expected shifts in digital privacy landscape due to generative AI
  ###########################################################################################################################################
  # Q11: ai.yes
  # 
  # Has the rise of generative AI changed the way you think about privacy policy? Yes, how so?
  
  
  output$ai_yes_chart <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 0) %>%
      hc_xAxis(categories = unique(wave3_yes$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Europe<br>(n = 23)", data = wave3_yes$share[wave3_yes$region == "Europe<br>(n = 23)"],
                    color = '#4B77C5',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "USA<br>(n = 24)", data = wave3_yes$share[wave3_yes$region == "USA<br>(n = 24)"],
                    color = '#7F956B',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(bar = list(borderRadius = "0%",
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")),
                                groupPadding = 0.1)) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(title = list(text = 'Region<br><i><span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</i></span>'),
                align = "left", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2, useHTML = TRUE,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  
  serverTextDisplay.wave3("ai.yes_sample", textContent.wave3)
  
  
  
  ###########################################################################################################################################
  # 1st Plot of Wave 2: Balance of interest in digital privacy laws
  ###########################################################################################################################################
  ## Q10 law_favor_tech
  # Do you think digital privacy laws and regulations in [the U.S. / the EU / country] more strongly favor the rights and needs of businesses or of individual users? 
  # ●	Strongly favor businesses
  # ●	Somewhat favor businesses
  # ●	Favor neither businesses nor individual users
  # ●	Somewhat favor individual users
  # ●	Strongly favor individual users
  
  output$plot_law_favor_tech <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(law_favor_tech$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Strongly) favor businesses", data = law_favor_tech$Value[law_favor_tech$Rating == "(Somewhat/Strongly) favor businesses"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Favor neither businesses nor individual users", data = law_favor_tech$Value[law_favor_tech$Rating == "Favor neither businesses nor individual users"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Strongly/Somewhat) favor individual users", data = law_favor_tech$Value[law_favor_tech$Rating == "(Strongly/Somewhat) favor individual users"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.wave2("tech_sample", textContent.wave2)
  
  
  
  ###########################################################################################################################################
  # 2nd Plot of Wave 2: Influence of laws on development of privacy-preserving practices and technologies
  ###########################################################################################################################################
  ## Q11 law_innovation
  #Do you think digital privacy laws and regulations in [the U.S. / the EU / country] encourage or discourage innovation and development of privacy-preserving practices and technologies in organizations?
  #●	Strongly encourage innovation and development
  #●	Somewhat encourage innovation and development
  #●	Neither encourage nor discourage innovation and development
  #●	Somewhat discourage innovation and development
  #●	Strongly discourage innovation and development
  
  
  output$plot_law_innovation <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(law_innovation$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Strongly/Somewhat) discourage innovation<br>and development", data = law_innovation$Value[law_innovation$Area == "(Strongly/Somewhat) discourage innovation and development"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neutral", data = law_innovation$Value[law_innovation$Area== "Neutral"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Somewhat/Strongly) encourage innovation<br>and development", data = law_innovation$Value[law_innovation$Area == "(Somewhat/Strongly) encourage innovation and development"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  
  serverTextDisplay.wave2("innovation_sample", textContent.wave2)
  
  ###########################################################################################################################################
  # 3rd plots of Wave 2: Current and future outlook of digital privacy laws
  ###########################################################################################################################################
  ##Question 16 
  # Q16  state_current_law
  # Overall, how would you rate digital privacy laws and regulations in [the U.S. / the EU / country] today?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  ##Combined: Question 16 & 17
  # Q16  state_current_law
  # Q17 state_outlook_law
  
  output$StatelawCurrent <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate digital privacy laws
                and regulations in your region [of expertise] today?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(orgs_rating_priv.16$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = orgs_rating_priv.16$Value[orgs_rating_priv.16$Rating == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = orgs_rating_priv.16$Value[orgs_rating_priv.16$Rating == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  output$StatelawFuture <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that
                digital privacy laws and regulations in your region [of expertise] will move in the
                direction you prefer?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(orgs_rating_priv.17$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = orgs_rating_priv.17$Value[orgs_rating_priv.17$Rating == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = orgs_rating_priv.17$Value[orgs_rating_priv.17$Rating == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = orgs_rating_priv.17$Value[orgs_rating_priv.17$Rating == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  
  serverTextDisplay.wave2("StatelawCombined_sample", textContent.wave2)
  
  ###########################################################################################################################################
  # 4th plots of Wave 2: Current and future outlook of organizational digital privacy practices
  ###########################################################################################################################################
  ## Combined Question 26 & 27
  ## Question 26 
  # Q26  state_current_pra
  # Overall, how would you rate organizations’ digital privacy policies and practices in [the U.S. / the EU / country] today?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  ## Question 27 
  # Q27 state_outlook_practice
  # In the next few years, how optimistic or pessimistic are you that organizations’ digital privacy policies and practices in [the U.S. / the EU / country] will move in the direction you prefer?
  # ●	Very optimistic
  # ●	Somewhat optimistic
  # ●	Neither optimistic nor pessimistic
  # ●	Somewhat pessimistic
  # ●	Very pessimistic
  
  output$StatePraCurrent <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate organizations’ digital privacy policies
                and practices in your region [of expertise] today?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(orgs_rating_priv.26$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = orgs_rating_priv.26$Value[orgs_rating_priv.26$Rating == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = orgs_rating_priv.26$Value[orgs_rating_priv.26$Rating == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  output$StatePraFuture <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that organizations’
                digital privacy policies and practices in your region [of expertise] will move in the direction 
                refer?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(orgs_rating_priv.27$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = orgs_rating_priv.27$Value[orgs_rating_priv.27$Rating == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = orgs_rating_priv.27$Value[orgs_rating_priv.27$Rating == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = orgs_rating_priv.27$Value[orgs_rating_priv.27$Rating == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle", useHTML = TRUE,
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave2("StatePraCombined_sample", textContent.wave2)
  
  ###########################################################################################################################################
  # 5th plots of Wave 2: Consumer privacy law requirements
  ###########################################################################################################################################
  ## Q20 model_law
  # How important is it for a national [consumer] privacy law to include the following requirements?
  #   ●	[model_civilrights] Civil rights: forbid data processing that may discriminate against a protected group or class
  # ●	[model_dataaccess] Data access and deletion: Ability to view, edit and delete data in most situations
  # ●	[model_datacontrols] Data controls: Data collection choice screens, data download and portability options
  # ●	[model_dataloyalty] Data loyalty: requirements that companies use data in a way consumers expect, including limits on third-party use
  # ●	[model_dataminimize] Data minimization: mandates that companies only collect data they need to operate a service
  # ●	[model_datasec] Data security: requires covered entity to secure data internally and protect against breaches or malicious attacks
  # ●	[model_transparency] Transparency: mandates that services disclose (in easy to read terms) the types of data collected, retained, processed and shared with third parties
  # ●	[model_youth] Youth protections: Specific data protection rules that recognize the developmental stages of youth
  # 
  # [Columns]
  # ●	Very important to include
  # ●	Important to include
  # ●	Somewhat important to include
  # ●	Not at all important to include
  
  output$model_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 160) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: How important is it for a national privacy law to include the following requirements?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(model_final_Europe$model_short),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Not at all important to include", data = model_final_Europe$Value[model_final_Europe$Important == "Not at all important to include"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Somewhat important to include", data = model_final_Europe$Value[model_final_Europe$Important == "Somewhat important to include"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Important to include", data = model_final_Europe$Value[model_final_Europe$Important == "Important to include"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Very important to include", data = model_final_Europe$Value[model_final_Europe$Important == "Very important to include"], color = '#681E13',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 23,
                                groupPadding = 0.1, 
                                pointPadding = 0.1, 
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: '{point.y:.0f}%'") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  output$model_chart_USA <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 160) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: How important is it for a national consumer privacy law to include the following requirements?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(model_final_USA$model_short),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Not at all important to include", data = model_final_USA$Value[model_final_USA$Important == "Not at all important to include"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Somewhat important to include", data = model_final_USA$Value[model_final_USA$Important == "Somewhat important to include"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Important to include", data = model_final_USA$Value[model_final_USA$Important == "Important to include"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Very important to include", data = model_final_USA$Value[model_final_USA$Important == "Very important to include"], color = '#681E13',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 23,
                                groupPadding = 0.7, 
                                pointPadding = 5, 
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave2("model_sample", textContent.wave2) 
  
  
  
  ###########################################################################################################################################
  # 6th Plot of Wave 2: Consumer privacy law enforcement mechanisms (USA)
  ###########################################################################################################################################
  ## Q21  model_enforce_us
  # How important is it for a national consumer privacy law to include the following enforcement mechanisms?
  # 
  # [Rows]
  # ●	[model_us_standard] A national standard, preempting state laws 
  # ●	[model_us_agency] Agency rulemaking: The executive agency tasked with enforcement is able to issue additional rules closely related to the rights outlined in the legislation without legislative branch direction
  # ●	[model_us_private] Private right of action: A consumer is entitled to enforce their own rights
  # ●	[model_us_staff] Funding and staffing requirements for agencies to be capable and motivated to enforce laws
  # 
  # [Columns]
  # ●	Very important to include
  # ●	Important to include
  # ●	Somewhat important to include
  # ●	Not at all important to include
  
  output$model_chart_q21 <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(model_final_q21$model_short),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Not at all important to include", data = model_final_q21$Value[model_final_q21$Important == "Not at all important to include"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Somewhat important to include", data = model_final_q21$Value[model_final_q21$Important == "Somewhat important to include"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Important to include", data = model_final_q21$Value[model_final_q21$Important == "Important to include"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Very important to include", data = model_final_q21$Value[model_final_q21$Important == "Very important to include"], color = '#681E13',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1, 
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
    
  })
  
  
  serverTextDisplay.wave2("model.q21_sample", textContent.wave2)
  
  
  ###########################################################################################################################################
  # 7th Plot of Wave 2: Offline privacy advocacy activities
  ###########################################################################################################################################
  ## Question 5 offline_activism
  # Have you ever engaged in any of the following activities?
  
  output$offline_activism_chart <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 0) %>%
      hc_xAxis(categories = unique(orgs_rating_priv.5$offline_activism_short),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Europe<br>(n = 43)", data = orgs_rating_priv.5$Share[orgs_rating_priv.5$category == "Europe<br>(n = 43)"],
                    color = '#4B77C5',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "USA<br>(n = 33)", data = orgs_rating_priv.5$Share[orgs_rating_priv.5$category == "USA<br>(n = 33)"],
                    color = '#7F956B',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(bar = list(borderRadius = "0%",
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")),
                                groupPadding = 0.1)) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(title = list(text = 'Region<br><i><span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</i></span>'),
                align = "left", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2, useHTML = TRUE,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.wave2("offline_activism_sample", textContent.wave2)
  
  
  ###########################################################################################################################################
  # 8th Plot of Wave 2: Online privacy advocacy activities
  ###########################################################################################################################################
  ## Question 6 online_activism
  # Have you ever engaged in any of the following activities?
  
  output$online_activism_chart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 0) %>%
      hc_xAxis(categories = unique(orgs_rating_priv.6$online_activism_short),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Europe<br>(n = 43)", data = orgs_rating_priv.6$Share[orgs_rating_priv.6$category == "Europe<br>(n = 43)"],
                    color = '#4B77C5',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "USA<br>(n = 34)", data = orgs_rating_priv.6$Share[orgs_rating_priv.6$category == "USA<br>(n = 34)"],
                    color = '#7F956B',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(bar = list(borderRadius = "0%",
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")),
                                groupPadding = 0.1)) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(title = list(text = 'Region<br><i><span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</i></span>'),
                align = "left", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2, useHTML = TRUE,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  
  serverTextDisplay.wave2("online_activism_sample", textContent.wave2)
  
  ###########################################################################################################################################
  # 9th plots of Wave 2: Ranking approaches to protecting digital privacy
  ###########################################################################################################################################
  ############# Q2 priorities (wave 2)
  # How would you rank the importance of the following approaches to protecting people’s digital privacy?
  #   To rank the items, drag and drop them. (range 1-5) = Rank 1 (most important), Rank 2, Rank 3, Rank 4, Rank 5 (least important)
  # ●	[priorities_techsolutions] Designing and deploying privacy-preserving technology
  # ●	[priorities_processing] Regulating how data is processed, stored and shared
  # ●	[priorities_enforcement] Enforcing rules about how data is processed, stored and shared
  # ●	[priorities_control] Giving individuals control over their data
  # ●	[priorities_adaptlaw] Adapting privacy laws to respond to current technological developments
  
  
  output$priorities_w2_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_w2_Europe$priorities_short),
               labels = list(style = list(fontSize = "12px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 5: Least important", data = priorities_w2_Europe$Value[priorities_w2_Europe$Important == "Rank 5 (Least important)"], color = '#D9E0D2',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 4) %>%
      hc_add_series(name = "Rank 4", data = priorities_w2_Europe$Value[priorities_w2_Europe$Important == "Rank 4"], color = '#B4C7A4',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Rank 3", data = priorities_w2_Europe$Value[priorities_w2_Europe$Important == "Rank 3"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Rank 2", data = priorities_w2_Europe$Value[priorities_w2_Europe$Important == "Rank 2"], color = '#51704A',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_w2_Europe$Value[priorities_w2_Europe$Important == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1,
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  
  output$priorities_w2_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_w2_USA$priorities_short),
               labels = list(style = list(fontSize = "12px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 5: Least important", data = priorities_w2_USA$Value[priorities_w2_USA$Important == "Rank 5 (Least important)"], color = '#D9E0D2',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 4) %>%
      hc_add_series(name = "Rank 4", data = priorities_w2_USA$Value[priorities_w2_USA$Important == "Rank 4"], color = '#B4C7A4',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Rank 3", data = priorities_w2_USA$Value[priorities_w2_USA$Important == "Rank 3"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Rank 2", data = priorities_w2_USA$Value[priorities_w2_USA$Important == "Rank 2"], color = '#51704A',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_w2_USA$Value[priorities_w2_USA$Important == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1,
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  serverTextDisplay.wave2("priorities_w2_sample", textContent.wave2)
  
  
  ###########################################################################################################################################
  # 10th plots of Wave 2: Policymaking approaches in digital privacy protection
  ###########################################################################################################################################
  # Question 18
  # [PRG: Ask if region is “United States”]
  # Q18  policymaking_US
  # Do you think that digital privacy policy in the US should be made at the …
  # ●	federal level
  # ●	state level
  # ●	Both [PRG: Anchor]
  #
  # Question 19
  # [PRG: Ask if EU is “Yes”]
  # Q19  policymaking_EU
  # Do you think that digital privacy policy in the EU should be made at the …
  # ●	EU-level
  # ●	EU member-state level
  # ●	Both [PRG: Anchor]
  
  # Render the selected plot for Europe
  output$policymaking_chart_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = policymaking_EU$category,
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "EU Level", data = policymaking_EU$Value[policymaking_EU$Area == "Europe Level"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "EU Member-State Level", data = policymaking_EU$Value[policymaking_EU$Area == "EU Member-State Level"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Both", data = policymaking_EU$Value[policymaking_EU$Area == "Both"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  # Render the selected plot for USA
  output$policymaking_chart_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = policymaking_US$category,
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Federal Level", data = policymaking_US$Value[policymaking_US$Area == "Federal Level"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "State Level", data = policymaking_US$Value[policymaking_US$Area == "State Level"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Both", data = policymaking_US$Value[policymaking_US$Area == "Both"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.wave2("policymakingCombined_sample", textContent.wave2)
  
  
  
  ###########################################################################################################################################
  # 1st Plot of Wave 1: Stakeholder ratings of digital privacy law specifications
  ###########################################################################################################################################
  # Q: law_depth
  # How would you rate the specifications of the requirements in current digital privacy laws in [the U.S. / EU / country]?
  # Excellent
  # Good
  # Fair
  # Poor
  
  output$law_depth_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(law_depth$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = law_depth$Value[law_depth$Area == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  
  serverTextDisplay.wave1("Law_depth_sample", textContent.wave1)
  
  
  ###########################################################################################################################################
  # 2nd plots of Wave 1: Ranking approaches to protecting digital privacy
  ###########################################################################################################################################
  ## Q2 priorities (wave 1)
  # How would you rank the importance of the following approaches to protecting people’s digital privacy?
  #   To rank the items, drag and drop them. (range 1-5) = Rank 1 (most important), Rank 2, Rank 3, Rank 4, Rank 5 (least important)
  # ●	[priorities_control] Giving individuals control over their data
  # ●	[priorities_enforcement] Enforcing rules about the circumstances under which particular kinds of data can be processed
  # ●	[priorities_restitution] Providing restitution for harms individuals suffer if their privacy is violated
  # ●	[priorities_adaptlaw] Adapting privacy laws to respond to current technological developments
  # ●	[priorities_techsolutions] Developing technical solutions to ensure individuals cannot be identified
  
  
  output$priorities_w1_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_w1_Europe$priorities_short),
               labels = list(style = list(fontSize = "12px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 5: Least important", data = priorities_w1_Europe$Value[priorities_w1_Europe$Important == "Rank 5 (Least important)"], color = '#D9E0D2',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 4) %>%
      hc_add_series(name = "Rank 4", data = priorities_w1_Europe$Value[priorities_w1_Europe$Important == "Rank 4"], color = '#B4C7A4',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Rank 3", data = priorities_w1_Europe$Value[priorities_w1_Europe$Important == "Rank 3"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Rank 2", data = priorities_w1_Europe$Value[priorities_w1_Europe$Important == "Rank 2"], color = '#51704A',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_w1_Europe$Value[priorities_w1_Europe$Important == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1,
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  
  output$priorities_w1_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_w1_USA$priorities_short),
               labels = list(style = list(fontSize = "12px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 5: Least important", data = priorities_w1_USA$Value[priorities_w1_USA$Important == "Rank 5 (Least important)"], color = '#D9E0D2',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 4) %>%
      hc_add_series(name = "Rank 4", data = priorities_w1_USA$Value[priorities_w1_USA$Important == "Rank 4"], color = '#B4C7A4',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Rank 3", data = priorities_w1_USA$Value[priorities_w1_USA$Important == "Rank 3"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Rank 2", data = priorities_w1_USA$Value[priorities_w1_USA$Important == "Rank 2"], color = '#51704A',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_w1_USA$Value[priorities_w1_USA$Important == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1,
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  
  
  serverTextDisplay.wave1("priorities_w1_sample", textContent.wave1)
  
  
  ###########################################################################################################################################
  # 3rd plots of Wave 1: Current and future outlook of digital privacy laws
  ###########################################################################################################################################
  ## Combined: Question state_current & state_outlook (wave 1)
  # Q: state_current
  # Overall, how would you rate today’s digital privacy laws and organizational practices in [the U.S. / the EU / country]?
  # ●	Excellent
  # ●	Good
  # ●	Fair
  # ●	Poor
  # Q: state_outlook
  # In the next few years, how optimistic or pessimistic are you that digital privacy laws and organizational practices in [the U.S. / the EU / country] will move in the direction you prefer?
  # ●	Very optimistic 
  # ●	Somewhat optimistic
  # ●	Neither optimistic nor pessimistic
  # ●	Somewhat pessimistic
  # ●	Very pessimistic
  
  output$StateCurrentW1 <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Current State</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: Overall, how would you rate today's digital privacy laws
                and organizational practices in your region [of expertise]?</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_current_w1$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Good/Excellent", data = state_current_w1$Value[state_current_w1$Area == "Good/Excellent"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Poor/Fair", data = state_current_w1$Value[state_current_w1$Area == "Poor/Fair"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  output$StateFutureW1 <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_title(text = "<b>Future Outlook</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_subtitle(text = "<i>Question: In the next few years, how optimistic or pessimistic are you that digital privacy laws
                  and organizational practices in your region [of expertise] will move in the direction you prefer??</i>",
                  align = "center",
                  style = list(color = "black", fontSize = "13px", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(state_outlook_w1$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "(Somewhat/Very)<br>optimistic", data = state_outlook_w1$Value[state_outlook_w1$Area == "(Somewhat/Very) optimistic"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither", data = state_outlook_w1$Value[state_outlook_w1$Area == "Neither"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "(Very/Somewhat)<br>pessimistic", data = state_outlook_w1$Value[state_outlook_w1$Area == "(Very/Somewhat) pessimistic"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "right", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave1("StateCombinedW1_sample", textContent.wave1)
  
  
  ###########################################################################################################################################
  # 1st Plot of Wave 4: Use of AI tools and systems for work
  ###########################################################################################################################################
  # Do you use AI tools or systems in your work?
  # ●	Yes
  # ●	No
  # ●	Don't know
  
  output$ai_work_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_work$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Yes", data = ai_work$Value[ai_work$Area == "Yes"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_work_sample", textContent.wave4)
  
  
  ###########################################################################################################################################
  # 2nd Plot of Wave 4: Privacy concerns for the use of AI tools or systems in their work
  ###########################################################################################################################################
  # How much do privacy concerns affect your use of AI tools or systems in your work? 
  # ●	Not at all
  # ●	Somewhat
  # ●	A great deal
  
  output$ai_concern_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_concern$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "A great deal", data = ai_concern$Value[ai_concern$Area == "A great deal"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Somewhat", data = ai_concern$Value[ai_concern$Area == "Somewhat"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Not at all", data = ai_concern$Value[ai_concern$Area == "Not at all"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_concern_sample", textContent.wave4)
  
  
  ###########################################################################################################################################
  # 3rd Plot of Wave 4: Organizational adoption of AI frameworks and guidelines
  ###########################################################################################################################################
  # Does your organization have a framework or guidelines in place for using AI in the workplace?
  # ●	Yes
  # ●	No
  # ●	Don't know
  
  output$ai_org_framework_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_org_framework$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Yes", data = ai_org_framework$Value[ai_org_framework$Area == "Yes"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_org_framework_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 4th Plot of Wave 4: AI frameworks and guidelines used by organizations
  ###########################################################################################################################################
  # Which frameworks and guidelines does your organization have for AI use? (e.g., OECD, IEEE, internal proprietary) 
  
  output$ai_framework_use_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_framework_use$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Internal", data = ai_framework_use$Value[ai_framework_use$Area == "Internal"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "External", data = ai_framework_use$Value[ai_framework_use$Area == "External"], color = '#6C8ACC',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Mixture of different frameworks", data = ai_framework_use$Value[ai_framework_use$Area == "Mixture of different frameworks"], color = '#BCCDE6',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_framework_use_sample", textContent.wave4)
  
  
  ###########################################################################################################################################
  # 5th Plot of Wave 4: Intentions for organizational adoption of AI frameworks and guidelines
  ###########################################################################################################################################
  # Does your organization plan to implement a framework or guidelines for future AI use?
  # ●	Yes
  # ●	No
  # ●	Don't know
  
  output$ai_framework_future_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_framework_future$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Yes", data = ai_framework_future$Value[ai_framework_future$Area == "Yes"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_framework_future_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 6th Plot of Wave 4: Stakeholder involvement in organizational AI framework and guidelines development
  ###########################################################################################################################################
  # Are you/ will you be involved in drafting the framework or guidelines? 
  # ●	Yes
  # ●	No
  # ●	Don't know
  
  output$ai_framework_draft_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_framework_draft$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 101) %>%
      hc_add_series(name = "Yes", data = ai_framework_draft$Value[ai_framework_draft$Area == "Yes"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_framework_draft_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 7th Plot of Wave 4: Organizational compliance with AI framework and guidelines
  ###########################################################################################################################################
  # Question: How does/ will your organization ensure that everyone complies with the framework or guidelines? 
  
  output$ai_compliance_chart <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 0) %>%
      hc_xAxis(categories = unique(ai_compliance$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Europe<br>(n = 30)", data = ai_compliance$share[ai_compliance$region == "Europe<br>(n = 30)"],
                    color = '#4B77C5',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "USA<br>(n = 18)", data = ai_compliance$share[ai_compliance$region == "USA<br>(n = 18)"],
                    color = '#7F956B',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(bar = list(borderRadius = "0%",
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")),
                                groupPadding = 0.1)) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(title = list(text = 'Region<br><i><span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</i></span>'),
                align = "left", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2, useHTML = TRUE,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_compliance_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 8th Plot of Wave 4:  Confidence in managing AI privacy challenges
  ###########################################################################################################################################
  # Do you feel confident that you/ your organization will be able to address privacy challenges that may arise when using AI tools or systems in your work?
  # ●	Yes
  # ●	No
  # ●	Don't know
  
  output$ai_challenge_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_challenge$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Yes", data = ai_challenge$Value[ai_challenge$Area == "Yes"], color = '#244775',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(column = list(borderWidth = 0,
                                   pointWidth = 55,
                                   dataLabels = list(enabled = TRUE,
                                                     align = "center",
                                                     verticalAlign = "bottom",
                                                     inside = FALSE,
                                                     borderWidth = 0,
                                                     y = -1,  # Adjust the y position to move the label above the bar
                                                     format = '{point.y:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_challenge_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 9th Plot of Wave 4: Intentions for managing AI privacy challenges
  ###########################################################################################################################################
  # Question: How do you/ does your organization plan to address such challenges? 
  
  output$ai_challenge_plan_chart <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 0) %>%
      hc_xAxis(categories = unique(ai_challenge_plan$topics_ai_challenge_plan_oe),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Europe<br>(n = 11)", data = ai_challenge_plan$share[ai_challenge_plan$region_combined == "Europe<br>(n = 11)"],
                    color = '#4B77C5',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "USA<br>(n = 3)", data = ai_challenge_plan$share[ai_challenge_plan$region_combined == "USA<br>(n = 3)"],
                    color = '#7F956B',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_plotOptions(bar = list(borderRadius = "0%",
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")),
                                groupPadding = 0.1)) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(title = list(text = 'Region<br><i><span style="font-size: 9px; color: #666; font-weight: normal">(Click to hide)</i></span>'),
                align = "left", layout = "vertical", verticalAlign = "middle",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2, useHTML = TRUE,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_challenge_plan_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 10th Plot of Wave 4:  Confidence in managing AI privacy challenges
  ###########################################################################################################################################
  # Are you familiar with “Responsible AI” principles?
  # ●	Not at all
  # ●	Somewhat
  # ●	A great deal
  
  output$ai_responsible_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(ai_responsible$category),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "A great deal", data = ai_responsible$Value[ai_responsible$Area == "A great deal"], color = '#E69037',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Somewhat", data = ai_responsible$Value[ai_responsible$Area == "Somewhat"], color = '#F6BB51',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Not at all", data = ai_responsible$Value[ai_responsible$Area == "Not at all"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave4("ai_responsible_sample", textContent.wave4)
  
  ###########################################################################################################################################
  # 1st plots of Wave 5: Ranking approaches to protecting digital privacy
  ###########################################################################################################################################
  ############# Q: priorities (wave 5)
  # How would you rank the importance of the following approaches to protecting people’s digital privacy?
  # priorities_techsolutions = Designing and deploying privacy-preserving technology
  # priorities_processing = Regulating how data is processed, stored and shared
  # priorities_enforcement = Enforcing rules about how data is processed, stored and shared
  # priorities_control = Giving individuals control over their data
  # priorities_adaptlaw = Adapting privacy laws to respond to current technological developments
  
  output$priorities_w5_Europe <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>Europe</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_w5_Europe$priorities_short),
               labels = list(style = list(fontSize = "12px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 5: Least important", data = priorities_w5_Europe$Value[priorities_w5_Europe$Important == "Rank 5 (Least important)"], color = '#D9E0D2',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 4) %>%
      hc_add_series(name = "Rank 4", data = priorities_w5_Europe$Value[priorities_w5_Europe$Important == "Rank 4"], color = '#B4C7A4',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Rank 3", data = priorities_w5_Europe$Value[priorities_w5_Europe$Important == "Rank 3"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Rank 2", data = priorities_w5_Europe$Value[priorities_w5_Europe$Important == "Rank 2"], color = '#51704A',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_w5_Europe$Value[priorities_w5_Europe$Important == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1,
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  
  output$priorities_w5_USA <- renderHighchart({
    highchart() %>%
      hc_chart(type = "bar",
               height = 400,
               marginTop = 130) %>%
      hc_title(text = "<b>USA</b>", margin = 1,
               align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = unique(priorities_w5_USA$priorities_short),
               labels = list(style = list(fontSize = "12px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "Rank 5: Least important", data = priorities_w5_USA$Value[priorities_w5_USA$Important == "Rank 5 (Least important)"], color = '#D9E0D2',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 4) %>%
      hc_add_series(name = "Rank 4", data = priorities_w5_USA$Value[priorities_w5_USA$Important == "Rank 4"], color = '#B4C7A4',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 3) %>%
      hc_add_series(name = "Rank 3", data = priorities_w5_USA$Value[priorities_w5_USA$Important == "Rank 3"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Rank 2", data = priorities_w5_USA$Value[priorities_w5_USA$Important == "Rank 2"], color = '#51704A',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "Rank 1: Most important", data = priorities_w5_USA$Value[priorities_w5_USA$Important == "Rank 1 (Most important)"], color = '#30482D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_plotOptions(bar = list(stacking = "normal",
                                borderWidth = 0,
                                pointWidth = 25,
                                groupPadding = 0.1, 
                                pointPadding = 0.1,
                                dataLabels = list(enabled = TRUE,
                                                  borderWidth = 0,
                                                  format = '{point.percentage:.0f}%',
                                                  style = list(textShadow = FALSE,
                                                               fontSize = "16px", fontFamily = "Assistant",
                                                               fontWeight = "normal",
                                                               textOutline = "none")
                                ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "horizontal", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
  })
  
  serverTextDisplay.wave5("priorities_w5_sample", textContent.wave5)
  
  ###########################################################################################################################################
  # 2nd plots of Wave 5: 
  ###########################################################################################################################################
  ############# Q: EU_AI_act
  # Question: In your opinion, will the EU AI Act be more likely to enable or hinder AI innovation?
  # Answers:
  # More likely to enable
  # Neither enable nor hinder
  # More likely to hinder
  # Don't know
  
  
  output$EU_AI_act_chart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column",
               height = 400,
               marginTop = 120) %>%
      hc_xAxis(categories = unique(EU_AI_act$category_n),
               labels = list(style = list(fontSize = "14px", color = "black")),
               lineColor = "lightgrey") %>%
      hc_yAxis(labels = list(enabled = FALSE), gridLineWidth = 0, minorGridLineWidth = 0, lineWidth = 0, tickWidth = 0, max = 100) %>%
      hc_add_series(name = "More likely to enable", data = EU_AI_act$Value[EU_AI_act$Answer == "More likely to enable"], color = '#071A2D',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 0) %>%
      hc_add_series(name = "Neither enable nor hinder", data = EU_AI_act$Value[EU_AI_act$Answer == "Neither enable nor hinder"], color = '#244775',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 1) %>%
      hc_add_series(name = "More likely to hinder", data = EU_AI_act$Value[EU_AI_act$Answer == "More likely to hinder"], color = '#7F956B',
                    dataLabels = list(color = "white",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_add_series(name = "Don't know", data = EU_AI_act$Value[EU_AI_act$Answer == "Don't know"], color = '#FCDC97',
                    dataLabels = list(color = "black",
                                      style = list(fontWeight = "normal"),
                                      format = '{point.y:.0f}%'),
                    legendIndex = 2) %>%
      hc_plotOptions(column = list(stacking = "normal",
                                   borderWidth = 0,
                                   pointWidth = 55,
                                   groupPadding = 0.1, # reduce the space between groups of columns
                                   pointPadding = 0.1, # reduce the space between individual columns within the same group
                                   dataLabels = list(enabled = TRUE,
                                                     borderWidth = 0,
                                                     format = '{point.percentage:.0f}%',
                                                     style = list(textShadow = FALSE,
                                                                  fontSize = "16px", fontFamily = "Assistant",
                                                                  fontWeight = "normal",
                                                                  textOutline = "none")
                                   ))) %>%
      hc_tooltip(headerFormat = "", pointFormat = "<b>{series.name}</b>: {point.y:.0f}%") %>%
      hc_legend(align = "center", layout = "vertical", verticalAlign = "top",
                symbolHeight = 10, symbolWidth = 10, symbolRadius = 2,
                itemStyle = list(fontFamily = "Assistant", fontSize = "15px", fontWeight = "normal",
                                 color= "black")) %>%
      hc_chart(events = list(load = JS("function(){this.update({marginBottom: this.legend.legendHeight + 20})}"))) %>%
      hc_exporting(enabled = FALSE) # set to true if you want the possibility to download the plot
    
  })
  
  serverTextDisplay.wave5("EU_AI_act_sample", textContent.wave5)
  
  
  
  ################################################################################################   
  
  output$tabelle <- renderDT({
    datatable(
      erstelle_tabelle(),
      options = list(
        dom = 'lBfrtip',
        pageLength = 50,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#195365', 'color': '#fff'});",
          "}"
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'),
        scrollX = TRUE
      )
    )
  })
}



# Run Shiny App
shinyApp(ui = ui, server = server)