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
  tabItems(erstelleTab_front()),
  
  tags$script(
    src = "https://cdn.jsdelivr.net/gh/Appsilon/shiny.tictoc@v0.2.0/shiny-tic-toc.min.js"
  )
  
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
  
  observe({
    shinyjs::click("btn_current_state")
    shinyjs::click("btn_law_favor_tech")
    shinyjs::click("btn_current")
    shinyjs::click("btn_current_w1")
    shinyjs::click("btn_policymaking_US")
  })
    
    
  ################################ code example that will draw stacked bar chart

  questionL2 <- reactive({
    req(input$Organisations.stacked.bar.chart)
    filter(orgs_rating_priv_update, Organisation %in% input$Organisations.stacked.bar.chart)
  })
  
  output$Organisations1.chart <- renderEcharts4r({
    input$Organisations.stacked.bar.chart
    questionL2()%>%
      e_charts(wave) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "20%", width = "auto", height = "auto") %>%
      e_bar(`Poor/Fair`, stack = "stack", name = "Poor/Fair", barWidth = 50) %>%
      e_bar(`Good/Excellent`, stack = "stack", name = "Good/Excellent", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775","#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show = FALSE, delay = 0)
    
  })
  
  
#Information button
  textVisible <- reactiveVal(FALSE)
  observeEvent(input$toggleText, {
    textVisible(!textVisible())
  })
  output$interpretation <- renderUI({
    if (textVisible()) {
             h6(HTML("The expert perception of privacy protection can vary greatly depending on the context of data collection
             and the methods of data protection.
             Government agencies like the U.S. Census Bureau, which collect data for statistical purposes and
             are bound by strict usage limitations, often garner higher trust and better privacy ratings.
             This trust is bolstered by their use of robust privacy-preserving techniques such as differential privacy,
             which are designed to ensure the anonymity of the data.
             <p/> Conversely, companies that collect data for multiple purposes
             including targeted advertising — may face greater skepticism and privacy concerns
             due to the potential for broader and more opaque use of personal information."))
    }
  })
 
  
serverTextDisplay.time.series("organisations_sample", textContent.time.series)

## Combined: Question 10 & Question 11

selected_data_tech.innovation <- reactiveVal(NULL)
selected_plot_tech.innovatio <- reactiveVal(NULL)

observeEvent(input$btn_law_favor_tech, {
  selected_data_tech.innovation(law_favor_tech)
  selected_plot_tech.innovatio("tech")
})

observeEvent(input$btn_law_innovation, {
  selected_data_tech.innovation(law_innovation)
  selected_plot_tech.innovatio("innovation")
})

output$innovation.tech.combinedTitle <- renderUI({
  req(selected_plot_tech.innovatio())
  if (selected_plot_tech.innovatio() == "tech") {
    tagList(
      h4(HTML("Do you think digital privacy laws and regulations in your region* more strongly favor
                                                                the rights and needs of businesses or of individual users?")),
      h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
    )
  } else if (selected_plot_tech.innovatio() == "innovation") {
    tagList(
      h4(HTML("Do you think digital privacy laws and regulations in your region*
           <br/>encourage or discourage innovation and development of privacy-preserving practices
           <br/>and technologies in organizations?")),
      h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
    )
  } else {
    NULL
  }
})

output$innovation.tech.combined.chart <- renderEcharts4r({
  req(selected_data_tech.innovation(), selected_plot_tech.innovatio())
  if (selected_plot_tech.innovatio() == "tech") {
    law_favor_tech <- law_favor_tech %>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`(Somewhat/Strongly) favor businesses`, stack = "stack", 
            name = "(Somewhat/Strongly) favor businesses", barWidth = 50) %>%
      e_bar(`Favor neither businesses nor individual users`, stack = "stack", name = "Favor neither businesses nor individual users", barWidth = 50) %>%
      e_bar(`(Strongly/Somewhat) favor individual users`, stack = "stack", 
            name = "(Strongly/Somewhat) favor individual users", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775","#7F956B", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
    law_favor_tech %>% htmlwidgets::onRender(resize_js)
    
  } else if (selected_plot_tech.innovatio() == "innovation") {
    law_innovation<- law_innovation %>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`(Strongly/Somewhat) discourage innovation and development`, stack = "total", 
            name = "(Strongly/Somewhat) discourage innovation and development", barWidth = 50) %>%
      e_bar(`Neutral`, stack = "total", name = "Neutral", barWidth = 50) %>%
      e_bar(`(Somewhat/Strongly) encourage innovation and development`, stack = "total", 
            name = "(Somewhat/Strongly) encourage innovation and development", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775", "#7F956B", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, inverse= TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
    law_innovation %>% htmlwidgets::onRender(resize_js) 
    
  }
})

textVisible.innovation.tech.combined <- reactiveVal(FALSE)
observeEvent(input$toggleText.innovation.tech.combined, {
  textVisible.innovation.tech.combined(!textVisible.innovation.tech.combined())
})
output$interpretation.innovation.tech.combined <- renderUI({
  if (textVisible.innovation.tech.combined()) {
    h6(HTML("The insights from privacy experts suggest that current digital privacy laws are 
            pro-business  are not inherently at odds with fostering innovation. </p>
            Across the board, there's a consensus that privacy laws are skewed towards business interests. 
            In the U.S., 82% of practitioners and 100% of academics, and in Europe, 64% of practitioners agree.
However, when it comes to fostering innovation, 
there's a perceived contradiction. In the U.S., practitioners are evenly split on whether laws encourage or discourage innovation.
European practitioners are more optimistic about the laws encouraging innovation, yet 54% of European academics see them as stifling.</p>
            The survey findings challenge the traditional view that privacy laws may hinder technological advancement: 
            the feedback from experts across the Atlantic indicates that such regulations are not universally viewed as a barrier to innovation.
            "
            
            
            ))
  }
})
serverTextDisplay.wave2("innovation.tech.combined_sample", textContent.wave2)

  
  # ###########################################################################################################################################

  Question.law_breadth <- reactive({
    req(input$law_breadth.category)
    filter(law_breadth, category %in% input$law_breadth.category)
  })
  
  output$law_breadth.chart <- renderEcharts4r({
    input$law_breadth.category
    Question.law_breadth()%>%
      e_charts(wave) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Fewer areas than needed`, stack = "stack", name = "Fewer areas than needed", barWidth = 50) %>%
      e_bar(`All areas needed`, stack = "stack", name = "All areas needed", barWidth = 50) %>%
      e_bar(`More areas than needed`, stack = "stack", name = "More areas than needed", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775","#7F956B", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show = FALSE, delay = 0)
    
  })
  
  # Information button
  textVisible.law_breadth <- reactiveVal(FALSE)

  observeEvent(input$Text_law_breadth, {
    textVisible.law_breadth(!textVisible.law_breadth())
  })

  output$interpretation_law_breadth <- renderUI({
    if (textVisible.law_breadth()) {
      h6(HTML("The perception of the sufficiency of digital privacy laws varies greatly by region, 
      reflecting the influence of local cultural attitudes and existing legal frameworks. 
      European practitioners' shift from a 50-50 split on the adequacy of privacy 
      laws to a 64% majority favoring more comprehensive coverage signals emerging concerns or gaps that may have been overlooked previously. 
      In the U.S., the persistent majority consistently indicating a deficiency in the laws.
      This suggests for a tailored approach to privacy legislation that considers the specific expectations 
      of each region players."))
    }
  })
serverTextDisplay.time.series("law_breadth_sample", textContent.time.series)
  

  ## Q11 
  
  output$law_depth.chart <- renderEcharts4r({
    law_depth %>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Poor/Fair`, stack = "stack", 
            name = "Poor/Fair", barWidth = 50) %>%
      e_bar(`Good/Excellent`, stack = "stack", 
            name = "Good/Excellent", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775","#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
  })
  
  textVisible.Law_depth <- reactiveVal(FALSE)
  observeEvent(input$toggleText.Law_depth, {
    textVisible.Law_depth(!textVisible.Law_depth())
  })
  output$interpretation.Law_depth<- renderUI({
    if (textVisible.Law_depth()) {
      h6(HTML("<br/>This chart is based on a sample size of 64 privacy experts. The data were collected in October 2022"))
    }
  })
  
serverTextDisplay.wave1("Law_depth_sample", textContent.wave1)
  
  ## Q20 

  question.model <- reactive({
    req(input$model.input)
    filter(model_final, model %in% input$model.input)
  })
  
  output$model.chart <- renderEcharts4r({
    input$model.input
    model.chart<-question.model()%>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Not at all important to include`, stack = "stack", 
            name = "Not at all important to include", barWidth = 50) %>%
      e_bar(`Somewhat important to include`, stack = "stack", 
            name = "Somewhat important to include", barWidth = 50) %>%
      e_bar(`Important to include`, stack = "stack", 
            name = "Important to include", barWidth = 50) %>%
      e_bar(`Very important to include`, stack = "stack", 
            name = "Very important to include", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#681E13","#E69037", "#F6BB51", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 12) %>%
      e_theme("wonderland") %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE)
    
    model.chart %>% htmlwidgets::onRender(resize_js)
  })
  
  textVisible.model <- reactiveVal(FALSE)
  observeEvent(input$toggleText.model, {
    textVisible.model(!textVisible.model())
  })
  output$interpretation.model <- renderUI({
    if (textVisible.model()) {
      h6(HTML("<br/>
          This chart is based on a sample size of 74 privacy experts. 
            The data were collected in August 2023"))
    }
    })   
  
  serverTextDisplay.wave2("model_sample", textContent.wave2) 
  
#=======


  ## Q15
  
  
  question15 <- reactive({
    req(input$Enforcement.stacked.bar.chart15)
    filter(orgs_rating_priv.15, category %in% input$Enforcement.stacked.bar.chart15)
  })
  
  output$Enforcement.chart15 <- renderEcharts4r({
    input$Enforcement.stacked.bar.chart15
    question15()%>%
      e_charts(wave) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Not at all`, stack = "stack", name = "Not at all", barWidth = 50) %>%
      e_bar(`A little`, stack = "stack", name = "A little", barWidth = 50) %>%
      e_bar(`Somewhat`, stack = "stack", name = "Somewhat", barWidth = 50) %>%
      e_bar(`Mostly`, stack = "stack", name = "Mostly", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#681E13","#E69037", "#F6BB51", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 14) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE)
    
  })
  
  
  textVisible.law_enforcement <- reactiveVal(FALSE)
  observeEvent(input$toggleText.law_enforcement, {
    textVisible.law_enforcement(!textVisible.law_enforcement())
  })
  output$interpretation.law_enforcement <- renderUI({
    if (textVisible.law_enforcement()) {
      h6(HTML("The responses reflect a broad skepticism about the enforcement of digital privacy laws, 
              with a notable discrepancy between policy and practice. 
              Particularly in the U.S., the increase in doubt from October 2022 to August 2023 among practitioners 
              points to a growing concern that current enforcement mechanisms are insufficient. 
              In Europe, although the sentiment is somewhat more positive, 
              there remains a significant portion of experts who question the efficacy of enforcement. 
              This data highlights the crucial need for a better understanding of regulatory structures 
              and their implementation."))
    }
  })
  
serverTextDisplay.time.series("law_enforcement_sample", textContent.time.series)
  
  # Q16 
  # Q17 
  
  selected_data_state_law <- reactiveVal(NULL)
  selected_plot_state_law <- reactiveVal(NULL)
  
  observeEvent(input$btn_current_state, {
    selected_data_state_law(orgs_rating_priv.16)
    selected_plot_state_law("current")
  })
  
  observeEvent(input$btn_future_state, {
    selected_data_state_law(orgs_rating_priv.17)
    selected_plot_state_law("future")
  })
  
  output$StatelawCombinedTitle <- renderUI({
    req(selected_plot_state_law())
    if (selected_plot_state_law() == "current") {
      tagList(
        h4(HTML("Overall, how would you rate digital <strong>privacy laws and regulations </strong> in your region* today?")),
        h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
      )
    } else if (selected_plot_state_law() == "future") {
      tagList(
        h4(HTML("In the next few years, how optimistic or pessimistic are you that digital <strong>privacy laws and regulations</strong> 
                                                 in your region* will move in the direction you prefer?")),
        h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
      )
    } else {
      NULL
    }
  })
  
  output$StatelawCombined.chart <- renderEcharts4r({
    req(selected_data_state_law(), selected_plot_state_law())
    if (selected_plot_state_law() == "current") {
      StateCurrentLaw.chart<- orgs_rating_priv.16 %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`Poor/Fair`, stack = "stack", 
              name = "Poor/Fair", barWidth = 50) %>%
        e_bar(`Good/Excellent`, stack = "stack", name = "Good/Excellent", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE)
      
      StateCurrentLaw.chart %>% htmlwidgets::onRender(resize_js)
      
    } else if (selected_plot_state_law() == "future") {
      StateOutlookLaw.chart<-orgs_rating_priv.17 %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`(very/somewhat) pessimistic`, stack = "stack", name = "(Very/Somewhat) Pessimistic", barWidth = 50) %>%
        e_bar(`neither`, stack = "stack", 
              name = "Neither", barWidth = 50) %>%
        e_bar(`(somewhat/very) optimistic`, stack = "stack", 
              name = "(Somewhat/Very) Optimistic", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775","#7F956B", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE)
      
      StateOutlookLaw.chart %>% htmlwidgets::onRender(resize_js)
    }
  })
  
  textVisible.StatelawCombined <- reactiveVal(FALSE)
  observeEvent(input$toggleText.StatelawCombined, {
    textVisible.StatelawCombined(!textVisible.StatelawCombined())
  })
  output$interpretation.StatelawCombined <- renderUI({
    if (textVisible.StatelawCombined()) {
      h6(HTML("In an era where digital policy shapes societal norms, 
      asking privacy experts on the effectiveness and future trajectory of digital privacy laws is vital. 
      Such insights gauge the legal framework's current efficacy and anticipate its adaptability to emerging digital challenges. </p>
All U.S. practitioners rate current digital privacy laws as poor/fair, with European practitioners slightly more optimistic.
Academics on both continents express concerns, with a majority rating the laws as poor/fair.
Looking ahead, there's a tempered optimism among experts, with no clear consensus on the future direction of privacy laws.</p>
Understanding these perspectives informs policymakers and stakeholders about the confidence 
              level experts hold in current laws and their expectations for future reforms. 
              These insights can help steer the evolution of privacy regulations 
              in a direction that earns expert approval and meets societal needs."))
    }
  })
  serverTextDisplay.wave2("StatelawCombined_sample", textContent.wave2)
  
  # Q26
  
  
  output$StateCurrentPra.chart <- renderEcharts4r({
    StateCurrentPra.chart <- orgs_rating_priv.26 %>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Poor/Fair`, stack = "stack", 
            name = "Poor/Fair", barWidth = 50) %>%
      e_bar(`Good/Excellent`, stack = "stack", name = "Good/Excellent", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
    StateCurrentPra.chart %>% htmlwidgets::onRender(resize_js)
    
  })
  
  
  textVisible.StateCurrentPra <- reactiveVal(FALSE)
  observeEvent(input$toggleText.StateCurrentPra, {
    textVisible.StateCurrentPra(!textVisible.StateCurrentPra())
  })
  output$interpretation.StateCurrentPra <- renderUI({
    if (textVisible.StateCurrentPra()) {
      h6(HTML("FILL OUT"))
    }
  })

  # Q27
  
  output$StateOutlookPra.chart <- renderEcharts4r({
    StateOutlookPra.chart <- orgs_rating_priv.27 %>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`(very/somewhat) pessimistic`, stack = "stack", name = "(very/somewhat) pessimistic", barWidth = 50) %>%
      e_bar(`neither`, stack = "stack", 
            name = "neither", barWidth = 50) %>%
      e_bar(`(somewhat/very) optimistic`, stack = "stack", 
            name = "(somewhat/very) optimistic", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775","#7F956B", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
    StateOutlookPra.chart %>% htmlwidgets::onRender(resize_js)
    
  })
  
  textVisible.StateOutlookPra <- reactiveVal(FALSE)
  observeEvent(input$toggleText.StateOutlookPra, {
    textVisible.StateOutlookPra(!textVisible.StateOutlookPra())
  })
  output$interpretation.StateOutlookPra <- renderUI({
    if (textVisible.StateOutlookPra()) {
      h6(HTML("FILL OUT"))
    }
  })

  # Q26 
  # Q27 
  
  
  selected_data <- reactiveVal(NULL)
  selected_plot <- reactiveVal(NULL)

  
  observeEvent(input$btn_current, {
    selected_data(orgs_rating_priv.26)
    selected_plot("current")
  })
  
  observeEvent(input$btn_future, {
    selected_data(orgs_rating_priv.27)
    selected_plot("future")
  })
  
  output$StatePraCombinedTitle <- renderUI({
    req(selected_plot())
    if (selected_plot() == "current") {
      tagList(
      h4(HTML("Overall, how would you rate <strong>organizations’ digital privacy policies and 
              practices</strong> in your region* today?")),
      h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
      )
    } else if (selected_plot() == "future") {
      tagList(
      h4(HTML("In the next few years, how optimistic or pessimistic are you that <strong>organizations’ digital privacy policies
              and practices</strong> in your region* will move in the direction you prefer?")),
      h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
      )
    } else {
      NULL
    }
  })
  
  output$StatePraCombined.chart <- renderEcharts4r({
    req(selected_data(), selected_plot())
    if (selected_plot() == "current") {
      orgs_rating_priv.26 %>%
        
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`Poor/Fair`, stack = "stack", 
              name = "Poor/Fair", barWidth = 50) %>%
        e_bar(`Good/Excellent`, stack = "stack", name = "Good/Excellent", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE) %>%
        htmlwidgets::onRender(resize_js)
      
    } else if (selected_plot() == "future") {
      
      orgs_rating_priv.27 %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`(very/somewhat) pessimistic`, stack = "stack", name = "(very/somewhat) pessimistic", barWidth = 50) %>%
        e_bar(`neither`, stack = "stack", 
              name = "neither", barWidth = 50) %>%
        e_bar(`(somewhat/very) optimistic`, stack = "stack", 
              name = "(somewhat/very) optimistic", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775","#7F956B", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE) %>%
        htmlwidgets::onRender(resize_js)
    }
  })
  
  textVisible.StatePraCombined <- reactiveVal(FALSE)
  observeEvent(input$toggleText.StatePraCombined, {
    textVisible.StatePraCombined(!textVisible.StatePraCombined())
  })
  output$interpretation.StatePraCombined <- renderUI({
    if (textVisible.StatePraCombined()) {
      h6(HTML("Assessing the current state and anticipated progression of organizations' 
      digital privacy policies offers critical insights into the readiness of corporations and agencies to adapt to evolving privacy standards.</p>
      There is a unanimous view among U.S. practitioners that current corporate privacy policies require considerable enhancement.
European experts present a slightly more favorable view, but the general consensus still suggests the need for substantial improvements.
The future outlook is cautiously optimistic with a significant portion of experts remaining neutral, 
reflecting the dynamic and evolving nature of data protection.</p>
These perspectives shed light on the necessity for ongoing improvement 
              in organizational data privacy practices. The cautious optimism for the future reflects a recognition 
              of the efforts underway to strengthen data protection measures. It is imperative for corporations to continue 
              to evolve their data privacy policies to meet both regulatory expectations and the trust of the public."))
    }
  })
  
  serverTextDisplay.wave2("StatePraCombined_sample", textContent.wave2)
  
  ## Q21 
  
  question.model.q21 <- reactive({
    req(input$model.input.q21)
    filter(model_final_q21, model %in% input$model.input.q21)
  })
  
  output$model.chart.q21 <- renderEcharts4r({
    input$model.input.q21
    model.chart.q21 <- question.model.q21()%>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Not at all important to include`, stack = "stack", 
            name = "Not at all important to include", barWidth = 50) %>%
      e_bar(`Somewhat important to include`, stack = "stack", 
            name = "Somewhat important to include", barWidth = 50) %>%
      e_bar(`Important to include`, stack = "stack", 
            name = "Important to include", barWidth = 50) %>%
      e_bar(`Very important to include`, stack = "stack", 
            name = "Very important to include", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#681E13","#E69037", "#F6BB51", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE)
    
    model.chart.q21 %>% htmlwidgets::onRender(resize_js)
  })
  
  textVisible.model.q21 <- reactiveVal(FALSE)
  observeEvent(input$toggleText.model.q21, {
    textVisible.model.q21(!textVisible.model.q21())
  })
  output$interpretation.model.q21 <- renderUI({
    if (textVisible.model.q21()) {
      h6(HTML("FILL OUT"))
    }
  })
  
  serverTextDisplay.wave2("model.q21_sample", textContent.wave2)

  ## Q5
  question.offline_activism <- reactive({
    req(input$offline_activism.input)
    filter(orgs_rating_priv.5, offline_activism %in% input$offline_activism.input)
  })
  
  output$offline_activism.chart <- renderEcharts4r({
    input$offline_activism.input
    offline_activism.chart <- question.offline_activism()%>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Share`, name = "% of experts", barWidth = 50, stack = "grp") %>%
      e_y_axis(max=100)%>%
      e_format_y_axis(suffix = "%")%>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_color(c("#071A2D"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE)
    
    offline_activism.chart %>% htmlwidgets::onRender(resize_js)
  
  })
  textVisible.offline_activism <- reactiveVal(FALSE)
  observeEvent(input$toggleText.offline_activism, {
    textVisible.offline_activism(!textVisible.offline_activism())
  })
  output$interpretation.offline_activism <- renderUI({
    if (textVisible.offline_activism()) {
      h6(HTML("<br/>
          FILL OUT"))
    }
  })
  serverTextDisplay.wave2("offline_activism_sample", textContent.wave2)


  ## Q6
  
  question.online_activism <- reactive({
    req(input$online_activism.input)
    filter(orgs_rating_priv.6, online_activism %in% input$online_activism.input)
  })
  
  output$online_activism.chart <- renderEcharts4r({
    input$online_activism.input
    online_activism.chart <- question.online_activism()%>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Share`, name = "% of experts", barWidth = 50, stack = "grp") %>%
      e_y_axis(max=100)%>%
      e_format_y_axis(suffix = "%")%>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_color(c("#681E13"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE)
    
    online_activism.chart %>% htmlwidgets::onRender(resize_js)
    
  })
  textVisible.online_activism <- reactiveVal(FALSE)
  observeEvent(input$toggleText.online_activism, {
    textVisible.online_activism(!textVisible.online_activism())
  })
  output$interpretation.online_activism <- renderUI({
    if (textVisible.online_activism()) {
      h6(HTML("<br/>
          FILL OUT"))
    }
  })
  serverTextDisplay.wave2("online_activism_sample", textContent.wave2)
  
  ## Q1 
  
  output$gen_AI.chart <- renderEcharts4r({
    gen_AI<- gen_AI %>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Yes`, stack = "total", 
            name = "Yes", barWidth = 50) %>%
     # e_bar(`No`, stack = "total", name = "No", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#244775", "#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, inverse= TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
    gen_AI %>% htmlwidgets::onRender(resize_js) 
    
  })
  
  textVisible.gen_AI <- reactiveVal(FALSE)
  observeEvent(input$toggleText.gen_AI, {
    textVisible.gen_AI(!textVisible.gen_AI())
  })
  output$interpretation.gen_AI <- renderUI({
    if (textVisible.gen_AI()) {
      h6(HTML("
      In an increasingly data-driven world, understanding how generative AI shapes privacy policy is crucial 
      for safeguarding personal freedoms and ensuring responsible AI governance.</p>
Over half (66%) of global privacy experts report that generative AI has influenced their views on privacy policy.
American professionals are more unified in their stance, with a significant majority 
(89% of practitioners and 79% of academics) affirming this influence.
European academics show a split perspective, with less than half (48%) agreeing, 
highlighting regional differences in the perception of AI's impact on privacy.</p>
The survey indicates a trend: generative AI is an important factor in rethinking privacy policies. 
              This necessitates a collaborative effort to align AI advancements with robust privacy standards, 
              especially in light of differing regional views that could affect international cooperation and policy harmonization." )) }
  })
  
  serverTextDisplay.wave3("gen_AI_sample", textContent.wave3)

  output$ai.yes.chart <- renderEcharts4r({
    ai.yes.temp <- wave3_yes %>%
      e_charts(category) %>%
      e_grid(left = "450px", right = "10px", top = "10%", bottom = "15%", width = "auto", height = "auto") %>%
      e_bar(`Share`, stack = "stack", 
            name = "Share", barWidth = 30) %>%
      e_flip_coords() %>%
      e_x_axis(max=55)%>%
      e_y_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_x_axis(suffix = "%")%>%
      e_color(c("#244775","#FCDC97"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = FALSE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show=FALSE)
    
    ai.yes.temp %>% htmlwidgets::onRender(resize_js) 
  })
  
  textVisible.ai.yes <- reactiveVal(FALSE)
  observeEvent(input$toggleText.ai.yes, {
    textVisible.ai.yes(!textVisible.ai.yes())
  })
  output$interpretation.ai.yes<- renderUI({
    if (textVisible.ai.yes()) {
      h6(HTML("Respondents noted several areas of concern, particularly with regard to the inadequacy or 
              lack of existing regulatory or normative consensus for uses of AI (such as deep fakes or copyright infringement), 
              the lack of transparency in data sourcing - particularly for training data - and the related issue of nonconsensual
              secondary uses of data, including training data."))
    }
  })
  
  serverTextDisplay.wave3("ai.yes_sample", textContent.wave3)
  
  
  ## Q2
  
  question.priorities <- reactive({
    req(input$Priorities.input)
    filter(priorities, Priorities %in% input$Priorities.input)
  })
  
  output$priorities.chart <- renderEcharts4r({
    input$Priorities.input
    question.priorities()%>%
      e_charts(wave) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
      e_bar(`Rank 1 (Most important)`, stack = "stack", name = "Rank 1 (Most important)", barWidth = 50) %>%
      e_bar(`Rank 2`, stack = "stack", name = "Rank 2", barWidth = 50) %>%
      e_bar(`Rank 3`, stack = "stack", name = "Rank 3", barWidth = 50) %>%
      e_bar(`Rank 4`, stack = "stack", name = "Rank 4", barWidth = 50) %>%
      e_bar(`Rank 5 (Least important)`, stack = "stack", name = "Rank 5 (Least important)", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#30482D","#51704A","#7F956B", "#B4C7A4", "#D9E0D2"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Save",
        type = "png",
        name = "echarts", lang = "Save")%>%
      e_animation(show = FALSE, delay = 0)
    
  })
  
  textVisible.priorities <- reactiveVal(FALSE)
  observeEvent(input$toggleText.priorities, {
    textVisible.priorities(!textVisible.priorities())
  })
  output$interpretation.priorities <- renderUI({
    if (textVisible.priorities()) {
      h6(HTML("The data shows a shift in privacy priorities from October 2022 to August 2023. 
              In October, the top priority was developing technical solutions to ensure individuals 
              cannot be identified, which aligns with a strong technical approach to privacy. 
              By August 2023, there's a notable shift towards adapting privacy laws, 
              which suggests an evolving perspective that emphasizes the need for legal frameworks 
              to keep pace with technological advancements. 
              The decrease in the ranking of giving individuals control over their data as 
              the most important priority could indicate a realization that individual control 
              is not sufficient on its own and must be complemented by systemic changes in privacy law and technology." )) }
  })
  
serverTextDisplay.time.series("priorities_sample", textContent.time.series)
    

  ## Q2

  question.priorities_w2 <- reactive({
    req(input$priorities_w2.input)
    filter(priorities_w2, priorities %in% input$priorities_w2.input)
  })
  
  output$priorities_w2.chart <- renderEcharts4r({
    input$priorities_w2.input
    priorities_w2.chart <- question.priorities_w2()%>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "40%", width = "auto", height = "auto") %>%
      e_bar(`Rank 1 (Most important)`, stack = "stack", name = "Rank 1 (Most important)", barWidth = 50) %>%
      e_bar(`Rank 2`, stack = "stack", name = "Rank 2", barWidth = 50) %>%
      e_bar(`Rank 3`, stack = "stack", name = "Rank 3", barWidth = 50) %>%
      e_bar(`Rank 4`, stack = "stack", name = "Rank 4", barWidth = 50) %>%
      e_bar(`Rank 5 (Least important)`, stack = "stack", name = "Rank 5 (Least important)", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#30482D","#51704A","#7F956B", "#B4C7A4", "#D9E0D2"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE, delay = 0)
    
    priorities_w2.chart %>% htmlwidgets::onRender(resize_js)
  })
  
  textVisible.priorities_w2 <- reactiveVal(FALSE)
  observeEvent(input$toggleText.priorities_w2, {
    textVisible.priorities_w2(!textVisible.priorities_w2())
  })
  
  output$interpretation.priorities_w2 <- renderUI({
    if (textVisible.priorities_w2()) {
      h6(HTML("Grasping the importance of strategic privacy priorities among professionals is critical for shaping an environment 
      where digital privacy is safeguarded within the evolving landscape of technology and regulation. </p>
      US practitioners view the design and deployment of privacy-preserving technology as a key priority, 
      with 45% ranking it as the most important. </p>
      Regulation of how data is processed, stored, and shared is given a highest priority among USA academics.</p>
      Enforcing rules about data processing is seen as less of a priority, especially in the USA. </p>
      Both practitioners and academics in the USA prioritize adapting privacy 
      laws to current technological developments, indicating an awareness of the rapid pace of digital innovation.The european practitioners have the least recognition for this area needing to be the top priority. </p>
      Giving individuals control over their data is ranked as least important priority by 59% of the US academics </p>


              laws that are not only reactive but also anticipatory of technological advancements. 
              This understanding is essential in the policy discourse to ensure that personal data protection 
              remains a fundamental right in an increasingly digital society." )) }
  })
  serverTextDisplay.wave2("priorities_w2_sample", textContent.wave2)

  ## Q2 
  
  question.priorities_w1 <- reactive({
    req(input$priorities_w1.input)
    filter(priorities_w1, priorities %in% input$priorities_w1.input)
  })
  
  output$priorities_w1.chart <- renderEcharts4r({
    input$priorities_w1.input
    priorities_w1.chart <- question.priorities_w1()%>%
      e_charts(category) %>%
      e_grid(left = "40px", right = "40px", top = "10%", bottom = "40%", width = "auto", height = "auto") %>%
      e_bar(`Rank 1 (Most important)`, stack = "stack", name = "Rank 1 (Most important)", barWidth = 50) %>%
      e_bar(`Rank 2`, stack = "stack", name = "Rank 2", barWidth = 50) %>%
      e_bar(`Rank 3`, stack = "stack", name = "Rank 3", barWidth = 50) %>%
      e_bar(`Rank 4`, stack = "stack", name = "Rank 4", barWidth = 50) %>%
      e_bar(`Rank 5 (Least important)`, stack = "stack", name = "Rank 5 (Least important)", barWidth = 50) %>%
      e_y_axis(max=100)%>%
      e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
      e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
      e_format_y_axis(suffix = "%")%>%
      e_color(c("#30482D","#51704A","#7F956B", "#B4C7A4", "#D9E0D2"))%>%
      e_labels(position="inside",fontSize = 16) %>%
      e_theme("wonderland") %>%
      etoolbox() %>%
      e_toolbox_feature(
        feature = "saveAsImage",
        show=TRUE,
        title="Speichern",
        type = "png",
        name = "echarts", lang = "Speichern")%>%
      e_animation(show=FALSE, delay = 0)
    
    priorities_w1.chart %>% htmlwidgets::onRender(resize_js)
  })
  
  textVisible.priorities_w1 <- reactiveVal(FALSE)
  observeEvent(input$toggleText.priorities_w1, {
    textVisible.priorities_w1(!textVisible.priorities_w1())
  })
  
  output$interpretation.priorities_w1 <- renderUI({
    if (textVisible.priorities_w1()) {
      h6(HTML("FILL OUT" )) }
  })
  
  serverTextDisplay.wave1("priorities_w1_sample", textContent.wave1)
  
  ##
  
  selected_data <- reactiveVal(NULL)
  selected_plot <- reactiveVal(NULL)
  
  observeEvent(input$btn_current_w1, {
    selected_data(state_current_w1)
    selected_plot("current")
  })
  
  observeEvent(input$btn_future_w1, {
    selected_data(state_outlook_w1)
    selected_plot("future")
  })
  
  # Render the selected title
  output$StateCombinedW1Title <- renderUI({
    req(selected_plot())
    if (selected_plot() == "current") {
      tagList(
        h4(HTML("Overall, how would you rate today’s <strong>digital privacy laws and organizational practices</strong> in your region*?")),
        h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
      )
    } else if (selected_plot() == "future") {
      tagList(
        h4(HTML("In the next few years, how optimistic or pessimistic are you that <strong>digital privacy laws and organizational practices</strong>
              in your region* will move in the direction you prefer?")),
        h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
      )
    } else {
      NULL
    }
  })
  
  # Render the selected plot
  output$StateCombinedW1.chart <- renderEcharts4r({
    req(selected_data(), selected_plot())
    if (selected_plot() == "current") {
      state_current_w1 %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`Poor/Fair`, stack = "stack", 
              name = "Poor/Fair", barWidth = 50) %>%
        e_bar(`Good/Excellent`, stack = "stack", name = "Good/Excellent", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE) %>%
        htmlwidgets::onRender(resize_js)
      
    } else if (selected_plot() == "future") {
      state_outlook_w1 %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`(Very/Somewhat) Pessimistic`, stack = "stack", name = "(Very/Somewhat) Pessimistic", barWidth = 50) %>%
        e_bar(`Neither`, stack = "stack", 
              name = "Neither", barWidth = 50) %>%
        e_bar(`(Somewhat/Very) Optimistic`, stack = "stack", 
              name = "(Somewhat/Very) Optimistic", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775","#7F956B", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE) %>%
        htmlwidgets::onRender(resize_js)
    }
  })
  
  textVisible.StateCombinedW1 <- reactiveVal(FALSE)
  observeEvent(input$toggleText.StateCombinedW1, {
    textVisible.StateCombinedW1(!textVisible.StateCombinedW1())
  })
  output$interpretation.StateCombinedW1 <- renderUI({
    if (textVisible.StateCombinedW1()) {
      h6(HTML("FILL OUT"))
    }
  })
  
  serverTextDisplay.wave1("StateCombinedW1_sample", textContent.wave1)
  
  #################### Combined: Question 18 & 19
  
  
  selected_data.policymaking <- reactiveVal(NULL)
  selected_plot.policymaking <- reactiveVal(NULL)
  
  observeEvent(input$btn_policymaking_US, {
    selected_data.policymaking(policymaking_US)
    selected_plot.policymaking("policymaking_US")
  })
  
  observeEvent(input$btn_policymaking_EU, {
    selected_data.policymaking(policymaking_EU)
    selected_plot.policymaking("policymaking_EU")
  })
  
  # Render the selected title
  output$policymakingCombinedTitle <- renderUI({
    req(selected_plot.policymaking())
    if (selected_plot.policymaking() == "policymaking_US") {
      h4(HTML("Do you think that digital privacy policy in the US should be made at the …"))
    } else if (selected_plot.policymaking() == "policymaking_EU") {
      h4(HTML("Do you think that digital privacy policy in the EU should be made at the …"))
    } else {
      NULL
    }
  })
  
  # Render the selected plot
  output$policymakingCombined.chart <- renderEcharts4r({
    req(selected_data.policymaking(), selected_plot.policymaking())
    if (selected_plot.policymaking() == "policymaking_US") {
      policymaking_US %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`Federal Level`, stack = "stack", name = "Federal Level", barWidth = 50) %>%
        e_bar(`State Level`, stack = "stack", name = "State Level", barWidth = 50) %>%
        e_bar(`Both`, stack = "stack", name = "Both", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775","#BCBCBC", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE) %>%
        htmlwidgets::onRender(resize_js)
      
    } else if (selected_plot.policymaking() == "policymaking_EU") {
      
      policymaking_EU %>%
        e_charts(category) %>%
        e_grid(left = "40px", right = "40px", top = "10%", bottom = "35%", width = "auto", height = "auto") %>%
        e_bar(`EU Level`, stack = "stack", name = "EU Level", barWidth = 50) %>%
        e_bar(`EU Member-State Level`, stack = "stack", name = "EU Member-State Level", barWidth = 50) %>%
        e_bar(`Both`, stack = "stack", 
              name = "Both", barWidth = 50) %>%
        e_y_axis(max=100)%>%
        e_x_axis(axisLabel = list(rotate = 0, textStyle = list(fontSize = 14)), splitLine = list(show = FALSE)) %>%
        e_format_y_axis(suffix = "%")%>%
        e_color(c("#244775","#BCBCBC", "#FCDC97"))%>%
        e_labels(position="inside",fontSize = 16) %>%
        e_theme("wonderland") %>%
        e_legend(show = TRUE, orient = "vertical", bottom = "0%", textStyle = list(fontSize = 14)) %>%
        etoolbox() %>%
        e_toolbox_feature(
          feature = "saveAsImage",
          show=TRUE,
          title="Save",
          type = "png",
          name = "echarts", lang = "Save")%>%
        e_animation(show=FALSE) %>%
        htmlwidgets::onRender(resize_js)
    }
  })
  
  textVisible.policymakingCombined <- reactiveVal(FALSE)
  observeEvent(input$toggleText.policymakingCombined, {
    textVisible.policymakingCombined(!textVisible.policymakingCombined())
  })
  output$interpretation.policymakingCombined <- renderUI({
    if (textVisible.policymakingCombined()) {
      h6(HTML("Majority of US practitioners, 82%, believe that digital privacy policy should be made at the federal level. 
              This suggests a strong preference for a unified national approach to digital privacy regulation among practitioners.</p>
              Majority of the US academics, 68%, prefer a dual approach where policy is made at both the federal and state levels. 
              This highlights a strong inclination towards a policy framework that integrates national uniformity 
              with the flexibility to cater to state-specific contexts.</p>
              A majority of EU practitioners 69% support the idea that digital privacy policy should be established at the EU level, 
              indicating a preference for a unified approach across member states. </p>
              The European academic community is split evenly, with 50% supporting EU-level 
              policymaking and another 50% advocating for a combination of EU and member-state level policymaking. </p>
              For businesses operating in multiple states or across national borders, a unified policy at the federal 
              or EU level could reduce complexity and help standardize privacy practices. 
              This clarity can lead to more efficient compliance strategies and potentially 
              lower costs for multi-state and international operations.
              
              
              "))
    }
  })
  
  serverTextDisplay.wave2("policymakingCombined_sample", textContent.wave2)
  

  # Q24
  # Q25

  output$opinions <- renderUI({
    req(input$Best.pratices)

    opinions <- best_practice[best_practice$org_model_oe == input$Best.pratices, "orgs_model_practice_oe", drop = FALSE]

    if (nrow(opinions) == 0) {
      return(HTML("No information available for this company."))
    } else {
      opinion_list <- lapply(opinions$orgs_model_practice_oe, function(opinion) {
        tags$li(h6(opinion))  # Inline style for color
      })

      do.call(tags$ul, opinion_list)
    }
  })
  #
  
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
