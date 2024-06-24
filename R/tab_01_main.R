load("datareadyforshiny.rds")

erstelleTab_front <- function() { 
  tabItem("front",class="active",
          fluidPage(id="main",
                    fluidRow(id="topRow",
                             column(width = 12,
                                    tabsetPanel(
                                      tabPanel(title = "Trend Analysis",
                                               fluidRow(
                                                 column(width=12,  
                                                        h3(HTML("Comprehensiveness of digital privacy laws")),
                                                        h4(HTML(" Increasing coverage, but improvements required ")),
                                                        h5(HTML("<i>Question: Do you think current digital privacy laws in your region [of expertise] cover more areas than needed,
                                                                fewer areas than needed, or all areas needed?</i>"))
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("law_breadth_chart_Europe")
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("law_breadth_chart_USA")
                                                 ),
                                                 column(width = 4, 
                                                        actionButton("toggleText.law_breadth_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.law_breadth_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Enforcement of digital privacy practices")),
                                                        h4(HTML("Mixed results by region")),
                                                        h5(HTML("<i>Question: Are the digital privacy practices required by your region's [of expertise]
                                                   law enforced?</i>"))
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("Enforcement_chart15_Europe")
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("Enforcement_chart15_USA")
                                                 ),
                                                 column(width = 4,
                                                        actionButton("toggleText.law_enforcement_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.law_enforcement_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width=12,  
                                                        h3(HTML("Stakeholder ratings of organizations’ privacy protection performance")),
                                                        h5(HTML("<i>Question: How would you rate the performance of these firms/agencies in protecting people's privacy?<br>Answer options: Excellent, Good, Fair, Poor</i>"))
                                                 ),
                                                 column(width = 2,
                                                        radioButtons(
                                                          width = 300,
                                                          inputId = "Organisations.stacked.bar.chart",
                                                          label = "Please choose a firm/agency",
                                                          choices = unique(orgs_rating_priv_update$Organisation), # Use levels to maintain order
                                                          selected = "Apple"
                                                        )),
                                                 column(width = 4,
                                                        highchartOutput("Organisations1_chart_Europe")
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("Organisations1_chart_USA")
                                                 ),
                                                 column(width = 2),
                                                 column(width = 4,
                                                        actionButton("toggleText.organisations_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.organisations_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Most important approach in protecting people‘s digital privacy")),
                                                        h4(HTML("Adapting privacy laws to technological developments")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?<br>Answer options: Adapting privacy laws to technological developments
                                                               </i>")
                                                        )),
                                                 column(width = 4,
                                                        highchartOutput("priorities_chart_ap1_Euro")
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("priorities_chart_ap1_USA")
                                                 ),
                                                 column(width = 4,
                                                        actionButton("toggleText.priorities_sample1", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_sample1")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Most important approach in protecting people‘s digital privacy")),
                                                        h4(HTML("Giving individuals control over their data")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?<br>Answer options: Giving individuals control over their data</i>"))
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("priorities_chart_ap2_Euro")
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("priorities_chart_ap2_USA")
                                                 ),
                                                 column(width = 4,
                                                        actionButton("toggleText.priorities_sample2", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_sample2")
                                                 )
                                               )
                                               
                                      ),
                                      tabPanel(title = "Wave 3 (December 2023)", #####  WAVE 3
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Generative AI’s influence on stakeholder views of privacy policy")),
                                                        h4(HTML("Majority of stakeholders anticipate change")),
                                                        h5(HTML("<i>Question: Has the rise of generative AI changed the way you think about privacy policy?<br>Answer options: Yes, No</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("gen_AI_chart")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.gen_AI_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.gen_AI_sample")
                                                 )
                                               ),
                                               
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Expected shifts in digital privacy landscape due to generative AI")),
                                                        h5(HTML("<i>Question: Has the rise of generative AI changed the way you think about privacy policy? Yes, how so?</i>"))),
                                                 column(width = 12,
                                                        highchartOutput("ai_yes_chart")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.ai.yes_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.ai.yes_sample")
                                                 )
                                               )
                                               
                                      ),
                                      tabPanel(title = "Wave 2 (August 2023)", ############# WAVE 2
                                               # Question 10
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Balance of interest in digital privacy laws")),
                                                        h4(HTML("Stakeholders perceive stronger support for businesses in USA")),
                                                        h5(HTML("<i>Question: Do you think digital privacy laws and regulations in your region [of expertise]
                                                                more strongly favor the rights and needs of businesses or of individual users?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("plot_law_favor_tech")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.tech_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.tech_sample")
                                                 )
                                               ),
                                               # Question 11
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Influence of laws on development of privacy-preserving practices and technologies")),
                                                        h4(HTML("Stakeholders perceive European laws as more innovation friendly")),
                                                        h5(HTML("<i>Question: Do you think digital privacy laws and regulations in your region [of expertise]
                                                                encourage or discourage innovation and development of privacy-preserving practices and technologies in organizations?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("plot_law_innovation")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.innovation_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.innovation_sample")
                                                 )
                                               ),
                                               # Question 16+ 17
                                               fluidRow(
                                                 column(width=12,  
                                                        h3(HTML("Current and future outlook of digital privacy laws")),
                                                        h4(HTML("European stakeholders more optimistic than USA, but both anticipate improvements"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatelawCurrent")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatelawFuture")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.StatelawCombined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StatelawCombined_sample")
                                                 )
                                               ),
                                               # Question 26 + 27 combined
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Current and future outlook of organizational digital privacy practices")),
                                                        h4(HTML("European stakeholders more optimistic than USA, but both anticipate improvements"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatePraCurrent")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatePraFuture")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.StatePraCombined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StatePraCombined_sample")
                                                 )
                                               ),
                                               # model_input
                                               fluidRow( 
                                                 column(width = 12,
                                                        h3(HTML("Consumer privacy law requirements"))),
                                                 column(width = 6,
                                                        highchartOutput("model_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("model_chart_USA")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.model_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.model_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Consumer privacy law enforcement mechanisms (USA)")),
                                                        h5(HTML("<i>Question: How important is it for a national consumer privacy law to
                                                                include the following enforcement mechanisms?</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("model_chart_q21")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.model.q21_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.model.q21_sample")
                                                 )
                                               ),
                                               
                                               # Q5 offline_activism
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Offline privacy advocacy activities")), 
                                                        h5(HTML("<i>Question: Have you ever engaged in any of the following activities?</i>"))),
                                                 column(width = 12,
                                                        highchartOutput("offline_activism_chart")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.offline_activism_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.offline_activism_sample")
                                                 )
                                               ),
                                               # Q6 online_activism
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Online privacy advocacy activities")),  
                                                        h5(HTML("<i>Question: On any social media, have you ever engaged in any of the following activities?</i>"))),
                                                 column(width = 12,
                                                        highchartOutput("online_activism_chart")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.online_activism_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.online_activism_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Ranking approaches to protecting digital privacy")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("priorities_w2_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_w2_USA")
                                                 ),
                                                 column(width= 4,
                                                        actionButton("toggleText.priorities_w2_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_w2_sample"))
                                               ),
                                               
                                               # Question 18 + 19 combined
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Policymaking approaches in digital privacy protection")),
                                                        h4(HTML("Stakeholders advocate policymaking at highest levels of government possible")),
                                                        h5(HTML("<i>Question: Do you think that digital privacy policy in your region should be made at the …</i>"))),
                                                 column(width = 4,
                                                        highchartOutput("policymaking_chart_Europe")
                                                 ),
                                                 column(width = 4,
                                                        highchartOutput("policymaking_chart_USA")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.policymakingCombined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.policymakingCombined_sample")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "Wave 1 (October 2022)",
                                               
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Stakeholder ratings of digital privacy law specifications")),
                                                        h5(HTML("<i>Question: How would you rate the specifications of the requirements in current digital privacy laws in your region [of expertise]?<br>
                                                              Answer options: Excellent, Good, Fair, Poor</i>")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_depth_plot")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.Law_depth_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.Law_depth_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Ranking approaches to protecting digital privacy")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("priorities_w1_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_w1_USA")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.priorities_w1_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.priorities_w1_sample"))
                                               ),
                                               
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Current and future outlook of digital privacy laws")),
                                                        h4(HTML("Europeans more satisfied with current laws, but Americans more optimistic for future"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StateCurrentW1")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StateFutureW1")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.StateCombinedW1_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StateCombinedW1_sample")
                                                 )
                                               )
                                      ),
                                      
                                      tabPanel(title = "Data Download",
                                               column(width = 1,
                                                      br()
                                               ),
                                               column(width = 10,
                                                      h4(HTML("We're currently transferring the anonymized raw data from our study to 
                                GESIS (Leibniz Institute for the Social Sciences), 
                                a German institute that handles social science data. The link to the archived data will be shared here once that's done.<br/>
                                <p/> <br/>
                                Need the data now? You can reach out to our research team directly for access."))
                                               ),
                                               column(width = 1,
                                                      br()
                                               )
                                      )#end of tabpanel two
                                      
                                    )  # End of tabsetPanel
                                    
                             ) # End of column
                    ) # End of fluidRow
                    
          ) # End of fluidPage
  ) # End of tabItem
}
