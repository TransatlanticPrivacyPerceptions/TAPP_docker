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
                                                        h5(HTML("<i>Question: Do you think current digital privacy laws in your region [of expertise] cover more areas than needed,
                                                                fewer areas than needed, or all areas needed?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_breadth_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_breadth_chart_USA")
                                                 ),
                                                 column(width = 6, 
                                                        actionButton("toggleText.law_breadth_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.law_breadth_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Enforcement of digital privacy practices")),
                                                        # h4(HTML("Mixed results by region")),
                                                        h5(HTML("<i>Question: Are the digital privacy practices required by your region's [of expertise]
                                                   law enforced?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("Enforcement_chart15_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("Enforcement_chart15_USA")
                                                 ),
                                                 column(width = 6,
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
                                                 column(width = 5,
                                                        highchartOutput("Organisations1_chart_Europe")
                                                 ),
                                                 column(width = 5,
                                                        highchartOutput("Organisations1_chart_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.organisations_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.organisations_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Most important approach in protecting people‘s digital privacy")),
                                                        h4(HTML("Adapting privacy laws to technological developments")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?<br>Ranked as most important: Adapting privacy laws to technological developments
                                                               </i>")
                                                        )),
                                                 column(width = 6,
                                                        highchartOutput("priorities_chart_approach_1_EU")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_chart_approach_1_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.priorities_sample1", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_sample1")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Most important approach in protecting people‘s digital privacy")),
                                                        h4(HTML("Giving individuals control over their data")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?<br>Ranked as most important: Giving individuals control over their data</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_chart_approach_2_EU")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_chart_approach_2_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.priorities_sample2", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_sample2")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Balance of interest in digital privacy laws")),
                                                        # h4(HTML("...")),
                                                        h5(HTML("<i>Question: Do you think digital privacy laws and regulations in your region [of expertise] more strongly favor the rights and needs of businesses or of individual users?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_favor_tech_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_favor_tech_chart_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.law_favor_tech_trend_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.law_favor_tech_trend_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Influence of laws on development of privacy-preserving practices and technologies")),
                                                        h5(HTML("<i>Question: Do you think digital privacy laws and regulations in your region [of expertise] encourage or discourage innovation and development of privacy-preserving practices and technologies in organizations?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_innovation_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("law_innovation_chart_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.law_innovation_trend_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.law_innovation_trend_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Current and future outlook of digital privacy laws in Europe")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatelawCurrent_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatelawFuture_Europe")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.StatelawCombined_Europe_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.StatelawCombined_Europe_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Current and future outlook of digital privacy laws in the US")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatelawCurrent_USA")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("StatelawFuture_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.StatelawCombined_USA_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.StatelawCombined_USA_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Current and future outlook of organizational digital privacy practices in Europe")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("state_current_pra_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("state_outlook_pra_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.StatePraCombined_EU_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.StatePraCombined_EU_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Current and future outlook of organizational digital privacy practices in the US")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("state_current_pra_chart_USA")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("state_outlook_pra_chart_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.StatePraCombined_USA_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.StatePraCombined_USA_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Policymaking approaches in digital privacy protection")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("policymaking_trend_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("policymaking_trend_chart_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.policymakingCombined_trend_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.policymakingCombined_trend_sample")
                                                 )
                                               )
                                               
                                      ),
                                      tabPanel(title = "Wave 5 (September 2024)", ##### WAVE 5
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Impact of the EU AI Act on AI innovation")),
                                                        h5(HTML("<i>Question: In your opinion, will the EU AI Act be more likely to enable or hinder AI innovation?</i>"))
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("EU_AI_act_chart")
                                                 ),
                                                 column(width = 5,
                                                        actionButton("toggleText.EU_AI_act_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.EU_AI_act_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Ranking approaches to protecting digital privacy")),
                                                        h5(HTML("<i>Question: How would you rank the importance of the following approaches to protecting people’s digital privacy?</i>")),
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_w5_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("priorities_w5_USA")
                                                 ),
                                                 column(width = 6,
                                                        actionButton("toggleText.priorities_w5_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_w5_sample")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "Wave 4 (May 2024)", ##### WAVE 4
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Use of AI tools and systems for work")),
                                                        h5(HTML("<i>Question: Do you use AI tools or systems in your work?<br>Answer options: Yes, No, Don't know</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_work_plot")),
                                                 column(width = 5, 
                                                        actionButton("toggleText.ai_work_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_work_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Privacy concerns for the use of AI tools or systems in their work")),
                                                        h5(HTML("<i>Question: How much do privacy concerns affect your use of AI tools or systems in your work?<br>Answer options: A great deal, Somewhat, Not at all</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_concern_plot")),
                                                 column(width = 5, 
                                                        actionButton("toggleText.ai_concern_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_concern_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Organizational adoption of AI frameworks and guidelines")),
                                                        h5(HTML("<i>Question: Does your organization have a framework or guidelines in place for using AI in the workplace?<br>Answer options: Yes, No, Don't know</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_org_framework_plot")),
                                                 column(width = 5, 
                                                        actionButton("toggleText.ai_org_framework_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_org_framework_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("AI frameworks and guidelines used by organizations")),
                                                        h5(HTML("<i>Question: Which frameworks and guidelines does your organization have for AI use? (e.g., OECD, IEEE, internal proprietary)</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_framework_use_plot")),
                                                 column(width = 5,
                                                        actionButton("toggleText.ai_framework_use_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_framework_use_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Intentions for organizational adoption of AI frameworks and guidelines")),
                                                        h5(HTML("<i>Question: Does your organization plan to implement a framework or guidelines for future AI use?<br>Answer options: Yes, No, Don't know</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_framework_future_plot")),
                                                 column(width = 5, 
                                                        actionButton("toggleText.ai_framework_future_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_framework_future_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Stakeholder involvement in organizational AI framework and guidelines development")),
                                                        h5(HTML("<i>Question: Are you/ will you be involved in drafting the framework or guidelines?</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_framework_draft_plot")),
                                                 column(width = 5, 
                                                        actionButton("toggleText.ai_framework_draft_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_framework_draft_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Organizational compliance with AI framework and guidelines")),
                                                        h5(HTML("<i>Question: How does /will your organization ensure that everyone complies with the framework or guidelines?<br>Answer options: Yes, No, Don't know</i>"))),
                                                 column(width = 12,
                                                        highchartOutput("ai_compliance_chart")),
                                                 column(width = 6, 
                                                        actionButton("toggleText.ai_compliance_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_compliance_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Confidence in managing AI privacy challenges")),
                                                        h5(HTML("<i>Question: Do you feel confident that you/ your organization will be able to address privacy challenges that may arise when using AI tools or systems in your work?</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_challenge_plot")),
                                                 column(width = 5,
                                                        actionButton("toggleText.ai_challenge_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_challenge_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Intentions for managing AI privacy challenges")),
                                                        h5(HTML("<i>Question: How do you/ does your organization plan to address such challenges?</i>"))),
                                                 column(width = 12,
                                                        highchartOutput("ai_challenge_plan_chart")),
                                                 column(width = 6,
                                                        actionButton("toggleText.ai_challenge_plan_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_challenge_plan_sample"))
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Familiarity with 'Responsible AI' Principles")),
                                                        h5(HTML("<i>Question: Are you familiar with “Responsible AI” principles?<br>Answer options: A great deal, Somewhat, Not at all</i>"))),
                                                 column(width = 6,
                                                        highchartOutput("ai_responsible_plot")),
                                                 column(width = 5, 
                                                        actionButton("toggleText.ai_responsible_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.ai_responsible_sample"))
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
                                                 column( width = 5,
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
                                                 column( width = 5,
                                                         actionButton("toggleText.ai.yes_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.ai.yes_sample")
                                                 )
                                               )
                                               
                                      ),
                                      tabPanel(title = "Wave 2 (August 2023)", ############# WAVE 2
                                               fluidRow( 
                                                 column(width = 12,
                                                        h3(HTML("Consumer privacy law requirements"))),
                                                 column(width = 6,
                                                        highchartOutput("model_chart_Europe")
                                                 ),
                                                 column(width = 6,
                                                        highchartOutput("model_chart_USA")
                                                 ),
                                                 column( width = 6,
                                                         actionButton("toggleText.model_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.model_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Consumer privacy law enforcement mechanisms (USA)")),
                                                        h5(HTML("<i>Question: How important is it for a national consumer privacy law to
                                                                include the following enforcement mechanisms?</i>"))),
                                                 column(width = 7,
                                                        highchartOutput("model_chart_q21")
                                                 ),
                                                 column( width= 5,
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
                                                 column( width = 6,
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
                                                 column( width = 6,
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
                                                 column(width = 6,
                                                        actionButton("toggleText.priorities_w2_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_w2_sample"))
                                               ),
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
                                                 column( width = 5,
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
                                                 column( width = 6,
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
                                                 column( width = 6,
                                                         actionButton("toggleText.StateCombinedW1_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StateCombinedW1_sample")
                                                 )
                                               )
                                      )#end of tabpanel two
                                      
                                    )  # End of tabsetPanel
                                    
                             ) # End of column
                    ) # End of fluidRow
                    
          ) # End of fluidPage
  ) # End of tabItem
}
