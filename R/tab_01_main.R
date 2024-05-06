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
                                                        h3("Digital Privacy Laws: Sufficient or Lacking?"),
                                                        h4("Do you think current digital privacy laws in your region* cover 
                                            more areas than needed, fewer areas than needed, or all areas needed?"),
                                                        h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))
                                                 ),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 300,
                                                          inputId = "law_breadth.category",
                                                          label = "Please choose a category",
                                                          choices = levels(law_breadth$category), # Use levels to maintain order
                                                          selected = "Practitioners\nEurope"
                                                        )),
                                                 column(width = 5,
                                                        echarts4rOutput("law_breadth.chart")
                                                        
                                                 ),
                                                 column( width= 4,
                                                         actionButton("Text_law_breadth", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation_law_breadth"),
                                                         br(),
                                                         actionButton("toggleText.law_breadth_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.law_breadth_sample")
                                                 )),
                                               # 
                                               # fluidRow(class="blue",
                                               #   column(width = 6,
                                               #          h4(HTML("What areas are currently covered that you feel shouldn’t be?")),
                                               #          actionButton(
                                               #            width = "100%",
                                               #            inputId = "law_breadth_more",
                                               #            label = "Display Responses (5 out of 5 total answers)"),
                                               #          uiOutput("law_breadth_m_oe")),
                                               #   #column(width = 1),
                                               #   column(width = 6,
                                               #          h4(HTML("What areas are NOT currently covered that you feel should be? ")),
                                               #          actionButton(
                                               #            width = "100%",
                                               #            inputId = "law_breadth_less",
                                               #            label = "Display Responses (random 5 out of 45 total answers)"),
                                               #          uiOutput("law_breadth_f_oe"))
                                               #   
                                               # ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Are Digital Privacy Laws Enforced?"),
                                                        h4(HTML("Are the digital privacy practices required by your region's* law enforced …")),
                                                        h5(HTML("* The term 'region' was specified to each expert according to their 
                                            area of expertise, with U.S. experts being asked about the U.S., 
                                            EU experts about the EU, and experts from individual countries in 
                                            Europe about their specific country"))),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 300,
                                                          inputId = "Enforcement.stacked.bar.chart15",
                                                          label = "Please choose a category",
                                                          choices = levels(orgs_rating_priv.15$category), # Use levels to maintain order 
                                                          selected = "Practitioners\nUSA"
                                                        )
                                                 ),
                                                 column(width = 5,
                                                        echarts4rOutput("Enforcement.chart15")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.law_enforcement", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.law_enforcement"),
                                                         br(),
                                                         actionButton("toggleText.law_enforcement_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.law_enforcement_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width=12,  
                                                        h3("Organizations’ Digital Privacy Policies and Practices"),
                                                        h4("How would you rate the performance of these firms/agencies in protecting people's privacy")
                                                 ),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 300,
                                                          inputId = "Organisations.stacked.bar.chart",
                                                          label = "Please choose an organisation",
                                                          choices = unique(orgs_rating_priv_update$Organisation), # Use levels to maintain order
                                                          selected = "Google"
                                                        )),
                                                 column(width = 5,
                                                        echarts4rOutput("Organisations1.chart")
                                                        
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation"),
                                                         br(),
                                                         actionButton("toggleText.organisations_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.organisations_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width=12,
                                                        h3("Best Practices and Admired Policies"),
                                                        h4("What, if any, organization(s) would you say provide a model for organizational best practices regarding digital privacy?"),
                                                        h4("What practices by this/these organizations do you particularly admire?"),
                                                 ),
                                                 column(width = 3,
                                                        selectInput(
                                                          width = 500,
                                                          inputId = "Best.pratices",
                                                          label = "Please choose an organisation",
                                                          choices = sort(unique(best_practice$org_model_oe)),
                                                          selected = "Apple"
                                                        )),
                                                 column(width = 4,
                                                        uiOutput("opinions")

                                                 ),
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Privacy Priorities"),
                                                        h4(HTML("How would you rank the importance of the following approaches to protecting people’s digital privacy?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "Priorities.input",
                                                          label = "Please choose a category",
                                                          choices = levels(priorities$Priorities),
                                                          selected = "Giving individuals control over their data"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("priorities.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.priorities", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.priorities"),
                                                         br(),
                                                         actionButton("toggleText.priorities_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.priorities_sample")
                                                 )
                                               ),
                                               
                                      ),
                                      tabPanel(title = "Wave 3 (December 2023)", #####  WAVE 3
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Generative AI and Privacy Policy"),
                                                        h4(HTML("Has the rise of generative AI changed the way you think about privacy policy?")),),
                                                 column(width = 7,
                                                        echarts4rOutput("gen_AI.chart")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.gen_AI", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.gen_AI"),
                                                         br(),
                                                         actionButton("toggleText.gen_AI_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.gen_AI_sample"),
                                                         
                                                 )
                                               ),
                                               
                                      fluidRow(
                                        column(width = 12,
                                               h3("Impact of Generative AI on Privacy Policy Views"),
                                               h4(HTML("Categorization of open-ended responses for experts who said their views have changed"))),
                                        column(width = 7,
                                               echarts4rOutput("ai.yes.chart")
                                        ),
                                        column( width= 4,
                                                actionButton("toggleText.ai.yes", "Show/Hide Interpretation"),
                                                uiOutput("interpretation.ai.yes")
                                                
                                        ))
                                      
                                    ),
                                      tabPanel(title = "Wave 2 (August 2023)", ############# WAVE 2
                                               # Question 10+ 11 combined
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("The Influence of Digital Privacy Regulations on Stakeholder Interests and Innovation")),),
                                                 column(width = 3,
                                                        actionButton("btn_law_favor_tech", "Impact on Business vs. User"),
                                                        actionButton("btn_law_innovation", "Impact on Innovation"),
                                                 ),
                                                 column(width = 12,
                                                        uiOutput("innovation.tech.combinedTitle")),
                                                 column(width = 6,
                                                        echarts4rOutput("innovation.tech.combined.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.innovation.tech.combined", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.innovation.tech.combined"),
                                                         br(),
                                                         actionButton("toggleText.innovation.tech.combined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.innovation.tech.combined_sample")
                                                 )
                                               ),
                                               # Question 16+ 17 combined
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("State Privacy Laws and Regulations")),),
                                                 column(width = 3,
                                                        actionButton("btn_current_state", "Current State"),
                                                        actionButton("btn_future_state", "Future Outlook"),
                                                 ),
                                                 column(width = 12,
                                                        uiOutput("StatelawCombinedTitle")),
                                                 column(width = 6,
                                                        echarts4rOutput("StatelawCombined.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.StatelawCombined", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.StatelawCombined"),
                                                         br(),
                                                         actionButton("toggleText.StatelawCombined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StatelawCombined_sample")
                                                 )
                                               ),
                                               # Question 26 + 27 combined
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Organizations’ Digital Privacy Policies and Practices")),),
                                                 column(width = 3,
                                                        actionButton("btn_current", "Current State"),
                                                        actionButton("btn_future", "Future Outlook"),
                                                 ),
                                                 column(width = 12,
                                                        uiOutput("StatePraCombinedTitle")),
                                                 column(width = 6,
                                                        echarts4rOutput("StatePraCombined.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.StatePraCombined", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.StatePraCombined"),
                                                         br(),
                                                         actionButton("toggleText.StatePraCombined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StatePraCombined_sample")
                                                 )
                                               ),
                                               fluidRow( #model_input
                                                 column(width = 12,
                                                        h3("Privacy Law Essentials"),
                                                        h4(HTML("How important is it for a national [consumer] privacy law 
         <br/>to include the following requirements?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "model.input",
                                                          label = "Please choose a category",
                                                          choices = levels(model_final$model),
                                                          selected = "Data access and deletion: 
                                            Ability to view,\nedit and delete data in most situations"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("model.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.model", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.model"),
                                                         br(),
                                                         actionButton("toggleText.model_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.model_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Privacy Law Essentials in the US"),
                                                        h4(HTML("How important is it for a national consumer privacy law to
                                                                include the following enforcement mechanisms?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "model.input.q21",
                                                          label = "Please choose a category",
                                                          choices = levels(model_final_q21$model),
                                                          selected = "A national standard, preempting state laws"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("model.chart.q21")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.model.q21", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.model.q21"),
                                                         br(),
                                                         actionButton("toggleText.model.q21_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.model.q21_sample")
                                                 )
                                               ),
                                               
                                               # Q5 offline_activism
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Offline Activism"),  
                                                        h4(HTML("Have you ever engaged in any of the following activities?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "offline_activism.input",
                                                          label = "Please choose a category",
                                                          choices = levels(orgs_rating_priv.5$offline_activism),
                                                          selected = "Signed a petition for a privacy-related topic"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("offline_activism.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.offline_activism", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.offline_activism"),
                                                         br(),
                                                         actionButton("toggleText.offline_activism_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.offline_activism_sample")
                                                 )
                                               ),
                                               # Q6 online_activism
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Online Activism"),  
                                                        h4(HTML("Have you ever engaged in any of the following activities?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "online_activism.input",
                                                          label = "Please choose a category",
                                                          choices = levels(orgs_rating_priv.6$online_activism),
                                                          selected = "Followed/friended individuals who share an interest in privacy topics"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("online_activism.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.online_activism", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.online_activism"),
                                                         br(),
                                                         actionButton("toggleText.online_activism_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.online_activism_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Privacy Priorities"),
                                                        h4(HTML("How would you rank the importance of the following approaches to protecting people’s digital privacy?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "priorities_w2.input",
                                                          label = "Please choose a category",
                                                          choices = levels(priorities_w2$priorities),
                                                          selected = "Giving individuals control over their data"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("priorities_w2.chart")
                                                 ),
                                                 column(width= 3,
                                                        actionButton("toggleText.priorities_w2", "Show/Hide Interpretation"),
                                                        uiOutput("interpretation.priorities_w2"),
                                                        br(),
                                                        actionButton("toggleText.priorities_w2_sample", "Show/Hide Sample Details"),
                                                        uiOutput("interpretation.priorities_w2_sample"))
                                               ),
                                               
                                               # Question 18 + 19 combined
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Policymaking"))),
                                                 column(width = 3,
                                                        actionButton("btn_policymaking_US", "US"),
                                                        actionButton("btn_policymaking_EU", "EU"),
                                                 ),
                                                 column(width = 12,
                                                        uiOutput("policymakingCombinedTitle")),
                                                 column(width = 6,
                                                        echarts4rOutput("policymakingCombined.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.policymakingCombined", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.policymakingCombined"),
                                                         br(),
                                                         actionButton("toggleText.policymakingCombined_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.policymakingCombined_sample")
                                                 )
                                               )
                                      ), 
                                      tabPanel(title = "Wave 1 (October  2022)",
                                               
                                               
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Privacy Law Scope Review"),
                                                        h4(HTML("How would you rate the specifications of the requirements in current digital privacy laws in your region*?")),
                                                        h6(HTML("*References to specific regions or countries within survey questions are tailored to each respondent's 
                               area of expertise[USA, EU, or an individual country].<p/> This approach allows for informed assessments 
                               that are relevant to the respondent's own regulatory environment.")),
                                                 ),
                                                 column(width = 7,
                                                        echarts4rOutput("law_depth.chart")
                                                 ),
                                                 column( width= 4,
                                                         actionButton("toggleText.Law_depth", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.Law_depth"),
                                                         br(),
                                                         actionButton("toggleText.Law_depth_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.Law_depth_sample")
                                                 )
                                               ),
                                               fluidRow(
                                                 column(width = 12,
                                                        h3("Privacy Priorities"),
                                                        h4(HTML("How would you rank the importance of the following approaches to protecting people’s digital privacy?")),),
                                                 column(width = 3,
                                                        radioButtons(
                                                          width = 500,
                                                          inputId = "priorities_w1.input",
                                                          label = "Please choose a category",
                                                          choices = levels(priorities_w1$priorities),
                                                          selected = "Giving individuals control over their data"
                                                        )),
                                                 column(width = 6,
                                                        echarts4rOutput("priorities_w1.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.priorities_w1", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.priorities_w1"),
                                                         br(),
                                                         actionButton("toggleText.priorities_w1_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.priorities_w1_sample"))
                                               ),
                                               
                                               fluidRow(
                                                 column(width = 12,
                                                        h3(HTML("Digital Privacy Laws and Organizational Practices")),),
                                                 column(width = 3,
                                                        actionButton("btn_current_w1", "Current State"),
                                                        actionButton("btn_future_w1", "Future Outlook"),
                                                 ),
                                                 column(width = 12,
                                                        uiOutput("StateCombinedW1Title")),
                                                 column(width = 6,
                                                        echarts4rOutput("StateCombinedW1.chart")
                                                 ),
                                                 column( width= 3,
                                                         actionButton("toggleText.StateCombinedW1", "Show/Hide Interpretation"),
                                                         uiOutput("interpretation.StateCombinedW1"),
                                                         br(),
                                                         actionButton("toggleText.StateCombinedW1_sample", "Show/Hide Sample Details"),
                                                         uiOutput("interpretation.StateCombinedW1_sample")
                                                 )
                                               )
                                      ),
                                      tabPanel(title = "Data Download",
                                               br(),
                                               br(),
                                               width= 4,
                                               h4(HTML("We're currently transferring the anonymized raw data from our study to 
                                GESIS (Leibniz Institute for the Social Sciences), 
                                a German institute that handles social science data. <br/>
                                The link to the archived data will be shared here once that's done.<br/>
                                <p/> <br/>
                                Need the data now? You can reach out to our research team directly for access."))
                                               
                                      ), #end of tabpanel two
                                      
                                    )  # End of tabsetPanel
                                    
                             ) # End of column
                    ), # End of fluidRow
                    
          ) # End of fluidPage
  ) # End of tabItem
}
