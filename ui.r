library(shiny)
library(shinydashboard)


shinyUI(

dashboardPage( 
  dashboardHeader(title = "Statistical Tests"),
  dashboardSidebar(
                   sidebarMenu(

                     menuItem(text = "Parametric Tests", tabName = "paratest",
                     
                     menuItem(text = "Two-tailed Tests", icon = icon("dashboard"), tabName = "twotail",
                      menuSubItem(text = "Normal Test", tabName = "nortest2", icon= icon("area-chart")),
                      menuSubItem(text = "T-Test", tabName = "ttest2", icon= icon("area-chart")),
                      menuSubItem(text = "Chi-Square Test", tabName = "chitest2", icon= icon("area-chart"))
                      
                      ),
                     
                     
                     ##############################################################################
                     
                     
                     menuItem(text = "One-tailed Tests (H1>...)", icon = icon("dashboard"), tabName = "onetailg",
                      menuSubItem(text = "Normal Test", tabName = "nortest1", icon = icon("area-chart")),
                      menuSubItem(text = "T-Test", tabName = "ttest1", icon= icon("area-chart")),
                      menuSubItem(text = "Chi- Square Test", tabName = "chitest1", icon= icon("area-chart"))
                     ),
                     ##############################################################################
                     
                     
                     menuItem(text = "One-tailed Tests (H1<...)", icon = icon("dashboard"), tabName = "onetails",
                              menuSubItem(text = "Normal Test", tabName = "nortest0", icon = icon("area-chart")),
                              menuSubItem(text = "T-Test", tabName = "ttest0", icon= icon("area-chart")),
                              menuSubItem(text = "Chi- Square Test", tabName = "chitest0", icon= icon("area-chart"))
                     )))),
  
  ##########################################################################################################
  
  dashboardBody(
    
    tabItems( 
      
        tabItem(tabName = "nortest2", h3("Two-tailed Normal Test"),
              fluidRow(
                box( plotOutput("norplottwo"),width = 8, status = "primary", title = "Normal Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the Normal test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("alfanor2", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="zo2",label = "Z Observe",value = 1.96  ),
                    br(),
                    actionButton("actionnormal2","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("nortest2"),
                    verbatimTextOutput("pvaluenortest2",placeholder = 0.05),
                    verbatimTextOutput("cvaluenortest2"))
                
                  
                ) ),
      
      ###################################################################################################################
      
      tabItem(tabName = "ttest2", h3("Two-tailed T-Test"),
              fluidRow(
                box( plotOutput("tplot2"),width = 8, status = "primary", title = "Student t Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the t-test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("tdf2", label = "Specify the Degree of Freedom Value", min = 1, max = 100, value = 10),
                    sliderInput("alfat2", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="to2",label = "T Observe",value = 1  ),
                    br(),
                    actionButton("actiont2","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("ttest2",placeholder = 1),
                    verbatimTextOutput("pvaluettest2",placeholder = 0.05),
                    verbatimTextOutput("cvaluettest2"))
                
              ) ),
      
      ###################################################################################################################
      tabItem(tabName = "chitest2", h3("Two-tailed T-Test"),
              fluidRow(
                box( plotOutput("chiplot2"),width = 8, status = "primary", title = "Chi-Square Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the Chi-Square test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("chidf2", label = "Specify the Degree of Freedom Value", min = 1, max = 100, value = 10),
                    sliderInput("alfachi2", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="chio2",label = "chi Observe",value = 1  ),
                    br(),
                    actionButton("actionchi2","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("chitest2"),
                    verbatimTextOutput("pvaluechitest2",placeholder = 0.05),
                    verbatimTextOutput("cvaluechitest2"))
                
              ) ),
      
      ###################################################################################################################
  
      tabItem(tabName = "nortest1", h3("One-tailed Normal Test (H1>...)"),
              fluidRow(
                box( plotOutput("norplot1"),width = 8, status = "primary", title = "Normal Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the Normal test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("alfanor1", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="zo1",label = "Z Observe",value = 1.96  ),
                    br(),
                    actionButton("actionnormal1","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("nortest1",placeholder = 1),
                    verbatimTextOutput("pvaluenortest1",placeholder = 0.05),
                    verbatimTextOutput("cvaluenortest1"))
                
              ) ),
      
      
      ###################################################################################################################
      
      tabItem(tabName = "ttest1", h3("One-tailed T-Test (H1>...)"),
              fluidRow(
                box( plotOutput("tplot1"),width = 8, status = "primary", title = "Student t Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the t-test",width = 4,status = "warning", background = "lime",
                    sliderInput("tdf1", label = "Specify the Degree of Freedom Value", min = 1, max = 100, value = 10),
                    sliderInput("alfat1", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="to1",label = "T Observe",value = 1.96  ),
                    br(),
                    actionButton("actiont1","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("ttest1",placeholder = 1),
                    verbatimTextOutput("pvaluettest1",placeholder = 0.05),
                    verbatimTextOutput("cvaluettest1"))
                
              ) ),
      
      ###################################################################################################################
      tabItem(tabName = "chitest1", h3("One-tailed T-Test"),
              fluidRow(
                box( plotOutput("chiplot1"),width = 8, status = "primary", title = "Chi-Square Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the Chi-Square test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("chidf1", label = "Specify the Degree of Freedom Value", min = 1, max = 100, value = 10),
                    sliderInput("alfachi1", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="chio1",label = "Chi Observe",value = 1  ),
                    br(),
                    actionButton("actionchi1","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("chitest1",placeholder = 1),
                    verbatimTextOutput("pvaluechitest1",placeholder = 0.05),
                    verbatimTextOutput("cvaluechitest1"))
                
              ) ),
      
      ###################################################################################################################
      
      tabItem(tabName = "nortest0", h3("One-tailed Normal Test (H1<...)"),
              fluidRow(
                box( plotOutput("norplot0"),width = 8, status = "primary", title = "Normal Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the Normal test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("alfanor0", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="zo0",label = "Z Observe (negative value)",value = -1.96  ),
                    br(),
                    actionButton("actionnormal0","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("nortest0",placeholder = 1),
                    verbatimTextOutput("pvaluenortest0",placeholder = 0.05),
                    verbatimTextOutput("cvaluenortest0"))
                
              ) ),
      
      
      ###################################################################################################################
      
      tabItem(tabName = "ttest0", h3("One-tailed T-Test (H1<...)"),
              fluidRow(
                box( plotOutput("tplot0"),width = 8, status = "primary", title = "Student t Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the t-test",width = 4,status = "warning", background = "lime",
                    sliderInput("tdf0", label = "Specify the Degree of Freedom Value", min = 1, max = 100, value = 10),
                    sliderInput("alfat0", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="to0",label = "T Observe",value = 1.96  ),
                    br(),
                    actionButton("actiont0","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("ttest0",placeholder = 1),
                    verbatimTextOutput("pvaluettest0",placeholder = 0.05),
                    verbatimTextOutput("cvaluettest0"))
                
              ) ),
      
      ###################################################################################################################
      tabItem(tabName = "chitest0", h3("One-tailed Chi-Square Test"),
              fluidRow(
                box( plotOutput("chiplot0"),width = 8, status = "primary", title = "Chi-Square Distribution", solidHeader = TRUE, background = "aqua"),
                
                box(title = "Control the Parameters of the Chi-Square test",width = 4,status = "warning", background = "lime",
                    
                    sliderInput("chidf0", label = "Specify the Degree of Freedom Value", min = 1, max = 100, value = 10),
                    sliderInput("alfachi0", label = "Specify the Alfa Value", min = 0, max = 0.1, value = 0.05, step = 0.01),
                    
                    textInput(inputId ="chio0",label = "Chi Observe",value = 1  ),
                    br(),
                    actionButton("actionchi0","Run the Test" ),
                    br(),br(),
                    verbatimTextOutput("chitest0",placeholder = 1),
                    verbatimTextOutput("pvaluechitest0",placeholder = 0.05),
                    verbatimTextOutput("cvaluechitest0"))
                
              ) ),
      
      #################################################################################################
      
      tabItem(tabName = "mean1sample2side",h3("Sample Size"),
              fluidRow(
                box(width = 4, title = "Parameters", solidHeader = TRUE,  status = "primary", background = "light-blue",
                    textInput(inputId="truemean11", label = "True Mean", value =  200),
                    textInput(inputId="nullmean11", label = "Null Hypothesis Mean", value = 198),
                    textInput(inputId="sd11", label = "Standard Deviation", value= 6),
                    textInput(inputId="alfa1_mean1", label = "Type I Error", value =  0.05),
                    textInput(inputId="beta1_mean1", label = "Type II Error", value = 0.1),
                    actionButton(inputId = "actmean11" ,label =  "Calculate")),
                box(width = 7, title = "Sample Size", status = "info", solidHeader = TRUE, background = "aqua", plotOutput("samplesize_mean1_1_plot1"))
               
              ),
              fluidRow( box(width = 4 , textOutput("samplesize_mean1_1"), title = "Sample Size", solidHeader = TRUE, status = "success", background = "maroon"))
              )
      ##################################################################################################################
     
      )
      )
      ))
