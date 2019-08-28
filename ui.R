library(shiny)
library(plotly)
df<-iris
shinyUI(fluidPage(
    tabsetPanel(
        tabPanel(
            "Info",
            mainPanel(width=12,
                      h2("Assignment 2: Sidhartha",align="center"),
                      h4("student Id:58651262",align="center"),
                      h4("Classification of Iris species using Machine Learning Algorithms",align="center"),br(),br()
                      )),
        
        tabPanel("Data Table",
                 
                 mainPanel(DT::dataTableOutput("data",width=500),tags$hr())),
        
        tabPanel("summary",
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons("data","Select",choices = c("Summary"="sum","Structure"="struct"))),
                     mainPanel(
                         verbatimTextOutput("sm")))),
        tabPanel("Plots",
                 sidebarLayout(
                     
                     sidebarPanel(
                         selectInput("plottype","plots",
                                     c(scatter="scatter",
                                       Boxplot="box",
                                       Three_Dimensional_scatter="3Dscatter")),
                         conditionalPanel(condition="input.plottype=='3Dscatter'",
                                          h4("Please scroll down a little to see the plot")),
                         
                         conditionalPanel(condition="input.plottype=='box'",
                                          selectInput("ybox","select y-axis",choices=names(df[,-5]),selected=colnames(df[,1])),
                                          sliderInput("drag","Range",min=1,max=8,value=1.5)
                         ),
                         conditionalPanel(condition="input.plottype=='scatter'",
                                          selectInput("x","x-axis",choices=names(df[,-5]),selected=colnames(df[1])),
                                          selectInput("y","y-axis",choices=names(df[,-5]),selected=colnames(df[2]))
                                          
                                          
                         )),
                     mainPanel(
                         #conditionalPanel(condition="input.plottype=='box'",
                         plotOutput("plot"),
                         #conditionalPanel(condition="input.plottype=='scatter'",
                         plotlyOutput("plotly")
                     ))
        ),
        tabPanel("Model",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("mod","Select one model for classification",choices=c("Decision Tree"="rpart","Random Forest"="rf","KNN"="knn","LDA"="lda"),selected = "rpart"),
                         sliderInput('s', 'Choose Sample Size', min=0.5, max=0.9,
                                     value=min(0.5, 0.9), step=0.1, round=0),br(),br(),
                         h4('Please scroll down to visualise misclassifications')
                         ),
                     mainPanel(
                         
                         h4('Assessing models based on their Prediction Accuracy'),br(),br(),
                         h5(''),
                         textOutput("confusionmatrix"),
                         verbatimTextOutput("cm"),
                         plotOutput("allu"),
                         h4("Misclassifications are marked as red")
                         )
                     )),
        tabPanel("Visualisation",
                 h4("Recursive Partitioning of features"),
                 plotOutput("vistree"),br(),br(),
                 verbatimTextOutput("summod"))
        
          
    )
    ))