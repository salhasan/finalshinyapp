#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(C50)
library(class)
library(e1071)
library(shiny)
library(randomcoloR)

# preprocessing the raw data
bcancer <- read.csv('https://raw.githubusercontent.com/melinabifx/finalprojectbreastcancer/master/data_new.csv')

bcancer$diagnosis <- factor(bcancer$diagnosis,levels=c("B","M"), labels=c("Benign","Malignant"))


##preparing the data. (bc=breast cancer)

#randomly shuffle the rows and select columns
set.seed(1)
bc <- bcancer[sample(nrow(bcancer)),]

#remove all the rows that contain missing data
bc <- na.omit(bc)

#normalize the data
bc_norm <- as.data.frame(apply(bc[, -11], 2, function(x) (x - min(x))/(max(x)-min(x))))
bc_norm$diagnosis <- bc$diagnosis






# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      radioButtons("pm","Prediction Method:",
                   c("k-Nearest Neighbors" = "KNN",
                     "Naive Bayes" = "NB",
                     "Decision Tree C5.0"= "DT")),
      actionButton("plotButtonKNN","run KNN"),
      actionButton("plotButtonNB","run NB")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", 
                 hover = hoverOpts(id = "plot_hover", delayType = "throttle"))
      #plotOutput("distPlotKNN"),
      #plotOutput("distPlotNB")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  
  #divide the data into training and testing sets corresponding to 80% and 20% of data respectively.
  n_rows <- nrow(bc_norm)
  n_train <- round(0.8*n_rows)
  n_test <- n_rows - n_train
  
  #creating the data structures corresponding to the outcome of the experiment to be predicted by ML algorithm.
  bc_train_label <- bc_norm[1:n_train,"diagnosis"]
  bc_test_label <- bc_norm[(n_train+1):n_rows,"diagnosis"]
  
  
  #check if the outcome are numeric, because we want to predict numeric outcomes.
  #is.numeric(bc_train_label)        #should be TRUE
  #is.numeric(bc_test_label)         #should be TRUE
  
  #creating the testing data used as the input data by ML algorithm to predict the outcome. Need to ignore predict labels.
  bc_train_data <- bc_norm[1:n_train,-11]
  bc_test_data <- bc_norm[(n_train+1):n_rows,-11]
  
  
  knnplot <- reactiveVal(NULL) 
  
  nbplot <- reactiveVal(NULL)
  
 # observeEvent(input$plotButtonKNN,{
    # knn
  #  knnVal <- knn(bc_train_data, bc_test_data, bc_train_label,k=2)
  #  knnplot(knnVal)
    

  #  })
  #observeEvent(input$plotButtonNB,{
    # nb
  #  nbVal <- predict(naiveBayes(bc_train_data,bc_train_label),bc_test_data )
  #  nbplot(nbVal)
  #})
    
  
  output$distPlot <- renderPlot({
    
    pm <- switch(input$pm,
                 KNN = knn(bc_train_data, bc_test_data, bc_train_label,k=2),
                 NB = predict(naiveBayes(bc_train_data,bc_train_label),bc_test_data ),
                 DT =  predict(C5.0(x=bc_train_data, y=factor(bc_train_label)), bc_test_data)
                 )
    #knn
    if (!is.null(pm) )
      plot(pm, ylim=c(0,75) ,col= distinctColorPalette(k = 100, altCol = FALSE, runTsne = FALSE),cex.axis=1.5)
    
  })
  
  output$plot_hoverinfo <- renderPrint({
    cat("Hover (throttled):\n")
    str(input$plot_hover)
  })

  
#   output$distPlotKNN <- renderPlot({
     #knn
#     if (!is.null(knnplot()) )
#       plot(knnplot())
   
#   })
   
#   output$distPlotNB <- renderPlot({
     #knn
#     if (!is.null(nbplot()))
#       plot(nbplot())
     
#   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

