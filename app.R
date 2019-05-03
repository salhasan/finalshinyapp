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
bcancer <- read.csv('https://raw.githubusercontent.com/salhasan/finalshinyapp/master/data.csv')

bcancer$diagnosis <- factor(bcancer$diagnosis,levels=c("B","M"), labels=c("Benign","Malignant"))


##preparing the data. (bc=breast cancer)

#randomly shuffle the rows and select columns
set.seed(1)
bc <- bcancer[sample(nrow(bcancer)),]


#remove all the rows that contain missing data
bc <- na.omit(bc)

#normalize the data
bc_norm <- as.data.frame(apply(bc[, 1:30], 2, function(x) (x - min(x))/(max(x)-min(x))))
bc_norm$diagnosis <- bc$diagnosis



# Define UI for application that draws a plot

ui <- fluidPage(
  
  # Application title
  titlePanel("Breast Cancer Prediction"),
  
  sidebarLayout(
    
    # Sidebar with a radioButtons and 2 slider inputs
    sidebarPanel(
      radioButtons("pm","Machine Learning Prediction Method:",
                   c("k-Nearest Neighbors" = "KNN",
                     "Naive Bayes" = "NB",
                     "Decision Tree C5.0"= "DT")),
      sliderInput("ptrain", "Percentage of training data:",
                  min = 0.10, max = 0.99, value = 0.80
      ),
      sliderInput("nattrib", "Number of Attributes:",
                  min = 2, max = 30, value = 15
      )
      
    ),
    
    
    mainPanel(
      # Show a plot of the generated prediction
      plotOutput("distPlot"),
      
      # Show Confusion Matrix & summary of the generated prediction
      tabsetPanel(position = "below",
                  tabPanel("Confusion Matrix", verbatimTextOutput("table")),
                  tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  
  # display plot
  output$distPlot <- renderPlot({
    
    
    #divide the data into training and testing sets corresponding to 80% and 20% of data respectively.
    n_rows <- nrow(bc_norm)
    n_train <- round(input$ptrain *n_rows)
    n_test <- n_rows - n_train
    
    #creating the data structures corresponding to the outcome of the experiment to be predicted by ML algorithm.
    bc_train_label <- bc_norm[1:n_train,"diagnosis"]
    bc_test_label <- bc_norm[(n_train+1):n_rows,"diagnosis"]
    
    
    #creating the testing data used as the input data by ML algorithm to predict the outcome. Need to ignore predict labels.
    bc_train_data <- bc_norm[1:n_train,1:input$nattrib]
    bc_test_data <- bc_norm[(n_train+1):n_rows,1:input$nattrib]
    
    # run ML prediction 
    predicted <- switch(input$pm,
                 KNN = knn(bc_train_data, bc_test_data, bc_train_label,k=4),
                 NB = predict(naiveBayes(bc_train_data,bc_train_label),bc_test_data ),
                 DT =  predict(C5.0(x=bc_train_data, y=factor(bc_train_label)), bc_test_data)
                 )
   
     # set y axis size
    y <- n_test*0.9
    
    # plot prediction
    if (!is.null(predicted) )
      plot(predicted, main = "Prediction",ylim=c(0,y) ,col= distinctColorPalette(k = 1000, altCol = FALSE, runTsne = FALSE),cex.axis=1.5)
    
    actual <- bc_test_label
    cm <- table(predicted,actual)
    
    # print Confusion Matrix
    output$table <- renderPrint({
      cm
    })
    
    # print summary
    output$summary <- renderPrint({
      
      accuracy<- round((cm[1,1] + cm[2,2])/ (cm[1,1]+ cm[1,2] + cm[2,1] + cm[2,2]) * 100, digits = 2)
      sensitivity<- round((cm[2,2] / (cm[2,2] + cm[2,1])) * 100, digits = 2)
      specificity <- round((cm[1,1] / (cm[1,1] + cm[1,2])) * 100, digits = 2)
      
      summarymtx <- matrix(c("Sensitivity", "Specificity", "Accuracy"), ncol = 1, nrow =3)
      summarymtx <- cbind(summarymtx, c(sensitivity,specificity,accuracy))
      summarymtx
    })
    
  })
  
  output$plot_hoverinfo <- renderPrint({
    cat("Hover (throttled):\n")
    str(input$plot_hover)
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

