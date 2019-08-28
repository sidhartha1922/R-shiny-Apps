library(shiny)
library(ggplot2)
library(plotly)
library(ggthemes)
library(dplyr)
library(caret)
library(randomForest)
library(tree)
library(alluvial)
df<-iris

shinyServer(function(input, output,session) {
    
    
    
    output$data<- DT::renderDataTable({
        df})
    
    observe(
        if(input$data=="sum"){
            output$sm<-renderPrint({
                summary(df)})})
    
    
    observe(
        if(input$data=="struct"){
            output$sm<-renderPrint({
                str(df)})})
    
    observe(
        if(input$plottype=="scatter"){
            species<-df[,5]
            output$plot<-renderPlot({
                ggplot(df,aes_string(x=df[,input$x],y=df[,input$y]))+aes(color=species) + geom_point(lwd=3)+theme(text = element_text(size=20))+labs(x=input$x,y=input$y)
                
            })})
    
    
    
    observe(
        if(input$plottype=="box"){
            #attach(df)
            #numdata<-df[,-5]
            #inf<-scale(x=dplyr::select_if(numdata,is.numeric), center = input$center, scale = input$scale)
            output$plot<-renderPlot({
                options(repr.plot.width = 5, repr.plot.height = 4)
                
                ggplot(data=df,aes(x=Species, y=df[,input$ybox],color=Species)) + geom_boxplot(coef=input$drag) +theme_minimal()+
                    theme(legend.position="none")+labs(x="Species",y=input$ybox)+ggtitle("Variation of Length & Width(cm) according to Species")
            })})
    
    
    
    observe(
        if(input$plottype=="3Dscatter"){
            output$plotly<-renderPlotly({
                plot_ly(
                    
                    data = df, x = ~Sepal.Length, y = ~Petal.Length, z = ~Petal.Width,
                    color = ~Species,  
                    type = "scatter3d",  # Makes a 3D scatterplot.
                    mode = "markers"   
                ) %>%  
                    layout(scene = list(xaxis = list(title = 'Sepal Length'), # Assign x, y, & z axes names. 
                                        yaxis = list(title = 'Petal length'),
                                        zaxis = list(title = 'Petal width')))
            })
        })
    
    inTrain <- reactive({
        inTrain <- createDataPartition(y = iris$Species, p = input$s, list = FALSE) 
        inTrain
    })
    
    
    getFit <- reactive({
        training <- iris[inTrain(),]
        testing <- iris[-inTrain(),]
        modelFit <- train(Species ~.,data = training , method = input$mod,metric="Accuracy",trControl=trainControl(method = "cv",number = 5))
        modelFit
    })
    
    
    alreac<-reactive({
        testing <- iris[-inTrain(),]
        predictions <- predict(getFit(), newdata = testing)
        cm<-confusionMatrix(predictions, testing$Species)
        cm  
    })
    
    
    output$cm <- renderPrint({
        testing <- iris[-inTrain(),]
        predictions <- predict(getFit(), newdata = testing)
        confusionMatrix(predictions, testing$Species)
    })
    
    observe(
        if(input$mod=="rpart"){
            
            output$confusionmatrix<-renderPrint({
                "Confusion matrix for Decision tree(rpart)" 
                
            })})
    observe(
        if(input$mod=="rf"){
            
            output$confusionmatrix<-renderPrint({
                "Confusion matrix for Random Forest" 
                
            })})
    
    observe(
        if(input$mod=="knn"){
            
            output$confusionmatrix<-renderPrint({
                "Confusion matrix for KNN" 
                
            })})
    observe(
        if(input$mod=="lda"){
            
            output$confusionmatrix<-renderPrint({
                "Confusion matrix for LDA" 
                
            })})
    
    
    output$allu<-renderPlot({
        require(reshape2, quietly = TRUE)
        require(alluvial, quietly = TRUE)
        melted <- melt(alreac()$table)
        par(pty = "s")
        alluvial::alluvial(
            melted[,1:2],
            freq = melted$value,
            col = ifelse(melted[, 1] == melted[, 2], "green", "red"),
            alpha = 0.5,
            hide  = melted$value == 0
        )
        mtext("Resampled confusion matrix", side = 3, line = 3, font = 2)})
    
    
    cart<-reactive({
        modelfit<-tree(Species~Sepal.Width+Petal.Width,data =iris)
        modelfit
    })
    
    
    
    output$vistree<-renderPlot({
        
        plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
        partition.tree(cart(),label = "Species",add = TRUE)
        legend(1.75,4.5,legend = unique(iris$Species),col = unique(as.numeric(iris$Species)),pch=19)
        output$summod<-renderPrint({
            summary(cart())
        })
    })
    
    # output$irisSpeciesPrediction<-renderText({
    #     userInput <-data.frame(input$id1,input$id2,input$id3,input$id4)
    #     names(userInput)<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
    #     levels(iris$Species)[predict(getFit,newdata=userInput)]
    # })
    # output$visdata <- renderPlot({
    #     plot(getFit$finalModel)
    #     text(getFit$finalModel,pretty=0)
    # })


})