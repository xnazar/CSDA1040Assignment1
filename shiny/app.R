# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(shinyjs)
library(wordcloud2)
library(recommenderlab)
library(data.table)
library(plotly)
library(ggplot2)
library(stringr)
library(plyr)
library(tidyr)
library(dplyr)
#library(rcompanion)
library(e1071)
library(outliers)
library(recommenderlab)

#=====================load recommender object
ratingmodel <- readRDS("ratingModel.Rds") 
#=====================load matrix object
rating_matrix <- readRDS("ratingMatrix.Rds")
#=====================load data
books_df <- readRDS("booksDf.Rds")
poplist <- readRDS("poplist.Rds")
topratedlist <- readRDS("topratedlist.Rds") 
booklist <- readRDS("bookList.RDS")
cloud <- readRDS("wordCloud.Rds")

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Book Recommender",id = "inTabset",
                    tabPanel("Rate a book",
                             sidebarPanel(
                                 tags$h3("Rate 3 books for recommendations"),
                                 selectInput("selection1", "Choose book one:", c(Select='', booklist$Book_Title), selectize=TRUE),
                                 sliderInput(inputId = "rates1",
                                             label = "Rate:",
                                             min = 1,
                                             max = 10,
                                             value = 5),
                                 
                                 selectInput("selection2", "Choose book two:", c(Select='', booklist$Book_Title), selectize=TRUE),
                                 #selectInput("selection2", "Choose book two:", choices =booklist$Book_Title),
                                 sliderInput(inputId = "rates2",
                                             label = "Rate:",
                                             min = 1,
                                             max = 10,
                                             value = 5),
                                 
                                 selectInput("selection3", "Choose book three:", c(Select='', booklist$Book_Title), selectize=TRUE),
                                 sliderInput(inputId = "rates3",
                                             label = "Rate:",
                                             min = 1,
                                             max = 10,
                                             value = 5),
                                 
                                 actionButton("update1", "Rate", class = "btn btn-primary"),
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 verbatimTextOutput("txtout"),
                                 #h3("Thank you for rating the book. "),
                                 h3 (style = "color:#008ae6; text-align: left;",
                                     textOutput("headingText")
                                     ),
                                 #h4("You have rated:"),
                                 h4 (style = "color:#008ae6; text-align: left;",
                                   textOutput("headingText2")
                                   ),
                                 h5 (style = "color:#555555; text-align: left;", 
                                     textOutput("text1")
                                 ),
                                 h5 (style = "color:#555555; text-align: left;", 
                                     textOutput("theRate1")
                                 ),
                                 h5 (style = "color:#555555; text-align: left;", 
                                     textOutput("text2")
                                 ),
                                 h5 (style = "color:#555555; text-align: left;", 
                                     textOutput("theRate2")
                                 ),
                                 h5 (style = "color:#555555; text-align: left;", 
                                     textOutput("text3")
                                 ),
                                 h5 (style = "color:#555555; text-align: left;", 
                                     textOutput("theRate3")
                                 ),
                                 
                                 #wordCloud
                                 wordcloud2Output("plotcloud")
                                 
                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    #tabPanel("About", "R markdown about the project"),
                    tabPanel("About", 
                             mainPanel(includeHTML("markdown.html"))),
                    tabPanel("Popular Books",DT::dataTableOutput('tblPopular')),
                    tabPanel("Top Rated",DT::dataTableOutput('tblrated')),
                    tabPanel(value = "Recommendations", "Recommendations", "Top 5 recommendations based on your history",DT::dataTableOutput('tblrec'))
                    
             
                    
                ) # navbarPage
) # fluidPage



# Define server function  
server <- function(input, output, session) {
  
    #=====================tables
    output$tblPopular <- DT::renderDataTable(
        DT::datatable(poplist, options = list(pageLength = 5))
    )
    output$tblrated <- DT::renderDataTable(
        DT::datatable(topratedlist, options = list(pageLength = 5))
    )
    
    #=====================word cloud
    output$plotcloud <- renderWordcloud2({
      wordcloud2(cloud)
    })
    
    #=====================button clicked event
    observeEvent(input$update1, {
        # events fired
      
      #hide plotcoud by removing data
      output$plotcloud <- renderWordcloud2({
        #nothing to load
      })
      
        #output$text <- renderText("button clicked")
        output$headingText <- renderText({paste("Thank you for rating the books.")})
        output$headingText2 <- renderText({paste("You have rated:")})
        output$text1 <- renderText({paste("Book one: ",input$selection1)})
        output$theRate1 <- renderText({paste("Rating: ", input$rates1)})
        output$text2 <- renderText({paste("Book two: ", input$selection2)})
        output$theRate2 <- renderText({paste("Rating: ", input$rates2)})
        output$text3 <- renderText({paste("Book three: ", input$selection3)})
        output$theRate3 <- renderText({paste("Rating: ", input$rates3)})
        
        #change tab
        updateTabsetPanel(session, "inTabset",
                          selected = "Recommendations")
        
        # create a vector with all NAs for 27 books 
        ratings <- reactiveVal(rep(NA,27))
        #print(ratings)
        ratingsVec <- ratings()
       
        
        # find Isbn from book title (1st rated book from user)
        isbn1<- booklist[which(booklist$Book_Title == input$selection1),]
        print(isbn1$ISBN)
        
        #str(isbn1)
        
        #find location of the isbn in rating_matrix_final
        index1<-which(colnames(rating_matrix) == isbn1$ISBN)
        print(index1)
        #str(index)
        #print(index1[2])
        val1<-index1
        
        # in the location , add the rating from user
        ratingsVec[val1] <- input$rates1
        
        # find Isbn from book title (2nd rated book from user)
        isbn2<- booklist[which(booklist$Book_Title == input$selection2),]
        #print(isbn2)
        #str(isbn2)
        
        #find location of the isbn in rating_matrix_final
        index2<-which(colnames(rating_matrix) == isbn2$ISBN)
        # in the location , add the rating from user
        val2<-index2
        ratingsVec[val2] <- input$rates2
       
        
        
        # find Isbn from book title (3rd rated book from user)
        isbn3<- booklist[which(booklist$Book_Title == input$selection3),]
        #print(isbn3)
        
        #find location of the isbn in rating_matrix_final
        index3<-which(colnames(rating_matrix) == isbn3$ISBN)
        # in the location , add the rating from user
        val3<-index3
        ratingsVec[val3] <- input$rates3
        
        
        #print the rating vector
        print(ratingsVec)
        
        #convert vector to Matrix
        m <- matrix(ratingsVec, nrow=1, ncol=(ncol(rating_matrix)), dimnames = list(NULL,NULL))
        #print(m)
        
        #Convert Matrix to real rating Matrix
        ratings.user <- as(m, "realRatingMatrix")
        #print(ratings.user)
        
        
        # Predict using  the matrix created
        Top_5_pred_for_user = predict(ratingmodel, ratings.user, n=5)
        Top_5_list_User <- as(Top_5_pred_for_user, "list")
        #print(Top_5_list_User)
        recomdf1=data.frame(Top_5_list_User)
        colnames(recomdf1)="ISBN"
        bookrecomdf<-left_join(recomdf1,books_df,by="ISBN")%>%
          select(Book_Title)
        #print()
        
        # out put the same table to Reco page
        output$tblrec <- DT::renderDataTable(
          DT::datatable(bookrecomdf, options = list(pageLength = 5)))
        
      })
    
  
    
    #=====================test code
    #output$text <- renderText({
    #    return(books[[3,"Book_Title"]])
    #})
  
    
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server) 


