library(shiny)
library(caret) # Package to traint the model
library(glmnet) # Package to fit ridge/lasso/elastic net models
library(DMwR)
library(boot) # Package to do bootstrap error estimates
library(elasticnet)
library(dplyr)
library(corrplot)
library(officer)
library(rmarkdown)
library(latexpdf)
library(kableExtra)


shinyServer(function(input, output, session) {
  
  # Create a reactive object with no values
  current_user_status <- reactiveValues()
  current_user_status$logged <- FALSE
  current_user_status$current_user <- NULL
  current_user_status$access <- NULL
  current_user_status$report <- NULL
  
  
  output$ui_page_1 <- renderUI({
    
    if(current_user_status$logged == TRUE){
      if("access_to_page_1" %in% current_user_status$access){
        tagList(
          verticalLayout(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            
          )
        )  
      } else {
        tagList(
          div("No access to this part.", style="color:purple")
        )
      }
      
    } else {
      tagList(
        div("Please log in", style="color:red")
      )
    }
    
  })
  
  output$ui_opt_enet <- renderUI({
    if(current_user_status$logged == TRUE){
      if("access_to_page_1" %in% current_user_status$access){
        tagList(
          verticalLayout(
            downloadButton("report","Download PDF Report")
            #div("To save as pdf: After downloading, open file in browser click print and save as pdf",style="color:red")
            )
        )  
      } else {
        tagList(
          div("No access to this part.", style="color:purple")
        )
      }
      
    } else {
      tagList(
        div("Please log in", style="color:red")
      )
    }
    
  })
  
  
  output$ui_opt_ols <- renderUI({
    if(current_user_status$logged == TRUE){
      if("access_to_page_1" %in% current_user_status$access){
        tagList(
          verticalLayout(
            downloadButton("report","Download PDF Report")
            #div("To save as pdf: After downloading, open file in browser click print and save as pdf",style="color:red")
          )
        )  
      } else {
        tagList(
          div("No access to this part.", style="color:purple")
        )
      }
      
    } else {
      tagList(
        div("Please log in", style="color:red")
      )
    }
    
  })
  
  
  
  
  output$data_1_for_authorized_user <- renderTable({
    head(iris)
  })
  
  
  user_db <- list("id"= c("Ason"),
                  "password" = c("Ason123"))
  
  print(user_db$id)
  
  observeEvent(input$button_login, {
    
    if(input$user!="" && input$password!="" && input$user %in% user_db$id && input$password == user_db$password[user_db$id == input$user]){
      current_user_status$logged <- TRUE
      current_user_status$current_user <- input$user
     # current_user_status$access <- user_db[user_db$id == current_user_status$current_user,c("access_to_page_1", "access_to_page_2")] %>%
    #                                        unlist %>% {.==1} %>% names(.)[.]
 
      ############################### TEMPORARY COS I CANT GET ABOVE TO WORK!!!!!!!!!     
      current_user_status$access <- c("access_to_page_1", "access_to_page_2")
      
      output$verification_result <- renderText({
        "Login succeeded"
      })
      
    } else {
      current_user_status$logged <- FALSE
      current_user_status$current_user <- NULL
      
      output$verification_result <- renderText({
        "Login failed"
      })
    }
  })
  
  
  output$contents <- renderDataTable({

    req(input$file1)
    
    tryCatch(
      {
        dataIn <- read.csv(input$file1$datapath,
                           header = T)
        df <- dataIn[1:(nrow(dataIn)-1),]
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  
  

  # reportType <- function(){
  #   observe({
  #     if (req(input$start) == "Elastic Net Regression (n < p)")
  #       report <- "report_enet.Rmd" 
  #     if (req(input$start) == "OLS Regression (n > p)")
  #       report <- "report_ols.Rmd" 
  #   }
  #   )
  #   return(report)
  # }
 
  observe({
    if (req(input$start) == "Elastic Net Regression (n < p)")
      current_user_status$report <- "report_enet.Rmd" 
    if (req(input$start) == "OLS Regression (n > p)")
      current_user_status$report <-"report_ols.Rmd"
    
  })
   

  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      if (file.exists('report.pdf')) file.remove('report.pdf')
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      # Removed the tempReport option to enable reading finding of R scripts used in the generation of
      # the report. 
      #tempReport <- file.path(tempdir(), "report.Rmd")
      tempReport <- current_user_status$report 
      
       #tempReport <- "report_ols.Rmd"  
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      dataIn <- read.csv(input$file1$datapath,
                         header = T)
      params <- list(dat_data = dataIn, dat_file = input$file1$name)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,"pdf_document",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
      
    }
  )

  
})