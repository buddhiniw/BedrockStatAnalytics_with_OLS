shinyServer(function(input, output, session) {
  
  #**************************************************
  #  UI code
  #**************************************************
  
  # for code development.
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      
      ##### Login Interface
      fluidPage(
        
        ## Applicatin name
        titlePanel("Bedrock Stat Analytics"),
       
         fluidRow(
          column(width = 5, offset = 2,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      #### Apps Interface
      fluidPage(
        
        ## Applicatin name
        titlePanel("Bedrock Stat Analytics"),
        
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", "1. Upload the CSV Data File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            selectInput("reg_opt","2. Select Regression Option & Run Analysis",c("Elasticnet (n<p)","OLS (n>p)")),
            actionButton("analyze","Analyze"),
            br(),br(),br(),
            
            "3. Download the Report\n",
            downloadButton("report","Download PDF")
            
          ),
          
          # Main panel for displaying outputs 
          mainPanel(
            uiOutput('results')

          )
          
        )
      )
    }
  })
  
  #**************************************************
  #  ANALYSIS Server code
  #**************************************************

  # Get the data file
  output$contents <- renderDataTable({
    
    req(input$file1)
    
    tryCatch({
      dataIn <- read.csv(input$file1$datapath,header = T)
      df <- dataIn[1:(nrow(dataIn)-1),]
        
    },
    error = function(e) {
    # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    return(df)
  })
  # 
  # #check the regression type and assign the correct RMD 
  reportType <- eventReactive(input$analyze, {
    # default
    user_input$report <- "report_enet.Rmd"
    # select based on user input
    if (req(input$reg_opt) == "Elasticnet (n<p)")
      user_input$report <- "report_enet.Rmd"
    if (req(input$reg_opt) == "OLS (n>p)")
      user_input$report <-"report_ols.Rmd"
  })
  
  # # updated when the analysis button is clicked
  # reportType <- eventReactive(input$analyze, {
  #   print("sdfsdfsdf")
  #   
  #   switch(input$tempReport,
  #          "Elasticnet (n<p)" = "report_enet.Rmd",
  #          "OLS (n>p)" = "report_ols.Rmd")
  # }, ignoreNULL = FALSE)
  
  # observe({
  #   if (req(input$reg_opt) == "Elasticnet (n<p)")
  #     user_input$report <- "report_enet.Rmd"
  #   if (req(input$reg_opt) == "OLS (n>p)")
  #     user_input$report <-"report_ols.Rmd"
  # 
  # })
  
  observeEvent(input$analyze, {
    # Set up parameters to pass to Rmd document
    dataIn <- read.csv(input$file1$datapath,header = T)
    print(input$file1$datapath)
    params <- list(dat_data = dataIn, dat_file = input$file1$name)
    
    reportType()    
    tempReport <- user_input$report
    #tempReport <- reportType()
    #print(tempReport)
    rmarkdown::render(tempReport,c("html_document", "pdf_document"), 
                      params = params,
                      envir = new.env(parent = globalenv()))

  })
  
  output$results <- renderUI({

    if(input$analyze){
      # # Set up parameters to pass to Rmd document
      # dataIn <- read.csv(input$file1$datapath,header = T)
      # print(input$file1$datapath)
      # params <- list(dat_data = dataIn, dat_file = input$file1$name)
      # 
      # reportType()    
      # tempReport <- user_input$report
      # #tempReport <- reportType()
      # #print(tempReport)
      # rmarkdown::render(tempReport,c("html_document", "pdf_document"), 
      #                   params = params,
      #                   envir = new.env(parent = globalenv()))
      withMathJax()
      #includeHTML("report_enet.html")
      includeMarkdown("report_enet.Rmd")
      
    }else{
      pre(includeText("format.txt"))
    }

  })
  
  
  output$report <- downloadHandler(
    filename = "bedrock_analytics_report.pdf",
    content = function(file) {
      # use file.copy to provide the file "in" the save-button
      file.copy("/home/buddhini/MyWork/Upwork/R_bedrock_gui/shinyapp/NewAppInterface/report_enet.pdf", file)
    }
  )

  
  
  #**************************************************
  #  PASSWORD Server code
  #**************************************************
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "", report="")
  
  # dataframe that holds usernames, passwords and other user data
  user_db <- data.frame(
    user = c("Ason", "Ason2"),
    password = c("Ason123", "Ason234"), 
    permissions = c("admin", "standard"),
    name = c("Ason", "Standard User"),
    stringsAsFactors = FALSE
  )
  
  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw). set status value for
  #       error message code below
  
  observeEvent(input$login_button, {
    # call login module supplying data frame, user and password cols
    # and reactive trigger

    row_username <- which(user_db$user == input$user_name)
    row_password <- which(user_db$password == input$password) # digest() makes md5 hash of password
    
    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$authenticated <- TRUE
    }
  
    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })
    
  
  
  #**************************************************
  #  Login screen
  #**************************************************
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),
      
      actionButton("login_button", "Log in")
    )
  })
  
  #**************************************************
  #  ERROR for bad credentials
  #**************************************************
  output$pass <- renderUI({
    if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })
  
})
