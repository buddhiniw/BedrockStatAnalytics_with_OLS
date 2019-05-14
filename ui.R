# ui <- fluidPage( #theme=shinytheme("flatly"),
#                  shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
#                  
#                  # App title
#                  titlePanel("Bedrock Stat Analytics"),
#                  
#                  # Login page 
#                  fluidRow(
#                    column(width = 8, offset = 2, br(), br(), br(), br(),"Please log in",
#                           #includeMarkdown("report_enet.Rmd"),
#                           wellPanel(
#                             textInput("user","User ID:"),
#                             passwordInput(inputId = 'password',label = 'Password'),
#                             actionButton("button_login", "Login")
#                           )
#                    )
#                  )
#                  
#                  
#                  
# )

shinyUI(
  uiOutput("ui")
)