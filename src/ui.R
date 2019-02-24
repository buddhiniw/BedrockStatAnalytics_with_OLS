library(shinythemes)



ui <- fluidPage( theme=shinytheme("cosmo"),
  titlePanel("Bedrock Stat Analytics"),
  navbarPage("Get Started!",
             id = "start",
             tabPanel("Login",
                      wellPanel(
                        textInput("user",
                                  "User ID:",
                                  width = "70%"),
                        passwordInput(inputId = 'password',
                                      label = 'Password',
                                      width = "70%"),
                        
                        actionButton("button_login", "Login"),
                        br(),
                        "For Upwork Client",
                        hr(),
                        strong(textOutput("verification_result"))
                      )
             ),
   
             navbarMenu("File",
                        tabPanel("Upload CSV",
                                 wellPanel(
                                   uiOutput("ui_page_1")
                                 )
                        )),
             
             navbarMenu("Analysis",
                        tabPanel("Elastic Net Regression (n < p)",
                                 wellPanel(
                                  uiOutput("ui_opt_enet")
                                )),
                        tabPanel("OLS Regression (n > p)",
                                wellPanel(
                                  uiOutput("ui_opt_ols")
                                ))
                        )
        
  )
  
  

)