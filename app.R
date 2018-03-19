#######################
#
# app.R
#
# code using shinyjs to extract cookies from the session
#
#
#
#######################
#
library(shiny)
library(shinyjs)

############
# Random sessionid
#
 sessionid <- paste(
   collapse = '', 
   sample(x = c(letters, LETTERS, 0:9), size = 64, replace = TRUE) )

###########
# if want to fix thesession id
#
# sessionid <- "OQGYIrpOvV3KnOpBSPgOhqGxz2dE5A9IpKhP6Dy2kd7xIQhLjwYzskn9mIhRAVHo" 


jsCode <- '
shinyjs.getcookie = function(params) {
var cookie = Cookies.get("id");
if (typeof cookie !== "undefined") {
Shiny.onInputChange("jscookie", cookie);
} else {
var cookie = "";
Shiny.onInputChange("jscookie", cookie);
}
}
shinyjs.setcookie = function(params) {
Cookies.set("id", escape(params), { expires: 0.5 });  
Shiny.onInputChange("jscookie", params);
}
shinyjs.rmcookie = function(params) {
Cookies.remove("id");
Shiny.onInputChange("jscookie", "");
}
'

server <- function(input, output, session) {
  
  status <- reactiveVal(value = NULL)
  
  # check if a cookie is present and matching our super random sessionid  
  observe({
    js$getcookie()
    if (!is.null(input$jscookie) && 
        input$jscookie == sessionid) {
      status(paste0('in with session cookie ', input$jscookie))
    }
    else {
      status('out')
    }
  })
  
  observeEvent(input$login, {
    if (input$username == "admin") { 
      js$setcookie(sessionid)
      js$getcookie()
      
      if (!is.null(input$jscookie)) {
        status(paste0('in with session cookie ', input$jscookie))
      }
      if (is.null(input$jscookie)) {
        status(paste0('in with a NULL session cookie returned from input$jscookie'))
      }
      
    } else {
      status('out, because the incorrect user name was entered')
    }
  })
  
  observeEvent(input$logout, {
    status('out')
    js$rmcookie()
  })
  
  observeEvent(input$login, {
       js$setcookie(sessionid)
       js$getcookie()
       # print("server cookie")
       # print(input$jscookie)
  })
  
  output$outputLoginStatus <- renderText(paste0('You are logged ', status()))
  
  output$sessionidInfo     <- renderText(paste0('SessionID: ', sessionid))
  
  output$cookieInfo        <- renderText(paste0("Cookie follows: ", input$jscookie, " :cookie should be before this text"))
  
}

ui <- fluidPage(
  tags$head(
    tags$script(src = "js.cookies.js")
  ),
  useShinyjs(),
  extendShinyjs(text = jsCode),
  
  sidebarLayout(
    sidebarPanel(
      textInput('username', 'User', placeholder = 'admin'),

      actionButton('login', 'Login'),
      
      actionButton('logout', 'Logout')
    ),
    mainPanel(
      verbatimTextOutput('outputLoginStatus'),
      br(),
      verbatimTextOutput('sessionidInfo'),
      br(),
      verbatimTextOutput('cookieInfo')
    )
  )
)

shinyApp(ui = ui, server = server)
