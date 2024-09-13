# Dashboard_Page
dashboardPage(
  header = dashboardHeader(#enable_rightsidebar = TRUE, 
    title = "CDR Analysis", 
    userOutput("user"),
    tags$li(class = "dropdown", style = "padding: 8px;", 
            shinyauthr::logoutUI("logout"))
    # tags$li(
    #   class = "dropdown",
    #   tags$a(
    #     icon("github"),
    #     href = "https://github.com/paulc91/shinyauthr",
    #     title = "See the code on github"
    #   )
    # )
  ),
  sidebar = dashboardSidebar(
    width = 350, 
    collapsed = TRUE, div(textOutput("welcome"), style = "padding: 20px"), sidebarMenu( sidebarMenuOutput("menu"))),
  
  body = dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  #shinyauthr_login(title = "Login"),
  shinyauthr::loginUI(
    "login", 
    cookie_expiry = cookie_expiry,
    additional_ui = tagList(
      tags$p("This beta version of CDR Analysis System is 
               designed, implemented, developed and tested by Eng. Ahmed Abulsalam Ali for INSS-G36", class = "text-center")
      #HTML(knitr::kable(user_base[, -3], format = "html", table.attr = "style='width:100%;'"))
    )
  ),
  scrollX = TRUE,
  tags$script(HTML("$('body').addClass('fixed');")),
  uiOutput("myappUI"),
  
  
  
  
  
  
  
  # tabItem(tabName = "case",
  #         fluidRow(
  #           column(
  #             12,
  #             align = "right",
  #             offset = 0,
  #             tabBox(
  #               title = "Insert Case Detailes",
  #               id = "tabset4",
  #               wellPanel(
  #                 DT::dataTableOutput("responses", width = 300),
  #                 tags$hr(),
  #                 tags$br(),
  #                 textInput("id", "Case Number", ""),
  #                 textInput("message", "Case Name", "")
  #               ),
  #               wellPanel(actionButton("submit", "Submit"))
  #               
  #             )
  #           ),
  #         ), )
  
  # ),
  
  ),
  dashboardControlbar(
    skin = "dark",
    controlbarMenu(
      id = "controlbar"
    )
  )
)