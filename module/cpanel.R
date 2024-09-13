controlpanel = fluidRow(
  column(
    width = 12,
    tags$h2(glue("Your permission level is: {user_info()$permissions}.
                     You logged in at: {user_info()$login_time}.")),
    box(
      width = NULL,
      status = "primary",
      title = "Data Dashboard",
      fluidPage(
        tags$head(tags$style(
          includeCSS("www/sstyle.css"),
          # HTML(
          # 
          #                      '
          # /* tabBox background */
          # .nav-tabs-custom>.nav-tabs {
          #     background-color: #2F4858;
          # }
          # .nav-tabs-custom > .nav-tabs > li.header {
          #     font-size: 40px;
          #     color: white;
          #     direction: rtl;
          # }'
          #                    ),
        )
        
        ),
        
        tabsetPanel(
          id = "tabset2",
          tabPanel(title = "Tables", value = "tablestabset",
                   tabsetPanel(id = "tabset3",
                               tabPanel(
                                 title="Raw Data", value = "rawdatatab",
                                 fluidRow(column(12, br(),
                                                 shinydashboard::box( width = 12, height = "auto",
                                                                      dataTableOutput('rawdata'))))
                               ),
                               tabPanel(
                                 title="Interactive Table", value = "intabletab",
                                 fluidRow(column(12, br(),
                                                 shinydashboard::box( width = 12, height = "auto",
                                                                      dataTableOutput('table'))))
                               ),
                               tabPanel(
                                 title = "Fillter s Table", value = "filtabletab",
                                 fluidRow(
                                   column(width = 12,
                                          div(style = 'overflow-x: scroll;', DT::dataTableOutput('fildt')) 
                                   )
                                 )
                                 
                               ),
                               tabPanel(
                                 title = "Matched Data", value = "mattabletab",
                                 fluidRow(
                                   box(
                                     title = "Matched Table1",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata'))
                                   ),
                                   box(
                                     title = "Matched Table2",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata1'))
                                   ),
                                   box(
                                     title = "Matched Table3",
                                     status = "primary",
                                     solidHeader = TRUE,
                                     div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata2'))
                                   )
                                 ),
                               )
                   )),
          
          
          ## Report
          tabPanel("Report", #column(6, offset = 3, 
                   uiOutput("markdown")),
          tabPanel("Map", leafletOutput("statmap",width = "100%", height = "800px")),
          tabPanel("Live Map", leafletOutput("mymap1", width = "100%", height = "800px")),
          tabPanel(
            "Plots",
            fluidPage( style = "max-height: 90vh; overflow-y: auto;",
                       # column( width = 12, 
                       #div(style = 'overflow-x: scroll;',
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot0")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot1")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot2")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot3")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot4")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot5")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot6")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot7")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot8")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot9")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot10")),
                       # ) 
                       #)
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot11")),
                       # ) 
                       #)
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot12")),
                       # ) 
                       #)
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("plot13")),
                       # ) 
                       #)
                       
            )
          ),
          tabPanel(
            "Interactive Plots",
            fluidPage( style = "max-height: 90vh; overflow-y: auto;",
                       # column( width = 12, 
                       #div(style = 'overflow-x: scroll;',
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("imei")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("imsi")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("location")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("location1")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("site")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("sector")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("date")),
                       tags$hr(),
                       #fluidRow(column(12, br(),
                       box(width = 12, height = "auto",plotlyOutput("time"))
                       # )
            )
          ),
          # tabPanel(
          #   "Network",
          #   simpleNetworkOutput("network", width = "100%", height = "800px"),
          # ),
          tabPanel("System Log", DT::dataTableOutput("slt")),
          # tabPanel("DB", DT::DTOutput('database')),
          # tabPanel("logtable", DT::dataTableOutput("log1table"))
          
          
          
          
          tabPanel(title = "Case Mangement", value = "mycasestabset",
                   tabsetPanel(id = "tabset4",
                               tabPanel(
                                 title="Create Case", value = "createcase",
                                 fluidRow(column(12, br(),
                                                 div(class = "rtl-column",
                                                     tags$div(class = "form-group",
                                                              # tags$label(class = "control-label",
                                                              #       "اسم:"),
                                                              shinydashboard::box(
                                                                width = 6,
                                                                title = "انشاء قضية جديدة",
                                                                status = "primary",
                                                                solidHeader = TRUE,
                                                                textInput("nameInput", "الاسم"),
                                                                textInput("descriptionInput", "الوصف"),
                                                                textInput("assignedToInput", "فريق التحليل المختص"),
                                                                selectInput("statusInput", "الحالة", choices = c("Open", "In Progress", "Closed")),
                                                                actionButton("createBtn", "Create")
                                                              )
                                                     )
                                                 )))
                               ),
                               tabPanel(
                                 title="Search Cases", value = "searchcase",
                                 fluidRow(column(12, br(),
                                                 shinydashboard::box( width = 12, height = "auto",
                                                                      dataTableOutput("searchcaseTable"))))
                               ),
                               tabPanel(
                                 title = "Update Cases", value = "updatecases",
                                 fluidRow(
                                   column(width = 12,
                                          div(style = 'overflow-x: scroll;', DT::dataTableOutput('updatecaseTable')) 
                                   )
                                 )
                                 
                               )
                               # tabPanel(
                               #   title = "Matched Data", value = "mattabletab",
                               #   fluidRow(
                               #     box(
                               #       title = "Matched Table1",
                               #       status = "primary",
                               #       solidHeader = TRUE,
                               #       div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata'))
                               #     ),
                               #     box(
                               #       title = "Matched Table2",
                               #       status = "primary",
                               #       solidHeader = TRUE,
                               #       div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata1'))
                               #     ),
                               #     box(
                               #       title = "Matched Table3",
                               #       status = "primary",
                               #       solidHeader = TRUE,
                               #       div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata2'))
                               #     )
                               #   ),
                               # )
                   )),
          
          
          
          
          
        )
        
        # Box
        #)
        
        
        
        
      )
    )
    
  )
)
