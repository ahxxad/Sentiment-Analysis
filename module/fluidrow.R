# Fluid Row
fluidRow(
  column(
    width = 12,
    #tags$h2(glue("Your permission level is: {user_info()$permissions}.
    #  You logged in at: {user_info()$login_time}.")),
    # box(
    # width = NULL,
    #status = "primary",
    #title = "Data Dashboard",
    
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
      
      tabItems(
        tabItem(tabName = "contpan",
                box(
                  width = NULL,
                  status = "primary",
                  title = "Data Dashboard",
                  tabsetPanel(
                    id = "tabset2",
                    tabPanel(title = "Data", value = "tablestabset",
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
                                           title = "Fillter Table", value = "filtabletab",
                                           fluidRow(
                                             column(width = 12,
                                                    div(style = 'overflow-x: scroll;', DT::dataTableOutput('fildt'))
                                             )
                                           )
                                           
                                         ),
                                         tabPanel(
                                           title = "Matched Data", value = "mattabletab",
                                           tabsetPanel(id="tabset8",
                                                       tabPanel(
                                                         title = "Matched Data", value = "mattabletab",
                                                         fluidRow(
                                                           box(
                                                             title = "Caller & Called VS. mob Column in IMEI Table",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata'))
                                                           ),
                                                           box(
                                                             title = "IMEI VS. IMEI Column in IMEI Table",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata1'))
                                                           ),
                                                           box(
                                                             title = "Caller & Called VS mob Column in mobi Table",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata2'))
                                                           ),
                                                           box(
                                                             title = "Matched Table 4",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata3'))
                                                           ),
                                                           box(
                                                             title = "Matched Table 5",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata4'))
                                                           ),
                                                           box(
                                                             title = "Matched Table 6",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchdata5'))
                                                           )
                                                         ),
                                                       ),
                                                       tabPanel(
                                                         title = "Ownership Data", value = "mattabletab",
                                                         fluidRow(
                                                           box(
                                                             title = "Caller & Called VS. AllNumbers",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('allnumbers'))
                                                           ),
                                                           box(
                                                             title = "Caller & Called VS. AllNumberAsia",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('allnumberasia'))
                                                           ),
                                                           box(
                                                             title = "Caller & Called VS. Duplicated",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('duplicated'))
                                                           ),
                                                           box(
                                                             title = "Caller & Called VS. Recycled",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('recycled'))
                                                           )
                                                           
                                                           
                                                         ),
                                                       ),
                                                       tabPanel(
                                                         title = "Facebook Data", value = "fbtabletab",
                                                         fluidRow(
                                                           box(
                                                             title = "Caller & Called VS. Facebook Data",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('fbtable'))
                                                           )
                                                         ),
                                                       )
                                                       
                                                       
                                                       
                                           )
                                           
                                         ),
                                         tabPanel(
                                           title = "Matched SQL Data", value = "matsqltabletab",
                                           tabsetPanel(id="tabset8",
                                                       tabPanel(
                                                         title = "Matched Data 1", value = "matsqltabletab1",
                                                         fluidRow(
                                                           box(
                                                             title = "Table 1",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata1'))
                                                           ),
                                                           box(
                                                             title = "Table 2",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata2'))
                                                           ),
                                                           box(
                                                             title = "Table 3",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata3'))
                                                           ),
                                                           box(
                                                             title = "Table 4",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata4'))
                                                           ),
                                                           box(
                                                             title = "Table 5",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata5'))
                                                           ),
                                                           box(
                                                             title = "Table 6",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata6'))
                                                           )
                                                         ),
                                                       ),
                                                       tabPanel(
                                                         title = "Matched Data 2", value = "matsqltabletab2",
                                                         fluidRow(
                                                           box(
                                                             title = "Table 7",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata7'))
                                                           ),
                                                           box(
                                                             title = "Table 8",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata8'))
                                                           ),
                                                           box(
                                                             title = "Table 9",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata9'))
                                                           ),
                                                           box(
                                                             title = "Table 10",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata10'))
                                                           )
                                                           
                                                           
                                                         ),
                                                       ),
                                                       tabPanel(
                                                         title = "Matched Facebook Data", value = "mfbtabletab",
                                                         fluidRow(
                                                           box(
                                                             title = "Table 11",
                                                             status = "primary",
                                                             solidHeader = TRUE,
                                                             div(style = 'overflow-x: scroll;', DT::dataTableOutput('matchtbdata11'))
                                                           )
                                                         ),
                                                       )
                                                       
                                                       
                                                       
                                           )
                                           
                                         )
                             )),
                    
                    
                    tabPanel("Maps", value = "mymapsid",         
                             tabsetPanel(id="mymaps",
                                         # box(
                                         #   width = NULL,
                                         #   status = "primary",
                                         #   title = "Maps",
                                         tabPanel("Map", value = "map1",
                                                  box(
                                                    width = NULL,
                                                    status = "primary",
                                                    title = "Map",
                                                    leafletOutput("statmap",width = "100%", height = "800px")
                                                  )),
                                         tabPanel("Live Map", value = "map2",
                                                  box(
                                                    width = NULL,
                                                    status = "primary",
                                                    title = "Map",
                                                    leafletOutput("mymap1",width = "100%", height = "800px")
                                                  ))
                             ),),
                    
                    tabPanel("Charts", value = "mycharts",
                             tabsetPanel(id="allplots",
                                         tabPanel(
                                           "Plots",
                                           box(
                                             width = NULL,
                                             status = "primary",
                                             title = "Stats",
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
                                           )
                                         ),
                                         tabPanel(
                                           "Interactive Plots",
                                           box(
                                             width = NULL,
                                             status = "primary",
                                             title = "Interactive Stats",
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
                                           )
                                         )
                             )
                    ),
                    
                    # tabPanel(
                    #   "Network",
                    #   simpleNetworkOutput("network", width = "100%", height = "800px"),
                    # ),
                    ## Report
                    tabPanel("Report", #column(6, offset = 3,
                             uiOutput("markdown")),
                    tabPanel("System Log", DT::dataTableOutput("slt")),
                    # tabPanel("DB", DT::DTOutput('database')),
                    # tabPanel("logtable", DT::dataTableOutput("log1table"))
                  ))
        ),
        tabItem(tabName = "alldatasearch",
                box(
                  width = NULL,
                  status = "primary",
                  title = "Data Search",
                  tabsetPanel(
                    id = "datasearch",
                    tabPanel(title = "ISIS DATA", value = "isisdatatabset",
                             tabsetPanel(id = "isisdatatabset1",
                                         tabPanel(
                                           title="Kafalat", value = "kafalatdata",
                                           fluidRow(column(12, br(),
                                                           shinydashboard::box( width = 12, height = "auto",
                                                                                dataTableOutput('kafdata'))))
                                         ),
                                         tabPanel(
                                           title="Nenava", value = "intabletab",
                                           fluidRow(column(12, br(),
                                                           shinydashboard::box( width = 12, height = "auto",
                                                                                dataTableOutput('nendata'))))
                                         ),
                                         tabPanel(
                                           title = "ISIS ROSTER", value = "filtabletab",
                                           fluidRow(
                                             fluidRow(column(12, br(),
                                                             shinydashboard::box( width = 12, height = "auto", dataTableOutput('rosdata'))
                                             )
                                             )
                                             
                                           )
                                         )
                             )),
                    tabPanel(title = "RC Data", value = "rcdatatabsetpanel",
                             tabsetPanel(id = "rcdatatabsetpanel11",
                                         tabPanel(
                                           title="Baghdad", value = "baghdaddata",
                                           fluidRow(column(12, br(),
                                                           shinydashboard::box( width = 12, height = "auto",
                                                                                dataTableOutput('baghdadsupplycarddata'))))
                                         ),
                                         tabPanel(
                                           title="Salahaldin", value = "salahdata",
                                           fluidRow(column(12, br(),
                                                           shinydashboard::box( width = 12, height = "auto",
                                                                                dataTableOutput('salahrcdata'))))
                                         )
                                         
                             )
                    )
                    #)
                    
                    
                  ))
                # tabItem(tabName = "casemgmt",
                #         box(
                #           width = NULL,
                #           status = "primary",
                #           title = "Create Case",
                #           tabPanel(
                #             title="Create Case", value = "createcase",
                #             fluidRow(column(12, br(),
                #                             shinydashboard::box(
                #                               width = 6,
                #                               title = "Create New Case",
                #                               status = "primary",
                #                               solidHeader = TRUE,
                #                               textInput("nameInput", "Name"),
                #                               textInput("descriptionInput", "Description"),
                #                               textInput("assignedToInput", "Assigned To"),
                #                               selectInput("statusInput", "Status", choices = c("Open", "In Progress", "Closed")),
                #                               actionButton("createBtn", "Create")
                #                             )))
                #           ))
                #         
                # ),
                # tabItem(tabName = "updatecase",
                #         h2("Update case"),
                #         box(
                #           width = NULL,
                #           status = "primary",
                #           title = "Update case",
                #           tabPanel(
                #             title = "Update Cases", value = "updatecases",
                #             fluidRow(
                #               column(width = 12,
                #                      div(style = 'overflow-x: scroll;', DT::dataTableOutput('updatecaseTable'))
                #               )
                #             )
                #             
                #           ))
                # ),
                # tabItem(tabName = "searchcase",
                #         
                #         h2("Search Case Tab"),
                #         box(
                #           width = NULL,
                #           status = "primary",
                #           title = "Search Case",
                #           tabPanel(
                #             title="Search Cases", value = "searchcase",
                #             fluidRow(column(12, br(),
                #                             shinydashboard::box( width = 12, height = "auto",
                #                                                  dataTableOutput("searchcaseTable"))))
                #           )))
                
                
                
                
                
                
                
        )
      ) # Fluid page
      
      #)
    ) # Column 24558
  )
)
