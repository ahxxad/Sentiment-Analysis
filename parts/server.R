server <- function(input, output, session) {
  
  # call login module supplying data frame, user and password cols and reactive trigger.
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password_hash,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out = reactive(logout_init())
  )
  
  # call the log out module with reactive trigger to hide/show
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  #req(credentials()$user_auth)
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      output$welcome <- renderText({
        req(credentials()$user_auth)
        glue("Welcome {user_info()$name}")
      })
      
      #Render user details
      output$user <- renderUser({
        req(credentials()$user_auth)
        dashboardUser(
          name = glue("{user_info()$name}"),
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
          title = "shinydashboardPlus",
          subtitle = "Author",
          footer = p("The footer", class = "text-center"),
          fluidRow(
            dashboardUserItem(
              width = 6,
              socialButton(
                href = "https://dropbox.com",
                icon = icon("dropbox")
              )
            ),
            dashboardUserItem(
              width = 6,
              socialButton(
                href = "https://github.com",
                icon = icon("github")
              )
            )
          )
        )
      })
      
      
      ### Rendering Menu, Working Just Fine
      output$menu = renderMenu({
        source("module/menu.R", local = TRUE)
      })
      
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
    
    ##
    output$myappUI <- renderUI({
      req(credentials()$user_auth)
      
      # Fluid Row
      source("module/fluidrow.R", local = TRUE)
      
    }) 
    
    
    ##
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  
  
  # Log
  event <- reactiveVal(NULL)
  
  create_and_write_log <- function(triggered_id) {
    t <- Sys.time()
    dtf <- data.frame(
      #id = rv$count,
      Action_Button  = paste("Button", triggered_id),
      Action_Value = file_names,
      #Br = session$request[['HTTP_USER_AGENT']],
      Action_DateTime = format(t, "%Y-%m-%d %I:%M:%S%p"),  
      App_Username = as.character(user_info()$user), #()  
      #App_Username = as.character(reactiveValuesToList(credentials)$user), #()  
      #App_Username = as.character(glue("Welcome {user_info()$user}")),  #glue("Welcome {user_info()$name}")
      User_IP = as.character(session$request[["REMOTE_ADDR"]])
    )
    #    rv$count <- rv$count + 1
    write.table(dtf, "app-usage-log.csv" , append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
    dtf
  }
  
  
  rv <- reactiveValues(dfnew = data.frame(matrix(ncol = 4, nrow = 0)), count = 1)
  
  ##
  
  
  ## Welcome
  
  
  
  
  
  # NEW APPROACH
  # Create a reactiveValues object to store the processed dataframes
  #processed_adata <- reactiveValues(Dataframe1 = NULL, Dataframe2 = NULL)
  processed_afbdata <-  reactiveVal(data.frame())
  processed_adata <- reactiveVal(data.frame())
  processed_kdata <- reactiveVal(data.frame())
  processed_zdata <- reactiveVal(data.frame())
  ##
  
  # Reading Data Module,  Working JF
  observeEvent(input$xlsxs, {
    withProgress(message = 'Uploading... Please wait...', {
      source("module/readdata.R", local = TRUE)
      output$rawdata <- DT::renderDataTable(
        readdata(),
        class = "display nowrap compact",
        filter = "top",
        #extensions = c("FixedColumns", "FixedHeader"),
        options = list(
          pageLength = 25,
          lengthMenu = c(25, 50, 75, 100),
          scrollX = TRUE,
          scrollY = "600px",
          fixedHeader = TRUE,
          fixedColumns = list(leftColumns = 1)
        )
      )
      #updateTabItems(session, "mymenu", selected = "showdata")
      updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
      updateTabsetPanel(inputId = "tabset3", selected = "rawdatatab")
      
    })
    showModal(
      modalDialog(
        title = "Notification",
        "You've uploaded your CDR successfully. 
        Now click Process button according to your CDR's Company! (Asia | Korek | Zain).",
        tags$head(tags$script(src = "dialog.js"))
      )
    )
    
    if (!is.null(input$xlsxs)) {
      uploaded_files <- input$xlsxs
      file_names <<- uploaded_files$name
      #session_token <<- as.character(UUIDgenerate())
      triggered_id <- "xlsxs"
      mylogdf_data <- create_and_write_log(triggered_id)
      
    }
    
    
  })
  
  
  
  ## NEW APPROACH
  
  observeEvent(input$cleana, {
    tryCatch(
      {
        if (is.null(input$xlsxs)) {
          showModal(modalDialog(
            title = "Error",
            "Please Upload Asia CDR First",
            #textAreaInput(inputId = "textField", label = "window", value = "ABC"),
            tags$head(tags$script(src = "dialog.js"))
          ))
          req(input$xlsxs)
        } else {
          withProgress(message = 'Processing... please wait!', {
            #source("dev/multidfreturn.R", local = TRUE) 
            source("module/asiadataprocessing.R", local = TRUE)
            output$table <-  DT::renderDataTable(
              isolate(processed_adata()),
              class = "display nowrap compact",
              filter = "top",
              #extensions = c("FixedColumns", "FixedHeader"),
              options = list(
                pageLength = 25,
                lengthMenu = c(25, 50, 75, 100),
                scrollX = TRUE,
                scrollY = "600px",
                fixedHeader = TRUE,
                fixedColumns = list(leftColumns = 1)
              )
            ) # Add with progress here !!!!!
            source("module/matching_asia_mobilesold.R", local = TRUE)
            output$matchdata <- DT::renderDataTable(
              isolate(matched_er_ed_imie()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            ) #matching_er_ed_allnumbers
            output$matchdata1 <- DT::renderDataTable(
              isolate(matched_imei()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            output$matchdata2 <- DT::renderDataTable(
              isolate(matchingmobi()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            output$matchdata3 <- DT::renderDataTable(
              isolate(matchingfollow()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            source("module/matching_asia_ownership.R", local = TRUE)
            output$allnumbers <- DT::renderDataTable(
              isolate(matched_er_ed_allnumbers()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            output$allnumberasia <- DT::renderDataTable(
              isolate(matched_er_ed_allnumberasia()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            output$duplicated <- DT::renderDataTable(
              isolate(matched_er_ed_duplicated()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            output$recycled <- DT::renderDataTable(
              isolate(matched_er_ed_recycle()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            ) 
            
            
            source("module/matching_asia_fb.R", local = TRUE)
            output$fbtable <- DT::renderDataTable(
              isolate(matched_er_ed_fb()),
              class = "display nowrap compact",
              filter = "top",
              options = list(
                pageLength = 15,
                lengthMenu = c(15, 50, 75, 100)
              )
            )
            
            # Searching and matching ISIS Data
            # tryCatch({
            #   matched_kafalat <- eventReactive(input$searchButton, {
            #     matching_kafalat(input, kafalat, input$select1, drop = FALSE)()
            #   })
            #   
            #   output$kafdata <- renderDT({
            #     matched_kafalat()
            #   })
            # },
            # error = function(e) {
            #   print(paste("Error:", conditionMessage(e)))
            # })
            # 
            # tryCatch({
            #   matched_nenava <- eventReactive(input$searchButton, {
            #     matching_nenava(input, nenava, input$select2, drop = FALSE)()
            #   })
            #   
            #   output$nendata <- renderDT({
            #     matched_nenava()
            #   })
            # },
            # error = function(e) {
            #   print(paste("Error:", conditionMessage(e)))
            # })
            # 
            # tryCatch({
            #   matched_isisroster <- eventReactive(input$searchButton, {
            #     matching_isisroster(input, isisroster, input$select3, drop = FALSE)()
            #   })
            #   
            #   output$rosdata <- renderDT({
            #     matched_isisroster()
            #   })
            # },
            # error = function(e) {
            #   print(paste("Error:", conditionMessage(e)))
            # })
            
            updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
            updateTabsetPanel(session, "tabset3", selected = "intabletab")
          })
          #source("module/acdrpp.R", local = TRUE)
        }
        
        if (!is.null(input$cleana)) {
          uploaded_files <- input$xlsxs
          file_names <<- uploaded_files$name
          #session_token <<- as.character(UUIDgenerate())
          triggered_id <- "cleana"
          mylogdf_data <- create_and_write_log(triggered_id)
          
        }
      }, error = function(e) {
        showModal(
          modalDialog(
            title = "Error",
            "You've clicked wrong --- button",
            tags$head(tags$script(src = "dialog.js"))
          )
        )
      }
    )
  })
  
  
  
  ## AD (Filter Table) Rendering Function, Working JF - Note! review interactive plots!
  observeEvent(input$DateRange, {
    tryCatch({
      output$fildt <- DT::renderDataTable(
        isolate(processed_adata()) %>% filter(Date >= input$DateRange[1] &
                                                Date <= input$DateRange[2]),
        class = "display nowrap compact",
        filter = "top",
        options = list(
          scrollX = TRUE,
          scrollY = "600px",
          pageLength = 25,
          lengthMenu = c(25, 50, 75, 100)
        )
      )
      # updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
      # updateTabsetPanel(session, "tabset3", selected = "filtabletab")
      if (!is.null(input$DateRange)) {
        start_value <- input$DateRange[1]
        end_value <- input$DateRange[2]
        file_names <<- as.character(paste(start_value,end_value))
        #session_token <<- as.character(UUIDgenerate())
        triggered_id <- "DateRange"
        mylogdf_data <- create_and_write_log(triggered_id)
        
      }
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've choosed wrong Date Range",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  ## AD Static Map Plotting Function + note dynamic map to be add + filter map + all towers !
  observeEvent(input$mapa, {
    tryCatch({
      withProgress(message = 'Rendering... please wait!', {
        source("module/towers_setting.R", local = TRUE)
        source("module/asiastaticmap.R", local = TRUE)
        updateTabsetPanel(inputId = "tabset2", selected = "mymapsid") #mycharts
        updateTabsetPanel(session, "mymaps", selected = "Map") 
      })
      if (!is.null(input$mapa)) {
        uploaded_files <- input$xlsxs
        file_names <<- uploaded_files$name
        #session_token <<- as.character(UUIDgenerate())
        triggered_id <- "mapa"
        mylogdf_data <- create_and_write_log(triggered_id)
        
      }
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've clicked wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  observeEvent(input$timex, {
    tryCatch({
      updateTabsetPanel(session, "tabset2", selected = "Map")  
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've choosed wrong Date Range",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  ## AD Plotting (Static + Dynamic Plots)
  observeEvent(input$asiaplots, {
    tryCatch({
      withProgress(message = 'Processing... please wait!', {
        source("module/asiaplots.R", local = TRUE)
        #updateTabItems(session, "mymenu", selected = "show")
        updateTabsetPanel(inputId = "tabset2", selected = "mycharts") 
        updateTabsetPanel(session, "tabset2", selected = "Plots")
      })  
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've choosed wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
    
  })
  
  ## Rendering | Printing Asia Report
  #observeEvent({})
  output$arbtn <- downloadHandler(
    filename = function() {
      "Asia CDR Report.docx"
    },
    content = function(file) {
      withProgress(message = 'The Asia CDR report is being generated... Please wait.', {
        tempReport <- file.path(tempdir(), "amarkdown.Rmd")
        file.copy("amarkdown.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render(
          "amarkdown.Rmd",
          output_format = "word_document",
          output_file = file,
          params = list(table = processed_data()),
          # here I'm passing data in params
          envir = new.env(parent = globalenv()),
          clean = F,
          encoding = "utf-8"
        )
      })
    }
  )
  
  ## Rendering | Printing Asia XLSX Data
  output$axlsx <- downloadHandler(
    
    filename = paste("data", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep=""),
    
    content = function(file) {
      
      # create a workbook
      wb <- createWorkbook()
      
      # add a worksheet to the workbook
      addWorksheet(wb, "Sheet1")
      
      # Loop through each cell in the data frame
      for (i in seq_len(nrow(acdrdata()))) {
        for (j in seq_len(ncol(acdrdata()))) {
          
          # Get the current cell value
          cell_value <- acdrdata()[i, j]
          
          # Check if the cell value is numeric
          if (is.numeric(cell_value)) {
            # If the cell value is numeric, write it to the cell and apply the "0.00" number format
            setCellStyle(wb, "Sheet1", i, j, CellStyle(wb, dataFormat = "0"))
            writeData(wb, "Sheet1", cell_value, startRow = i, endRow = i, startCol = j, endCol = j)
          } else if (is.character(cell_value)) {
            # If the cell value is a character string, try to convert it to a number
            num_value <- as.numeric(gsub(",", ".", cell_value))
            
            # If the conversion is successful, write the number to the cell and apply the "0.00" number format
            if (!is.na(num_value)) {
              setCellStyle(wb, "Sheet1", i, j, CellStyle(wb, dataFormat = "@"))
              writeData(wb, "Sheet1", num_value, startRow = i, endRow = i, startCol = j, endCol = j)
            } else {
              # If the conversion fails, write the original string to the cell without any formatting
              writeData(wb, "Sheet1", cell_value, startRow = i, endRow = i, startCol = j, endCol = j)
            }
          }
        }
      }
      
      # write the data to the worksheet
      
      writeData(wb, "Sheet1", acdrdata())
      
      
      
      # save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType ="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    #contentType = "application/octet-stream",
    #contentDisposition = "attachment"
  )
  
  #
  
  ### KD Part
  #
  ## KD Processing Module, Working JF
  observeEvent(input$cleank, {
    tryCatch({
      if (is.null(input$xlsxs)) {
        showModal(modalDialog(
          title = "Error",
          "Please Upload Korek CDR First",
          tags$head(tags$script(src = "dialog.js"))
        ))
        
        req(input$xlsxs)
        
      } else {
        source("module/korekdataprocessing.R", local = TRUE)
        output$table <-  DT::renderDataTable(
          isolate(processed_kdata()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 25,
            lengthMenu = c(25, 50, 75, 100),
            scrollX = TRUE,
            scrollY = "600px",
            fixedHeader = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        )
        source("module/matching_korek_mobilesold.R", local = TRUE)
        output$matchdata <- DT::renderDataTable(
          isolate(matched_er_ed_imie()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        ) #matching_er_ed_allnumbers
        output$matchdata1 <- DT::renderDataTable(
          isolate(matched_imei()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$matchdata2 <- DT::renderDataTable(
          isolate(matchingmobi()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$matchdata3 <- DT::renderDataTable(
          isolate(matchingfollow()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        source("module/matching_korek_ownership.R", local = TRUE)
        output$allnumbers <- DT::renderDataTable(
          isolate(matched_er_ed_allnumbers()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$allnumberasia <- DT::renderDataTable(
          isolate(matched_er_ed_allnumberasia()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$duplicated <- DT::renderDataTable(
          isolate(matched_er_ed_duplicated()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$recycled <- DT::renderDataTable(
          isolate(matched_er_ed_recycle()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        source("module/matching_korek_fb.R", local = TRUE)
        output$fbtable <- DT::renderDataTable(
          isolate(matched_er_ed_fb()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
        updateTabsetPanel(session, "tabset3", selected = "intabletab")
      }
      source("module/kcdrpp.R", local = TRUE)
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've clicked wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    }
    )
  })
  
  observeEvent(input$DateRange, {
    tryCatch({
      output$fil <- DT::renderDataTable(
        isolate(kdata()) %>% filter(Date >= input$DateRange[1] &
                                      Date <= input$DateRange[2]),
        class = "display nowrap compact",
        filter = "top",
        options = list(
          scrollX = TRUE,
          scrollY = "600px",
          pageLength = 25,
          lengthMenu = c(25, 50, 75, 100)
        )
        
      )
      # updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
      # updateTabsetPanel(session, "tabset3", selected = "filtabletab")
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've choosed wrong Date Range",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  ## KD Static Map Plotting Function + note dynamic map to be add + filter map + all towers !
  observeEvent(input$mapk, {
    tryCatch({
      source("module/korekstaticmap.R", local = TRUE)
      updateTabsetPanel(inputId = "tabset2", selected = "mymapsid") #mycharts
      updateTabsetPanel(session, "mymaps", selected = "Map")  
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've choosed wrong Date Range",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  ## KD Plotting (Static + Dynamic Plots)
  observeEvent(input$korekplot, {
    tryCatch({
      source("module/korekplots.R", local = TRUE)
      #updateTabItems(session, "mymenu", selected = "show")
      updateTabsetPanel(inputId = "tabset2", selected = "mycharts") 
      updateTabsetPanel(session, "tabset2", selected = "Plots")
      
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've choosed wrong Date Range",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  ## Rendering | Printing Korek Report
  output$krbtn <- downloadHandler(
    filename = function() {
      "Korek CDR Report.docx"
    },
    content = function(file) {
      withProgress(message = 'The Korek CDR report is being generated... Please wait.', {
        tempReport <- file.path(tempdir(), "kmarkdown.Rmd")
        file.copy("kmarkdown.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render(
          "kmarkdown.Rmd",
          output_format = "word_document",
          output_file = file,
          params = list(table = processed_kdata()),
          # Passing data in params
          envir = new.env(parent = globalenv()),
          clean = F,
          encoding = "utf-8"
        )
      })
      
    }
  )
  
  ## Rendering | Printing Korek XLSX Data
  output$kxlsx <- downloadHandler(
    
    filename = paste("data", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep=""),
    
    content = function(file) {
      
      # create a workbook
      wb <- createWorkbook()
      
      # add a worksheet to the workbook
      addWorksheet(wb, "Sheet1")
      
      # Loop through each cell in the data frame
      for (i in seq_len(nrow(kcdrdata()))) {
        for (j in seq_len(ncol(kcdrdata()))) {
          
          # Get the current cell value
          cell_value <- kcdrdata()[i, j]
          
          # Check if the cell value is numeric
          if (is.numeric(cell_value)) {
            # If the cell value is numeric, write it to the cell and apply the "0.00" number format
            setCellStyle(wb, "Sheet1", i, j, CellStyle(wb, dataFormat = "0"))
            writeData(wb, "Sheet1", cell_value, startRow = i, endRow = i, startCol = j, endCol = j)
          } else if (is.character(cell_value)) {
            # If the cell value is a character string, try to convert it to a number
            num_value <- as.numeric(gsub(",", ".", cell_value))
            
            # If the conversion is successful, write the number to the cell and apply the "0.00" number format
            if (!is.na(num_value)) {
              setCellStyle(wb, "Sheet1", i, j, CellStyle(wb, dataFormat = "@"))
              writeData(wb, "Sheet1", num_value, startRow = i, endRow = i, startCol = j, endCol = j)
            } else {
              # If the conversion fails, write the original string to the cell without any formatting
              writeData(wb, "Sheet1", cell_value, startRow = i, endRow = i, startCol = j, endCol = j)
            }
          }
        }
      }
      
      # write the data to the worksheet
      
      writeData(wb, "Sheet1", kcdrdata())
      
      
      
      # save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType ="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    #contentType = "application/octet-stream",
    #contentDisposition = "attachment"
  )
  
  #
  
  
  ### ZD Part
  ## ZD Processing Module, Working JF
  observeEvent(input$cleanz, {
    tryCatch({
      if (is.null(input$xlsxs)) {
        showModal(modalDialog(
          title = "Error",
          "Please Upload Zain CDR First",
          #textAreaInput(inputId = "textField", label = "window", value = "ABC"),
          tags$head(tags$script(src = "dialog.js"))
        ))
        
        req(input$xlsxs)
        
      } else {
        source("module/zaindataprocessing.R", local = TRUE)
        output$table <- DT::renderDataTable(
          isolate(processed_zdata()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 25,
            lengthMenu = c(25, 50, 75, 100),
            scrollX = TRUE,
            scrollY = "600px",
            fixedHeader = TRUE,
            fixedColumns = list(leftColumns = 1)
          )
        )
        source("module/matching_zain_mobilesold.R", local = TRUE)
        output$matchdata <- DT::renderDataTable(
          isolate(matched_er_ed_imie()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        ) #matching_er_ed_allnumbers
        output$matchdata1 <- DT::renderDataTable(
          isolate(matched_imei()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$matchdata2 <- DT::renderDataTable(
          isolate(matchingmobi()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$matchdata3 <- DT::renderDataTable(
          isolate(matchingfollow()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        source("module/matching_zain_ownership.R", local = TRUE)
        output$allnumbers <- DT::renderDataTable(
          isolate(matched_er_ed_allnumbers()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$allnumberasia <- DT::renderDataTable(
          isolate(matched_er_ed_allnumberasia()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$duplicated <- DT::renderDataTable(
          isolate(matched_er_ed_duplicated()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        output$recycled <- DT::renderDataTable(
          isolate(matched_er_ed_recycle()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        source("module/matching_zain_fb.R", local = TRUE)
        output$fbtable <- DT::renderDataTable(
          isolate(matched_er_ed_fb()),
          class = "display nowrap compact",
          filter = "top",
          options = list(
            pageLength = 15,
            lengthMenu = c(15, 50, 75, 100)
          )
        )
        updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
        updateTabsetPanel(session, "tabset3", selected = "intabletab")
      }
      source("module/zcdrpp.R", local = TRUE)
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've clicked wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  # Review the following lines
  reactive({
    if (input$choice == 'cleanz') {
      Sys.setlocale()
    }  else {
      Sys.setlocale(category = "LC_ALL", locale = "Arabic")
    }
  })
  ## End of revision 
  
  
  observeEvent(input$DateRange, {
    tryCatch({
      output$fil <- DT::renderDataTable(
        isolate(zdata()) %>% filter(Date >= input$DateRange[1] &
                                      Date <= input$DateRange[2]),
        class = "display nowrap compact",
        filter = "top",
        options = list(
          scrollX = TRUE,
          scrollY = "600px",
          pageLength = 25,
          lengthMenu = c(25, 50, 75, 100)
        )
        
      )
      # updateTabsetPanel(inputId = "tabset2", selected = "tablestabset")
      # updateTabsetPanel(session, "tabset3", selected = "filtabletab")
      
      ##
      
      # updateTabsetPanel(inputId = "tabset2", selected = "tablestabset"),
      # updateTabsetPanel(session, "tabset3", selected = "filtabletab")
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've clicked wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    } )
  })
  
  ## ZD Static Map Plotting Function + note dynamic map to be add + filter map + all towers !
  observeEvent(input$mapz, {
    tryCatch({
      source("module/zainstaticmap.R", local = TRUE)
      #updateTabItems(session, "mymenu", selected = "show")
      #updateTabsetPanel(session, "tabset2", selected = "Map")
      updateTabsetPanel(inputId = "tabset2", selected = "mymapsid") #mycharts
      updateTabsetPanel(session, "mymaps", selected = "Map")
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've clicked wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    }) 
  })
  
  ## ZD Plotting (Static + Dynamic Plots)
  observeEvent(input$zainplot, {
    tryCatch({
      source("module/zainplots.R", local = TRUE)
      #updateTabItems(session, "mymenu", selected = "show")
      updateTabsetPanel(inputId = "tabset2", selected = "mycharts") 
      updateTabsetPanel(session, "tabset2", selected = "Plots")
    }, error = function(e) {
      showModal(
        modalDialog(
          title = "Error",
          "You've clicked wrong button",
          tags$head(tags$script(src = "dialog.js"))
        )
      )
    })
  })
  
  ## ZD Rendering | Printing Zain Report
  output$zrbtn <- downloadHandler(
    filename = function() {
      "Zain CDR Report.docx"
    },
    content = function(file) {
      withProgress(message = 'The Zain CDR report is being generated... Please wait.', {
        tempReport <- file.path(tempdir(), "zmarkdown.Rmd")
        file.copy("zmarkdown.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render(
          "zmarkdown.Rmd",
          output_format = "word_document",
          output_file = file,
          params = list(table = processed_zdata()), 
          # here I'm passing data in params
          envir = new.env(parent = globalenv()),
          clean = F,
          encoding = "utf-8"
        )
      })
      
      
    }
  )
  
  ## Rendering | Printing Zain XLSX Data
  output$zxlsx <- downloadHandler(
    
    filename = paste("data", format(Sys.time(), "%Y%m%d%H%M%S"), ".xlsx", sep=""),
    
    content = function(file) {
      
      # create a workbook
      wb <- createWorkbook()
      
      # add a worksheet to the workbook
      addWorksheet(wb, "Sheet1")
      
      # Loop through each cell in the data frame
      for (i in seq_len(nrow(zcdrdata()))) {
        for (j in seq_len(ncol(zcdrdata()))) {
          
          # Get the current cell value
          cell_value <- zcdrdata()[i, j]
          
          # Check if the cell value is numeric
          if (is.numeric(cell_value)) {
            # If the cell value is numeric, write it to the cell and apply the "0.00" number format
            setCellStyle(wb, "Sheet1", i, j, CellStyle(wb, dataFormat = "0"))
            writeData(wb, "Sheet1", cell_value, startRow = i, endRow = i, startCol = j, endCol = j)
          } else if (is.character(cell_value)) {
            # If the cell value is a character string, try to convert it to a number
            num_value <- as.numeric(gsub(",", ".", cell_value))
            
            # If the conversion is successful, write the number to the cell and apply the "0.00" number format
            if (!is.na(num_value)) {
              setCellStyle(wb, "Sheet1", i, j, CellStyle(wb, dataFormat = "@"))
              writeData(wb, "Sheet1", num_value, startRow = i, endRow = i, startCol = j, endCol = j)
            } else {
              # If the conversion fails, write the original string to the cell without any formatting
              writeData(wb, "Sheet1", cell_value, startRow = i, endRow = i, startCol = j, endCol = j)
            }
          }
        }
      }
      
      # write the data to the worksheet
      
      writeData(wb, "Sheet1", zcdrdata())
      
      
      
      # save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType ="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    #contentType = "application/octet-stream",
    #contentDisposition = "attachment"
  )
  
  # 14-7-2024
  
  tryCatch({
    matched_kafalat <- eventReactive(input$searchButton, {
      matching_kafalat(input, kafalat, input$select1, drop = FALSE)()
    })
    
    output$kafdata <- renderDT({
      matched_kafalat()
    })
  },
  error = function(e) {
    print(paste("Error:", conditionMessage(e)))
  })
  
  tryCatch({
    matched_nenava <- eventReactive(input$searchButton, {
      matching_nenava(input, nenava, input$select2, drop = FALSE)()
    })
    
    output$nendata <- renderDT({
      matched_nenava()
    })
  },
  error = function(e) {
    print(paste("Error:", conditionMessage(e)))
  })
  
  tryCatch({
    matched_isisroster <- eventReactive(input$searchButton, {
      matching_isisroster(input, isisroster, input$select3, drop = FALSE)()
    })
    
    output$rosdata <- renderDT({
      matched_isisroster()
    })
  },
  error = function(e) {
    print(paste("Error:", conditionMessage(e)))
  })
  
  # End # 14-7-2024
  
  tryCatch({
    matched_baghdasc <- eventReactive(input$searchButton, {
      matching_baghdadsc(input, baghdad, input$select4, drop = FALSE)()
    })
    
    output$baghdadsupplycarddata <- renderDT({
      matched_baghdasc()
    })
  },
  error = function(e) {
    print(paste("Error:", conditionMessage(e)))
  })
  
  tryCatch({
    matched_salahrc <- eventReactive(input$searchButton, {
      matching_salahrc(input, salah, input$select4, drop = FALSE)()
    })
    
    output$salahrcdata <- renderDT({
      matched_salahrc()
    })
  },
  error = function(e) {
    print(paste("Error:", conditionMessage(e)))
  })
  # 
  
  # 14-7-2024
  
  #Observe the select4 input and update select5 accordingly
  observeEvent(input$select4, {
    selected_dataset <- input$select4
    
    columns <- if (selected_dataset == "baghdad") {
      names(baghdad)
    } else if (selected_dataset == "salah") {
      names(salah)
    } else {
      character(0)
    }
    
    updateSelectInput(session, "select5", choices = columns)
  })
  
  # Reactive function to filter Baghdad data
  matched_baghdasc <- reactive({
    req(input$select4 == "baghdad", input$select5, input$searchText)
    selectedColumn <- input$select5
    searchText <- input$searchText
    filteredData <- baghdad[startsWith(baghdad[[selectedColumn]], searchText), ]
    filteredData
  })
  
  # Reactive function to filter Salah data
  matched_salahrc <- reactive({
    req(input$select4 == "salah", input$select5, input$searchText)
    selectedColumn <- input$select5
    searchText <- input$searchText
    filteredData <- salah[startsWith(salah[[selectedColumn]], searchText), ]
    filteredData
  })
  
  # Conditional rendering based on selected dataset
  output$baghdadsupplycarddata <- renderDT({
    if (input$select4 == "baghdad") {
      matched_baghdasc()
    }
  })
  
  output$salahrcdata <- renderDT({
    if (input$select4 == "salah") {
      matched_salahrc()
    }
  })
  # 
  
  
  #end # 14-7-2024
  
  
  #output$rosdata <- renderDT({d3()})
  
  # Live Map / Slider Input Selector Module
  source("module/sliderinput.R", local = TRUE)
  
  # Case Details
  #source("module/casedetailes.R", local = TRUE)
  
  
  
  
  
  
  
  
  # using for loop
  
  #Define an array of input IDs.
  
  # input_ids <- c("cleana", "mapa", "asiaplots", "arbtn", "axlsx", "cleank", "mapk", "korekplot",
  #                "krbtn", "kxlsx", "cleanz", "mapz", "zainplot","zrbtn", "zxlsx") #"DateRange", "ctype", "colname"
  # 
  # # Loop through the input IDs
  # for (id in input_ids) {
  #   observe({
  #     if (!is.null(input[[id]])) {
  #       triggered_id <- id
  #       mylogdf_data <- create_and_write_log(triggered_id)
  #       # Use the mylogdf_data as needed...
  #     }
  #   })
  # }
  
  ##
  
  # Single Log 
  
  # 
  
  # observe({
  #   if (!is.null(input$cleana)) {
  #     triggered_id <- "cleana"
  #     mylogdf_data <- create_and_write_log(triggered_id)
  #     # Use the mylogdf_data as needed...
  #   }
  # })
  
  # 
  #  observe({
  #    if (!is.null(input$mapa)) {
  #      triggered_id <- "mapa"
  #      mylogdf_data <- create_and_write_log(triggered_id)
  #      # Use the mylogdf_data as needed...
  #    }
  #  })
  # # 
  #  observe({
  #    if (!is.null(input$asiaplots)) {
  #      triggered_id <- "asiaplots"
  #      mylogdf_data <- create_and_write_log(triggered_id)
  #      # Use the mylogdf_data as needed...
  #    }
  #  })
  
  #source("module/logfunction.R", local = TRUE)
  
  
}
