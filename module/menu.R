# # menu 1
sidebarMenu(
  id = "mymenu",
  div(fileInput(
    "xlsxs",
    'Choose file',
    multiple = TRUE,
    accept = c(".xlsx")
  )),
  menuItem(
    "Control Panel",
    tabName = "cpanel",
    selected = TRUE,
    startExpanded = TRUE,
    icon = icon("dashboard", lib = "font-awesome"),
    menuItem(
      "Control Panel",
      tabName = "contpan",
      selected = TRUE,
      startExpanded = TRUE,
      icon = icon("keyboard", lib = "font-awesome")
    ),
    div(
      tags$strong("Data Matching"),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "datamatching",
          "Search",
          icon("search"),
          style = "color: #fff;  background-color: #E82228; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton("find", "find", icon("info"), style = "color: #fff;  background-color: #E82228; border-color: #2e6da4", class = "AsiabtnClass")
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "databind",
          "Bind",
          icon("bar-chart"),
          style = "color: #fff; background-color: #E82228; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      )
      # tags$br(),
      # div(
      #   style = "display:inline-block;width:32%;text-align: center;",
      #   downloadButton("arbtn", "WORD", style = "color: #000104; background-color: white; border-color: Black;")
      # ),
      # div(style = "display:inline-block;width:32%;text-align: center;"),
      # div(
      #   style = "display:inline-block;width:32%;text-align: center;",
      #   downloadButton("axlsx", "XLSX", style = "color: #000104; background-color: white; border-color: Black;")
      # )
    ),
    div(
      tags$strong("Asia CDR"),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "cleana",
          "Process",
          icon("cog"),
          style = "color: #fff;  background-color: #E82228; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton("mapa", "Map", icon("map"), style = "color: #fff;  background-color: #E82228; border-color: #2e6da4", class = "AsiabtnClass")
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "asiaplots",
          "Charts",
          icon("bar-chart"),
          style = "color: #fff; background-color: #E82228; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        downloadButton("arbtn", "WORD", style = "color: #000104; background-color: white; border-color: Black;")
      ),
      div(style = "display:inline-block;width:32%;text-align: center;"),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        downloadButton("axlsx", "XLSX", style = "color: #000104; background-color: white; border-color: Black;")
      )
    ),
    div(
      tags$strong("Korek CDR"),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "cleank",
          "Process",
          icon("cog"),
          style = "color: #fff;  background-color: #0971BA; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton("mapk", "Map", icon("map"), style = "color: #fff;  background-color: #0971BA; border-color: #2e6da4", class = "AsiabtnClass")
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "korekplot",
          "Charts",
          icon("bar-chart"),
          style = "color: #fff; background-color: #0971BA; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        downloadButton("krbtn", "WORD", style = "color: #000104; background-color: white; border-color: Black;")
      ),
      div(style = "display:inline-block;width:32%;text-align: center;"),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        downloadButton("kxlsx", "XLSX", style = "color: #000104; background-color: white; border-color: Black;")
      )
    ),
    div(
      tags$strong("Zain CDR"),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "cleanz",
          "Process",
          icon("cog"),
          style = "color: #fff;  background-color: #9881B9; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton("mapz", "Map", icon("map"), style = "color: #fff;  background-color: #9881B9; border-color: #2e6da4", class = "AsiabtnClass")
      ),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        actionButton(
          "zainplot",
          "Charts",
          icon("bar-chart"),
          style = "color: #fff; background-color: #9881B9; border-color: #2e6da4",
          class = "AsiabtnClass"
        )
      ),
      tags$br(),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        downloadButton("zrbtn", "WORD", style = "color: #000104; background-color: white; border-color: Black;")
      ),
      div(style = "display:inline-block;width:32%;text-align: center;"),
      div(
        style = "display:inline-block;width:32%;text-align: center;",
        downloadButton("zxlsx", "XLSX", style = "color: #000104; background-color: white; border-color: Black;")
      )
    ),
    div(
      dateRangeInput(
        inputId = "DateRange",
        label = "Filter using Date Range",
        start = NULL,
        end = NULL,
        min = NULL,
        max = NULL
      )
    ),
    div(selectInput("ctype", "Filter Using Call Type", "")),
    div(selectInput("colname", "Filter Using Column Name", "")),
    uiOutput("slider1")
  ),
  menuItem(
    "Search Data",
    tabName = "isisdata1",
    icon = icon("search"),
    menuItem(
      "Search Data",
      tabName = "alldatasearch",
      selected = FALSE,
      icon = icon("search", lib = "font-awesome")
    ),
    
    
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton",
      label = "Search In Datasets",
      icon = icon("search")
    ),
    selectInput(
      "select1",
      "Select Table in your Database to display",
      # Database Name or Dataframe Name
      dbListTables(con_mysql),
      #character(0),
      multiple = FALSE
    ),
    selectInput(
      "select2",
      "Select columns in Nenava to display",
      dbListTables(con_mssql),
      #character(0),
      multiple = FALSE
    ),
    selectInput(
      "select3",
      "Select columns in ISIS Roster to display",
      character(0),
      multiple = FALSE
    ),
    # selectInput(
    #   "select4",
    #   "Select RC Data to display",
    #   c("baghdad", "salah"),
    #   multiple = FALSE
    # ),
    selectInput(
      "select5",
      "Select columns in Baghdad SC Data to display",
      character(0),
      multiple = FALSE
    )
    
    
  )
  # menuItem(
  #   "Cases Management",
  #   tabName = "mycasestabset",
  #   icon = icon("bars-progress"),
  #   startExpanded = TRUE,
  #   menuSubItem('Create Case', tabName = 'casemgmt', icon = icon("plus")),
  #   menuSubItem('Update Case', tabName = 'updatecase', icon = icon("info")),
  #   menuSubItem('Search Case', tabName = 'searchcase', icon = icon("search"))
  # )
)
