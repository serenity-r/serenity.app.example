#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(serenity.viz)

# Define UI for application that draws a histogram
library(shinydashboard)
library(ggplot2)
library(magrittr)
library(dplyr)
library(DT)
library(tusklessness)
library(readr)

# HTML(markdown::markdownToHTML(knitr::knit(system.file("vignettes/teaching_module.Rmd", package = "tusklessness"), quiet = TRUE)))
teaching_mods <- c("teaching_module.md", "q1a.md", "loremipsum.md")

dataset <- elephantMorphometricsAndTuskSize %>%
  mutate(Sex = as.factor(Sex),
         `Years of sample collection` = as.factor(`Years of sample collection`),
         `Elephant ID` = as.factor(`Elephant ID`))
attr(dataset, "df_name") <- "elephantMorphometricsAndTuskSize"
resourcePath <- system.file("www", package = "tusklessness")

ui <- dashboardPage(
  dashboardHeader(title = "Tusklessness"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Module", tabName = "module", icon = icon("chalkboard-teacher")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Explore", tabName = "explore", icon = icon("chart-area")),
      menuItem("Analyze", tabName = "analyze", icon = icon("calculator"))
    ),
    downloadButton('download', 'Download')
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "module",
              includeMarkdown(system.file("www/teaching_module.md", package = "tusklessness")),
              includeMarkdown(system.file("www/q1a.md", package = "tusklessness")),
              includeMarkdown(system.file("www/loremipsum.md", package = "tusklessness")),
              textAreaInput("q1a", NULL, width="600px", rows = 6, placeholder = "Enter answer here..."),
              actionButton("getPlot", "Grab Plot"),
              plotOutput("myplot", width = "50%")
      ),

      # Second tab content
      tabItem(tabName = "data",
              DTOutput('table')
      ),

      # Third tab content
      tabItem(tabName = "explore",
              serenityVizUI(id = "explore", dataset = dataset, height="700px")
      ),

      # Third tab content
      tabItem(tabName = "analyze",
              tabBox(
                id = "analysis",
                width = 12,
                tabPanel("Summary",
                         fillPage(
                           fillRow(
                             flex = c(2, 6),
                             wellPanel(
                               height = "100%",
                               selectInput(
                                 "variable",
                                 "Compute statistics for:",
                                 choices = names(dataset),
                                 multiple = TRUE
                               ),
                               selectizeInput(
                                 "groupby",
                                 "Group by:",
                                 choices = names(dataset),
                                 multiple = TRUE,
                                 options = list(
                                   'plugins' = list('remove_button',
                                                    'drag_drop'),
                                   'create' = TRUE,
                                   'persist' = FALSE
                                 )
                               ),
                               selectizeInput(
                                 "statistics",
                                 "Statistics:",
                                 choices = c("Min" = "min",
                                             "1st Qu." = "firstquartile",
                                             "Median" = "median",
                                             "Mean" = "mean",
                                             "3rd Qu." = "thirdquartile",
                                             "Max" = "max"),
                                 multiple = TRUE,
                                 selected = c("min", "firstquartile", "median", "mean", "thirdquartile", "max"),
                                 options = list(
                                   'plugins' = list('remove_button',
                                                    'drag_drop'),
                                   'create' = TRUE,
                                   'persist' = FALSE
                                 )
                               )
                             ),
                             DTOutput('statsummary')
                           ))
                         ),
                tabPanel("Tests")
              )
      )
    )
  ),
  tags$head(includeCSS(file.path(resourcePath, "app.css")))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  explore_plot <- callModule(module = serenityVizServer,
                             id = "explore",
                             dataset = dataset)

  plots <- reactiveValues(myplot_text = NULL)

  observeEvent(input$getPlot, {
    plots$myplot_text <- explore_plot()
  }, ignoreInit = TRUE)

  output$myplot <- renderPlot({
    req(plots$myplot_text)
    eval(parse(text=plots$myplot_text))
  })

  output$table <- renderDT(
    dataset,
    filter = "top",
    rownames = FALSE,
    style = "bootstrap",
    selection = "none",
    options = list(server = FALSE)
    )

  statsummary <- reactive({
    req(input$variable, input$groupby)

    # tmp <- elephant %>% group_by(!!sym(groupby)) %>% summarize_at(vars(variable), list(Q1 = ~quantile(., probs=0.25), Q3 = ~quantile(., probs=0.75)), na.rm=TRUE)
    funmap <- list(min = min,
                   firstquartile = ~quantile(., 0.25),
                   median = median,
                   mean = mean,
                   thirdquartile = ~quantile(., 0.75),
                   max = max)
    funmap <- funmap[input$statistics]

    # Gotta be a cleaner way then using an if statement here...
    statsum <- dataset %>%
      dplyr::group_by(!!!syms(input$groupby)) %>%
      dplyr::summarize_at(vars(input$variable), funmap, na.rm=TRUE)

    if (length(input$variable) > 1) {
      statsum <- statsum %>%
        tidyr::gather_(key = "var_fun", value = "value", setdiff(names(.), input$groupby)) %>%
        tidyr::separate(var_fun, into=c("variable", "fun"), sep = "_") %>%
        tidyr::spread(fun, value) %>%
        dplyr::select(!!!syms(input$groupby), "variable", input$statistics) # Needed to keep ordering
    }

    statsum
  })

  output$statsummary <- DT::renderDataTable({
    # In case you want to control for # decimal digits in future:
    # https://groups.google.com/forum/#!topic/shiny-discuss/2jlYOYFp2-A
    DT::datatable(
      statsummary(),
      filter = "top",
      rownames = FALSE,
      style = "bootstrap",
      selection = "none",
      options = list(
        dom = "t",
        searchCols = NULL
      )
    )
  })

  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$download <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      "myreport.html"
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      tmp <- tempfile(fileext = ".Rmd")
      content <- paste(purrr::map(teaching_mods, ~ read_file(system.file(paste0("www/", .), package = "tusklessness"))), collapse = "\n")
      content <- paste(c(content, input$q1a), collapse = "\n")

      # Get plot
      if (!is.null(plots$myplot_text)) {
        plotrmd <- paste(c("\n```{r, echo=FALSE, warning=FALSE}", paste(plots$myplot_text, collapse = "\n"), "```\n"), collapse = "\n")
        content <- paste(c(content, plotrmd), collapse = "\n")
      }
      cat(content, file = tmp)
      rmarkdown::render(tmp, output_file = file)
    }
  )
}

# Run the application
runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
