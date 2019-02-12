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
teaching_mods <- c("teaching_module.md", "q1a.md")

dataset <- elephantMorphometricsAndTuskSize %>%
  mutate(Sex = as.factor(Sex),
         `Years of sample collection` = as.factor(`Years of sample collection`),
         `Elephant ID` = as.factor(`Elephant ID`))
attr(dataset, "df_name") <- "elephantMorphometricsAndTuskSize"

dataset_all <- colnames(dataset)
dataset_num <- dataset_all[sapply(dataset, is.numeric)]
dataset_cat <- dataset_all[-which(dataset_all %in% dataset_num)] # setdiff?

resourcePath <- system.file("www", package = "tusklessness")

ui <- dashboardPage(
  dashboardHeader(title = "Tusklessness"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Module", tabName = "module", icon = icon("chalkboard-teacher")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Explore", tabName = "explore", icon = icon("chart-area")),
      menuItem("Analyze",
               tabName = "analyze",
               icon = icon("calculator"),
               menuSubItem("Statistical Summaries",
                           tabName = "summaries"),
               menuSubItem("Hypothesis Tests",
                           tabName = "tests")
      )
    ),
    hr(),
    inputPanel(
    conditionalPanel(condition = "input.sidebar == 'module'",
                     downloadButton('download', 'Download')
    ),
    conditionalPanel(condition = "input.sidebar == 'summaries'",
                     selectizeInput(
                       "variable",
                       "Compute statistics for:",
                       choices = dataset_num,
                       multiple = TRUE,
                       options = list(
                         'plugins' = list('remove_button'),
                         'create' = TRUE,
                         'persist' = FALSE
                       )
                     ),
                     selectizeInput(
                       "groupby",
                       "Group by:",
                       choices = dataset_cat,
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
    conditionalPanel(condition = "input.sidebar == 'tests'",
                     selectInput(
                       "tests",
                       "Please select a test:",
                       choices = c("One-Sample T-Test" = "onesample",
                                   "Two-Sample T-Test" = "twosample",
                                   "ANOVA" = "anova",
                                   "Linear Regression" = "regression")
                     ),
                     conditionalPanel(condition = "input.tests == 'onesample'",
                                      selectInput(
                                        "onesample_var",
                                        "Variable:",
                                        choices = dataset_num
                                        ),
                                      numericInput(
                                        "onesample_null",
                                        "Null Value:",
                                        value = 0
                                        ),
                                      selectInput(
                                        "onesample_side",
                                        "One or Two Sided:",
                                        choices = c("Two sided" = "two.sided",
                                                    "One sided: Less" = "less",
                                                    "One sided: Greater" = "greater")
                                      ),
                                      sliderInput(
                                        "onesample_alpha",
                                        "Significance Level:",
                                        value = 0.05,
                                        min = 0, max = 1, step = 0.01
                                      )
                     )
    )
    )
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
              plotOutput("myplot", width = "50%"),
              actionButton("getTable", "Grab Table"),
              DTOutput('mytable')
      ),
      tabItem(tabName = "data",
              DTOutput('table')
      ),
      tabItem(tabName = "explore",
              serenityVizUI(id = "explore", dataset = dataset, showcode = FALSE, height="700px")
      ),
      tabItem(tabName = "summaries",
              DTOutput('statsummary')
      ),
      tabItem(tabName = "tests",
              fillRow(
                plotOutput("tests_plot"),
                verbatimTextOutput("tests_results")
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

  # User plots ----
  plots <- reactiveValues(myplot_text = NULL)

  observeEvent(input$getPlot, {
    plots$myplot_text <- explore_plot()
  }, ignoreInit = TRUE)

  output$myplot <- renderPlot({
    req(plots$myplot_text)
    eval(parse(text=plots$myplot_text))
  })

  # User tables ----
  tables <- reactiveValues(mytab = NULL)

  observeEvent(input$getTable, {
    tables$mytab <- DTstatsummary()
  }, ignoreInit = TRUE)

  output$mytable <- DT::renderDataTable({
    req(tables$mytab)
    tables$mytab
  })

  # Main Data Table ----
  output$table <- renderDT(
    dataset,
    filter = "top",
    rownames = FALSE,
    style = "bootstrap",
    selection = "none",
    options = list(server = FALSE)
    )

  # Statistical Tests ----
  onesample <- reactive({
    t.test(dataset[[input$onesample_var]],
           alternative = input$onesample_side,
           mu = input$onesample_null,
           conf.level = 1-input$onesample_alpha)
  })

  output$tests_plot <- renderPlot({
    if (input$tests == "onesample") {
      dataset %>%
        ggplot(aes(x = 1, y = !!sym(input$onesample_var))) +
        geom_point(position = position_jitter(width = 0.1, seed = 42)) +
        scale_x_continuous(limits = c(0, 2)) +
        geom_hline(yintercept = input$onesample_null,
                   linetype = 2,
                   color = "red") +
        geom_point(aes(x = 1.25, y = onesample()$estimate)) +
        geom_errorbar(aes(x = 1.25,
                          ymin = onesample()$conf.int[1],
                          ymax = onesample()$conf.int[2]),
                      width = 0.05)
    }
  })

  output$tests_results <- renderPrint({
    onesample()
  })

  # Summary statistics ----
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

  DTstatsummary <- reactive({
    # In case you want to control for # decimal digits in future:
    # https://groups.google.com/forum/#!topic/shiny-discuss/2jlYOYFp2-A

    # See radiant.data dtab.explore
    cn_all <- colnames(statsummary())
    cn_num <- cn_all[sapply(statsummary(), is.numeric)]
    cn_cat <- cn_all[-which(cn_all %in% cn_num)]

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
    ) %>% DT::formatStyle(cn_cat,
                          color = "white",
                          backgroundColor = "grey")
  })

  output$statsummary <- DT::renderDataTable({
    DTstatsummary()
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
