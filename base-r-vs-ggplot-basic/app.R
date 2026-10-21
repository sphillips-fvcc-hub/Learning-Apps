
library(shiny)
library(ggplot2)

# ----------------------------------------
# Base R vs. ggplot2 Visualization Explorer
# ----------------------------------------

defaults <- list(
  scatter = c("Weight vs. MPG",
              "Weight (1000 lbs)",
              "Miles per gallon"),
  hist = c("Distribution of MPG",
           "Miles per gallon",
           "Frequency"),
  box = c("MPG by Transmission",
          "Transmission",
          "Miles per gallon"),
  bar = c("Cars by Cylinder Count",
          "Cylinders",
          "Number of cars")
)

colors <- c(
  "Steel blue" = "steelblue",
  "Tomato" = "tomato",
  "Sea green" = "seagreen",
  "Purple" = "mediumpurple"
)

cars <- mtcars

cars$transmission <- factor(
  cars$am,
  levels = c(0, 1),
  labels = c("Automatic", "Manual")
)

cars$cylinders <- factor(
  cars$cyl,
  levels = c(4, 6, 8)
)

# Safely format text as R string literals
r_string <- function(x) {
  encodeString(x, quote = '"')
}

# ----------------------------------------
# Generate Base R code
# ----------------------------------------

make_base_code <- function(kind, title, xl, yl,
                           color, bins, size, grid_on) {

  title <- r_string(title)
  xl <- r_string(xl)
  yl <- r_string(yl)
  color <- r_string(color)

  lines <- switch(
    kind,

    scatter = c(
      "plot(mtcars$wt, mtcars$mpg,",
      paste0("     main = ", title, ","),
      paste0("     xlab = ", xl, ","),
      paste0("     ylab = ", yl, ","),
      paste0("     col = ", color, ","),
      "     pch = 19,",
      paste0("     cex = ", size / 2.5, ")")
    ),

    hist = c(
      "hist(mtcars$mpg,",
      paste0("     breaks = ", bins, ","),
      paste0("     main = ", title, ","),
      paste0("     xlab = ", xl, ","),
      paste0("     ylab = ", yl, ","),
      paste0("     col = ", color, ","),
      '     border = "white")'
    ),

    box = c(
      "mtcars$transmission <- factor(",
      "  mtcars$am, levels = c(0, 1),",
      '  labels = c("Automatic", "Manual"))',
      "",
      "boxplot(mpg ~ transmission,",
      "        data = mtcars,",
      paste0("        main = ", title, ","),
      paste0("        xlab = ", xl, ","),
      paste0("        ylab = ", yl, ","),
      paste0("        col = ", color, ")")
    ),

    bar = c(
      "counts <- table(factor(",
      "  mtcars$cyl, levels = c(4, 6, 8)))",
      "",
      "barplot(counts,",
      paste0("        main = ", title, ","),
      paste0("        xlab = ", xl, ","),
      paste0("        ylab = ", yl, ","),
      paste0("        col = ", color, ")")
    )
  )

  if (grid_on) {
    lines <- c(lines, "grid()")
  }

  paste(lines, collapse = "\n")
}

# ----------------------------------------
# Generate ggplot2 code
# ----------------------------------------

make_gg_code <- function(kind, title, xl, yl,
                         color, bins, size,
                         minimal, grid_on) {

  title <- r_string(title)
  xl <- r_string(xl)
  yl <- r_string(yl)
  color <- r_string(color)

  setup <- switch(
    kind,

    scatter =
      "ggplot(mtcars, aes(x = wt, y = mpg)) +",

    hist =
      "ggplot(mtcars, aes(x = mpg)) +",

    box = paste(
      "mtcars$transmission <- factor(",
      "  mtcars$am, levels = c(0, 1),",
      '  labels = c("Automatic", "Manual"))',
      "",
      "ggplot(mtcars,",
      "       aes(x = transmission, y = mpg)) +",
      sep = "\n"
    ),

    bar = paste(
      "mtcars$cylinders <- factor(",
      "  mtcars$cyl, levels = c(4, 6, 8))",
      "",
      "ggplot(mtcars, aes(x = cylinders)) +",
      sep = "\n"
    )
  )

  geom <- switch(
    kind,

    scatter = paste0(
      "  geom_point(color = ", color,
      ", size = ", size, ") +"
    ),

    hist = paste0(
      "  geom_histogram(bins = ", bins,
      ", fill = ", color,
      ', color = "white") +'
    ),

    box = paste0(
      "  geom_boxplot(fill = ", color, ") +"
    ),

    bar = paste0(
      "  geom_bar(fill = ", color, ") +"
    )
  )

  theme_code <- if (minimal) {
    "  theme_minimal()"
  } else {
    "  theme_classic()"
  }

  if (!grid_on && minimal) {
    theme_code <- paste0(
      theme_code,
      " +\n  theme(panel.grid = element_blank())"
    )
  }

  if (grid_on && !minimal) {
    theme_code <- paste0(
      theme_code,
      " +\n  theme(panel.grid.major = ",
      'element_line(color = "grey85"))'
    )
  }

  paste(
    setup,
    geom,
    paste0(
      "  labs(title = ", title, ",\n",
      "       x = ", xl, ",\n",
      "       y = ", yl, ") +"
    ),
    theme_code,
    sep = "\n"
  )
}

# ----------------------------------------
# User interface
# ----------------------------------------

ui <- fluidPage(

  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),

    tags$style(HTML("
      body {
        background: #f7f9fc;
        color: #182438;
      }

      .container-fluid {
        max-width: 1260px;
        margin: auto;
      }

      .panel-box {
        background: white;
        border: 1px solid #dfe5ee;
        border-radius: 12px;
        padding: 16px;
        margin-bottom: 16px;
      }

      .plot-card {
        background: white;
        border: 1px solid #dfe5ee;
        border-radius: 12px;
        padding: 10px;
        margin-bottom: 12px;
      }

      pre.solution {
        background: #f2f5f9;
        border: 1px solid #dfe5ee;
        border-radius: 8px;
        padding: 14px;
        overflow-x: auto;
        white-space: pre;
        font-size: 12px;
        line-height: 1.5;
      }

      .hidden-note {
        text-align: center;
        padding: 24px 12px;
        background: #f2f5f9;
        border: 1px dashed #b7c4d5;
        border-radius: 10px;
      }

      .muted {
        color: #526477;
      }
    "))
  ),

  h2("Base R vs. ggplot2 Visualization Explorer"),

  p(
    class = "muted",
    "Choose graph settings, reproduce the plots ",
    "in RStudio or Positron, and reveal the code ",
    "when you are ready."
  ),

  fluidRow(

    # LEFT COLUMN: Controls
    column(
      4,

      div(
        class = "panel-box",

        h4("Graph Settings"),

        selectInput(
          "kind",
          "Graph type",
          choices = c(
            "Scatterplot" = "scatter",
            "Histogram" = "hist",
            "Boxplot" = "box",
            "Bar chart" = "bar"
          )
        ),

        textInput(
          "title", "Graph title",
          value = defaults$scatter[1]
        ),

        textInput(
          "xl", "X-axis label",
          value = defaults$scatter[2]
        ),

        textInput(
          "yl", "Y-axis label",
          value = defaults$scatter[3]
        ),

        selectInput(
          "color",
          "Plot color",
          choices = colors,
          selected = "steelblue"
        ),

        conditionalPanel(
          "input.kind == 'hist'",

          sliderInput(
            "bins",
            "Number of histogram bins",
            min = 4,
            max = 16,
            value = 8,
            step = 1
          )
        ),

        conditionalPanel(
          "input.kind == 'scatter'",

          sliderInput(
            "size",
            "Point size (ggplot2)",
            min = 1,
            max = 5,
            value = 2.5,
            step = 0.5
          )
        ),

        checkboxInput(
          "minimal",
          "Minimal ggplot2 theme",
          value = TRUE
        ),

        checkboxInput(
          "grid_on",
          "Show gridlines",
          value = TRUE
        )
      ),

      div(
        class = "panel-box",

        h4("Practice Mode"),

        p(
          class = "muted",
          "The code starts hidden. Work in RStudio, ",
          "Positron, or your notebook. ",
          "Nothing needs to be typed here."
        ),

        uiOutput("practice_controls"),

        br(),

        textOutput("practice_status")
      )
    ),

    # RIGHT COLUMN: Plots and code
    column(
      8,

      div(
        class = "panel-box",

        h3("Graph Previews"),

        fluidRow(

          column(
            6,
            div(
              class = "plot-card",
              h4("Base R"),
              plotOutput(
                "base_plot",
                height = "300px"
              )
            )
          ),

          column(
            6,
            div(
              class = "plot-card",
              h4("ggplot2"),
              plotOutput(
                "gg_plot",
                height = "300px"
              )
            )
          )
        ),

        p(
          class = "muted",
          "These graphs are generated using R. ",
          "Histogram bin boundaries may differ ",
          "between Base R and ggplot2."
        )
      ),

      div(
        class = "panel-box",

        h3("Example Code"),

        uiOutput("code_area")
      )
    )
  )
)

# ----------------------------------------
# Server
# ----------------------------------------

server <- function(input, output, session) {

  # Code is HIDDEN when the app starts.
  code_hidden <- reactiveVal(TRUE)

  # Update labels when the graph type changes.
  observeEvent(input$kind, {

    labels <- defaults[[input$kind]]

    updateTextInput(
      session, "title", value = labels[1]
    )

    updateTextInput(
      session, "xl", value = labels[2]
    )

    updateTextInput(
      session, "yl", value = labels[3]
    )

  }, ignoreInit = TRUE)

  # Hide code when students change settings.
  observeEvent(
    list(
      input$kind,
      input$title,
      input$xl,
      input$yl,
      input$color,
      input$bins,
      input$size,
      input$minimal,
      input$grid_on
    ),
    {
      code_hidden(TRUE)
    },
    ignoreInit = TRUE
  )

  # Reveal / hide buttons
  observeEvent(input$reveal, {
    code_hidden(FALSE)
  })

  observeEvent(input$hide, {
    code_hidden(TRUE)
  })

  output$practice_controls <- renderUI({

    if (code_hidden()) {

      actionButton(
        "reveal",
        "Reveal Code",
        icon = icon("eye"),
        class = "btn-primary"
      )

    } else {

      actionButton(
        "hide",
        "Hide Code / Try Again",
        icon = icon("eye-slash")
      )
    }
  })

  output$practice_status <- renderText({

    if (code_hidden()) {
      "Code hidden — your turn!"
    } else {
      "Solutions visible — compare with your code."
    }
  })

  # --------------------------------------
  # Base R plot
  # --------------------------------------

  output$base_plot <- renderPlot({

    req(input$kind)

    kind <- input$kind
    color <- input$color

    if (kind == "scatter") {

      plot(
        cars$wt, cars$mpg,
        main = input$title,
        xlab = input$xl,
        ylab = input$yl,
        col = color,
        pch = 19,
        cex = input$size / 2.5
      )

    } else if (kind == "hist") {

      hist(
        cars$mpg,
        breaks = input$bins,
        main = input$title,
        xlab = input$xl,
        ylab = input$yl,
        col = color,
        border = "white"
      )

    } else if (kind == "box") {

      boxplot(
        mpg ~ transmission,
        data = cars,
        main = input$title,
        xlab = input$xl,
        ylab = input$yl,
        col = color
      )

    } else if (kind == "bar") {

      barplot(
        table(cars$cylinders),
        main = input$title,
        xlab = input$xl,
        ylab = input$yl,
        col = color
      )
    }

    if (isTRUE(input$grid_on)) {
      grid()
    }

  }, res = 96)

  # --------------------------------------
  # ggplot2 plot
  # --------------------------------------

  output$gg_plot <- renderPlot({

    req(input$kind)

    p <- switch(
      input$kind,

      scatter = ggplot(
        cars, aes(x = wt, y = mpg)
      ) +
        geom_point(
          color = input$color,
          size = input$size
        ),

      hist = ggplot(
        cars, aes(x = mpg)
      ) +
        geom_histogram(
          bins = input$bins,
          fill = input$color,
          color = "white"
        ),

      box = ggplot(
        cars, aes(x = transmission, y = mpg)
      ) +
        geom_boxplot(
          fill = input$color
        ),

      bar = ggplot(
        cars, aes(x = cylinders)
      ) +
        geom_bar(
          fill = input$color
        )
    )

    p <- p + labs(
      title = input$title,
      x = input$xl,
      y = input$yl
    )

    if (isTRUE(input$minimal)) {
      p <- p + theme_minimal()
    } else {
      p <- p + theme_classic()
    }

    if (!isTRUE(input$grid_on) &&
        isTRUE(input$minimal)) {

      p <- p + theme(
        panel.grid = element_blank()
      )
    }

    if (isTRUE(input$grid_on) &&
        !isTRUE(input$minimal)) {

      p <- p + theme(
        panel.grid.major =
          element_line(color = "grey85")
      )
    }

    print(p)

  }, res = 96)

  # --------------------------------------
  # Show or hide the code examples
  # --------------------------------------

  output$code_area <- renderUI({

    if (code_hidden()) {

      return(
        div(
          class = "hidden-note",

          h4("Code Hidden — Your Turn!"),

          p(
            "Use the graphs and selected settings ",
            "to write the code in RStudio, ",
            "Positron, or your notebook."
          ),

          p(
            class = "muted",
            "Click Reveal Code when ready."
          )
        )
      )
    }

    tagList(

      h4("Base R"),

      tags$pre(
        class = "solution",

        make_base_code(
          input$kind,
          input$title,
          input$xl,
          input$yl,
          input$color,
          input$bins,
          input$size,
          input$grid_on
        )
      ),

      h4("ggplot2"),

      tags$pre(
        class = "solution",

        make_gg_code(
          input$kind,
          input$title,
          input$xl,
          input$yl,
          input$color,
          input$bins,
          input$size,
          input$minimal,
          input$grid_on
        )
      )
    )
  })
}

# ----------------------------------------
# Launch the application
# ----------------------------------------

shinyApp(ui, server)
