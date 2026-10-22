
library(shiny)
library(ggplot2)

# ============================================
# ADVANCED BASE R VS. GGPLOT2 EXPLORER
# ============================================

graph_types <- c(
  "Scatterplot" = "scatter",
  "Histogram" = "hist",
  "Boxplot" = "box",
  "Bar chart" = "bar",
  "Violin plot" = "violin",
  "Side-by-side bar chart" = "grouped",
  "Scatterplot with regression line" = "regression",
  "Density plot" = "density"
)

graph_defaults <- list(
  scatter = c("Weight vs. MPG", "Weight (1000 lbs)", "MPG"),
  hist = c("Distribution of MPG", "MPG", "Frequency"),
  box = c("MPG by Transmission", "Transmission", "MPG"),
  bar = c("Cars by Cylinders", "Cylinders", "Count"),
  violin = c("Petal Length by Species", "Species", "Petal length"),
  grouped = c("Cylinders by Transmission", "Cylinders", "Count"),
  regression = c("Weight vs. MPG", "Weight (1000 lbs)", "MPG"),
  density = c("Density of MPG", "MPG", "Density")
)

color_choices <- c(
  "Ocean blue" = "steelblue",
  "Teal" = "#178A82",
  "Terracotta" = "#C16D53",
  "Violet" = "#8056B4",
  "Forest green" = "#3B8657"
)

theme_choices <- c(
  "Minimal" = "minimal",
  "Classic" = "classic",
  "Gray" = "gray",
  "Black & White" = "bw",
  "Dark" = "dark"
)

measure_choices <- c(
  "Petal length" = "Petal.Length",
  "Petal width" = "Petal.Width",
  "Sepal length" = "Sepal.Length",
  "Sepal width" = "Sepal.Width"
)

# Prepare datasets
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

# ============================================
# USER INTERFACE
# ============================================

ui <- fluidPage(

  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),

    tags$style(HTML("
      body {
        background: #f6f8fc;
        color: #1d2a3c;
      }

      .container-fluid {
        max-width: 1450px;
        margin: auto;
      }

      .panel-box {
        background: white;
        border: 1px solid #dce3ee;
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 14px;
      }

      .control-section {
        border: 1px solid #dce3ee;
        border-radius: 9px;
        margin-bottom: 9px;
        background: white;
      }

      .control-section summary {
        cursor: pointer;
        padding: 13px;
        font-weight: 600;
      }

      .control-content {
        padding: 0 14px 12px;
      }

      .plot-box {
        border: 1px solid #e1e7ef;
        border-radius: 10px;
        padding: 9px;
        margin-bottom: 10px;
      }

      .solution {
        white-space: pre;
        overflow-x: auto;
        background: #f1f5f9;
        border: 1px solid #dce3ee;
        border-radius: 8px;
        padding: 12px;
        font-size: 12px;
        line-height: 1.5;
      }

      .practice-note {
        border: 1px dashed #afc3d9;
        border-radius: 10px;
        background: #f3f7fc;
        text-align: center;
        padding: 25px 12px;
      }

      .muted {
        color: #52667b;
      }
    "))
  ),

  h2("Base R vs. ggplot2: Advanced Visualization Explorer"),

  p(
    class = "muted",
    "Customize the graphs, practice writing R code, ",
    "and reveal the examples when you are ready."
  ),

  fluidRow(

    # ========================================
    # LEFT SIDEBAR
    # ========================================

    column(
      4,

      tags$details(
        class = "control-section",
        open = "open",

        tags$summary("1. Graph & Data"),

        div(
          class = "control-content",

          selectInput(
            "kind",
            "Graph type",
            choices = graph_types
          ),

          conditionalPanel(
            "input.kind == 'violin'",

            selectInput(
              "measure",
              "Iris measurement",
              choices = measure_choices
            ),

            checkboxInput(
              "overlay",
              "Show boxplot inside violin",
              value = TRUE
            )
          ),

          textOutput("dataset_note")
        )
      ),

      tags$details(
        class = "control-section",

        tags$summary("2. Colors & Themes"),

        div(
          class = "control-content",

          selectInput(
            "color",
            "Main color",
            choices = color_choices
          ),

          selectInput(
            "theme",
            "Plot theme",
            choices = theme_choices
          ),

          sliderInput(
            "alpha",
            "Opacity",
            min = 0.2,
            max = 1,
            value = 0.85,
            step = 0.05
          ),

          checkboxInput(
            "grid",
            "Show gridlines",
            value = TRUE
          )
        )
      ),

      tags$details(
        class = "control-section",

        tags$summary("3. Axes & Labels"),

        div(
          class = "control-content",

          textInput(
            "title",
            "Graph title",
            value = graph_defaults$scatter[1]
          ),

          textInput(
            "xl",
            "X-axis title",
            value = graph_defaults$scatter[2]
          ),

          textInput(
            "yl",
            "Y-axis title",
            value = graph_defaults$scatter[3]
          ),

          selectInput(
            "x_angle",
            "X tick-label angle",
            choices = c(
              "0 degrees" = 0,
              "45 degrees" = 45,
              "90 degrees" = 90
            )
          ),

          selectInput(
            "y_angle",
            "Y tick-label angle",
            choices = c(
              "0 degrees" = 0,
              "90 degrees" = 90
            ),
            selected = 0

          )
        )
      ),

      tags$details(
        class = "control-section",

        tags$summary("4. Text & Typography"),

        div(
          class = "control-content",

          sliderInput(
            "title_size",
            "Title size",
            min = 12,
            max = 26,
            value = 18
          ),

          sliderInput(
            "axis_size",
            "Axis title size",
            min = 9,
            max = 20,
            value = 12
          ),

          sliderInput(
            "tick_size",
            "Tick-label size",
            min = 8,
            max = 16,
            value = 10
          )
        )
      ),

      tags$details(
        class = "control-section",

        tags$summary("5. Plot-Specific Options"),

        div(
          class = "control-content",

          conditionalPanel(
            "input.kind == 'hist'",

            sliderInput(
              "bins",
              "Histogram bins",
              min = 4,
              max = 18,
              value = 8,
              step = 1
            )
          ),

          conditionalPanel(
            "input.kind == 'scatter' || input.kind == 'regression'",

            sliderInput(
              "point_size",
              "Point size",
              min = 1,
              max = 5,
              value = 2.5,
              step = 0.5
            )
          ),

          conditionalPanel(
            "input.kind == 'scatter' || input.kind == 'regression' || input.kind == 'hist' || input.kind == 'density'",

            checkboxInput(
              "facet",
              "Facet by transmission",
              value = FALSE
            )
          )
        )
      ),

      div(
        class = "panel-box",

        h3("Practice Mode"),

        p(
          class = "muted",
          "Code is hidden at startup and after ",
          "changing graph settings."
        ),

        uiOutput("practice_button"),

        br(),

        actionButton(
          "reset",
          "Reset to Defaults"
        ),

        br(),
        br(),

        textOutput("practice_status")
      )
    ),

    # ========================================
    # RIGHT SIDE
    # ========================================

    column(
      8,

      div(
        class = "panel-box",

        h3("Graph Previews"),

        fluidRow(

          column(
            6,

            div(
              class = "plot-box",

              h4("Base R"),

              plotOutput(
                "base_plot",
                height = "320px"
              )
            )
          ),

          column(
            6,

            div(
              class = "plot-box",

              h4("ggplot2"),

              plotOutput(
                "gg_plot",
                height = "320px"
              )
            )
          )
        )
      ),

      div(
        class = "panel-box",

        h3("Example Code"),

        uiOutput("code_area")
      ),

      div(
        class = "panel-box",

        h3("What's Being Plotted?"),

        textOutput("mapping_note")
      )
    )
  )
)

# ============================================
# SERVER
# ============================================

server <- function(input, output, session) {

  # Code hidden when the app opens
  code_hidden <- reactiveVal(TRUE)

  # Update titles when graph type changes
  observeEvent(input$kind, {

    d <- graph_defaults[[input$kind]]

    updateTextInput(
      session, "title", value = d[1]
    )

    updateTextInput(
      session, "xl", value = d[2]
    )

    updateTextInput(
      session, "yl", value = d[3]
    )

  }, ignoreInit = TRUE)

  # Hide code when any setting changes
  observeEvent(
    list(
      input$kind,
      input$measure,
      input$overlay,
      input$color,
      input$theme,
      input$alpha,
      input$grid,
      input$title,
      input$xl,
      input$yl,
      input$x_angle,
      input$y_angle,
      input$title_size,
      input$axis_size,
      input$tick_size,
      input$bins,
      input$point_size,
      input$facet
    ),
    {
      code_hidden(TRUE)
    },
    ignoreInit = TRUE
  )

  observeEvent(input$reveal, {
    code_hidden(FALSE)
  })

  observeEvent(input$hide, {
    code_hidden(TRUE)
  })

  # Reset controls
  observeEvent(input$reset, {

    updateSelectInput(
      session, "kind", selected = "scatter"
    )

    updateSelectInput(
      session, "measure", selected = "Petal.Length"
    )

    updateCheckboxInput(
      session, "overlay", value = TRUE
    )

    updateSelectInput(
      session, "color", selected = "steelblue"
    )

    updateSelectInput(
      session, "theme", selected = "minimal"
    )

    updateSliderInput(
      session, "alpha", value = 0.85
    )

    updateCheckboxInput(
      session, "grid", value = TRUE
    )

    updateSelectInput(
      session, "x_angle", selected = "0"
    )

    updateSelectInput(
      session, "y_angle", selected = "0"
    )

    updateSliderInput(
      session, "title_size", value = 18
    )

    updateSliderInput(
      session, "axis_size", value = 12
    )

    updateSliderInput(
      session, "tick_size", value = 10
    )

    updateSliderInput(
      session, "bins", value = 8
    )

    updateSliderInput(
      session, "point_size", value = 2.5
    )

    updateCheckboxInput(
      session, "facet", value = FALSE
    )

    updateTextInput(
      session, "title",
      value = graph_defaults$scatter[1]
    )

    updateTextInput(
      session, "xl",
      value = graph_defaults$scatter[2]
    )

    updateTextInput(
      session, "yl",
      value = graph_defaults$scatter[3]
    )

    code_hidden(TRUE)
  })

  output$practice_button <- renderUI({

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
      "Code visible — compare with your solution."
    }
  })

  # ========================================
  # COMMON SETTINGS
  # ========================================

  settings <- reactive({

    list(
      kind = input$kind,
      title = input$title,
      xl = input$xl,
      yl = input$yl,
      color = input$color,
      theme = input$theme,
      alpha = input$alpha,
      grid = isTRUE(input$grid),
      x_angle = as.numeric(input$x_angle),
      y_angle = as.numeric(input$y_angle),
      title_size = input$title_size,
      axis_size = input$axis_size,
      tick_size = input$tick_size,
      bins = input$bins,
      point_size = input$point_size,
      measure = input$measure,
      overlay = isTRUE(input$overlay),
      facet = isTRUE(input$facet)
    )
  })

  # ========================================
  # BASE R GRAPH
  # ========================================

  draw_base <- function(s) {

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)

    dark <- s$theme == "dark"

    bg <- if (dark) {
      "#1C2836"
    } else if (s$theme == "gray") {
      "#ECECEC"
    } else {
      "white"
    }

    fg <- if (dark) "white" else "black"

    par(
      bg = bg,
      fg = fg,
      col.axis = fg,
      col.lab = fg,
      col.main = fg,
      cex.main = s$title_size / 16,
      cex.lab = s$axis_size / 12,
      cex.axis = s$tick_size / 10,
      las = if (s$y_angle == 90) 0 else 1
    )

    color <- adjustcolor(
      s$color,
      alpha.f = s$alpha
    )

    # Faceting for applicable graph types
    use_facet <- s$facet &&
      s$kind %in% c(
        "scatter", "regression", "hist", "density"
      )

    if (use_facet) {
      par(mfrow = c(1, 2))
    }

    groups <- if (use_facet) {
      list(
        Automatic = subset(cars, am == 0),
        Manual = subset(cars, am == 1)
      )
    } else {
      list(All = cars)
    }

    if (s$kind == "violin") {

      values <- split(
        iris[[s$measure]],
        iris$Species
      )

      densities <- lapply(values, density)

      ylimits <- range(
        unlist(lapply(densities, function(d) d$x))
      )

      plot(
        NA,
        xlim = c(0.5, length(values) + 0.5),
        ylim = ylimits,
        xaxt = "n",
        xlab = s$xl,
        ylab = s$yl,
        main = s$title
      )

      axis(
        1,
        at = seq_along(values),
        labels = names(values)
      )

      if (s$grid) {
        grid()
      }

      for (i in seq_along(densities)) {

        d <- densities[[i]]

        width <- 0.35 * d$y / max(d$y)

        polygon(
          c(i - width, rev(i + width)),
          c(d$x, rev(d$x)),
          col = color,
          border = fg
        )
      }

      if (s$overlay) {

        boxplot(
          values,
          add = TRUE,
          at = seq_along(values),
          boxwex = 0.10,
          xaxt = "n",
          col = "white",
          outline = FALSE
        )
      }

      return(invisible(NULL))
    }

    for (group_name in names(groups)) {

      d <- groups[[group_name]]

      plot_title <- if (use_facet) {
        group_name
      } else {
        s$title
      }

      if (s$kind %in% c("scatter", "regression")) {

        plot(
          d$wt,
          d$mpg,
          xlim = range(cars$wt),
          ylim = range(cars$mpg),
          main = plot_title,
          xlab = s$xl,
          ylab = s$yl,
          pch = 19,
          col = color,
          cex = s$point_size / 2.5
        )

        if (s$grid) grid()

        if (s$kind == "regression") {

          abline(
            lm(mpg ~ wt, data = d),
            col = "firebrick",
            lwd = 2
          )
        }

      } else if (s$kind == "hist") {

        hist(
          d$mpg,
          breaks = s$bins,
          main = plot_title,
          xlab = s$xl,
          ylab = s$yl,
          col = color,
          border = "white"
        )

        if (s$grid) grid()

      } else if (s$kind == "box") {

        boxplot(
          mpg ~ transmission,
          data = d,
          main = plot_title,
          xlab = s$xl,
          ylab = s$yl,
          col = color
        )

        if (s$grid) grid()

      } else if (s$kind == "bar") {

        barplot(
          table(d$cylinders),
          main = plot_title,
          xlab = s$xl,
          ylab = s$yl,
          col = color
        )

        if (s$grid) grid()

      } else if (s$kind == "grouped") {

        counts <- table(
          d$transmission,
          d$cylinders
        )

        barplot(
          counts,
          beside = TRUE,
          col = c(color, "#D98765"),
          legend.text = TRUE,
          main = plot_title,
          xlab = s$xl,
          ylab = s$yl
        )

        if (s$grid) grid()

      } else if (s$kind == "density") {

        plot(
          density(d$mpg),
          main = plot_title,
          xlab = s$xl,
          ylab = s$yl,
          col = s$color,
          lwd = 2
        )

        if (s$grid) grid()
      }
    }
  }

  output$base_plot <- renderPlot({
    draw_base(settings())
  }, res = 96)

  # ========================================
  # GGPLOT2 GRAPH
  # ========================================

  make_ggplot <- function(s) {

    color <- s$color

    if (s$kind == "scatter") {

      p <- ggplot(cars, aes(wt, mpg)) +
        geom_point(
          color = color,
          size = s$point_size,
          alpha = s$alpha
        )

    } else if (s$kind == "regression") {

      p <- ggplot(cars, aes(wt, mpg)) +
        geom_point(
          color = color,
          size = s$point_size,
          alpha = s$alpha
        ) +
        geom_smooth(
          method = "lm",
          se = FALSE,
          color = "firebrick"
        )

    } else if (s$kind == "hist") {

      p <- ggplot(cars, aes(mpg)) +
        geom_histogram(
          bins = s$bins,
          fill = color,
          color = "white",
          alpha = s$alpha
        )

    } else if (s$kind == "box") {

      p <- ggplot(
        cars,
        aes(transmission, mpg)
      ) +
        geom_boxplot(
          fill = color,
          alpha = s$alpha
        )

    } else if (s$kind == "bar") {

      p <- ggplot(
        cars,
        aes(cylinders)
      ) +
        geom_bar(
          fill = color,
          alpha = s$alpha
        )

    } else if (s$kind == "grouped") {

      p <- ggplot(
        cars,
        aes(cylinders, fill = transmission)
      ) +
        geom_bar(
          position = "dodge",
          alpha = s$alpha
        ) +
        scale_fill_manual(
          values = c(color, "#D98765")
        )

    } else if (s$kind == "violin") {

      p <- ggplot(
        iris,
        aes(
          x = Species,
          y = .data[[s$measure]]
        )
      ) +
        geom_violin(
          fill = color,
          alpha = s$alpha
        )

      if (s$overlay) {

        p <- p +
          geom_boxplot(
            width = 0.12,
            fill = "white",
            outlier.shape = NA
          )
      }

    } else if (s$kind == "density") {

      p <- ggplot(cars, aes(mpg)) +
        geom_density(
          fill = color,
          alpha = s$alpha
        )
    }

    p <- p + labs(
      title = s$title,
      x = s$xl,
      y = s$yl
    )

    p <- p + switch(
      s$theme,
      minimal = theme_minimal(),
      classic = theme_classic(),
      gray = theme_gray(),
      bw = theme_bw(),
      dark = theme_dark()
    )

    p <- p + theme(
      plot.title = element_text(
        size = s$title_size
      ),

      axis.title = element_text(
        size = s$axis_size
      ),

      axis.text = element_text(
        size = s$tick_size
      ),

      axis.text.x = element_text(
        angle = s$x_angle,
        hjust = if (s$x_angle == 0) 0.5 else 1
      ),

      axis.text.y = element_text(
        angle = s$y_angle,
        hjust = 0.5,
        vjust = 0.5
      ),

      panel.grid = if (s$grid) {
        element_line(
          color = "grey85",
          linewidth = 0.3
        )
      } else {
        element_blank()
      }
    )

    if (
      s$facet &&
      s$kind %in% c(
        "scatter", "regression", "hist", "density"
      )
    ) {

      p <- p + facet_wrap(
        ~ am,
        labeller = as_labeller(
          c(
            "0" = "Automatic",
            "1" = "Manual"
          )
        )
      )
    }

    p
  }

  output$gg_plot <- renderPlot({
    print(make_ggplot(settings()))
  }, res = 96)

  # ========================================
  # EXAMPLE CODE GENERATOR
  # ========================================

  # Convert user-entered text into quoted R strings
  q <- function(x) {
    encodeString(as.character(x), quote = '"')
  }

  make_base_code <- function(s) {

    # The code examples use standard plotting syntax.
    # Advanced formatting is shown separately below.

    body <- switch(
      s$kind,

      scatter = paste0(
        "plot(mtcars$wt, mtcars$mpg,\n",
        "     pch = 19, col = ", q(s$color), ",\n",
        "     cex = ", s$point_size / 2.5, ")"
      ),

      regression = paste0(
        "plot(mtcars$wt, mtcars$mpg,\n",
        "     pch = 19, col = ", q(s$color), ")\n",
        "abline(lm(mpg ~ wt, data = mtcars),\n",
        '       col = "firebrick", lwd = 2)'
      ),

      hist = paste0(
        "hist(mtcars$mpg,\n",
        "     breaks = ", s$bins, ",\n",
        "     col = ", q(s$color), ")"
      ),

      box = paste0(
        "mtcars$transmission <- factor(\n",
        "  mtcars$am, levels = c(0, 1),\n",
        '  labels = c("Automatic", "Manual"))\n\n',
        "boxplot(mpg ~ transmission,\n",
        "        data = mtcars,\n",
        "        col = ", q(s$color), ")"
      ),

      bar = paste0(
        "counts <- table(mtcars$cyl)\n",
        "barplot(counts,\n",
        "        col = ", q(s$color), ")"
      ),

      grouped = paste0(
        "counts <- table(mtcars$am, mtcars$cyl)\n",
        "barplot(counts,\n",
        "        beside = TRUE,\n",
        '        col = c("steelblue", "tomato"),\n',
        "        legend.text = TRUE)"
      ),

      violin = paste0(
        "# Base R does not have a built-in violin plot.\n",
        "# Calculate densities for each group and\n",
        "# draw mirrored shapes using polygon().\n\n",
        "groups <- split(iris[[",
        q(s$measure), "]], iris$Species)\n",
        "densities <- lapply(groups, density)\n",
        "ylim <- range(unlist(lapply(densities,\n",
        "  function(d) d$x)))\n\n",
        "plot(NA, xlim = c(0.5, 3.5),\n",
        "     ylim = ylim, xaxt = 'n')\n",
        "axis(1, at = 1:3, labels = names(groups))\n\n",
        "for (i in 1:3) {\n",
        "  d <- densities[[i]]\n",
        "  w <- 0.35 * d$y / max(d$y)\n",
        "  polygon(c(i-w, rev(i+w)),\n",
        "          c(d$x, rev(d$x)),\n",
        "          col = ", q(s$color), ")\n",
        "}"
      ),

      density = paste0(
        "plot(density(mtcars$mpg),\n",
        "     col = ", q(s$color), ",\n",
        "     lwd = 2)"
      )
    )

    paste0(
      body,
      "\n\n",
      "title(main = ", q(s$title), ",\n",
      "      xlab = ", q(s$xl), ",\n",
      "      ylab = ", q(s$yl), ")\n\n",
      "# Formatting examples:\n",
      "par(cex.main = ", s$title_size / 16, ",\n",
      "    cex.lab = ", s$axis_size / 12, ",\n",
      "    cex.axis = ", s$tick_size / 10, ")\n",
      "# Set par() before plotting to affect the graph."
    )
  }

  make_gg_code <- function(s) {

    geom <- switch(
      s$kind,

      scatter = paste0(
        "ggplot(mtcars, aes(wt, mpg)) +\n",
        "  geom_point(color = ", q(s$color),
        ", size = ", s$point_size, ")"
      ),

      regression = paste0(
        "ggplot(mtcars, aes(wt, mpg)) +\n",
        "  geom_point(color = ", q(s$color), ") +\n",
        '  geom_smooth(method = "lm", se = FALSE)'
      ),

      hist = paste0(
        "ggplot(mtcars, aes(mpg)) +\n",
        "  geom_histogram(bins = ", s$bins,
        ", fill = ", q(s$color), ")"
      ),

      box = paste0(
        "mtcars$transmission <- factor(\n",
        "  mtcars$am, levels = c(0, 1),\n",
        '  labels = c("Automatic", "Manual"))\n\n',
        "ggplot(mtcars, aes(transmission, mpg)) +\n",
        "  geom_boxplot(fill = ", q(s$color), ")"
      ),

      bar = paste0(
        "ggplot(mtcars, aes(factor(cyl))) +\n",
        "  geom_bar(fill = ", q(s$color), ")"
      ),

      grouped = paste0(
        "ggplot(mtcars,\n",
        "       aes(factor(cyl), fill = factor(am))) +\n",
        '  geom_bar(position = "dodge")'
      ),

      violin = paste0(
        "ggplot(iris,\n",
        "       aes(Species, .data[[",
        q(s$measure), "]])) +\n",
        "  geom_violin(fill = ", q(s$color), ")",
        if (s$overlay) {
          ' +\n  geom_boxplot(width = 0.12, fill = "white")'
        } else {
          ""
        }
      ),

      density = paste0(
        "ggplot(mtcars, aes(mpg)) +\n",
        "  geom_density(fill = ", q(s$color), ")"
      )
    )

    paste0(
      "library(ggplot2)\n\n",
      geom,
      " +\n",
      "  labs(title = ", q(s$title), ",\n",
      "       x = ", q(s$xl), ",\n",
      "       y = ", q(s$yl), ") +\n",
      "  theme_", s$theme, "() +\n",
      "  theme(\n",
      "    plot.title = element_text(size = ",
      s$title_size, "),\n",
      "    axis.title = element_text(size = ",
      s$axis_size, "),\n",
      "    axis.text = element_text(size = ",
      s$tick_size, "),\n",
      "    axis.text.x = element_text(angle = ",
      s$x_angle, "),\n",
      "    axis.text.y = element_text(angle = ",
      s$y_angle, ")\n",
      "  )"
    )
  }

  output$code_area <- renderUI({

    if (code_hidden()) {

      return(
        div(
          class = "practice-note",

          h4("Code Hidden — Your Turn!"),

          p(
            "Recreate the graphs in RStudio or Positron, ",
            "then reveal the example code."
          )
        )
      )
    }

    s <- settings()

    tagList(

      h4("Base R"),

      tags$pre(
        class = "solution",
        make_base_code(s)
      ),

      h4("ggplot2"),

      tags$pre(
        class = "solution",
        make_gg_code(s)
      ),

      p(
        class = "muted",
        "These examples demonstrate the core syntax. ",
        "Some advanced options, including faceting and ",
        "transparency, are not yet included in every ",
        "generated code example."
      )
    )
  })

  # ========================================
  # DATASET AND VARIABLE EXPLANATIONS
  # ========================================

  output$dataset_note <- renderText({

    if (input$kind == "violin") {
      "Dataset: iris (150 flowers)"
    } else {
      "Dataset: mtcars (32 cars)"
    }
  })

  output$mapping_note <- renderText({

    switch(
      input$kind,

      scatter =
        "Each point represents a car. Weight is plotted against MPG.",

      regression =
        "Weight vs. MPG with a fitted least-squares regression line.",

      hist =
        "A frequency distribution of miles per gallon.",

      box =
        "Compare MPG for automatic and manual cars.",

      bar =
        "Counts of cars with 4, 6, or 8 cylinders.",

      grouped =
        "Cylinder counts separated by transmission type.",

      violin =
        paste(
          "Distribution of",
          input$measure,
          "across the three iris species."
        ),

      density =
        "A smoothed estimate of the distribution of MPG."
    )
  })
}

# ============================================
# START APP
# ============================================

shinyApp(ui, server)
