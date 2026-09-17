library(shiny)
library(ggplot2)

# -------------------------------------------------------------------------
# Starting data
# -------------------------------------------------------------------------

data_A_start <- data.frame(
  id = 1:7,
  x = 1:7,
  y = c(2, 3, 4, 5, 6, 7, 8)
)

data_B_start <- data.frame(
  id = 1:7,
  x = 1:7,
  y = c(5.0, 5.4, 4.7, 5.2, 4.8, 5.3, 4.9)
)


# -------------------------------------------------------------------------
# Functions
# -------------------------------------------------------------------------

get_stats <- function(dat) {

  fit <- lm(y ~ x, data = dat)

  r_value <- cor(dat$x, dat$y)

  list(
    intercept = unname(coef(fit)[1]),
    slope = unname(coef(fit)[2]),
    r = r_value,
    r2 = r_value^2
  )
}


make_stats_text <- function(dat) {

  s <- get_stats(dat)

  intercept_sign <- ifelse(
    s$intercept >= 0,
    "+",
    "-"
  )

  equation <- paste0(
    "\u0177 = ",
    round(s$slope, 3),
    "x ",
    intercept_sign,
    " ",
    abs(round(s$intercept, 3))
  )

  paste0(
    equation,
    "\n",
    "r = ",
    round(s$r, 3),
    "     R\u00b2 = ",
    round(s$r2, 3)
  )
}


# -------------------------------------------------------------------------
# User interface
# -------------------------------------------------------------------------

ui <- fluidPage(

  tags$head(

    tags$style(
      HTML("

        body {
          background-color: white;
        }

        .container-fluid {
          max-width: 1400px;
          margin: auto;
          padding-left: 22px;
          padding-right: 22px;
        }

        .app-title {
          text-align: center;
          font-size: 30px;
          font-weight: 600;
          margin-top: 10px;
          margin-bottom: 10px;
        }

        .controls-row {
          display: flex;
          align-items: center;
          gap: 20px;
          margin-bottom: 0px;
          flex-wrap: wrap;
        }

        .controls-row .form-group {
          margin-bottom: 0px;
        }

        .instruction {
          font-size: 14px;
        }

        .results-row {
          display: flex;
          gap: 10px;
          width: 100%;
          margin-top: 0px;
          margin-bottom: 10px;
        }

        .result-box {
          flex: 1;
          border-radius: 8px;
          padding: 8px 14px;
          min-width: 0;
        }

        .result-a {
          background-color: #fff0f0;
          border: 1px solid #f3a6a6;
        }

        .result-b {
          background-color: #eef9fc;
          border: 1px solid #8fd4df;
        }

        .result-title-a {
          color: #a61b29;
          font-size: 17px;
          font-weight: bold;
        }

        .result-title-b {
          color: #087b8c;
          font-size: 17px;
          font-weight: bold;
        }

        .result-box pre {
          background: transparent;
          border: none;
          padding: 4px 0 0 0;
          margin: 0;
          font-size: 15px;
        }

        @media (max-width: 700px) {

          .app-title {
            font-size: 23px;
          }

          .results-row {
            flex-direction: column;
          }

          .controls-row {
            gap: 10px;
          }
        }

      ")
    )
  ),


  # Title -----------------------------------------------------------------

  div(
    class = "app-title",
    "Correlation and Variance Comparison of Two Linear Regressions"
  ),


  # Controls --------------------------------------------------------------

  div(
    class = "controls-row",

    radioButtons(
      "active_dataset",
      NULL,
      choices = c(
        "Dataset A" = "A",
        "Dataset B" = "B"
      ),
      selected = "A",
      inline = TRUE
    ),

    actionButton(
      "reset",
      "Reset"
    ),

    tags$span(
      class = "instruction",
      "Select a dataset, then click the graph to move its nearest point."
    )
  ),


  # Graph -----------------------------------------------------------------

  plotOutput(
    "regression_plot",
    click = "plot_click",
    height = "380px"
  ),


  # Results ---------------------------------------------------------------

  div(
    class = "results-row",

    div(
      class = "result-box result-a",

      div(
        class = "result-title-a",
        "Dataset A"
      ),

      verbatimTextOutput(
        "stats_A"
      )
    ),

    div(
      class = "result-box result-b",

      div(
        class = "result-title-b",
        "Dataset B"
      ),

      verbatimTextOutput(
        "stats_B"
      )
    )
  )
)


# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

server <- function(input, output, session) {


  # Reactive data ---------------------------------------------------------

  data_A <- reactiveVal(
    data_A_start
  )

  data_B <- reactiveVal(
    data_B_start
  )


  # Move nearest point ----------------------------------------------------

  observeEvent(input$plot_click, {

    req(input$plot_click)

    new_x <- input$plot_click$x
    new_y <- input$plot_click$y


    if (input$active_dataset == "A") {

      dat <- data_A()

      distances <- sqrt(
        (dat$x - new_x)^2 +
        (dat$y - new_y)^2
      )

      nearest <- which.min(
        distances
      )

      dat$x[nearest] <- new_x
      dat$y[nearest] <- new_y

      data_A(dat)

    } else {

      dat <- data_B()

      distances <- sqrt(
        (dat$x - new_x)^2 +
        (dat$y - new_y)^2
      )

      nearest <- which.min(
        distances
      )

      dat$x[nearest] <- new_x
      dat$y[nearest] <- new_y

      data_B(dat)
    }
  })


  # Reset -----------------------------------------------------------------

  observeEvent(input$reset, {

    data_A(
      data_A_start
    )

    data_B(
      data_B_start
    )
  })


  # Graph -----------------------------------------------------------------

  output$regression_plot <- renderPlot({

    A <- transform(
      data_A(),
      Dataset = "Dataset A"
    )

    B <- transform(
      data_B(),
      Dataset = "Dataset B"
    )

    combined <- rbind(
      A,
      B
    )


    ggplot(
      combined,
      aes(
        x = x,
        y = y,
        color = Dataset,
        shape = Dataset
      )
    ) +

      geom_point(
        size = 4
      ) +

      geom_smooth(
        method = "lm",
        se = FALSE,
        linewidth = 1.1
      ) +

      coord_cartesian(
        xlim = c(0, 8),
        ylim = c(0, 10),
        expand = FALSE
      ) +

      scale_x_continuous(
        breaks = 0:8
      ) +

      scale_y_continuous(
        breaks = 0:10
      ) +

      labs(
        x = "x",
        y = "y",
        color = NULL,
        shape = NULL
      ) +

      theme_minimal(
        base_size = 14
      ) +

      theme(
        legend.position = "top",

        legend.margin = margin(
          0,
          0,
          0,
          0
        ),

        plot.margin = margin(
          2,
          8,
          2,
          5
        ),

        panel.grid.minor = element_blank()
      )
  })


  # Dataset A results -----------------------------------------------------

  output$stats_A <- renderText({

    make_stats_text(
      data_A()
    )
  })


  # Dataset B results -----------------------------------------------------

  output$stats_B <- renderText({

    make_stats_text(
      data_B()
    )
  })
}


# -------------------------------------------------------------------------
# Run app
# -------------------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
