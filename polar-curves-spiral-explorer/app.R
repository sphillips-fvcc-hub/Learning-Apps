library(shiny)

# ============================================================
# Helper function: draw polar grid
# ============================================================

draw_polar_grid <- function(limit = 6) {

  t <- seq(
    0,
    2 * pi,
    length.out = 361
  )

  for (radius in seq(1, limit, by = 1)) {

    lines(
      radius * cos(t),
      radius * sin(t),
      col = "gray85",
      lwd = 1
    )
  }

  ang <- seq(
    0,
    11 * pi / 6,
    by = pi / 6
  )

  for (a in ang) {

    segments(
      0,
      0,
      limit * cos(a),
      limit * sin(a),
      col = "gray90",
      lwd = 1
    )
  }

  abline(
    h = 0,
    v = 0,
    col = "gray55"
  )

  labs <- c(
    "0",
    "π/6",
    "π/3",
    "π/2",
    "2π/3",
    "5π/6",
    "π",
    "7π/6",
    "4π/3",
    "3π/2",
    "5π/3",
    "11π/6"
  )

  lr <- limit * 1.07

  for (i in seq_along(ang)) {

    text(
      lr * cos(ang[i]),
      lr * sin(ang[i]),
      labs[i],
      cex = 0.68
    )
  }
}


# ============================================================
# User Interface
# ============================================================

ui <- fluidPage(

  tags$head(

    tags$style(
      HTML("

        body {
          font-size: 15px;
        }

        h2 {
          font-size: 30px;
          margin-top: 0;
          margin-bottom: 3px;
        }

        .app-layout {
          display: grid;
          grid-template-columns: 350px minmax(0, 1fr);
          column-gap: 22px;
          align-items: start;
          width: 100%;
        }

        .controls {
          min-width: 0;
        }

        .graph {
          min-width: 0;
        }

        .graph .shiny-plot-output {
          margin: 0;
        }

        .intro {
          font-size: 13px;
          line-height: 1.25;
          margin-bottom: 7px;
        }

        .intro p {
          margin: 2px 0 5px 0;
        }

        .form-group {
          margin-bottom: 7px;
        }

        .info {
          border: 1px solid #ddd;
          border-radius: 5px;
          padding: 7px 10px;
          background: #f8f8f8;
          margin-top: 7px;
          font-size: 14px;
          line-height: 1.3;
        }

        .info h4 {
          margin: 0 0 4px 0;
          font-size: 15px;
        }

        .investigate {
          display: grid;
          grid-template-columns: repeat(3, 1fr);
          column-gap: 14px;
          margin-top: 8px;
        }

        .card {
          border-top: 1px solid #ddd;
          padding: 7px 9px 0 9px;
          font-size: 13px;
          line-height: 1.25;
        }

        .card h4 {
          font-size: 15px;
          margin: 0 0 4px 0;
        }

        .card p {
          margin: 2px 0;
        }

        @media (max-width: 900px) {

          .app-layout {
            grid-template-columns: 1fr;
          }

          .graph {
            margin-top: 8px;
          }

          .investigate {
            grid-template-columns: 1fr;
            row-gap: 8px;
          }
        }

      ")
    )
  ),

  # ==========================================================
  # Title
  # ==========================================================

  h2("Polar Spiral Explorer"),

  # ==========================================================
  # Main app layout
  # ==========================================================

  div(
    class = "app-layout",

    # --------------------------------------------------------
    # Controls
    # --------------------------------------------------------

    div(
      class = "controls",

      div(
        class = "intro",

        p(
          "Explore an Archimedean spiral of the form ",
          strong("r = a + bθ"),
          "."
        ),

        p(
          "Change ",
          strong("a"),
          ", ",
          strong("b"),
          ", and the amount of θ shown. Watch how the spiral grows as θ increases."
        )
      ),

      sliderInput(
        "a",
        "a:",
        min = -3,
        max = 3,
        value = 0,
        step = 0.25
      ),

      sliderInput(
        "b",
        "b:",
        min = 0.1,
        max = 2,
        value = 0.5,
        step = 0.1
      ),

      sliderInput(
        "turns",
        "Number of turns:",
        min = 1,
        max = 8,
        value = 3,
        step = 1
      ),

      div(
        class = "info",

        h4(
          "Current Equation"
        ),

        uiOutput(
          "eq"
        )
      ),

      div(
        class = "info",

        h4(
          "Things to Notice"
        ),

        p(
          "Where does the spiral begin?"
        ),

        p(
          "How far apart are successive turns?"
        ),

        p(
          "What does b control?"
        )
      )
    ),

    # --------------------------------------------------------
    # Graph
    # --------------------------------------------------------

    div(
      class = "graph",

      plotOutput(
        "plot",
        height = "430px",
        width = "100%"
      )
    )
  ),

  # ==========================================================
  # Investigation prompts
  # ==========================================================

  div(
    class = "investigate",

    div(
      class = "card",

      h4(
        "Investigate a"
      ),

      p(
        "Keep b fixed and change a. What happens to the starting radius?"
      )
    ),

    div(
      class = "card",

      h4(
        "Investigate b"
      ),

      p(
        "Keep a fixed and change b. What happens to the spacing between turns?"
      )
    ),

    div(
      class = "card",

      h4(
        "Investigate θ"
      ),

      p(
        "Increase the number of turns. What stays constant as the spiral grows?"
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Current equation
  # ----------------------------------------------------------

  output$eq <- renderUI({

    sign <-
      if (input$b >= 0) {
        "+"
      } else {
        "−"
      }

    HTML(
      paste0(
        "<div style='font-size:20px;'>",
        "r = ",
        input$a,
        " ",
        sign,
        " ",
        abs(input$b),
        "θ",
        "</div>"
      )
    )
  })


  # ----------------------------------------------------------
  # Spiral graph
  # ----------------------------------------------------------

  output$plot <- renderPlot({

    req(
      input$a,
      input$b,
      input$turns
    )

    theta <- seq(
      0,
      2 * pi * input$turns,
      length.out = 5000
    )

    r <-
      input$a +
      input$b * theta

    x <-
      r *
      cos(theta)

    y <-
      r *
      sin(theta)

    limit <-
      max(
        2,
        ceiling(
          max(
            abs(r)
          ) +
          0.5
        )
      )

    par(
      mar = c(
        2.4,
        2.4,
        1.7,
        0.8
      )
    )

    plot(
      NA,
      xlim = c(
        -limit,
        limit
      ),
      ylim = c(
        -limit,
        limit
      ),
      asp = 1,
      xlab = "x",
      ylab = "y",
      main = "Archimedean Spiral"
    )

    draw_polar_grid(
      limit
    )

    lines(
      x,
      y,
      col = "blue",
      lwd = 4
    )

    points(
      0,
      0,
      pch = 19,
      cex = 0.8
    )
  })
}


# ============================================================
# Run app
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
