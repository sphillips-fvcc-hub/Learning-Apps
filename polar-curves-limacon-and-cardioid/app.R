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

  h2("Limaçon and Cardioid Explorer"),

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
          "Explore equations of the form ",
          strong("r = a + b cos(θ)"),
          " and ",
          strong("r = a + b sin(θ)"),
          "."
        ),

        p(
          "Change ",
          strong("a"),
          " and ",
          strong("b"),
          ". Watch how the relationship between |a| and |b| changes the shape."
        )
      ),

      radioButtons(
        "fun",
        "Choose the form:",
        choices = c(
          "r = a + b cos(θ)" = "cos",
          "r = a + b sin(θ)" = "sin"
        ),
        selected = "cos"
      ),

      sliderInput(
        "a",
        "a:",
        min = 0,
        max = 6,
        value = 2,
        step = 0.5
      ),

      sliderInput(
        "b",
        "b:",
        min = 1,
        max = 6,
        value = 2,
        step = 0.5
      ),

      div(
        class = "info",

        h4("Current Equation"),

        uiOutput("eq")
      ),

      div(
        class = "info",

        h4("Current Relationship"),

        uiOutput("relation")
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

      h4("Look for a Cardioid"),

      p(
        "What happens when |a| = |b|?"
      )
    ),

    div(
      class = "card",

      h4("Change the Ratio"),

      p(
        "Try |a| < |b| and |a| > |b|. How does the shape change?"
      )
    ),

    div(
      class = "card",

      h4("Compare Sine and Cosine"),

      p(
        "Keep a and b fixed. What changes when you switch forms?"
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

    trig <-
      ifelse(
        input$fun == "cos",
        "cos",
        "sin"
      )

    HTML(
      paste0(
        "<div style='font-size:20px;'>",
        "r = ",
        input$a,
        " + ",
        input$b,
        " ",
        trig,
        "(θ)",
        "</div>"
      )
    )
  })


  # ----------------------------------------------------------
  # Current relationship between a and b
  # ----------------------------------------------------------

  output$relation <- renderUI({

    txt <-
      if (
        abs(input$a - input$b) < 1e-9
      ) {

        "|a| = |b|"

      } else if (
        abs(input$a) < abs(input$b)
      ) {

        "|a| < |b|"

      } else {

        "|a| > |b|"
      }

    HTML(
      paste0(
        "<b>",
        txt,
        "</b>"
      )
    )
  })


  # ----------------------------------------------------------
  # Limaçon / cardioid graph
  # ----------------------------------------------------------

  output$plot <- renderPlot({

    req(
      input$a,
      input$b,
      input$fun
    )

    theta <- seq(
      0,
      2 * pi,
      length.out = 4000
    )

    r <-
      if (input$fun == "cos") {

        input$a +
          input$b * cos(theta)

      } else {

        input$a +
          input$b * sin(theta)
      }

    x <- r * cos(theta)
    y <- r * sin(theta)

    limit <-
      max(
        2,
        ceiling(
          abs(input$a) +
            abs(input$b) +
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
      main = "Limaçon / Cardioid"
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
