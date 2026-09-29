library(shiny)

# ============================================================
# Helper function: draw polar grid
# ============================================================

draw_polar_grid <- function(limit = 4) {

  angles <- seq(
    0,
    2 * pi,
    length.out = 361
  )

  # Concentric circles
  for (radius in seq(1, limit, by = 1)) {

    lines(
      radius * cos(angles),
      radius * sin(angles),
      col = "gray85",
      lwd = 1
    )
  }

  # Radial lines every 30 degrees
  radial_angles <- seq(
    0,
    11 * pi / 6,
    by = pi / 6
  )

  for (angle in radial_angles) {

    segments(
      0,
      0,
      limit * cos(angle),
      limit * sin(angle),
      col = "gray90",
      lwd = 1
    )
  }

  abline(
    h = 0,
    v = 0,
    col = "gray55"
  )

  # Angle labels
  label_radius <- limit * 1.08

  angle_labels <- c(
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

  for (i in seq_along(radial_angles)) {

    text(
      label_radius * cos(radial_angles[i]),
      label_radius * sin(radial_angles[i]),
      labels = angle_labels[i],
      cex = 0.72
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

        /* ----------------------------------------------------------
           Main upper layout
           ---------------------------------------------------------- */

        .rose-layout {
          display: grid;
          grid-template-columns: 350px minmax(0, 1fr);
          column-gap: 22px;
          align-items: start;
          width: 100%;
        }

        .rose-controls {
          min-width: 0;
        }

        .rose-graph {
          min-width: 0;
          align-self: start;
        }

        .rose-graph .shiny-plot-output {
          margin-top: 0;
          margin-bottom: 0;
        }

        /* ----------------------------------------------------------
           Introduction
           ---------------------------------------------------------- */

        .rose-intro {
          font-size: 13px;
          line-height: 1.25;
          margin-bottom: 8px;
        }

        .rose-intro p {
          margin-top: 2px;
          margin-bottom: 5px;
        }

        .rose-formulas {
          font-size: 16px;
          margin: 4px 0 6px 0;
        }

        /* ----------------------------------------------------------
           Controls
           ---------------------------------------------------------- */

        .form-group {
          margin-bottom: 7px;
        }

        .control-label {
          margin-bottom: 2px;
        }

        .radio {
          margin-top: 2px;
          margin-bottom: 2px;
        }

        /* ----------------------------------------------------------
           Information boxes
           ---------------------------------------------------------- */

        .info-box {
          border: 1px solid #dddddd;
          border-radius: 5px;
          padding: 7px 10px;
          background-color: #f8f8f8;
          margin-top: 7px;
        }

        .info-box h4 {
          margin-top: 0;
          margin-bottom: 4px;
          font-size: 15px;
        }

        /* ----------------------------------------------------------
           Investigation area
           ---------------------------------------------------------- */

        .investigation-layout {
          display: grid;
          grid-template-columns: repeat(4, 1fr);
          column-gap: 14px;
          margin-top: 8px;
        }

        .investigation-card {
          border-top: 1px solid #dddddd;
          padding: 7px 9px 0 9px;
          font-size: 13px;
          line-height: 1.25;
        }

        .investigation-card h4 {
          font-size: 15px;
          margin-top: 0;
          margin-bottom: 4px;
        }

        .investigation-card p {
          margin: 2px 0;
        }

        .rule-box {
          background-color: #f8f8f8;
          border: 1px solid #dddddd;
          border-radius: 5px;
          padding: 7px 10px;
        }

        /* ----------------------------------------------------------
           Responsive layout
           ---------------------------------------------------------- */

        @media (max-width: 900px) {

          .rose-layout {
            grid-template-columns: 1fr;
          }

          .rose-graph {
            margin-top: 10px;
          }

          .investigation-layout {
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

  h2("Rose Curve Explorer"),

  # ==========================================================
  # Main layout
  # ==========================================================

  div(
    class = "rose-layout",

    # ========================================================
    # LEFT SIDE
    # ========================================================

    div(
      class = "rose-controls",

      div(
        class = "rose-intro",

        p(
          strong("Rose curves"),
          " have the form"
        ),

        div(
          class = "rose-formulas",

          HTML(
            "<b>r = a cos(nθ)</b> &nbsp;&nbsp; or &nbsp;&nbsp; <b>r = a sin(nθ)</b>"
          )
        ),

        p(
          "Change ",
          strong("a"),
          ", ",
          strong("n"),
          ", and the trigonometric function to investigate how each affects the graph."
        ),

        p(
          strong(
            "Before changing a value, predict what you think will happen."
          )
        )
      ),

      radioButtons(
        "rose_function",
        "Choose sine or cosine:",
        choices = c(
          "cosine" = "cos",
          "sine" = "sin"
        ),
        selected = "cos"
      ),

      sliderInput(
        "rose_a",
        "a:",
        min = 1,
        max = 6,
        value = 3,
        step = 0.5
      ),

      sliderInput(
        "rose_n",
        "n:",
        min = 1,
        max = 12,
        value = 3,
        step = 1
      ),

      div(
        class = "info-box",

        h4(
          "Current Equation"
        ),

        uiOutput(
          "rose_equation"
        )
      ),

      div(
        class = "info-box",

        h4(
          "Current Values"
        ),

        uiOutput(
          "rose_summary"
        )
      )
    ),

    # ========================================================
    # RIGHT SIDE
    # ========================================================

    div(
      class = "rose-graph",

      plotOutput(
        "rose_plot",
        height = "430px",
        width = "100%"
      )
    )
  ),

  # ==========================================================
  # Investigation area
  # ==========================================================

  div(
    class = "investigation-layout",

    # --------------------------------------------------------
    # Role of n
    # --------------------------------------------------------

    div(
      class = "investigation-card",

      h4(
        "Investigate n"
      ),

      p(
        "Keep a fixed and try several values of n."
      ),

      p(
        strong(
          "Count the petals."
        )
      ),

      p(
        "What happens when n is odd?"
      ),

      p(
        "What happens when n is even?"
      )
    ),

    # --------------------------------------------------------
    # Role of a
    # --------------------------------------------------------

    div(
      class = "investigation-card",

      h4(
        "Investigate a"
      ),

      p(
        "Keep n fixed and change a."
      ),

      p(
        "Does the number of petals change?"
      ),

      p(
        "What does a appear to control?"
      )
    ),

    # --------------------------------------------------------
    # Sine vs cosine
    # --------------------------------------------------------

    div(
      class = "investigation-card",

      h4(
        "Compare Sine and Cosine"
      ),

      p(
        "Keep a and n fixed."
      ),

      p(
        "Switch between cosine and sine."
      ),

      p(
        "What stays the same?"
      ),

      p(
        "What changes?"
      )
    ),

    # --------------------------------------------------------
    # Check the pattern
    # --------------------------------------------------------

    div(
      class = "investigation-card rule-box",

      h4(
        "Check Your Pattern"
      ),

      p(
        "If n is odd, the rose has ",
        strong("n petals"),
        "."
      ),

      p(
        "If n is even, the rose has ",
        strong("2n petals"),
        "."
      ),

      p(
        "The maximum distance from the pole is ",
        strong("|a|"),
        "."
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

  output$rose_equation <- renderUI({

    req(
      input$rose_function,
      input$rose_a,
      input$rose_n
    )

    trig_text <-
      ifelse(
        input$rose_function == "cos",
        "cos",
        "sin"
      )

    HTML(
      paste0(
        "<div style='font-size:20px;'>",
        "r = ",
        input$rose_a,
        " ",
        trig_text,
        "(",
        input$rose_n,
        "θ)",
        "</div>"
      )
    )
  })


  # ----------------------------------------------------------
  # Current values
  # ----------------------------------------------------------

  output$rose_summary <- renderUI({

    req(
      input$rose_a,
      input$rose_n
    )

    if (
      input$rose_n %% 2 == 0
    ) {

      petal_count <-
        2 *
        input$rose_n

    } else {

      petal_count <-
        input$rose_n
    }

    HTML(
      paste0(
        "<div style='font-size:14px; line-height:1.3;'>",

        "<b>a = </b>",
        input$rose_a,

        "<br>",

        "<b>n = </b>",
        input$rose_n,

        "<br>",

        "<b>Number of petals = </b>",
        petal_count,

        "<br>",

        "<b>Maximum distance = </b>",
        abs(
          input$rose_a
        ),

        "</div>"
      )
    )
  })


  # ----------------------------------------------------------
  # Rose plot
  # ----------------------------------------------------------

  output$rose_plot <- renderPlot({

    req(
      input$rose_function,
      input$rose_a,
      input$rose_n
    )

    theta <-
      seq(
        0,
        2 * pi,
        length.out = 4000
      )

    if (
      input$rose_function == "cos"
    ) {

      r <-
        input$rose_a *
        cos(
          input$rose_n *
          theta
        )

    } else {

      r <-
        input$rose_a *
        sin(
          input$rose_n *
          theta
        )
    }

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
          abs(
            input$rose_a
          ) +
          0.5
        )
      )

    # Compact plot margins
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
      main = "Rose Curve"
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
      cex = 0.9
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
