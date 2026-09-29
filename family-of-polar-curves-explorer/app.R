library(shiny)

# ============================================================
# Helper function: draw polar grid
# ============================================================

draw_polar_grid <- function(limit = 5) {

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

  # Radial lines
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
  label_radius <- limit * 1.06

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
          margin-bottom: 4px;
        }

        .intro-text {
          font-size: 14px;
          line-height: 1.25;
          margin-bottom: 8px;
        }

        .intro-text p {
          margin-top: 2px;
          margin-bottom: 2px;
        }

        /* ----------------------------------------------------------
           Main two-column layout
           ---------------------------------------------------------- */

        .polar-layout {
          display: grid;
          grid-template-columns: 330px minmax(0, 1fr);
          column-gap: 24px;
          align-items: start;
          width: 100%;
        }

        .polar-controls {
          min-width: 0;
        }

        .polar-graph {
          min-width: 0;
          align-self: start;
        }

        /* ----------------------------------------------------------
           Controls
           ---------------------------------------------------------- */

        .form-group {
          margin-bottom: 8px;
        }

        .control-label {
          margin-bottom: 3px;
        }

        .radio {
          margin-top: 3px;
          margin-bottom: 3px;
        }

        .equation-box {
          border: 1px solid #dddddd;
          border-radius: 5px;
          padding: 8px 12px;
          margin-top: 8px;
          margin-bottom: 8px;
          background-color: #f8f8f8;
        }

        .investigation-box {
          border-top: 1px solid #dddddd;
          margin-top: 7px;
          padding-top: 6px;
          font-size: 14px;
          line-height: 1.3;
        }

        .investigation-box p {
          margin: 2px 0;
        }

        .polar-graph .shiny-plot-output {
          margin-top: 0;
        }

        /* On a narrow screen, stack the two areas */

        @media (max-width: 900px) {

          .polar-layout {
            grid-template-columns: 1fr;
          }

          .polar-graph {
            margin-top: 12px;
          }
        }

      ")
    )
  ),

  # ==========================================================
  # Title and introduction
  # ==========================================================

  h2("Polar Curve Family Explorer"),

  div(
    class = "intro-text",

    p(
      "Choose a family of polar curves and change its parameters. ",
      "Look for connections between the ",
      tags$strong("equation"),
      " and the ",
      tags$strong("shape of the graph"),
      ". Change ",
      tags$strong("one parameter at a time"),
      " and predict what will happen."
    )
  ),

  # ==========================================================
  # Main layout
  # ==========================================================

  div(
    class = "polar-layout",

    # ========================================================
    # LEFT SIDE
    # ========================================================

    div(
      class = "polar-controls",

      selectInput(
        "family",
        "Choose a family:",
        choices = c(
          "Circles" = "circle",
          "Limaçons and Cardioids" = "limacon",
          "Roses" = "rose",
          "Lemniscates" = "lemniscate",
          "Spirals" = "spiral"
        ),
        selected = "circle"
      ),

      uiOutput(
        "family_controls"
      ),

      div(
        class = "equation-box",

        tags$strong(
          "Current Equation"
        ),

        uiOutput(
          "family_equation"
        )
      ),

      div(
        class = "investigation-box",

        tags$strong(
          "Things to Investigate"
        ),

        p(
          "What does each parameter appear to control?"
        ),

        p(
          "Where is the graph symmetric?"
        ),

        p(
          "When does r = 0?"
        ),

        p(
          "What is the largest distance from the pole?"
        )
      )
    ),

    # ========================================================
    # RIGHT SIDE
    # ========================================================

    div(
      class = "polar-graph",

      plotOutput(
        "family_plot",
        height = "500px",
        width = "100%"
      )
    )
  )
)


# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Controls for each family
  # ----------------------------------------------------------

  output$family_controls <- renderUI({

    switch(
      input$family,

      # ------------------------------------------------------
      # Circles
      # ------------------------------------------------------

      circle =
        tagList(

          radioButtons(
            "circle_function",
            "Choose the form:",
            choices = c(
              "r = a cos(θ)" = "cos",
              "r = a sin(θ)" = "sin"
            ),
            selected = "cos"
          ),

          sliderInput(
            "circle_a",
            "a:",
            min = -6,
            max = 6,
            value = 4,
            step = 0.5
          )
        ),

      # ------------------------------------------------------
      # Limaçons and cardioids
      # ------------------------------------------------------

      limacon =
        tagList(

          radioButtons(
            "limacon_function",
            "Choose the form:",
            choices = c(
              "r = a + b cos(θ)" = "cos",
              "r = a + b sin(θ)" = "sin"
            ),
            selected = "cos"
          ),

          sliderInput(
            "limacon_a",
            "a:",
            min = 0,
            max = 6,
            value = 2,
            step = 0.5
          ),

          sliderInput(
            "limacon_b",
            "b:",
            min = 0.5,
            max = 6,
            value = 2,
            step = 0.5
          )
        ),

      # ------------------------------------------------------
      # Roses
      # ------------------------------------------------------

      rose =
        tagList(

          radioButtons(
            "rose_function",
            "Choose the form:",
            choices = c(
              "r = a cos(nθ)" = "cos",
              "r = a sin(nθ)" = "sin"
            ),
            selected = "cos"
          ),

          sliderInput(
            "rose_a",
            "a:",
            min = 1,
            max = 5,
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
          )
        ),

      # ------------------------------------------------------
      # Lemniscates
      # ------------------------------------------------------

      lemniscate =
        tagList(

          radioButtons(
            "lemniscate_function",
            "Choose the form:",
            choices = c(
              "r² = a² cos(2θ)" = "cos",
              "r² = a² sin(2θ)" = "sin"
            ),
            selected = "cos"
          ),

          sliderInput(
            "lemniscate_a",
            "a:",
            min = 1,
            max = 6,
            value = 3,
            step = 0.5
          )
        ),

      # ------------------------------------------------------
      # Spirals
      # ------------------------------------------------------

      spiral =
        sliderInput(
          "spiral_a",
          "a:",
          min = 0.1,
          max = 2,
          value = 0.5,
          step = 0.1
        )
    )
  })


  # ----------------------------------------------------------
  # Current equation
  # ----------------------------------------------------------

  output$family_equation <- renderUI({

    req(
      input$family
    )

    equation_text <-
      switch(
        input$family,

        # ----------------------------------------------------
        # Circle
        # ----------------------------------------------------

        circle = {

          req(
            input$circle_function,
            input$circle_a
          )

          if (
            input$circle_function == "cos"
          ) {

            paste0(
              "r = ",
              input$circle_a,
              " cos(θ)"
            )

          } else {

            paste0(
              "r = ",
              input$circle_a,
              " sin(θ)"
            )
          }
        },

        # ----------------------------------------------------
        # Limacon
        # ----------------------------------------------------

        limacon = {

          req(
            input$limacon_function,
            input$limacon_a,
            input$limacon_b
          )

          trig_text <-
            ifelse(
              input$limacon_function == "cos",
              "cos(θ)",
              "sin(θ)"
            )

          paste0(
            "r = ",
            input$limacon_a,
            " + ",
            input$limacon_b,
            " ",
            trig_text
          )
        },

        # ----------------------------------------------------
        # Rose
        # ----------------------------------------------------

        rose = {

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

          paste0(
            "r = ",
            input$rose_a,
            " ",
            trig_text,
            "(",
            input$rose_n,
            "θ)"
          )
        },

        # ----------------------------------------------------
        # Lemniscate
        # ----------------------------------------------------

        lemniscate = {

          req(
            input$lemniscate_function,
            input$lemniscate_a
          )

          trig_text <-
            ifelse(
              input$lemniscate_function == "cos",
              "cos(2θ)",
              "sin(2θ)"
            )

          paste0(
            "r² = ",
            input$lemniscate_a^2,
            " ",
            trig_text
          )
        },

        # ----------------------------------------------------
        # Spiral
        # ----------------------------------------------------

        spiral = {

          req(
            input$spiral_a
          )

          paste0(
            "r = ",
            input$spiral_a,
            "θ"
          )
        }
      )

    HTML(
      paste0(
        "<div style='font-size:22px; margin-top:5px;'>",
        equation_text,
        "</div>"
      )
    )
  })


  # ----------------------------------------------------------
  # Polar graph
  # ----------------------------------------------------------

  output$family_plot <- renderPlot({

    req(
      input$family
    )

    theta <-
      seq(
        0,
        2 * pi,
        length.out = 2500
      )

    # --------------------------------------------------------
    # Circle
    # --------------------------------------------------------

    if (
      input$family == "circle"
    ) {

      req(
        input$circle_function,
        input$circle_a
      )

      if (
        input$circle_function == "cos"
      ) {

        r <-
          input$circle_a *
          cos(theta)

      } else {

        r <-
          input$circle_a *
          sin(theta)
      }
    }

    # --------------------------------------------------------
    # Limacon
    # --------------------------------------------------------

    if (
      input$family == "limacon"
    ) {

      req(
        input$limacon_function,
        input$limacon_a,
        input$limacon_b
      )

      if (
        input$limacon_function == "cos"
      ) {

        r <-
          input$limacon_a +
          input$limacon_b *
          cos(theta)

      } else {

        r <-
          input$limacon_a +
          input$limacon_b *
          sin(theta)
      }
    }

    # --------------------------------------------------------
    # Rose
    # --------------------------------------------------------

    if (
      input$family == "rose"
    ) {

      req(
        input$rose_function,
        input$rose_a,
        input$rose_n
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
    }

    # --------------------------------------------------------
    # Lemniscate
    # --------------------------------------------------------

    if (
      input$family == "lemniscate"
    ) {

      req(
        input$lemniscate_function,
        input$lemniscate_a
      )

      if (
        input$lemniscate_function == "cos"
      ) {

        inside <-
          input$lemniscate_a^2 *
          cos(
            2 * theta
          )

      } else {

        inside <-
          input$lemniscate_a^2 *
          sin(
            2 * theta
          )
      }

      r <-
        ifelse(
          inside >= 0,
          sqrt(inside),
          NA_real_
        )
    }

    # --------------------------------------------------------
    # Spiral
    # --------------------------------------------------------

    if (
      input$family == "spiral"
    ) {

      req(
        input$spiral_a
      )

      theta <-
        seq(
          0,
          4 * pi,
          length.out = 2500
        )

      r <-
        input$spiral_a *
        theta
    }

    # --------------------------------------------------------
    # Convert to rectangular coordinates
    # --------------------------------------------------------

    x <-
      r *
      cos(theta)

    y <-
      r *
      sin(theta)

    # --------------------------------------------------------
    # Determine graph limits
    # --------------------------------------------------------

    max_distance <-
      max(
        sqrt(
          x^2 +
            y^2
        ),
        na.rm = TRUE
      )

    if (
      !is.finite(max_distance) ||
      max_distance < 2
    ) {

      max_distance <- 2
    }

    limit <-
      ceiling(
        max_distance + 0.5
      )

    # --------------------------------------------------------
    # Draw graph
    # --------------------------------------------------------

    par(
      mar = c(
        2.7,
        2.7,
        1.8,
        1.0
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
      main = "Polar Curve"
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
