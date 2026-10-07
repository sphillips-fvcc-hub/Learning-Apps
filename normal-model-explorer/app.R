library(shiny)

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
        font-family: Arial, sans-serif;
      }

      .app-container {
        max-width: 1100px;
        margin: 10px auto;
      }

      .title-panel {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 10px 20px;
        margin-bottom: 10px;
        text-align: center;
      }

      .title-panel h2 {
        margin: 0 0 3px 0;
        color: #17365D;
        font-weight: 700;
      }

      .title-panel p {
        margin: 0;
        color: #555;
        font-size: 14px;
      }

      .main-panel {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 14px 18px;
      }

      .controls-column {
        padding-right: 20px;
        border-right: 1px solid #e3e8ee;
      }

      .graphs-column {
        padding-left: 22px;
      }

      .section-title {
        color: #17365D;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 10px;
      }

      .probability-selector input[type='radio'] {
        display: none;
      }

      .probability-selector .radio-inline {
        display: inline-block;
        padding: 6px 10px;
        margin: 2px 2px;
        background-color: white;
        border: 1px solid #cccccc;
        border-radius: 4px;
        color: #333333;
        cursor: pointer;
      }

      .probability-selector .radio-inline:has(input:checked) {
        background-color: #337ab7;
        border-color: #2e6da4;
        color: white;
      }

      .info-box {
        background: #eef5fb;
        border-left: 5px solid #2F75B5;
        padding: 9px 11px;
        margin-top: 10px;
        border-radius: 4px;
        font-size: 14px;
        line-height: 1.45;
      }

      .calculation-box {
        background: #f8f9fa;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 10px 12px;
        margin-top: 10px;
        font-size: 14px;
        line-height: 1.5;
      }

      .calc-title {
        color: #17365D;
        font-weight: 700;
        margin-bottom: 5px;
      }

      .probability-result {
        margin-top: 8px;
        padding-top: 7px;
        border-top: 1px solid #d9e2ec;
        font-size: 15px;
      }

      .rcode-box {
        background: #f3f5f7;
        border: 1px solid #ccd5df;
        border-radius: 6px;
        padding: 7px 12px;
        margin: 2px auto 8px auto;
        width: 88%;
        font-size: 13px;
      }

      .rcode-title {
        color: #17365D;
        font-weight: 700;
        margin-bottom: 6px;
        text-align: center;
      }

      .rcode-label {
        font-weight: 700;
        margin-top: 5px;
      }

      .rcode {
        font-family: Consolas, 'Courier New', monospace;
        background: white;
        border: 1px solid #e0e4e8;
        border-radius: 4px;
        padding: 5px 7px;
        margin: 3px 0 7px 0;
        overflow-x: auto;
        white-space: nowrap;
      }

      .graph-title {
        text-align: center;
        color: #17365D;
        font-weight: 700;
        margin: 0;
      }

      .graph-subtitle {
        text-align: center;
        color: #555;
        font-size: 13px;
        margin: 0;
      }

      .graph-divider {
        border: 0;
        border-top: 1px solid #e3e8ee;
        margin: 8px 0;
      }

      .z-graph-container {
        width: 72%;
        margin-left: auto;
        margin-right: auto;
      }

      .irs {
        margin-bottom: 5px;
      }

      @media (max-width: 800px) {

        .controls-column {
          border-right: none;
          padding-right: 15px;
        }

        .graphs-column {
          padding-left: 15px;
          margin-top: 15px;
        }

        .z-graph-container {
          width: 88%;
        }
      }
    "))
  ),

  div(
    class = "app-container",

    div(
      class = "title-panel",

      h2("STAT 216 Normal Model Explorer"),

      p(
        "Explore probabilities in a Normal model and see how X-values correspond to z-scores."
      )
    ),

    div(
      class = "main-panel",

      fluidRow(

        # ==================================================
        # LEFT SIDE: CONTROLS AND CALCULATIONS
        # ==================================================

        column(
          width = 4,
          class = "controls-column",

          h4(
            class = "section-title",
            "Normal Model"
          ),

          sliderInput(
            "mu",
            "Mean (μ)",
            min = -50,
            max = 150,
            value = 100,
            step = 1
          ),

          sliderInput(
            "sigma",
            "Standard Deviation (σ)",
            min = 1,
            max = 50,
            value = 15,
            step = 1
          ),

          h4(
            class = "section-title",
            "Find a Probability"
          ),

          div(
            class = "probability-selector",

            radioButtons(
              "prob_type",
              label = NULL,

              choices = c(
                "Left" = "left",
                "Right" = "right",
                "Between" = "between",
                "Outside" = "outside"
              ),

              selected = "left",
              inline = TRUE
            )
          ),

          uiOutput("boundary_controls"),

          div(
            class = "info-box",

            HTML(
              "<b>Standardization</b><br>
               z = (x − μ) / σ"
            )
          ),

          uiOutput("calculation_panel")
        ),


        # ==================================================
        # RIGHT SIDE: X GRAPH, R CODE, Z GRAPH
        # ==================================================

        column(
          width = 8,
          class = "graphs-column",

          # X Normal Model

          h4(
            class = "graph-title",
            "X Normal Model"
          ),

          uiOutput("x_model_label"),

          plotOutput(
            "x_plot",
            height = "245px"
          ),


          # R code between the graphs

          uiOutput("r_code_panel"),


          # Standard Normal Model

          tags$hr(
            class = "graph-divider"
          ),

          div(
            class = "z-graph-container",

            h4(
              class = "graph-title",
              "Z Standard Normal Model"
            ),

            p(
              class = "graph-subtitle",
              "Z ~ N(0, 1)"
            ),

            plotOutput(
              "z_plot",
              height = "205px"
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {


  # --------------------------------------------------
  # X model label
  # --------------------------------------------------

  output$x_model_label <- renderUI({

    p(
      class = "graph-subtitle",

      paste0(
        "X ~ N(",
        input$mu,
        ", ",
        input$sigma,
        ")"
      )
    )

  })


  # --------------------------------------------------
  # Boundary controls
  # --------------------------------------------------

  output$boundary_controls <- renderUI({

    req(
      input$mu,
      input$sigma
    )

    lower_limit <- input$mu - 4 * input$sigma
    upper_limit <- input$mu + 4 * input$sigma


    if (input$prob_type %in% c("left", "right")) {

      sliderInput(
        "x_value",
        "Boundary value (x)",
        min = lower_limit,
        max = upper_limit,
        value = input$mu + input$sigma,
        step = input$sigma / 10
      )


    } else {

      sliderInput(
        "x_range",
        "Boundary values",
        min = lower_limit,
        max = upper_limit,
        value = c(
          input$mu - input$sigma,
          input$mu + input$sigma
        ),
        step = input$sigma / 10
      )
    }

  })


  # --------------------------------------------------
  # Calculation panel
  # --------------------------------------------------

  output$calculation_panel <- renderUI({

    req(
      input$mu,
      input$sigma
    )

    mu <- input$mu
    sigma <- input$sigma
    type <- input$prob_type


    # LEFT / RIGHT

    if (type %in% c("left", "right")) {

      req(input$x_value)

      x <- input$x_value
      z <- (x - mu) / sigma


      if (type == "left") {

        probability <- pnorm(
          x,
          mean = mu,
          sd = sigma
        )

        x_statement <- sprintf(
          "P(X < %.2f)",
          x
        )

        z_statement <- sprintf(
          "P(Z < %.2f)",
          z
        )


      } else {

        probability <- pnorm(
          x,
          mean = mu,
          sd = sigma,
          lower.tail = FALSE
        )

        x_statement <- sprintf(
          "P(X > %.2f)",
          x
        )

        z_statement <- sprintf(
          "P(Z > %.2f)",
          z
        )
      }


      calculation <- paste0(

        "<div class='calc-title'>Probability Calculation</div>",

        "<b>X model:</b> ",
        x_statement,

        "<br><br>",

        "<b>Convert x to z:</b><br>",

        "z = (",
        sprintf("%.2f", x),
        " − ",
        sprintf("%.2f", mu),
        ") / ",
        sprintf("%.2f", sigma),

        " = <b>",
        sprintf("%.2f", z),
        "</b>",

        "<br><br>",

        "<b>Standard normal:</b> ",
        z_statement,

        "<div class='probability-result'>",

        "<b>Probability = ",
        sprintf("%.4f", probability),
        "</b><br>",

        sprintf(
          "%.2f%%",
          100 * probability
        ),

        "</div>"
      )


    # BETWEEN / OUTSIDE

    } else {

      req(input$x_range)

      a <- min(input$x_range)
      b <- max(input$x_range)

      za <- (a - mu) / sigma
      zb <- (b - mu) / sigma


      if (type == "between") {

        probability <-
          pnorm(
            b,
            mean = mu,
            sd = sigma
          ) -
          pnorm(
            a,
            mean = mu,
            sd = sigma
          )

        x_statement <- sprintf(
          "P(%.2f < X < %.2f)",
          a,
          b
        )

        z_statement <- sprintf(
          "P(%.2f < Z < %.2f)",
          za,
          zb
        )


      } else {

        probability <-
          pnorm(
            a,
            mean = mu,
            sd = sigma
          ) +
          pnorm(
            b,
            mean = mu,
            sd = sigma,
            lower.tail = FALSE
          )

        x_statement <- sprintf(
          "P(X < %.2f or X > %.2f)",
          a,
          b
        )

        z_statement <- sprintf(
          "P(Z < %.2f or Z > %.2f)",
          za,
          zb
        )
      }


      calculation <- paste0(

        "<div class='calc-title'>Probability Calculation</div>",

        "<b>X model:</b> ",
        x_statement,

        "<br><br>",

        "<b>Convert x-values to z-scores:</b><br>",

        "z₁ = (",
        sprintf("%.2f", a),
        " − ",
        sprintf("%.2f", mu),
        ") / ",
        sprintf("%.2f", sigma),

        " = <b>",
        sprintf("%.2f", za),
        "</b>",

        "<br>",

        "z₂ = (",
        sprintf("%.2f", b),
        " − ",
        sprintf("%.2f", mu),
        ") / ",
        sprintf("%.2f", sigma),

        " = <b>",
        sprintf("%.2f", zb),
        "</b>",

        "<br><br>",

        "<b>Standard normal:</b> ",
        z_statement,

        "<div class='probability-result'>",

        "<b>Probability = ",
        sprintf("%.4f", probability),
        "</b><br>",

        sprintf(
          "%.2f%%",
          100 * probability
        ),

        "</div>"
      )
    }


    div(
      class = "calculation-box",
      HTML(calculation)
    )

  })


  # --------------------------------------------------
  # R code panel
  # --------------------------------------------------

  output$r_code_panel <- renderUI({

    req(
      input$mu,
      input$sigma
    )

    mu <- input$mu
    sigma <- input$sigma
    type <- input$prob_type


    # LEFT / RIGHT

    if (type %in% c("left", "right")) {

      req(input$x_value)

      x <- input$x_value
      z <- (x - mu) / sigma


      if (type == "left") {

        x_code <- sprintf(
          "pnorm(%.2f, mean = %.2f, sd = %.2f)",
          x,
          mu,
          sigma
        )

        z_code <- sprintf(
          "pnorm(%.2f)",
          z
        )


      } else {

        x_code <- sprintf(
          "pnorm(%.2f, mean = %.2f, sd = %.2f, lower.tail = FALSE)",
          x,
          mu,
          sigma
        )

        z_code <- sprintf(
          "pnorm(%.2f, lower.tail = FALSE)",
          z
        )
      }


    # BETWEEN / OUTSIDE

    } else {

      req(input$x_range)

      a <- min(input$x_range)
      b <- max(input$x_range)

      za <- (a - mu) / sigma
      zb <- (b - mu) / sigma


      if (type == "between") {

        x_code <- sprintf(
          paste0(
            "pnorm(%.2f, mean = %.2f, sd = %.2f) - ",
            "pnorm(%.2f, mean = %.2f, sd = %.2f)"
          ),
          b,
          mu,
          sigma,
          a,
          mu,
          sigma
        )

        z_code <- sprintf(
          "pnorm(%.2f) - pnorm(%.2f)",
          zb,
          za
        )


      } else {

        x_code <- sprintf(
          paste0(
            "pnorm(%.2f, mean = %.2f, sd = %.2f) + ",
            "pnorm(%.2f, mean = %.2f, sd = %.2f, lower.tail = FALSE)"
          ),
          a,
          mu,
          sigma,
          b,
          mu,
          sigma
        )

        z_code <- sprintf(
          paste0(
            "pnorm(%.2f) + ",
            "pnorm(%.2f, lower.tail = FALSE)"
          ),
          za,
          zb
        )
      }
    }


    div(
      class = "rcode-box",

      div(
        class = "rcode-title",
        "RStudio Code"
      ),

      div(
        class = "rcode-label",
        "Using the X model:"
      ),

      div(
        class = "rcode",
        x_code
      ),

      div(
        class = "rcode-label",
        "Using the Z model:"
      ),

      div(
        class = "rcode",
        z_code
      )
    )

  })


  # --------------------------------------------------
  # Helper function for drawing normal curves
  # --------------------------------------------------

  draw_normal_plot <- function(
    mean_value,
    sd_value,
    type,
    boundary1,
    boundary2 = NULL,
    x_label = "x"
  ) {

    x_min <- mean_value - 4 * sd_value
    x_max <- mean_value + 4 * sd_value

    x <- seq(
      x_min,
      x_max,
      length.out = 1000
    )

    y <- dnorm(
      x,
      mean = mean_value,
      sd = sd_value
    )


    plot(
      x,
      y,
      type = "l",
      lwd = 3,
      xlab = x_label,
      ylab = "Density",
      main = "",
      ylim = c(
        0,
        max(y) * 1.15
      )
    )


    # LEFT

    if (type == "left") {

      shade_x <- x[x <= boundary1]

      shade_y <- dnorm(
        shade_x,
        mean = mean_value,
        sd = sd_value
      )

      polygon(
        c(shade_x, rev(shade_x)),
        c(
          shade_y,
          rep(0, length(shade_y))
        ),
        col = adjustcolor(
          "#337ab7",
          alpha.f = 0.35
        ),
        border = NA
      )

      abline(
        v = boundary1,
        lty = 2,
        lwd = 2
      )


    # RIGHT

    } else if (type == "right") {

      shade_x <- x[x >= boundary1]

      shade_y <- dnorm(
        shade_x,
        mean = mean_value,
        sd = sd_value
      )

      polygon(
        c(shade_x, rev(shade_x)),
        c(
          shade_y,
          rep(0, length(shade_y))
        ),
        col = adjustcolor(
          "#337ab7",
          alpha.f = 0.35
        ),
        border = NA
      )

      abline(
        v = boundary1,
        lty = 2,
        lwd = 2
      )


    # BETWEEN

    } else if (type == "between") {

      shade_x <- x[
        x >= boundary1 &
        x <= boundary2
      ]

      shade_y <- dnorm(
        shade_x,
        mean = mean_value,
        sd = sd_value
      )

      polygon(
        c(shade_x, rev(shade_x)),
        c(
          shade_y,
          rep(0, length(shade_y))
        ),
        col = adjustcolor(
          "#337ab7",
          alpha.f = 0.35
        ),
        border = NA
      )

      abline(
        v = c(
          boundary1,
          boundary2
        ),
        lty = 2,
        lwd = 2
      )


    # OUTSIDE

    } else {

      left_x <- x[
        x <= boundary1
      ]

      left_y <- dnorm(
        left_x,
        mean = mean_value,
        sd = sd_value
      )

      polygon(
        c(left_x, rev(left_x)),
        c(
          left_y,
          rep(0, length(left_y))
        ),
        col = adjustcolor(
          "#337ab7",
          alpha.f = 0.35
        ),
        border = NA
      )


      right_x <- x[
        x >= boundary2
      ]

      right_y <- dnorm(
        right_x,
        mean = mean_value,
        sd = sd_value
      )

      polygon(
        c(right_x, rev(right_x)),
        c(
          right_y,
          rep(0, length(right_y))
        ),
        col = adjustcolor(
          "#337ab7",
          alpha.f = 0.35
        ),
        border = NA
      )

      abline(
        v = c(
          boundary1,
          boundary2
        ),
        lty = 2,
        lwd = 2
      )
    }


    # Redraw curve over shaded region

    lines(
      x,
      y,
      lwd = 3
    )


    # Mean line

    abline(
      v = mean_value,
      lty = 3,
      lwd = 1.5
    )

  }


  # --------------------------------------------------
  # X Normal Model plot
  # --------------------------------------------------

  output$x_plot <- renderPlot({

    req(
      input$mu,
      input$sigma
    )

    mu <- input$mu
    sigma <- input$sigma
    type <- input$prob_type

    par(
      mar = c(4, 4.2, 0.5, 0.8),
      las = 1
    )


    if (type %in% c("left", "right")) {

      req(input$x_value)

      draw_normal_plot(
        mean_value = mu,
        sd_value = sigma,
        type = type,
        boundary1 = input$x_value,
        x_label = "X"
      )


    } else {

      req(input$x_range)

      a <- min(input$x_range)
      b <- max(input$x_range)

      draw_normal_plot(
        mean_value = mu,
        sd_value = sigma,
        type = type,
        boundary1 = a,
        boundary2 = b,
        x_label = "X"
      )
    }

  })


  # --------------------------------------------------
  # Z Standard Normal Model plot
  # --------------------------------------------------

  output$z_plot <- renderPlot({

    req(
      input$mu,
      input$sigma
    )

    mu <- input$mu
    sigma <- input$sigma
    type <- input$prob_type

    par(
      mar = c(4, 4.2, 0.5, 0.8),
      las = 1
    )


    if (type %in% c("left", "right")) {

      req(input$x_value)

      z <- (
        input$x_value - mu
      ) / sigma

      draw_normal_plot(
        mean_value = 0,
        sd_value = 1,
        type = type,
        boundary1 = z,
        x_label = "Z"
      )


    } else {

      req(input$x_range)

      a <- min(input$x_range)
      b <- max(input$x_range)

      za <- (
        a - mu
      ) / sigma

      zb <- (
        b - mu
      ) / sigma

      draw_normal_plot(
        mean_value = 0,
        sd_value = 1,
        type = type,
        boundary1 = za,
        boundary2 = zb,
        x_label = "Z"
      )
    }

  })

}


shinyApp(ui, server)
