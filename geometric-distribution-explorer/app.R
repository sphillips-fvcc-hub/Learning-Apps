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

      .graph-column {
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
        padding: 6px 9px;
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

      .definition-box {
        background: #eef5fb;
        border-left: 5px solid #2F75B5;
        padding: 9px 11px;
        margin-top: 10px;
        border-radius: 4px;
        font-size: 14px;
        line-height: 1.45;
      }

      .summary-box {
        background: #f8f9fa;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 9px 11px;
        margin-top: 10px;
        font-size: 14px;
        line-height: 1.5;
      }

      .calculation-box {
        background: #f8f9fa;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 9px 11px;
        margin-top: 10px;
        font-size: 14px;
        line-height: 1.5;
      }

      .box-title {
        color: #17365D;
        font-weight: 700;
        margin-bottom: 5px;
      }

      .probability-result {
        margin-top: 7px;
        padding-top: 7px;
        border-top: 1px solid #d9e2ec;
        font-size: 15px;
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
        margin: 2px 0 4px 0;
      }

      .trial-box {
        background: #f8f9fa;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 8px 12px;
        margin: 4px auto 10px auto;
        text-align: center;
        width: 92%;
      }

      .trial-sequence {
        font-family: Consolas, 'Courier New', monospace;
        font-size: 15px;
        line-height: 1.7;
        overflow-x: auto;
        white-space: nowrap;
      }

      .rcode-box {
        background: #f3f5f7;
        border: 1px solid #ccd5df;
        border-radius: 6px;
        padding: 8px 12px;
        margin: 8px auto 0 auto;
        width: 92%;
        font-size: 13px;
      }

      .rcode-title {
        color: #17365D;
        font-weight: 700;
        margin-bottom: 5px;
        text-align: center;
      }

      .rcode {
        font-family: Consolas, 'Courier New', monospace;
        background: white;
        border: 1px solid #e0e4e8;
        border-radius: 4px;
        padding: 6px 8px;
        margin-top: 4px;
        overflow-x: auto;
        white-space: nowrap;
      }

      .r-note {
        color: #555;
        font-size: 12px;
        margin-top: 6px;
        line-height: 1.35;
      }

      @media (max-width: 800px) {

        .controls-column {
          border-right: none;
          padding-right: 15px;
        }

        .graph-column {
          padding-left: 15px;
          margin-top: 15px;
        }

        .trial-box,
        .rcode-box {
          width: 100%;
        }
      }
    "))
  ),

  div(
    class = "app-container",

    div(
      class = "title-panel",

      h2("STAT 216 Geometric Distribution Explorer"),

      p(
        "Explore the number of trials needed to obtain the first success."
      )
    ),

    div(
      class = "main-panel",

      fluidRow(

        # ==================================================
        # LEFT SIDE
        # ==================================================

        column(
          width = 4,
          class = "controls-column",

          h4(
            class = "section-title",
            "Geometric Model"
          ),

          sliderInput(
            "p",
            "Probability of success (p)",
            min = 0.05,
            max = 0.95,
            value = 0.25,
            step = 0.05
          ),

          div(
            class = "definition-box",

            HTML(
              "<b>Definition</b><br>
               X = trial number of the first success<br><br>
               X = 1, 2, 3, ..."
            )
          ),

          uiOutput("summary_panel"),

          h4(
            class = "section-title",
            style = "margin-top: 14px;",
            "Find a Probability"
          ),

          div(
            class = "probability-selector",

            radioButtons(
              "prob_type",
              label = NULL,

              choices = c(
                "Exactly" = "exact",
                "At Most" = "atmost",
                "Less Than" = "less",
                "At Least" = "atleast",
                "Greater Than" = "greater",
                "Between" = "between"
              ),

              selected = "exact",
              inline = TRUE
            )
          ),

          uiOutput("boundary_controls"),

          uiOutput("calculation_panel")
        ),


        # ==================================================
        # RIGHT SIDE
        # ==================================================

        column(
          width = 8,
          class = "graph-column",

          h4(
            class = "graph-title",
            "Geometric Probability Distribution"
          ),

          uiOutput("model_label"),

          plotOutput(
            "geom_plot",
            height = "330px"
          ),

          uiOutput("trial_panel"),

          uiOutput("r_code_panel")
        )
      )
    )
  )
)


server <- function(input, output, session) {


  # --------------------------------------------------
  # Useful maximum x value for display
  # --------------------------------------------------

  max_x <- reactive({

    p <- input$p

    max(
      10,
      min(
        40,
        ceiling(
          qgeom(0.995, prob = p) + 1
        )
      )
    )

  })


  # --------------------------------------------------
  # Model label
  # --------------------------------------------------

  output$model_label <- renderUI({

    p(
      class = "graph-subtitle",

      paste0(
        "p = ",
        sprintf("%.2f", input$p),
        "     |     q = 1 - p = ",
        sprintf("%.2f", 1 - input$p)
      )
    )

  })


  # --------------------------------------------------
  # Mean and SD
  # --------------------------------------------------

  output$summary_panel <- renderUI({

    p <- input$p

    mu <- 1 / p
    sd <- sqrt(1 - p) / p

    div(
      class = "summary-box",

      div(
        class = "box-title",
        "Mean and Standard Deviation"
      ),

      HTML(
        paste0(
          "<b>Mean:</b> μ = 1/p = ",
          sprintf("%.2f", mu),

          "<br>",

          "<b>SD:</b> σ = √(1 − p)/p = ",
          sprintf("%.2f", sd)
        )
      )
    )

  })


  # --------------------------------------------------
  # Boundary controls
  # --------------------------------------------------

  output$boundary_controls <- renderUI({

    upper <- max_x()

    if (input$prob_type == "between") {

      sliderInput(
        "x_range",
        "Trial numbers",
        min = 1,
        max = upper,
        value = c(
          min(2, upper),
          min(5, upper)
        ),
        step = 1
      )

    } else {

      sliderInput(
        "x_value",
        "Trial number (x)",
        min = 1,
        max = upper,
        value = min(4, upper),
        step = 1
      )
    }

  })


  # --------------------------------------------------
  # Probability calculation
  # --------------------------------------------------

  probability_info <- reactive({

    p <- input$p
    type <- input$prob_type


    # EXACTLY

    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- dgeom(
        x - 1,
        prob = p
      )

      statement <- sprintf(
        "P(X = %d)",
        x
      )

      formula <- paste0(
        "(1 − p)<sup>x−1</sup>p = ",
        sprintf("%.2f", 1 - p),
        "<sup>",
        x - 1,
        "</sup>(",
        sprintf("%.2f", p),
        ")"
      )


    # AT MOST

    } else if (type == "atmost") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- pgeom(
        x - 1,
        prob = p
      )

      statement <- sprintf(
        "P(X ≤ %d)",
        x
      )

      formula <- paste0(
        "P(X = 1) + ... + P(X = ",
        x,
        ")"
      )


    # LESS THAN

    } else if (type == "less") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- if (x <= 1) {
        0
      } else {
        pgeom(
          x - 2,
          prob = p
        )
      }

      statement <- sprintf(
        "P(X < %d)",
        x
      )

      formula <- if (x <= 1) {

        "There are no possible trial numbers below 1."

      } else {

        paste0(
          "P(X ≤ ",
          x - 1,
          ")"
        )
      }


    # AT LEAST

    } else if (type == "atleast") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- if (x <= 1) {
        1
      } else {
        pgeom(
          x - 2,
          prob = p,
          lower.tail = FALSE
        )
      }

      statement <- sprintf(
        "P(X ≥ %d)",
        x
      )

      formula <- paste0(
        "1 − P(X ≤ ",
        x - 1,
        ")"
      )


    # GREATER THAN

    } else if (type == "greater") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- pgeom(
        x - 1,
        prob = p,
        lower.tail = FALSE
      )

      statement <- sprintf(
        "P(X > %d)",
        x
      )

      formula <- paste0(
        "1 − P(X ≤ ",
        x,
        ")"
      )


    # BETWEEN

    } else {

      req(input$x_range)

      a <- round(min(input$x_range))
      b <- round(max(input$x_range))

      probability <-
        pgeom(
          b - 1,
          prob = p
        ) -
        if (a <= 1) {
          0
        } else {
          pgeom(
            a - 2,
            prob = p
          )
        }

      statement <- sprintf(
        "P(%d ≤ X ≤ %d)",
        a,
        b
      )

      formula <- paste0(
        "P(X ≤ ",
        b,
        ") − P(X ≤ ",
        a - 1,
        ")"
      )
    }


    list(
      probability = probability,
      statement = statement,
      formula = formula
    )

  })


  # --------------------------------------------------
  # Calculation panel
  # --------------------------------------------------

  output$calculation_panel <- renderUI({

    info <- probability_info()

    div(
      class = "calculation-box",

      div(
        class = "box-title",
        "Probability Calculation"
      ),

      HTML(
        paste0(
          "<b>",
          info$statement,
          "</b>",

          "<br><br>",

          info$formula,

          "<div class='probability-result'>",

          "<b>Probability = ",
          sprintf(
            "%.4f",
            info$probability
          ),
          "</b><br>",

          sprintf(
            "%.2f%%",
            100 * info$probability
          ),

          "</div>"
        )
      )
    )

  })


  # --------------------------------------------------
  # Trial sequence
  # --------------------------------------------------

  output$trial_panel <- renderUI({

    if (input$prob_type != "exact") {

      return(
        div(
          class = "trial-box",

          HTML(
            "<b>Interpretation</b><br>
             Each value of X represents the trial on which
             the first success occurs."
          )
        )
      )
    }


    req(input$x_value)

    x <- round(input$x_value)


    if (x <= 12) {

      trials <- 1:x

      outcomes <- c(
        rep("F", max(0, x - 1)),
        "S"
      )

      sequence_text <- paste(
        paste0(
          trials,
          ":",
          outcomes
        ),
        collapse = "   "
      )


    } else {

      sequence_text <- paste0(
        "1:F   2:F   3:F   ...   ",
        x - 1,
        ":F   ",
        x,
        ":S"
      )
    }


    div(
      class = "trial-box",

      div(
        class = "box-title",
        paste0(
          "What does X = ",
          x,
          " mean?"
        )
      ),

      div(
        class = "trial-sequence",
        sequence_text
      ),

      HTML(
        paste0(
          x - 1,
          " failure",
          ifelse(
            x - 1 == 1,
            "",
            "s"
          ),
          " followed by the first success."
        )
      )
    )

  })


  # --------------------------------------------------
  # R code panel
  # --------------------------------------------------

  output$r_code_panel <- renderUI({

    p <- input$p
    type <- input$prob_type


    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "dgeom(%d, prob = %.2f)",
        x - 1,
        p
      )


    } else if (type == "atmost") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "pgeom(%d, prob = %.2f)",
        x - 1,
        p
      )


    } else if (type == "less") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= 1) {

        code <- "0"

      } else {

        code <- sprintf(
          "pgeom(%d, prob = %.2f)",
          x - 2,
          p
        )
      }


    } else if (type == "atleast") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= 1) {

        code <- "1"

      } else {

        code <- sprintf(
          "pgeom(%d, prob = %.2f, lower.tail = FALSE)",
          x - 2,
          p
        )
      }


    } else if (type == "greater") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "pgeom(%d, prob = %.2f, lower.tail = FALSE)",
        x - 1,
        p
      )


    } else {

      req(input$x_range)

      a <- round(min(input$x_range))
      b <- round(max(input$x_range))


      if (a <= 1) {

        code <- sprintf(
          "pgeom(%d, prob = %.2f)",
          b - 1,
          p
        )

      } else {

        code <- sprintf(
          paste0(
            "pgeom(%d, prob = %.2f) - ",
            "pgeom(%d, prob = %.2f)"
          ),
          b - 1,
          p,
          a - 2,
          p
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
        class = "rcode",
        code
      ),

      div(
        class = "r-note",

        HTML(
          paste0(
            "<b>Why does R use a different number?</b> ",
            "In this course, X is the <i>trial number</i> ",
            "of the first success. R's geometric distribution ",
            "counts the number of <i>failures before</i> the ",
            "first success. Therefore, the R value is X − 1."
          )
        )
      )
    )

  })


  # --------------------------------------------------
  # Geometric distribution plot
  # --------------------------------------------------

  output$geom_plot <- renderPlot({

    p <- input$p
    type <- input$prob_type
    upper <- max_x()

    x <- 1:upper

    probabilities <- dgeom(
      x - 1,
      prob = p
    )


    # Determine which bars should be shaded

    shaded <- rep(
      FALSE,
      length(x)
    )


    if (type == "exact") {

      req(input$x_value)

      value <- round(input$x_value)

      shaded <- x == value


    } else if (type == "atmost") {

      req(input$x_value)

      value <- round(input$x_value)

      shaded <- x <= value


    } else if (type == "less") {

      req(input$x_value)

      value <- round(input$x_value)

      shaded <- x < value


    } else if (type == "atleast") {

      req(input$x_value)

      value <- round(input$x_value)

      shaded <- x >= value


    } else if (type == "greater") {

      req(input$x_value)

      value <- round(input$x_value)

      shaded <- x > value


    } else {

      req(input$x_range)

      a <- round(min(input$x_range))
      b <- round(max(input$x_range))

      shaded <- (
        x >= a &
        x <= b
      )
    }


    bar_colors <- ifelse(
      shaded,
      "#337ab7",
      "#cfd8e3"
    )


    par(
      mar = c(4.2, 4.5, 1, 0.8),
      las = 1
    )


    # Save the bar positions so the mean line can be
    # placed correctly in the barplot coordinate system.

    bar_positions <- barplot(
      probabilities,
      names.arg = x,
      col = bar_colors,
      border = "white",
      space = 0.25,
      xlab = "Trial Number of First Success (X)",
      ylab = "Probability",
      ylim = c(
        0,
        max(probabilities) * 1.18
      ),
      cex.names = ifelse(
        upper > 20,
        0.65,
        0.85
      )
    )


    # ------------------------------------------------
    # Mean line
    # ------------------------------------------------

    mean_x <- 1 / p

    # Convert the mean from the X scale to the
    # internal horizontal coordinates used by barplot().

    mean_position <- approx(
      x = x,
      y = bar_positions,
      xout = mean_x,
      rule = 2
    )$y


    abline(
      v = mean_position,
      lty = 2,
      lwd = 2
    )


    text(
      x = mean_position,
      y = max(probabilities) * 1.10,
      labels = paste0(
        "Mean = ",
        sprintf("%.2f", mean_x)
      ),
      pos = 4,
      cex = 0.85
    )


    # ------------------------------------------------
    # Legend
    # ------------------------------------------------

    legend(
      "topright",
      legend = c(
        "Selected probability",
        "Other outcomes"
      ),
      fill = c(
        "#337ab7",
        "#cfd8e3"
      ),
      border = NA,
      bty = "n",
      cex = 0.8
    )

  })

}


shinyApp(ui, server)
