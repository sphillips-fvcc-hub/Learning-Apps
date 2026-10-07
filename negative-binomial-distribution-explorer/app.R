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

      .connection-box {
        background: #f8f9fa;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 9px 11px;
        margin-top: 10px;
        font-size: 13px;
        line-height: 1.5;
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
        font-size: 14px;
        line-height: 1.5;
      }

      .trial-sequence {
        font-family: Consolas, 'Courier New', monospace;
        font-size: 14px;
        margin: 5px 0;
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
        margin-top: 6px;
        font-size: 12px;
        line-height: 1.4;
        color: #555;
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

      h2("STAT 216 Negative Binomial Distribution Explorer"),

      p(
        "Explore the trial on which the rth success occurs."
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
            "Negative Binomial Model"
          ),

          sliderInput(
            "r",
            "Number of successes needed (r)",
            min = 1,
            max = 10,
            value = 3,
            step = 1
          ),

          sliderInput(
            "p",
            "Probability of success (p)",
            min = 0.05,
            max = 0.95,
            value = 0.30,
            step = 0.05
          ),

          div(
            class = "definition-box",

            HTML(
              "<b>Definition</b><br>
               X = trial number of the rth success<br><br>
               X = r, r + 1, r + 2, ..."
            )
          ),

          div(
            class = "connection-box",

            div(
              class = "box-title",
              "Connection to Geometric"
            ),

            HTML(
              "When <b>r = 1</b>, the negative binomial
               distribution becomes the <b>geometric
               distribution</b>.<br><br>
               Geometric: trial of the first success<br>
               Negative Binomial: trial of the rth success"
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
            "Negative Binomial Probability Distribution"
          ),

          uiOutput("model_label"),

          plotOutput(
            "nb_plot",
            height = "340px"
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
  # Largest X displayed on graph
  # --------------------------------------------------

  max_x <- reactive({

    r <- input$r
    p <- input$p

    failures_995 <- qnbinom(
      0.995,
      size = r,
      prob = p
    )

    max(
      r + 8,
      min(
        60,
        ceiling(failures_995 + r)
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
        "r = ",
        input$r,
        " | p = ",
        sprintf("%.2f", input$p),
        " | q = ",
        sprintf("%.2f", 1 - input$p)
      )
    )

  })


  # --------------------------------------------------
  # Mean and SD
  # --------------------------------------------------

  output$summary_panel <- renderUI({

    r <- input$r
    p <- input$p

    mu <- r / p

    sd <- sqrt(
      r * (1 - p)
    ) / p

    div(
      class = "summary-box",

      div(
        class = "box-title",
        "Mean and Standard Deviation"
      ),

      HTML(
        paste0(
          "<b>Mean:</b> μ = r/p = ",
          sprintf("%.2f", mu),

          "<br>",

          "<b>SD:</b> σ = √[r(1 − p)]/p = ",
          sprintf("%.2f", sd)
        )
      )
    )

  })


  # --------------------------------------------------
  # Boundary controls
  # --------------------------------------------------

  output$boundary_controls <- renderUI({

    r <- input$r
    upper <- max_x()

    if (input$prob_type == "between") {

      sliderInput(
        "x_range",
        "Trial number",
        min = r,
        max = upper,
        value = c(
          r,
          min(r + 5, upper)
        ),
        step = 1
      )

    } else {

      sliderInput(
        "x_value",
        "Trial number (x)",
        min = r,
        max = upper,
        value = min(
          r + 3,
          upper
        ),
        step = 1
      )
    }

  })


  # --------------------------------------------------
  # Probability information
  # --------------------------------------------------

  probability_info <- reactive({

    r <- input$r
    p <- input$p
    type <- input$prob_type


    # Remember:
    # X = trial number of rth success
    #
    # R negative binomial functions use:
    # number of failures before rth success
    #
    # failures = X - r


    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      failures <- x - r

      probability <- dnbinom(
        failures,
        size = r,
        prob = p
      )

      statement <- sprintf(
        "P(X = %d)",
        x
      )

      formula <- paste0(
        "C(",
        x - 1,
        ", ",
        r - 1,
        ")(",
        sprintf("%.2f", p),
        ")<sup>",
        r,
        "</sup>(",
        sprintf("%.2f", 1 - p),
        ")<sup>",
        x - r,
        "</sup>"
      )


    # AT MOST

    } else if (type == "atmost") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- pnbinom(
        x - r,
        size = r,
        prob = p
      )

      statement <- sprintf(
        "P(X ≤ %d)",
        x
      )

      formula <- paste0(
        "P(X = ",
        r,
        ") + ... + P(X = ",
        x,
        ")"
      )


    # LESS THAN

    } else if (type == "less") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= r) {

        probability <- 0

      } else {

        probability <- pnbinom(
          x - r - 1,
          size = r,
          prob = p
        )
      }

      statement <- sprintf(
        "P(X < %d)",
        x
      )

      formula <- if (x <= r) {

        paste0(
          "The earliest possible ",
          r,
          "th success is trial ",
          r,
          "."
        )

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

      if (x <= r) {

        probability <- 1

      } else {

        probability <- pnbinom(
          x - r - 1,
          size = r,
          prob = p,
          lower.tail = FALSE
        )
      }

      statement <- sprintf(
        "P(X ≥ %d)",
        x
      )

      formula <- if (x <= r) {

        "All possible values of X are included."

      } else {

        paste0(
          "1 − P(X ≤ ",
          x - 1,
          ")"
        )
      }


    # GREATER THAN

    } else if (type == "greater") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- pnbinom(
        x - r,
        size = r,
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

      a <- round(
        min(input$x_range)
      )

      b <- round(
        max(input$x_range)
      )

      upper_prob <- pnbinom(
        b - r,
        size = r,
        prob = p
      )

      lower_prob <- if (a <= r) {

        0

      } else {

        pnbinom(
          a - r - 1,
          size = r,
          prob = p
        )
      }

      probability <-
        upper_prob -
        lower_prob

      statement <- sprintf(
        "P(%d ≤ X ≤ %d)",
        a,
        b
      )

      formula <- if (a <= r) {

        paste0(
          "P(X ≤ ",
          b,
          ")"
        )

      } else {

        paste0(
          "P(X ≤ ",
          b,
          ") − P(X ≤ ",
          a - 1,
          ")"
        )
      }
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
  # Trial interpretation
  # --------------------------------------------------

  output$trial_panel <- renderUI({

    r <- input$r
    type <- input$prob_type


    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      failures <- x - r


      if (x <= 15) {

        # Create one possible sequence ending
        # with the rth success on trial x.

        if (r == 1) {

          sequence_text <- paste0(
            paste0(
              1:(x - 1),
              ":F",
              collapse = "   "
            ),
            ifelse(
              x > 1,
              "   ",
              ""
            ),
            x,
            ":S"
          )

        } else {

          # Put the first r-1 successes first,
          # followed by failures, then the rth success.

          first_successes <- if (r > 1) {
            paste0(
              1:(r - 1),
              ":S",
              collapse = "   "
            )
          } else {
            ""
          }

          middle_start <- r
          middle_end <- x - 1

          middle_failures <- if (
            middle_end >= middle_start
          ) {

            paste0(
              middle_start:middle_end,
              ":F",
              collapse = "   "
            )

          } else {

            ""
          }

          pieces <- c(
            first_successes,
            middle_failures,
            paste0(
              x,
              ":S"
            )
          )

          pieces <- pieces[
            pieces != ""
          ]

          sequence_text <- paste(
            pieces,
            collapse = "   "
          )
        }


        display_sequence <- paste0(
          "<div class='trial-sequence'>",
          sequence_text,
          "</div>"
        )


      } else {

        display_sequence <- paste0(
          "<div class='trial-sequence'>",
          "Example sequence omitted because x is large.",
          "</div>"
        )
      }


      text <- paste0(
        "<b>One Possible Trial Sequence</b>",

        display_sequence,

        "The ",
        r,
        ifelse(
          r == 1,
          "st",
          ifelse(
            r == 2,
            "nd",
            ifelse(
              r == 3,
              "rd",
              "th"
            )
          )
        ),
        " success occurs on trial ",
        x,
        ".<br>",

        "That means there are ",
        r - 1,
        " successes and ",
        failures,
        " failures in the first ",
        x - 1,
        " trials, followed by a success."
      )


    } else {

      text <- paste0(
        "<b>Interpretation</b><br>",
        "X is the trial number on which success #",
        r,
        " occurs."
      )
    }


    div(
      class = "trial-box",
      HTML(text)
    )

  })


  # --------------------------------------------------
  # R code panel
  # --------------------------------------------------

  output$r_code_panel <- renderUI({

    r <- input$r
    p <- input$p
    type <- input$prob_type


    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "dnbinom(%d, size = %d, prob = %.2f)",
        x - r,
        r,
        p
      )


    } else if (type == "atmost") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "pnbinom(%d, size = %d, prob = %.2f)",
        x - r,
        r,
        p
      )


    } else if (type == "less") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= r) {

        code <- "0"

      } else {

        code <- sprintf(
          "pnbinom(%d, size = %d, prob = %.2f)",
          x - r - 1,
          r,
          p
        )
      }


    } else if (type == "atleast") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= r) {

        code <- "1"

      } else {

        code <- sprintf(
          paste0(
            "pnbinom(%d, size = %d, prob = %.2f, ",
            "lower.tail = FALSE)"
          ),
          x - r - 1,
          r,
          p
        )
      }


    } else if (type == "greater") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        paste0(
          "pnbinom(%d, size = %d, prob = %.2f, ",
          "lower.tail = FALSE)"
        ),
        x - r,
        r,
        p
      )


    } else {

      req(input$x_range)

      a <- round(
        min(input$x_range)
      )

      b <- round(
        max(input$x_range)
      )


      if (a <= r) {

        code <- sprintf(
          "pnbinom(%d, size = %d, prob = %.2f)",
          b - r,
          r,
          p
        )

      } else {

        code <- sprintf(
          paste0(
            "pnbinom(%d, size = %d, prob = %.2f) - ",
            "pnbinom(%d, size = %d, prob = %.2f)"
          ),
          b - r,
          r,
          p,
          a - r - 1,
          r,
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
            "<b>Important:</b> In this course, X is the ",
            "trial number of the rth success. ",
            "R's negative binomial distribution counts ",
            "the number of <b>failures</b> before the rth success. ",
            "Therefore, the value entered into R is ",
            "<b>X − r</b>."
          )
        )
      )
    )

  })


  # --------------------------------------------------
  # Negative binomial plot
  # --------------------------------------------------

  output$nb_plot <- renderPlot({

    r <- input$r
    p <- input$p
    type <- input$prob_type

    upper <- max_x()

    x <- r:upper

    failures <- x - r

    probabilities <- dnbinom(
      failures,
      size = r,
      prob = p
    )


    # Determine which bars should be shaded

    shaded <- rep(
      FALSE,
      length(x)
    )


    if (type == "exact") {

      req(input$x_value)

      value <- round(
        input$x_value
      )

      shaded <- x == value


    } else if (type == "atmost") {

      req(input$x_value)

      value <- round(
        input$x_value
      )

      shaded <- x <= value


    } else if (type == "less") {

      req(input$x_value)

      value <- round(
        input$x_value
      )

      shaded <- x < value


    } else if (type == "atleast") {

      req(input$x_value)

      value <- round(
        input$x_value
      )

      shaded <- x >= value


    } else if (type == "greater") {

      req(input$x_value)

      value <- round(
        input$x_value
      )

      shaded <- x > value


    } else {

      req(input$x_range)

      a <- round(
        min(input$x_range)
      )

      b <- round(
        max(input$x_range)
      )

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


    bar_positions <- barplot(
      probabilities,
      names.arg = x,
      col = bar_colors,
      border = "white",
      space = 0.25,
      xlab = "Trial Number of the rth Success (X)",
      ylab = "Probability",
      ylim = c(
        0,
        max(probabilities) * 1.20
      ),
      cex.names = ifelse(
        length(x) > 35,
        0.50,
        ifelse(
          length(x) > 25,
          0.60,
          0.80
        )
      )
    )


    # ------------------------------------------------
    # Mean line
    # ------------------------------------------------

    mean_x <- r / p


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
      y = max(probabilities) * 1.12,
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
