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

      .model-box {
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

      .interpretation-box {
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

      @media (max-width: 800px) {

        .controls-column {
          border-right: none;
          padding-right: 15px;
        }

        .graph-column {
          padding-left: 15px;
          margin-top: 15px;
        }

        .interpretation-box,
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

      h2("STAT 216 Poisson Distribution Explorer"),

      p(
        "Explore the number of events occurring in a fixed interval."
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
            "Poisson Model"
          ),

          sliderInput(
            "lambda",
            "Average number of events (λ)",
            min = 0.5,
            max = 30,
            value = 5,
            step = 0.5
          ),

          div(
            class = "definition-box",

            HTML(
              "<b>Definition</b><br>
               X = number of events occurring in a fixed interval<br><br>
               X = 0, 1, 2, 3, ..."
            )
          ),

          div(
            class = "model-box",

            div(
              class = "box-title",
              "Poisson Setting"
            ),

            HTML(
              "Use a Poisson model when counting events occurring
               over a fixed interval of <b>time, distance, area,
               volume, or another exposure</b>.<br><br>
               λ represents the average number of events per interval."
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
            "Poisson Probability Distribution"
          ),

          uiOutput("model_label"),

          plotOutput(
            "poisson_plot",
            height = "340px"
          ),

          uiOutput("interpretation_panel"),

          uiOutput("r_code_panel")
        )
      )
    )
  )
)


server <- function(input, output, session) {


  # --------------------------------------------------
  # Largest X displayed
  # --------------------------------------------------

  max_x <- reactive({

    lambda <- input$lambda

    max(
      10,
      min(
        60,
        ceiling(
          qpois(
            0.999,
            lambda = lambda
          )
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
        "X ~ Poisson(λ = ",
        sprintf("%.2f", input$lambda),
        ")"
      )
    )

  })


  # --------------------------------------------------
  # Mean and SD
  # --------------------------------------------------

  output$summary_panel <- renderUI({

    lambda <- input$lambda

    mu <- lambda
    sd <- sqrt(lambda)

    div(
      class = "summary-box",

      div(
        class = "box-title",
        "Mean and Standard Deviation"
      ),

      HTML(
        paste0(
          "<b>Mean:</b> μ = λ = ",
          sprintf("%.2f", mu),

          "<br>",

          "<b>SD:</b> σ = √λ = ",
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
        "Number of events",
        min = 0,
        max = upper,
        value = c(
          min(2, upper),
          min(7, upper)
        ),
        step = 1
      )

    } else {

      sliderInput(
        "x_value",
        "Number of events (x)",
        min = 0,
        max = upper,
        value = min(
          5,
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

    lambda <- input$lambda
    type <- input$prob_type


    # EXACTLY

    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- dpois(
        x,
        lambda = lambda
      )

      statement <- sprintf(
        "P(X = %d)",
        x
      )

      formula <- paste0(
        "e<sup>−",
        sprintf("%.2f", lambda),
        "</sup>(",
        sprintf("%.2f", lambda),
        ")<sup>",
        x,
        "</sup> / ",
        x,
        "!"
      )


    # AT MOST

    } else if (type == "atmost") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- ppois(
        x,
        lambda = lambda
      )

      statement <- sprintf(
        "P(X ≤ %d)",
        x
      )

      formula <- paste0(
        "P(X = 0) + ... + P(X = ",
        x,
        ")"
      )


    # LESS THAN

    } else if (type == "less") {

      req(input$x_value)

      x <- round(input$x_value)

      probability <- if (x <= 0) {

        0

      } else {

        ppois(
          x - 1,
          lambda = lambda
        )
      }

      statement <- sprintf(
        "P(X < %d)",
        x
      )

      formula <- if (x <= 0) {

        "There are no possible values below 0."

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

      probability <- if (x <= 0) {

        1

      } else {

        ppois(
          x - 1,
          lambda = lambda,
          lower.tail = FALSE
        )
      }

      statement <- sprintf(
        "P(X ≥ %d)",
        x
      )

      formula <- if (x <= 0) {

        "All possible values of X are at least 0."

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

      probability <- ppois(
        x,
        lambda = lambda,
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

      upper_prob <- ppois(
        b,
        lambda = lambda
      )

      lower_prob <- if (a <= 0) {

        0

      } else {

        ppois(
          a - 1,
          lambda = lambda
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

      formula <- if (a <= 0) {

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
  # Interpretation
  # --------------------------------------------------

  output$interpretation_panel <- renderUI({

    lambda <- input$lambda
    type <- input$prob_type


    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      text <- paste0(
        "<b>Interpretation</b><br>",
        "X = ",
        x,
        " means exactly ",
        x,
        " event",
        ifelse(x == 1, "", "s"),
        " occur in the interval.<br>",
        "The long-run average is λ = ",
        sprintf("%.2f", lambda),
        " events per interval."
      )


    } else {

      text <- paste0(
        "<b>Interpretation</b><br>",
        "X counts the number of events occurring in one fixed interval. ",
        "The long-run average is λ = ",
        sprintf("%.2f", lambda),
        " events per interval."
      )
    }


    div(
      class = "interpretation-box",
      HTML(text)
    )

  })


  # --------------------------------------------------
  # R code panel
  # --------------------------------------------------

  output$r_code_panel <- renderUI({

    lambda <- input$lambda
    type <- input$prob_type


    if (type == "exact") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "dpois(%d, lambda = %.2f)",
        x,
        lambda
      )


    } else if (type == "atmost") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        "ppois(%d, lambda = %.2f)",
        x,
        lambda
      )


    } else if (type == "less") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= 0) {

        code <- "0"

      } else {

        code <- sprintf(
          "ppois(%d, lambda = %.2f)",
          x - 1,
          lambda
        )
      }


    } else if (type == "atleast") {

      req(input$x_value)

      x <- round(input$x_value)

      if (x <= 0) {

        code <- "1"

      } else {

        code <- sprintf(
          paste0(
            "ppois(%d, lambda = %.2f, ",
            "lower.tail = FALSE)"
          ),
          x - 1,
          lambda
        )
      }


    } else if (type == "greater") {

      req(input$x_value)

      x <- round(input$x_value)

      code <- sprintf(
        paste0(
          "ppois(%d, lambda = %.2f, ",
          "lower.tail = FALSE)"
        ),
        x,
        lambda
      )


    } else {

      req(input$x_range)

      a <- round(
        min(input$x_range)
      )

      b <- round(
        max(input$x_range)
      )


      if (a <= 0) {

        code <- sprintf(
          "ppois(%d, lambda = %.2f)",
          b,
          lambda
        )

      } else {

        code <- sprintf(
          paste0(
            "ppois(%d, lambda = %.2f) - ",
            "ppois(%d, lambda = %.2f)"
          ),
          b,
          lambda,
          a - 1,
          lambda
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
      )
    )

  })


  # --------------------------------------------------
  # Poisson distribution plot
  # --------------------------------------------------

  output$poisson_plot <- renderPlot({

    lambda <- input$lambda
    type <- input$prob_type

    upper <- max_x()

    x <- 0:upper

    probabilities <- dpois(
      x,
      lambda = lambda
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
      xlab = "Number of Events (X)",
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

    mean_x <- lambda


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
