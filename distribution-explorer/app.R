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
        margin: 12px auto;
      }

      .title-panel {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 12px 20px;
        margin-bottom: 10px;
        text-align: center;
      }

      .title-panel h2 {
        margin: 0 0 4px 0;
        color: #17365D;
        font-weight: 700;
      }

      .title-panel p {
        font-size: 15px;
        color: #555;
        margin: 0;
      }

      .distribution-selector {
        text-align: center;
        margin-bottom: 10px;
      }

      .distribution-selector input[type='radio'] {
        display: none;
      }

      .distribution-selector .radio-inline {
        display: inline-block;
        min-width: 145px;
        padding: 8px 12px;
        margin: 3px 3px;
        background-color: white;
        border: 1px solid #cccccc;
        border-radius: 4px;
        color: #333333;
        cursor: pointer;
        font-weight: normal;
      }

      .distribution-selector .radio-inline:has(input:checked) {
        background-color: #337ab7;
        border-color: #2e6da4;
        color: white;
      }

      .main-panel {
        background: white;
        border: 1px solid #d9e2ec;
        border-radius: 12px;
        padding: 16px 18px;
      }

      .controls-column {
        padding-right: 20px;
        border-right: 1px solid #e3e8ee;
      }

      .plot-column {
        padding-left: 22px;
      }

      .distribution-title {
        color: #17365D;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 14px;
      }

      .parameter-note {
        background: #eef5fb;
        border-left: 5px solid #2F75B5;
        padding: 10px 12px;
        margin-top: 10px;
        border-radius: 4px;
        font-size: 14px;
        line-height: 1.4;
      }

      .summary-box {
        background: #f8f9fa;
        border: 1px solid #d9e2ec;
        border-radius: 6px;
        padding: 10px 12px;
        margin-top: 10px;
        font-size: 14px;
        line-height: 1.45;
      }

      .summary-title {
        color: #17365D;
        font-weight: 700;
      }

      .summary-divider {
        margin-top: 6px;
        margin-bottom: 7px;
      }

      .plot-heading {
        color: #17365D;
        font-weight: 700;
        text-align: center;
        margin-top: 0;
        margin-bottom: 0;
      }

      .irs {
        margin-bottom: 8px;
      }

      @media (max-width: 800px) {

        .distribution-selector .radio-inline {
          min-width: 120px;
        }

        .controls-column {
          border-right: none;
          padding-right: 15px;
        }

        .plot-column {
          padding-left: 15px;
        }
      }
    "))
  ),

  div(
    class = "app-container",

    div(
      class = "title-panel",

      h2("STAT 216 Distribution Explorer"),

      p(
        "Choose a probability distribution and explore how its parameters affect its shape."
      )
    ),

    div(
      class = "distribution-selector",

      radioButtons(
        "distribution",
        label = NULL,

        choices = c(
          "Normal",
          "Geometric",
          "Binomial",
          "Negative Binomial",
          "Poisson"
        ),

        selected = "Normal",
        inline = TRUE
      )
    ),

    div(
      class = "main-panel",

      fluidRow(

        column(
          width = 4,
          class = "controls-column",

          h3(
            class = "distribution-title",

            textOutput(
              "distribution_name",
              inline = TRUE
            )
          ),

          uiOutput("parameter_controls"),

          uiOutput("parameter_description"),

          uiOutput("summary_statistics")
        ),

        column(
          width = 8,
          class = "plot-column",

          h4(
            class = "plot-heading",
            "Probability Distribution"
          ),

          plotOutput(
            "distribution_plot",
            height = "355px"
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {


  # --------------------------------------------------
  # Distribution title
  # --------------------------------------------------

  output$distribution_name <- renderText({

    paste(
      input$distribution,
      "Distribution"
    )

  })


  # --------------------------------------------------
  # Parameter controls
  # --------------------------------------------------

  output$parameter_controls <- renderUI({

    if (input$distribution == "Normal") {

      tagList(

        sliderInput(
          "mean",
          "Mean (μ)",
          min = -10,
          max = 10,
          value = 0,
          step = 0.5
        ),

        sliderInput(
          "sd",
          "Standard Deviation (σ)",
          min = 0.5,
          max = 5,
          value = 1,
          step = 0.1
        )
      )


    } else if (input$distribution == "Geometric") {

      sliderInput(
        "geom_p",
        "Probability of Success (p)",
        min = 0.05,
        max = 0.95,
        value = 0.30,
        step = 0.05
      )


    } else if (input$distribution == "Binomial") {

      tagList(

        sliderInput(
          "binom_n",
          "Number of Trials (n)",
          min = 1,
          max = 100,
          value = 20,
          step = 1
        ),

        sliderInput(
          "binom_p",
          "Probability of Success (p)",
          min = 0.05,
          max = 0.95,
          value = 0.50,
          step = 0.05
        )
      )


    } else if (input$distribution == "Negative Binomial") {

      tagList(

        sliderInput(
          "nb_r",
          "Number of Successes (r)",
          min = 1,
          max = 20,
          value = 5,
          step = 1
        ),

        sliderInput(
          "nb_p",
          "Probability of Success (p)",
          min = 0.05,
          max = 0.95,
          value = 0.50,
          step = 0.05
        )
      )


    } else {

      sliderInput(
        "pois_lambda",
        "Rate (λ)",
        min = 0.5,
        max = 30,
        value = 5,
        step = 0.5
      )
    }

  })


  # --------------------------------------------------
  # Parameter descriptions
  # --------------------------------------------------

  output$parameter_description <- renderUI({

    description <- switch(

      input$distribution,

      "Normal" =
        paste0(
          "<b>μ</b> controls the center of the distribution.<br>",
          "<b>σ</b> controls the spread."
        ),

      "Geometric" =
        paste0(
          "<b>p</b> is the probability of success ",
          "on each independent trial."
        ),

      "Binomial" =
        paste0(
          "<b>n</b> is the number of independent trials.<br>",
          "<b>p</b> is the probability of success on each trial."
        ),

      "Negative Binomial" =
        paste0(
          "<b>r</b> is the target number of successes.<br>",
          "<b>p</b> is the probability of success on each independent trial."
        ),

      "Poisson" =
        paste0(
          "<b>λ</b> is the average number of events ",
          "occurring in the specified interval."
        )
    )

    div(
      class = "parameter-note",
      HTML(description)
    )

  })


  # --------------------------------------------------
  # Mean and standard deviation
  # --------------------------------------------------

  output$summary_statistics <- renderUI({

    dist <- input$distribution


    # ==================================================
    # NORMAL
    # ==================================================

    if (dist == "Normal") {

      req(
        input$mean,
        input$sd
      )

      mu <- input$mean
      sigma <- input$sd

      mean_formula <- "μ"

      mean_calc <- sprintf(
        "%.2f",
        mu
      )

      sd_formula <- "σ"

      sd_calc <- sprintf(
        "%.2f",
        sigma
      )


    # ==================================================
    # GEOMETRIC
    # X = trial number of first success
    # ==================================================

    } else if (dist == "Geometric") {

      req(input$geom_p)

      p <- input$geom_p

      mean_value <- 1 / p

      sd_value <- sqrt(1 - p) / p

      mean_formula <- "1 / p"

      mean_calc <- sprintf(
        "1 / %.2f = %.2f",
        p,
        mean_value
      )

      sd_formula <- "√(1 − p) / p"

      sd_calc <- sprintf(
        "√(1 − %.2f) / %.2f = %.2f",
        p,
        p,
        sd_value
      )


    # ==================================================
    # BINOMIAL
    # ==================================================

    } else if (dist == "Binomial") {

      req(
        input$binom_n,
        input$binom_p
      )

      n <- input$binom_n
      p <- input$binom_p

      mean_value <- n * p

      sd_value <- sqrt(
        n * p * (1 - p)
      )

      mean_formula <- "np"

      mean_calc <- sprintf(
        "%d(%.2f) = %.2f",
        n,
        p,
        mean_value
      )

      sd_formula <- "√[np(1 − p)]"

      sd_calc <- sprintf(
        "√[%d(%.2f)(1 − %.2f)] = %.2f",
        n,
        p,
        p,
        sd_value
      )


    # ==================================================
    # NEGATIVE BINOMIAL
    # X = trial number of the rth success
    # ==================================================

    } else if (dist == "Negative Binomial") {

      req(
        input$nb_r,
        input$nb_p
      )

      r <- input$nb_r
      p <- input$nb_p

      mean_value <- r / p

      sd_value <- sqrt(
        r * (1 - p)
      ) / p

      mean_formula <- "r / p"

      mean_calc <- sprintf(
        "%d / %.2f = %.2f",
        r,
        p,
        mean_value
      )

      sd_formula <- "√[r(1 − p)] / p"

      sd_calc <- sprintf(
        "√[%d(1 − %.2f)] / %.2f = %.2f",
        r,
        p,
        p,
        sd_value
      )


    # ==================================================
    # POISSON
    # ==================================================

    } else {

      req(input$pois_lambda)

      lambda <- input$pois_lambda

      mean_value <- lambda

      sd_value <- sqrt(lambda)

      mean_formula <- "λ"

      mean_calc <- sprintf(
        "%.2f",
        mean_value
      )

      sd_formula <- "√λ"

      sd_calc <- sprintf(
        "√%.2f = %.2f",
        lambda,
        sd_value
      )
    }


    div(
      class = "summary-box",

      div(
        class = "summary-title",
        "Mean and Standard Deviation"
      ),

      tags$hr(
        class = "summary-divider"
      ),

      HTML(
        paste0(

          "<b>Mean:</b> ",
          mean_formula,

          "<br>",

          mean_calc,

          "<br><br>",

          "<b>Standard Deviation:</b> ",
          sd_formula,

          "<br>",

          sd_calc
        )
      )
    )

  })


  # --------------------------------------------------
  # Distribution plot
  # --------------------------------------------------

  output$distribution_plot <- renderPlot({

    dist <- input$distribution

    par(
      mar = c(4.2, 4.4, 1.0, 0.8),
      las = 1
    )


    # ==================================================
    # NORMAL
    # ==================================================

    if (dist == "Normal") {

      req(
        input$mean,
        input$sd
      )

      mu <- input$mean
      sigma <- input$sd

      x <- seq(
        mu - 4 * sigma,
        mu + 4 * sigma,
        length.out = 500
      )

      y <- dnorm(
        x,
        mean = mu,
        sd = sigma
      )

      plot(
        x,
        y,
        type = "l",
        lwd = 3,
        xlab = "x",
        ylab = "Density",
        main = "",
        ylim = c(
          0,
          max(y) * 1.12
        )
      )

      abline(
        v = mu,
        lty = 2,
        lwd = 2
      )

      text(
        x = mu,
        y = max(y) * 1.06,
        labels = "Mean",
        pos = 4,
        cex = 0.85
      )


    # ==================================================
    # GEOMETRIC
    # ==================================================

    } else if (dist == "Geometric") {

      req(input$geom_p)

      p <- input$geom_p

      mean_value <- 1 / p

      max_x <- max(
        10,
        qgeom(
          0.995,
          prob = p
        )
      )

      x <- 1:(max_x + 1)

      y <- dgeom(
        x - 1,
        prob = p
      )

      plot(
        x,
        y,
        type = "h",
        lwd = 5,
        lend = 1,
        xlab = "Trial Number of First Success",
        ylab = "Probability",
        main = "",
        ylim = c(
          0,
          max(y) * 1.16
        )
      )

      points(
        x,
        y,
        pch = 16,
        cex = 0.9
      )

      abline(
        v = mean_value,
        lty = 2,
        lwd = 2
      )

      text(
        x = mean_value,
        y = max(y) * 1.09,
        labels = "Mean",
        pos = 4,
        cex = 0.85
      )


    # ==================================================
    # BINOMIAL
    # ==================================================

    } else if (dist == "Binomial") {

      req(
        input$binom_n,
        input$binom_p
      )

      n <- input$binom_n
      p <- input$binom_p

      mean_value <- n * p

      x <- 0:n

      y <- dbinom(
        x,
        size = n,
        prob = p
      )

      plot(
        x,
        y,
        type = "h",
        lwd = 5,
        lend = 1,
        xlab = "Number of Successes",
        ylab = "Probability",
        main = "",
        ylim = c(
          0,
          max(y) * 1.16
        )
      )

      points(
        x,
        y,
        pch = 16,
        cex = 0.85
      )

      abline(
        v = mean_value,
        lty = 2,
        lwd = 2
      )

      text(
        x = mean_value,
        y = max(y) * 1.09,
        labels = "Mean",
        pos = 4,
        cex = 0.85
      )


    # ==================================================
    # NEGATIVE BINOMIAL
    # ==================================================

    } else if (dist == "Negative Binomial") {

      req(
        input$nb_r,
        input$nb_p
      )

      r <- input$nb_r
      p <- input$nb_p

      mean_value <- r / p

      max_failures <- max(
        10,
        qnbinom(
          0.995,
          size = r,
          prob = p
        )
      )

      failures <- 0:max_failures

      y <- dnbinom(
        failures,
        size = r,
        prob = p
      )

      trials <- failures + r

      plot(
        trials,
        y,
        type = "h",
        lwd = 5,
        lend = 1,

        xlab = paste(
          "Trial Number of the",
          r,
          "th Success"
        ),

        ylab = "Probability",
        main = "",

        ylim = c(
          0,
          max(y) * 1.16
        )
      )

      points(
        trials,
        y,
        pch = 16,
        cex = 0.85
      )

      abline(
        v = mean_value,
        lty = 2,
        lwd = 2
      )

      text(
        x = mean_value,
        y = max(y) * 1.09,
        labels = "Mean",
        pos = 4,
        cex = 0.85
      )


    # ==================================================
    # POISSON
    # ==================================================

    } else {

      req(input$pois_lambda)

      lambda <- input$pois_lambda

      mean_value <- lambda

      max_x <- max(
        10,
        qpois(
          0.995,
          lambda = lambda
        )
      )

      x <- 0:max_x

      y <- dpois(
        x,
        lambda = lambda
      )

      plot(
        x,
        y,
        type = "h",
        lwd = 5,
        lend = 1,
        xlab = "Number of Events",
        ylab = "Probability",
        main = "",
        ylim = c(
          0,
          max(y) * 1.16
        )
      )

      points(
        x,
        y,
        pch = 16,
        cex = 0.85
      )

      abline(
        v = mean_value,
        lty = 2,
        lwd = 2
      )

      text(
        x = mean_value,
        y = max(y) * 1.09,
        labels = "Mean",
        pos = 4,
        cex = 0.85
      )
    }

  })

}


shinyApp(ui, server)
