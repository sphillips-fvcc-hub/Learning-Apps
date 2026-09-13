library(shiny)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

trig_value <- function(fun, z) {
  switch(
    fun,
    sine      = sin(z),
    cosine    = cos(z),
    tangent   = tan(z),
    cotangent = 1 / tan(z),
    secant    = 1 / cos(z),
    cosecant  = 1 / sin(z)
  )
}

parent_period <- function(fun) {
  if (fun %in% c("tangent", "cotangent")) {
    pi
  } else {
    2 * pi
  }
}

display_name <- function(fun) {
  switch(
    fun,
    sine      = "sin",
    cosine    = "cos",
    tangent   = "tan",
    cotangent = "cot",
    secant    = "sec",
    cosecant  = "csc"
  )
}

clean_discontinuous_curve <- function(y, fun) {

  y[!is.finite(y)] <- NA

  if (fun %in% c(
    "tangent",
    "cotangent",
    "secant",
    "cosecant"
  )) {

    # Remove values close to vertical asymptotes
    y[abs(y) > 20] <- NA

    # Break the curve when adjacent values jump sharply
    jumps <- which(abs(diff(y)) > 8)

    if (length(jumps) > 0) {

      break_points <- unique(c(
        jumps,
        jumps + 1
      ))

      break_points <- break_points[
        break_points >= 1 &
          break_points <= length(y)
      ]

      y[break_points] <- NA
    }
  }

  y
}


# ------------------------------------------------------------
# b values and labels
# ------------------------------------------------------------

b_multipliers <- -8:8

b_values <- b_multipliers * pi / 4

b_labels <- c(
  "-2\u03C0",
  "-7\u03C0/4",
  "-3\u03C0/2",
  "-5\u03C0/4",
  "-\u03C0",
  "-3\u03C0/4",
  "-\u03C0/2",
  "-\u03C0/4",
  "0",
  "\u03C0/4",
  "\u03C0/2",
  "3\u03C0/4",
  "\u03C0",
  "5\u03C0/4",
  "3\u03C0/2",
  "7\u03C0/4",
  "2\u03C0"
)

b_choices <- setNames(
  as.character(b_values),
  b_labels
)


# ------------------------------------------------------------
# User interface
# ------------------------------------------------------------

ui <- fluidPage(

  titlePanel("Trigonometric Function Explorer"),

  sidebarLayout(

    sidebarPanel(

      # --------------------------------------------------------
      # Function selector
      # --------------------------------------------------------

      radioButtons(
        inputId = "trig_fun",
        label = "Choose a trigonometric function:",
        choices = c(
          "Sine" = "sine",
          "Cosine" = "cosine",
          "Tangent" = "tangent",
          "Cotangent" = "cotangent",
          "Secant" = "secant",
          "Cosecant" = "cosecant"
        ),
        selected = "sine"
      ),

      hr(),

      uiOutput("formula"),

      hr(),

      sliderInput(
        inputId = "a",
        label = "a",
        min = -4,
        max = 4,
        value = 1,
        step = 0.25
      ),

      sliderInput(
        inputId = "k",
        label = "k",
        min = -4,
        max = 4,
        value = 1,
        step = 0.25
      ),

      selectInput(
        inputId = "b",
        label = "b",
        choices = b_choices,
        selected = as.character(0)
      ),

      sliderInput(
        inputId = "d",
        label = "d",
        min = -4,
        max = 4,
        value = 0,
        step = 0.25
      ),

      actionButton(
        inputId = "reset",
        label = "Reset"
      ),

      hr(),

      uiOutput("parameter_info")
    ),

    mainPanel(

      plotOutput(
        outputId = "trig_plot",
        height = "600px"
      )
    )
  )
)


# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {

  # Convert selected b value back to numeric
  b_numeric <- reactive({
    as.numeric(input$b)
  })


  # ----------------------------------------------------------
  # Reset controls
  # ----------------------------------------------------------

  observeEvent(input$reset, {

    updateRadioButtons(
      session,
      "trig_fun",
      selected = "sine"
    )

    updateSliderInput(
      session,
      "a",
      value = 1
    )

    updateSliderInput(
      session,
      "k",
      value = 1
    )

    updateSelectInput(
      session,
      "b",
      selected = as.character(0)
    )

    updateSliderInput(
      session,
      "d",
      value = 0
    )
  })


  # ----------------------------------------------------------
  # Formula shown in sidebar
  # ----------------------------------------------------------

  output$formula <- renderUI({

    f <- display_name(input$trig_fun)

    HTML(
      paste0(
        "<div style='font-size: 20px;'>",
        "<strong>y = a ",
        f,
        "(kx - b) + d</strong>",
        "</div>"
      )
    )
  })


  # ----------------------------------------------------------
  # Parameter information
  # ----------------------------------------------------------

  output$parameter_info <- renderUI({

    req(input$trig_fun)

    b <- b_numeric()

    if (input$k == 0) {

      period_text <-
        "The usual period is not defined when k = 0."

      shift_text <-
        "The usual horizontal shift is not defined when k = 0."

    } else {

      base_period <- parent_period(input$trig_fun)

      period <- base_period / abs(input$k)

      shift <- b / input$k

      period_text <- paste0(
        "Period = ",
        round(period / pi, 3),
        "\u03C0"
      )

      shift_text <- paste0(
        "Horizontal shift = ",
        round(shift / pi, 3),
        "\u03C0"
      )
    }

    if (input$trig_fun %in% c(
      "sine",
      "cosine"
    )) {

      a_text <- paste0(
        "Amplitude = ",
        abs(input$a)
      )

    } else {

      a_text <- paste0(
        "Vertical scale factor = ",
        input$a
      )
    }

    tagList(

      strong("Transformation information"),

      br(),
      a_text,

      br(),
      period_text,

      br(),
      shift_text,

      br(),
      paste0(
        "Vertical shift = ",
        input$d
      )
    )
  })


  # ----------------------------------------------------------
  # Main graph
  # ----------------------------------------------------------

  output$trig_plot <- renderPlot({

    fun <- input$trig_fun

    a <- input$a
    k <- input$k
    b <- b_numeric()
    d <- input$d

    x <- seq(
      -4 * pi,
      4 * pi,
      length.out = 8001
    )


    # --------------------------------------------------------
    # Parent function
    # --------------------------------------------------------

    y_parent <- trig_value(
      fun,
      x
    )

    y_parent <- clean_discontinuous_curve(
      y_parent,
      fun
    )


    # --------------------------------------------------------
    # Transformed function
    # --------------------------------------------------------

    z <- k * x - b

    y_transformed <-
      a * trig_value(fun, z) + d

    y_transformed <- clean_discontinuous_curve(
      y_transformed,
      fun
    )


    # --------------------------------------------------------
    # Empty plot
    # --------------------------------------------------------

    plot(
      NA,
      xlim = c(-4 * pi, 4 * pi),
      ylim = c(-4, 4),
      xlab = "x",
      ylab = "y",
      xaxt = "n",
      yaxt = "n",
      bty = "n"
    )


    # --------------------------------------------------------
    # Grid
    # --------------------------------------------------------

    x_grid <- seq(
      -4 * pi,
      4 * pi,
      by = pi / 4
    )

    y_grid <- seq(
      -4,
      4,
      by = 1
    )

    abline(
      v = x_grid,
      h = y_grid,
      col = "gray90",
      lty = 1
    )


    # Main axes
    abline(
      h = 0,
      v = 0,
      col = "gray30"
    )


    # --------------------------------------------------------
    # x-axis labels in terms of pi
    # --------------------------------------------------------

    x_ticks <- seq(
      -4 * pi,
      4 * pi,
      by = pi / 2
    )

    x_labels <- expression(
      -4*pi,
      -7*pi/2,
      -3*pi,
      -5*pi/2,
      -2*pi,
      -3*pi/2,
      -pi,
      -pi/2,
      0,
      pi/2,
      pi,
      3*pi/2,
      2*pi,
      5*pi/2,
      3*pi,
      7*pi/2,
      4*pi
    )

    axis(
      side = 1,
      at = x_ticks,
      labels = x_labels,
      cex.axis = 0.85
    )


    # --------------------------------------------------------
    # y-axis
    # --------------------------------------------------------

    axis(
      side = 2,
      at = -4:4,
      las = 1
    )


    # --------------------------------------------------------
    # Parent function
    # --------------------------------------------------------

    lines(
      x,
      y_parent,
      lwd = 2,
      lty = 1
    )


    # --------------------------------------------------------
    # Transformed function
    # --------------------------------------------------------

    lines(
      x,
      y_transformed,
      lwd = 3,
      lty = 2
    )


    # --------------------------------------------------------
    # Legend
    # --------------------------------------------------------

    f <- display_name(fun)

    legend(
      "topright",
      legend = c(
        paste0(
          "Parent: y = ",
          f,
          "(x)"
        ),
        paste0(
          "Transformed: y = a ",
          f,
          "(kx - b) + d"
        )
      ),
      lty = c(1, 2),
      lwd = c(2, 3),
      bty = "n"
    )
  })
}


# ------------------------------------------------------------
# Run app
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
