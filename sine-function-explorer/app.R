library(shiny)

# ------------------------------------------------------------
# M151 Trigonometric Function Explorer
# Sine Function
#
# Transformation:
#
# y = a sin(kx - b) + d
# ------------------------------------------------------------


# ------------------------------------------------------------
# Choices for b
#
# b ranges from -2pi to 2pi in increments of pi/4
# ------------------------------------------------------------

b_choices <- c(
  "-2\u03c0"     = "-8",
  "-7\u03c0/4"   = "-7",
  "-3\u03c0/2"   = "-6",
  "-5\u03c0/4"   = "-5",
  "-\u03c0"      = "-4",
  "-3\u03c0/4"   = "-3",
  "-\u03c0/2"    = "-2",
  "-\u03c0/4"    = "-1",
  "0"            = "0",
  "\u03c0/4"     = "1",
  "\u03c0/2"     = "2",
  "3\u03c0/4"    = "3",
  "\u03c0"       = "4",
  "5\u03c0/4"    = "5",
  "3\u03c0/2"    = "6",
  "7\u03c0/4"    = "7",
  "2\u03c0"      = "8"
)


# ------------------------------------------------------------
# Function for displaying b nicely
# ------------------------------------------------------------

b_label <- function(n) {
  
  labels <- c(
    "-8" = "-2\u03c0",
    "-7" = "-7\u03c0/4",
    "-6" = "-3\u03c0/2",
    "-5" = "-5\u03c0/4",
    "-4" = "-\u03c0",
    "-3" = "-3\u03c0/4",
    "-2" = "-\u03c0/2",
    "-1" = "-\u03c0/4",
    "0"  = "0",
    "1"  = "\u03c0/4",
    "2"  = "\u03c0/2",
    "3"  = "3\u03c0/4",
    "4"  = "\u03c0",
    "5"  = "5\u03c0/4",
    "6"  = "3\u03c0/2",
    "7"  = "7\u03c0/4",
    "8"  = "2\u03c0"
  )
  
  labels[as.character(n)]
}


# ------------------------------------------------------------
# Function for x-axis labels
#
# Used for labels every pi/2
# ------------------------------------------------------------

make_pi_label <- function(n) {
  
  if (n == 0) {
    return(expression(0)[[1]])
  }
  
  sign_value <- ifelse(n < 0, -1, 1)
  n_abs <- abs(n)
  
  # Here n represents multiples of pi/2
  
  if (n_abs %% 2 == 0) {
    
    coefficient <- n_abs / 2
    
    if (coefficient == 1) {
      
      if (sign_value < 0) {
        return(expression(-pi)[[1]])
      } else {
        return(expression(pi)[[1]])
      }
      
    } else {
      
      if (sign_value < 0) {
        return(
          substitute(
            -A * pi,
            list(A = coefficient)
          )
        )
      } else {
        return(
          substitute(
            A * pi,
            list(A = coefficient)
          )
        )
      }
    }
    
  } else {
    
    if (n_abs == 1) {
      
      if (sign_value < 0) {
        return(expression(-pi / 2)[[1]])
      } else {
        return(expression(pi / 2)[[1]])
      }
      
    } else {
      
      if (sign_value < 0) {
        return(
          substitute(
            -(A * pi) / 2,
            list(A = n_abs)
          )
        )
      } else {
        return(
          substitute(
            (A * pi) / 2,
            list(A = n_abs)
          )
        )
      }
    }
  }
}


# ------------------------------------------------------------
# User Interface
# ------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      HTML("
      
      body {
        font-size: 18px;
      }
      
      .app-panel {
        max-width: 1180px;
        margin-left: auto;
        margin-right: auto;
        margin-top: 10px;
        margin-bottom: 20px;
        padding: 18px 22px 20px 22px;
        border: 2px solid #777;
        border-radius: 6px;
      }
      
      .formula-box {
        text-align: center;
        font-size: 24px;
        font-weight: bold;
        margin-top: 4px;
        margin-bottom: 14px;
      }
      
      .instruction {
        text-align: center;
        font-size: 17px;
        margin-bottom: 15px;
      }
      
      .parameter-panel {
        padding-right: 15px;
      }
      
      .parameter-heading {
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 10px;
      }
      
      .current-function {
        margin-top: 18px;
        font-size: 17px;
        font-weight: bold;
        text-align: center;
        line-height: 1.6;
      }
      
      .graph-panel {
        padding-left: 10px;
      }
      
      .graph-note {
        text-align: center;
        font-size: 15px;
        margin-top: 4px;
      }
      
      .form-control {
        font-size: 17px;
      }
      
      @media (max-width: 767px) {
        
        .parameter-panel,
        .graph-panel {
          padding-left: 0;
          padding-right: 0;
        }
        
        .graph-panel {
          margin-top: 18px;
        }
      }
      
      ")
    )
  ),
  
  
  titlePanel("M151 Sine Function Explorer"),
  
  
  div(
    class = "app-panel",
    
    
    # ----------------------------------------------------------
    # Formula
    # ----------------------------------------------------------
    
    div(
      class = "formula-box",
      "y = a sin(kx \u2212 b) + d"
    ),
    
    
    div(
      class = "instruction",
      paste(
        "Change the parameters below.",
        "The basic sine function will remain on the graph for comparison."
      )
    ),
    
    
    fluidRow(
      
      # --------------------------------------------------------
      # LEFT SIDE: Parameter controls
      # --------------------------------------------------------
      
      column(
        width = 3,
        
        div(
          class = "parameter-panel",
          
          div(
            class = "parameter-heading",
            "Parameters"
          ),
          
          
          numericInput(
            inputId = "a",
            label = "a",
            value = 1,
            step = 0.1
          ),
          
          
          numericInput(
            inputId = "k",
            label = "k",
            value = 1,
            step = 0.1
          ),
          
          
          selectInput(
            inputId = "b",
            label = "b",
            choices = b_choices,
            selected = "0"
          ),
          
          
          numericInput(
            inputId = "d",
            label = "d",
            value = 0,
            step = 0.1
          ),
          
          
          actionButton(
            inputId = "reset",
            label = "Reset to y = sin(x)"
          ),
          
          
          div(
            class = "current-function",
            textOutput("parameterText")
          )
        )
      ),
      
      
      # --------------------------------------------------------
      # RIGHT SIDE: Graph
      # --------------------------------------------------------
      
      column(
        width = 9,
        
        div(
          class = "graph-panel",
          
          plotOutput(
            outputId = "trigPlot",
            height = "570px"
          ),
          
          div(
            class = "graph-note",
            paste(
              "Graphing window:",
              "\u22124\u03c0 \u2264 x \u2264 4\u03c0 and \u22124 \u2264 y \u2264 4."
            )
          )
        )
      )
    )
  )
)


# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Numerical value of b
  #
  # input$b records the number of pi/4 units
  # ----------------------------------------------------------
  
  b_value <- reactive({
    
    as.numeric(input$b) * pi / 4
    
  })
  
  
  # ----------------------------------------------------------
  # Reset button
  # ----------------------------------------------------------
  
  observeEvent(
    input$reset,
    
    {
      
      updateNumericInput(
        session,
        "a",
        value = 1
      )
      
      updateNumericInput(
        session,
        "k",
        value = 1
      )
      
      updateSelectInput(
        session,
        "b",
        selected = "0"
      )
      
      updateNumericInput(
        session,
        "d",
        value = 0
      )
      
    }
  )
  
  
  # ----------------------------------------------------------
  # Determine whether function is still basic sine
  # ----------------------------------------------------------
  
  is_basic_sine <- reactive({
    
    req(
      input$a,
      input$k,
      input$b,
      input$d
    )
    
    abs(input$a - 1) < 1e-10 &&
      abs(input$k - 1) < 1e-10 &&
      abs(b_value()) < 1e-10 &&
      abs(input$d) < 1e-10
  })
  
  
  # ----------------------------------------------------------
  # Display current parameter values
  # ----------------------------------------------------------
  
  output$parameterText <- renderText({
    
    req(
      input$a,
      input$k,
      input$b,
      input$d
    )
    
    paste0(
      "a = ", input$a,
      "\n",
      "k = ", input$k,
      "\n",
      "b = ", b_label(input$b),
      "\n",
      "d = ", input$d
    )
  })
  
  
  # ----------------------------------------------------------
  # Trigonometric graph
  # ----------------------------------------------------------
  
  output$trigPlot <- renderPlot({
    
    req(
      input$a,
      input$k,
      input$b,
      input$d
    )
    
    
    # --------------------------------------------------------
    # x-values
    # --------------------------------------------------------
    
    x <- seq(
      from = -4 * pi,
      to = 4 * pi,
      length.out = 6000
    )
    
    
    # --------------------------------------------------------
    # Basic sine function
    # --------------------------------------------------------
    
    y_basic <- sin(x)
    
    
    # --------------------------------------------------------
    # Transformed sine function
    #
    # y = a sin(kx - b) + d
    # --------------------------------------------------------
    
    y_new <- input$a *
      sin(
        input$k * x -
          b_value()
      ) +
      input$d
    
    
    # --------------------------------------------------------
    # Grid positions every pi/4
    #
    # -4pi to 4pi corresponds to
    # -16(pi/4) through 16(pi/4)
    # --------------------------------------------------------
    
    grid_n <- -16:16
    
    grid_ticks <- grid_n * pi / 4
    
    
    # --------------------------------------------------------
    # Label positions every pi/2
    #
    # -4pi to 4pi corresponds to
    # -8(pi/2) through 8(pi/2)
    # --------------------------------------------------------
    
    label_n <- -8:8
    
    label_ticks <- label_n * pi / 2
    
    x_labels <- as.expression(
      lapply(
        label_n,
        make_pi_label
      )
    )
    
    
    # --------------------------------------------------------
    # Graph margins
    # --------------------------------------------------------
    
    par(
      mar = c(6, 5, 2, 2) + 0.1
    )
    
    
    # --------------------------------------------------------
    # Start blank graph
    # --------------------------------------------------------
    
    plot(
      x,
      y_basic,
      type = "n",
      xlim = c(-4 * pi, 4 * pi),
      ylim = c(-4, 4),
      xlab = "",
      ylab = "",
      xaxt = "n",
      yaxt = "n",
      bty = "n"
    )
    
    
    # --------------------------------------------------------
    # Horizontal grid lines
    # --------------------------------------------------------
    
    abline(
      h = seq(-4, 4, by = 1),
      col = "gray90",
      lwd = 1
    )
    
    
    # --------------------------------------------------------
    # Vertical grid lines every pi/4
    # --------------------------------------------------------
    
    abline(
      v = grid_ticks,
      col = "gray92",
      lwd = 1
    )
    
    
    # --------------------------------------------------------
    # Slightly darker vertical lines at multiples of pi
    # --------------------------------------------------------
    
    abline(
      v = (-4:4) * pi,
      col = "gray82",
      lwd = 1
    )
    
    
    # --------------------------------------------------------
    # x-axis and y-axis
    # --------------------------------------------------------
    
    abline(
      h = 0,
      col = "gray40",
      lwd = 1.5
    )
    
    abline(
      v = 0,
      col = "gray40",
      lwd = 1.5
    )
    
    
    # --------------------------------------------------------
    # x-axis
    #
    # Grid/tick spacing is pi/4,
    # but labels are shown every pi/2.
    # --------------------------------------------------------
    
    axis(
      side = 1,
      at = grid_ticks,
      labels = FALSE,
      tck = -0.012
    )
    
    
    axis(
      side = 1,
      at = label_ticks,
      labels = x_labels,
      las = 2,
      cex.axis = 0.72,
      tck = 0
    )
    
    
    # --------------------------------------------------------
    # y-axis
    # --------------------------------------------------------
    
    axis(
      side = 2,
      at = -4:4,
      labels = -4:4,
      las = 1,
      tck = -0.015
    )
    
    
    # --------------------------------------------------------
    # Axis labels
    # --------------------------------------------------------
    
    mtext(
      "x",
      side = 1,
      line = 4.8,
      cex = 1.1
    )
    
    mtext(
      "y",
      side = 2,
      line = 3.2,
      cex = 1.1
    )
    
    
    # --------------------------------------------------------
    # Draw functions
    #
    # Initially:
    #     y = sin(x) is solid
    #
    # After a parameter changes:
    #     y = sin(x) is dashed
    #     transformed function is solid
    # --------------------------------------------------------
    
    if (is_basic_sine()) {
      
      lines(
        x,
        y_basic,
        lwd = 3,
        lty = 1
      )
      
      
      legend(
        "topright",
        legend = "y = sin(x)",
        lty = 1,
        lwd = 3,
        bty = "n"
      )
      
    } else {
      
      # Original sine function
      
      lines(
        x,
        y_basic,
        lwd = 2,
        lty = 2
      )
      
      
      # Transformed sine function
      
      lines(
        x,
        y_new,
        lwd = 3,
        lty = 1
      )
      
      
      legend(
        "topright",
        legend = c(
          "Basic y = sin(x)",
          "Transformed function"
        ),
        lty = c(2, 1),
        lwd = c(2, 3),
        bty = "n"
      )
    }
    
  })
}


# ------------------------------------------------------------
# Run App
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
