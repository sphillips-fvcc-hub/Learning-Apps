library(shiny)

# ------------------------------------------------------------
# M151 Unit Circle x-Coordinate Explorer
# ------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      HTML("
      
      body {
        font-size: 18px;
      }
      
      .app-panel {
        max-width: 720px;
        margin-left: auto;
        margin-right: auto;
        margin-top: 12px;
        padding: 16px 20px 20px 20px;
        border: 2px solid #777;
        border-radius: 6px;
      }
      
      .instruction {
        text-align: center;
        font-size: 18px;
        margin-bottom: 4px;
      }
      
      #circle-container {
        width: 100%;
        max-width: 500px;
        margin-left: auto;
        margin-right: auto;
      }
      
      #unit-circle-svg {
        width: 100%;
        height: auto;
        display: block;
      }
      
      .unit-circle {
        fill: none;
        stroke: #444;
        stroke-width: 3;
      }
      
      .axis-line {
        stroke: #777;
        stroke-width: 2;
      }
      
      .guide-line {
        stroke: #888;
        stroke-width: 2;
        stroke-dasharray: 7 7;
      }
      
      .selected-point {
        fill: #222;
        stroke: white;
        stroke-width: 3;
      }
      
      .axis-label {
        font-size: 21px;
        font-weight: bold;
      }
      
      .tick-label {
        font-size: 17px;
      }
      
      .slider-area {
        max-width: 520px;
        margin-left: auto;
        margin-right: auto;
        margin-top: 2px;
      }
      
      .x-value {
        text-align: center;
        font-size: 22px;
        font-weight: bold;
        margin-top: 4px;
      }
      
      .coordinate-display {
        text-align: center;
        font-size: 20px;
        margin-top: 5px;
      }
      
      .form-group {
        margin-bottom: 4px;
      }
      
      ")
    )
  ),
  
  
  titlePanel("M151 Unit Circle x-Coordinate Explorer"),
  
  
  div(
    class = "app-panel",
    
    div(
      class = "instruction",
      "Move the slider and watch where that x-coordinate occurs on the unit circle."
    ),
    
    
    # ----------------------------------------------------------
    # Unit circle
    # ----------------------------------------------------------
    
    div(
      id = "circle-container",
      
      tags$svg(
        
        id = "unit-circle-svg",
        viewBox = "50 50 500 440",
        
        
        # Horizontal axis
        
        tags$line(
          x1 = "90",
          y1 = "260",
          x2 = "510",
          y2 = "260",
          class = "axis-line"
        ),
        
        
        # Vertical axis
        
        tags$line(
          x1 = "300",
          y1 = "50",
          x2 = "300",
          y2 = "470",
          class = "axis-line"
        ),
        
        
        # Unit circle
        
        tags$circle(
          cx = "300",
          cy = "260",
          r = "170",
          class = "unit-circle"
        ),
        
        
        # Vertical guide through selected x-coordinate
        
        tags$line(
          id = "x-guide",
          class = "guide-line"
        ),
        
        
        # Upper point
        
        tags$circle(
          id = "upper-point",
          class = "selected-point",
          r = "11"
        ),
        
        
        # Lower point
        
        tags$circle(
          id = "lower-point",
          class = "selected-point",
          r = "11"
        ),
        
        
        # x-axis label
        
        tags$text(
          x = "505",
          y = "247",
          class = "axis-label",
          "x"
        ),
        
        
        # y-axis label
        
        tags$text(
          x = "315",
          y = "70",
          class = "axis-label",
          "y"
        ),
        
        
        # x-axis tick labels
        
        tags$text(
          x = "122",
          y = "286",
          class = "tick-label",
          "-1"
        ),
        
        tags$text(
          x = "296",
          y = "286",
          class = "tick-label",
          "0"
        ),
        
        tags$text(
          x = "465",
          y = "286",
          class = "tick-label",
          "1"
        ),
        
        
        # y-axis tick labels
        
        tags$text(
          x = "315",
          y = "96",
          class = "tick-label",
          "1"
        ),
        
        tags$text(
          x = "310",
          y = "438",
          class = "tick-label",
          "-1"
        )
      )
    ),
    
    
    # ----------------------------------------------------------
    # Slider beneath the circle
    # ----------------------------------------------------------
    
    div(
      class = "slider-area",
      
      sliderInput(
        inputId = "x_value",
        label = "x-coordinate",
        min = -1,
        max = 1,
        value = 0.5,
        step = 0.001
      ),
      
      div(
        class = "x-value",
        textOutput("xText")
      ),
      
      div(
        class = "coordinate-display",
        uiOutput("coordinateText")
      )
    )
  ),
  
  
  # ------------------------------------------------------------
  # JavaScript
  #
  # This moves the two highlighted points on the SVG as
  # the Shiny slider changes.
  # ------------------------------------------------------------
  
  tags$script(
    HTML("
    
    Shiny.addCustomMessageHandler(
      'moveXPoints',
      
      function(message) {
        
        const cx = 300;
        const cy = 260;
        const r = 170;
        
        const xValue = message.x;
        
        // Convert mathematical x-coordinate to SVG position
        
        const svgX = cx + r * xValue;
        
        
        // On the unit circle:
        //
        // x^2 + y^2 = 1
        //
        // so
        //
        // y = +/- sqrt(1 - x^2)
        
        const yValue = Math.sqrt(
          Math.max(0, 1 - xValue * xValue)
        );
        
        
        const upperY = cy - r * yValue;
        const lowerY = cy + r * yValue;
        
        
        const upperPoint =
          document.getElementById('upper-point');
        
        const lowerPoint =
          document.getElementById('lower-point');
        
        const guide =
          document.getElementById('x-guide');
        
        
        // Move upper point
        
        upperPoint.setAttribute(
          'cx',
          svgX
        );
        
        upperPoint.setAttribute(
          'cy',
          upperY
        );
        
        
        // Move lower point
        
        lowerPoint.setAttribute(
          'cx',
          svgX
        );
        
        lowerPoint.setAttribute(
          'cy',
          lowerY
        );
        
        
        // Vertical guide connects the two points
        
        guide.setAttribute(
          'x1',
          svgX
        );
        
        guide.setAttribute(
          'x2',
          svgX
        );
        
        guide.setAttribute(
          'y1',
          upperY
        );
        
        guide.setAttribute(
          'y2',
          lowerY
        );
      }
    );
    
    ")
  )
)


# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Move highlighted points whenever x changes
  # ----------------------------------------------------------
  
  observeEvent(
    input$x_value,
    
    {
      
      session$sendCustomMessage(
        type = "moveXPoints",
        message = list(
          x = input$x_value
        )
      )
      
    },
    
    ignoreInit = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Display x-value
  # ----------------------------------------------------------
  
  output$xText <- renderText({
    
    paste0(
      "x = ",
      sprintf("%.3f", input$x_value)
    )
    
  })
  
  
  # ----------------------------------------------------------
  # Display corresponding coordinates
  # ----------------------------------------------------------
  
  output$coordinateText <- renderUI({
    
    x <- input$x_value
    
    y <- sqrt(
      max(0, 1 - x^2)
    )
    
    
    # At x = +/-1 there is only one point
    
    if (abs(y) < 0.0000001) {
      
      HTML(
        paste0(
          "Point on the unit circle: (",
          sprintf("%.3f", x),
          ", 0)"
        )
      )
      
    } else {
      
      HTML(
        paste0(
          "Points on the unit circle: (",
          sprintf("%.3f", x),
          ", ",
          sprintf("%.3f", y),
          ") and (",
          sprintf("%.3f", x),
          ", ",
          sprintf("%.3f", -y),
          ")"
        )
      )
    }
    
  })
}


# ------------------------------------------------------------
# Run app
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
