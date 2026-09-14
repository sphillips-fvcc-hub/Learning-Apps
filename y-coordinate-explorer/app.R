library(shiny)

# ------------------------------------------------------------
# M151 Unit Circle y-Coordinate Explorer
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
      
      .explorer-row {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
      }
      
      #circle-container {
        width: 100%;
        max-width: 500px;
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
      
      .vertical-slider-area {
        width: 95px;
        height: 410px;
        position: relative;
        flex-shrink: 0;
      }
      
      .vertical-slider-label {
        text-align: center;
        font-weight: bold;
        margin-bottom: 8px;
      }
      
      #y-slider {
        writing-mode: vertical-lr;
        direction: rtl;
        width: 28px;
        height: 290px;
        position: absolute;
        left: 33px;
        top: 82px;
        cursor: pointer;
      }
      
      .slider-max {
        position: absolute;
        top: 63px;
        left: 7px;
        width: 24px;
        text-align: center;
        font-size: 14px;
      }
      
      .slider-zero {
        position: absolute;
        top: 220px;
        left: 7px;
        width: 24px;
        text-align: center;
        font-size: 14px;
      }
      
      .slider-min {
        position: absolute;
        top: 378px;
        left: 7px;
        width: 24px;
        text-align: center;
        font-size: 14px;
      }
      
      .y-value {
        text-align: center;
        font-size: 22px;
        font-weight: bold;
        margin-top: 2px;
      }
      
      .coordinate-display {
        text-align: center;
        font-size: 20px;
        margin-top: 5px;
      }
      
      ")
    )
  ),
  
  
  titlePanel("M151 Unit Circle y-Coordinate Explorer"),
  
  
  div(
    class = "app-panel",
    
    div(
      class = "instruction",
      "Move the slider and watch where that y-coordinate occurs on the unit circle."
    ),
    
    
    # ----------------------------------------------------------
    # Circle and vertical slider
    # ----------------------------------------------------------
    
    div(
      class = "explorer-row",
      
      
      # --------------------------------------------------------
      # Vertical y-coordinate slider
      # --------------------------------------------------------
      
      div(
        class = "vertical-slider-area",
        
        div(
          class = "vertical-slider-label",
          "y-coordinate"
        ),
        
        div(
          class = "slider-max",
          "1"
        ),
        
        div(
          class = "slider-zero",
          "0"
        ),
        
        tags$input(
          id = "y-slider",
          type = "range",
          min = "-1",
          max = "1",
          step = "0.001",
          value = "0.5"
        ),
        
        div(
          class = "slider-min",
          "-1"
        )
      ),
      
      
      # --------------------------------------------------------
      # Unit circle
      # --------------------------------------------------------
      
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
          
          
          # Horizontal guide through selected y-coordinate
          
          tags$line(
            id = "y-guide",
            class = "guide-line"
          ),
          
          
          # Left point
          
          tags$circle(
            id = "left-point",
            class = "selected-point",
            r = "11"
          ),
          
          
          # Right point
          
          tags$circle(
            id = "right-point",
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
      )
    ),
    
    
    # ----------------------------------------------------------
    # Value and coordinate display
    # ----------------------------------------------------------
    
    div(
      class = "y-value",
      textOutput("yText")
    ),
    
    div(
      class = "coordinate-display",
      uiOutput("coordinateText")
    )
  ),
  
  
  # ------------------------------------------------------------
  # JavaScript
  #
  # The native vertical slider sends its value to Shiny.
  # Shiny then returns the point positions to the SVG.
  # ------------------------------------------------------------
  
  tags$script(
    HTML("
    
    const ySlider =
      document.getElementById('y-slider');
    
    
    // --------------------------------------------------------
    // Send slider value to Shiny
    // --------------------------------------------------------
    
    function sendYValue() {
      
      const yValue =
        parseFloat(ySlider.value);
      
      Shiny.setInputValue(
        'y_value',
        yValue,
        {
          priority: 'event'
        }
      );
    }
    
    
    ySlider.addEventListener(
      'input',
      sendYValue
    );
    
    
    // Send the initial value once Shiny is connected
    
    $(document).on(
      'shiny:connected',
      function() {
        sendYValue();
      }
    );
    
    
    // --------------------------------------------------------
    // Move points on the unit circle
    // --------------------------------------------------------
    
    Shiny.addCustomMessageHandler(
      'moveYPoints',
      
      function(message) {
        
        const cx = 300;
        const cy = 260;
        const r = 170;
        
        const yValue = message.y;
        
        
        // Convert mathematical y-coordinate to SVG position.
        //
        // SVG y-values increase downward, so we subtract.
        
        const svgY =
          cy - r * yValue;
        
        
        // On the unit circle:
        //
        // x^2 + y^2 = 1
        //
        // so
        //
        // x = +/- sqrt(1 - y^2)
        
        const xValue =
          Math.sqrt(
            Math.max(
              0,
              1 - yValue * yValue
            )
          );
        
        
        const leftX =
          cx - r * xValue;
        
        const rightX =
          cx + r * xValue;
        
        
        const leftPoint =
          document.getElementById(
            'left-point'
          );
        
        const rightPoint =
          document.getElementById(
            'right-point'
          );
        
        const guide =
          document.getElementById(
            'y-guide'
          );
        
        
        // Move left point
        
        leftPoint.setAttribute(
          'cx',
          leftX
        );
        
        leftPoint.setAttribute(
          'cy',
          svgY
        );
        
        
        // Move right point
        
        rightPoint.setAttribute(
          'cx',
          rightX
        );
        
        rightPoint.setAttribute(
          'cy',
          svgY
        );
        
        
        // Horizontal guide connects the two points
        
        guide.setAttribute(
          'x1',
          leftX
        );
        
        guide.setAttribute(
          'x2',
          rightX
        );
        
        guide.setAttribute(
          'y1',
          svgY
        );
        
        guide.setAttribute(
          'y2',
          svgY
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
  # Move highlighted points whenever y changes
  # ----------------------------------------------------------
  
  observeEvent(
    input$y_value,
    
    {
      
      session$sendCustomMessage(
        type = "moveYPoints",
        message = list(
          y = input$y_value
        )
      )
      
    },
    
    ignoreInit = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Display y-value
  # ----------------------------------------------------------
  
  output$yText <- renderText({
    
    req(input$y_value)
    
    paste0(
      "y = ",
      sprintf(
        "%.3f",
        input$y_value
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # Display corresponding coordinates
  # ----------------------------------------------------------
  
  output$coordinateText <- renderUI({
    
    req(input$y_value)
    
    y <- input$y_value
    
    x <- sqrt(
      max(
        0,
        1 - y^2
      )
    )
    
    
    # At y = +/-1 there is only one point
    
    if (abs(x) < 0.0000001) {
      
      HTML(
        paste0(
          "Point on the unit circle: (0, ",
          sprintf(
            "%.3f",
            y
          ),
          ")"
        )
      )
      
    } else {
      
      HTML(
        paste0(
          "Points on the unit circle: (",
          sprintf(
            "%.3f",
            x
          ),
          ", ",
          sprintf(
            "%.3f",
            y
          ),
          ") and (",
          sprintf(
            "%.3f",
            -x
          ),
          ", ",
          sprintf(
            "%.3f",
            y
          ),
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
