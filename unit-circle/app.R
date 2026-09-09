library(shiny)

# ------------------------------------------------------------
# M151 Unit Circle Explorer
# Coterminal-angle version
# ------------------------------------------------------------


# ------------------------------------------------------------
# Standard unit-circle positions
#
# Each row represents one standard angle from 0 through 2pi.
# Each radio-button label displays four coterminal angles.
# ------------------------------------------------------------

angles <- data.frame(
  
  id = paste0("a", 1:17),
  
  standard = c(
    "0",
    "π/6",
    "π/4",
    "π/3",
    "π/2",
    "2π/3",
    "3π/4",
    "5π/6",
    "π",
    "7π/6",
    "5π/4",
    "4π/3",
    "3π/2",
    "5π/3",
    "7π/4",
    "11π/6",
    "2π"
  ),
  
  menu_label = c(
    "0, −2π, 2π, −4π",
    "π/6, −11π/6, 13π/6, −23π/6",
    "π/4, −7π/4, 9π/4, −15π/4",
    "π/3, −5π/3, 7π/3, −11π/3",
    "π/2, −3π/2, 5π/2, −7π/2",
    "2π/3, −4π/3, 8π/3, −10π/3",
    "3π/4, −5π/4, 11π/4, −13π/4",
    "5π/6, −7π/6, 17π/6, −19π/6",
    "π, −π, 3π, −3π",
    "7π/6, −5π/6, 19π/6, −17π/6",
    "5π/4, −3π/4, 13π/4, −11π/4",
    "4π/3, −2π/3, 10π/3, −8π/3",
    "3π/2, −π/2, 7π/2, −5π/2",
    "5π/3, −π/3, 11π/3, −7π/3",
    "7π/4, −π/4, 15π/4, −9π/4",
    "11π/6, −π/6, 23π/6, −13π/6",
    "2π, 0, 4π, −2π"
  ),
  
  radians = c(
    0,
    pi/6,
    pi/4,
    pi/3,
    pi/2,
    2*pi/3,
    3*pi/4,
    5*pi/6,
    pi,
    7*pi/6,
    5*pi/4,
    4*pi/3,
    3*pi/2,
    5*pi/3,
    7*pi/4,
    11*pi/6,
    2*pi
  ),
  
  x_label = c(
    "1",
    "√3/2",
    "√2/2",
    "1/2",
    "0",
    "-1/2",
    "-√2/2",
    "-√3/2",
    "-1",
    "-√3/2",
    "-√2/2",
    "-1/2",
    "0",
    "1/2",
    "√2/2",
    "√3/2",
    "1"
  ),
  
  y_label = c(
    "0",
    "1/2",
    "√2/2",
    "√3/2",
    "1",
    "√3/2",
    "√2/2",
    "1/2",
    "0",
    "-1/2",
    "-√2/2",
    "-√3/2",
    "-1",
    "-√3/2",
    "-√2/2",
    "-1/2",
    "0"
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# Named vector for radio buttons
#
# The long coterminal-angle expression is what students see.
# The short ID is what Shiny uses internally.
# ------------------------------------------------------------

angle_choices <- setNames(
  angles$id,
  angles$menu_label
)


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
        border: 2px solid #777;
        border-radius: 6px;
        padding: 16px 20px 14px 20px;
        margin-top: 8px;
        margin-bottom: 20px;
        max-width: 1180px;
        margin-left: auto;
        margin-right: auto;
      }
      
      .left-content {
        padding-right: 12px;
      }
      
      .right-content {
        padding-left: 12px;
      }
      
      .remember-box {
        font-size: 17px;
        margin-bottom: 12px;
      }
      
      .remember-box h3,
      .angle-box h3 {
        margin-top: 0;
        margin-bottom: 7px;
      }
      
      .remember-formula {
        font-size: 20px;
        font-weight: bold;
        text-align: center;
        margin-top: 7px;
        margin-bottom: 12px;
      }
      
      .angle-box {
        margin-bottom: 0;
      }
      
      .angle-box p {
        font-size: 16px;
        margin-bottom: 7px;
      }
      
      /*
      The coterminal labels are longer than the old labels,
      so make the radio-button text slightly smaller.
      */
      
      .radio {
        margin-top: 2px;
        margin-bottom: 2px;
      }
      
      .radio label {
        font-size: 15px;
        line-height: 1.25;
      }
      
      .instruction {
        text-align: center;
        font-size: 17px;
        margin-bottom: 2px;
      }
      
      .angle-display {
        font-size: 21px;
        font-weight: bold;
        margin-top: 2px;
        margin-bottom: 2px;
        text-align: center;
      }
      
      .coordinate-display {
        font-size: 23px;
        font-weight: bold;
        margin-top: 2px;
        text-align: center;
      }
      
      #circle-container {
        width: 100%;
        max-width: 430px;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 0;
      }
      
      #unit-circle-svg {
        width: 100%;
        height: auto;
        display: block;
        touch-action: none;
        user-select: none;
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
        stroke: #999;
        stroke-width: 2;
        stroke-dasharray: 7 7;
      }
      
      .radius-line {
        stroke: #333;
        stroke-width: 4;
      }
      
      .stop-point {
        fill: white;
        stroke: #555;
        stroke-width: 2;
        cursor: pointer;
      }
      
      .stop-point:hover {
        stroke-width: 4;
      }
      
      .slider-point {
        fill: #222;
        stroke: white;
        stroke-width: 4;
        cursor: grab;
      }
      
      .slider-point:active {
        cursor: grabbing;
      }
      
      .axis-label {
        font-size: 22px;
        font-weight: bold;
      }
      
      .tick-label {
        font-size: 18px;
      }
      
      @media (max-width: 767px) {
        
        .left-content,
        .right-content {
          padding-left: 0;
          padding-right: 0;
        }
        
        .right-content {
          margin-top: 18px;
        }
      }
      
      ")
    )
  ),
  
  
  titlePanel("M151 Unit Circle Explorer"),
  
  
  # ----------------------------------------------------------
  # Main compact panel
  # ----------------------------------------------------------
  
  div(
    class = "app-panel",
    
    fluidRow(
      
      # --------------------------------------------------------
      # LEFT SIDE
      # --------------------------------------------------------
      
      column(
        width = 5,
        
        div(
          class = "left-content",
          
          
          # ----------------------------------------------------
          # Remember
          # ----------------------------------------------------
          
          div(
            class = "remember-box",
            
            h3("Remember"),
            
            p(
              "On the unit circle, the coordinates of the point are"
            ),
            
            div(
              class = "remember-formula",
              "(x, y) = (cos t, sin t)"
            )
          ),
          
          
          # ----------------------------------------------------
          # Angle choices
          # ----------------------------------------------------
          
          div(
            class = "angle-box",
            
            h3("Choose an angle"),
            
            p(
              paste(
                "Each choice shows four coterminal angles.",
                "Choose a group or move the point around the circle."
              )
            ),
            
            radioButtons(
              inputId = "angle",
              label = NULL,
              choices = angle_choices,
              selected = "a3"
            )
          )
        )
      ),
      
      
      # --------------------------------------------------------
      # RIGHT SIDE
      # --------------------------------------------------------
      
      column(
        width = 7,
        
        div(
          class = "right-content",
          
          
          # ----------------------------------------------------
          # Instructions
          # ----------------------------------------------------
          
          div(
            class = "instruction",
            
            strong(
              "Drag the large point around the circumference."
            ),
            
            br(),
            
            "It will snap to the nearest standard position."
          ),
          
          
          # ----------------------------------------------------
          # Interactive unit circle
          # ----------------------------------------------------
          
          div(
            id = "circle-container",
            
            tags$svg(
              
              id = "unit-circle-svg",
              
              viewBox = "75 100 450 400",
              
              
              # ------------------------------------------------
              # Horizontal axis
              # ------------------------------------------------
              
              tags$line(
                x1 = "125",
                y1 = "300",
                x2 = "475",
                y2 = "300",
                class = "axis-line"
              ),
              
              
              # ------------------------------------------------
              # Vertical axis
              # ------------------------------------------------
              
              tags$line(
                x1 = "300",
                y1 = "125",
                x2 = "300",
                y2 = "475",
                class = "axis-line"
              ),
              
              
              # ------------------------------------------------
              # Unit circle
              # ------------------------------------------------
              
              tags$circle(
                cx = "300",
                cy = "300",
                r = "140",
                class = "unit-circle"
              ),
              
              
              # ------------------------------------------------
              # Coordinate guide lines
              # ------------------------------------------------
              
              tags$line(
                id = "vertical-guide",
                class = "guide-line"
              ),
              
              tags$line(
                id = "horizontal-guide",
                class = "guide-line"
              ),
              
              
              # ------------------------------------------------
              # Radius
              # ------------------------------------------------
              
              tags$line(
                id = "radius-line",
                x1 = "300",
                y1 = "300",
                class = "radius-line"
              ),
              
              
              # ------------------------------------------------
              # 16 physical stopping points
              #
              # 0 and 2pi occupy the same point, so only one
              # physical point is needed there.
              # ------------------------------------------------
              
              lapply(
                1:16,
                
                function(i) {
                  
                  t <- angles$radians[i]
                  
                  tags$circle(
                    class = "stop-point",
                    `data-index` = i,
                    cx = 300 + 140*cos(t),
                    cy = 300 - 140*sin(t),
                    r = "7"
                  )
                }
              ),
              
              
              # ------------------------------------------------
              # Draggable slider point
              # ------------------------------------------------
              
              tags$circle(
                id = "slider-point",
                class = "slider-point",
                cx = 300 + 140*cos(pi/4),
                cy = 300 - 140*sin(pi/4),
                r = "14"
              ),
              
              
              # ------------------------------------------------
              # x-axis labels
              # ------------------------------------------------
              
              tags$text(
                x = "470",
                y = "290",
                class = "axis-label",
                "x"
              ),
              
              tags$text(
                x = "435",
                y = "325",
                class = "tick-label",
                "1"
              ),
              
              tags$text(
                x = "148",
                y = "325",
                class = "tick-label",
                "-1"
              ),
              
              
              # ------------------------------------------------
              # y-axis labels
              # ------------------------------------------------
              
              tags$text(
                x = "315",
                y = "135",
                class = "axis-label",
                "y"
              ),
              
              tags$text(
                x = "315",
                y = "165",
                class = "tick-label",
                "1"
              ),
              
              tags$text(
                x = "310",
                y = "450",
                class = "tick-label",
                "-1"
              )
            )
          ),
          
          
          # ----------------------------------------------------
          # Results
          # ----------------------------------------------------
          
          uiOutput("angleText"),
          
          uiOutput("coordinateText")
        )
      )
    )
  ),
  
  
  # ------------------------------------------------------------
  # JavaScript for the circular slider
  # ------------------------------------------------------------
  
  tags$script(
    HTML("
    
    $(document).on('shiny:connected', function() {
      
      const svg = document.getElementById('unit-circle-svg');
      const slider = document.getElementById('slider-point');
      const radius = document.getElementById('radius-line');
      const verticalGuide = document.getElementById('vertical-guide');
      const horizontalGuide = document.getElementById('horizontal-guide');
      
      const cx = 300;
      const cy = 300;
      const r = 140;
      
      
      // --------------------------------------------------------
      // The 17 standard selections
      // --------------------------------------------------------
      
      const standardAngles = [
        0,
        Math.PI / 6,
        Math.PI / 4,
        Math.PI / 3,
        Math.PI / 2,
        2 * Math.PI / 3,
        3 * Math.PI / 4,
        5 * Math.PI / 6,
        Math.PI,
        7 * Math.PI / 6,
        5 * Math.PI / 4,
        4 * Math.PI / 3,
        3 * Math.PI / 2,
        5 * Math.PI / 3,
        7 * Math.PI / 4,
        11 * Math.PI / 6,
        2 * Math.PI
      ];
      
      
      let dragging = false;
      
      
      // --------------------------------------------------------
      // Move slider to a standard position
      // --------------------------------------------------------
      
      function moveSlider(index) {
        
        const theta = standardAngles[index - 1];
        
        const x = cx + r * Math.cos(theta);
        const y = cy - r * Math.sin(theta);
        
        
        // Slider point
        
        slider.setAttribute('cx', x);
        slider.setAttribute('cy', y);
        
        
        // Radius
        
        radius.setAttribute('x2', x);
        radius.setAttribute('y2', y);
        
        
        // Vertical coordinate guide
        
        verticalGuide.setAttribute('x1', x);
        verticalGuide.setAttribute('y1', cy);
        verticalGuide.setAttribute('x2', x);
        verticalGuide.setAttribute('y2', y);
        
        
        // Horizontal coordinate guide
        
        horizontalGuide.setAttribute('x1', cx);
        horizontalGuide.setAttribute('y1', y);
        horizontalGuide.setAttribute('x2', x);
        horizontalGuide.setAttribute('y2', y);
      }
      
      
      // --------------------------------------------------------
      // Convert pointer position to SVG coordinates
      // --------------------------------------------------------
      
      function pointerToSVG(event) {
        
        const point = svg.createSVGPoint();
        
        point.x = event.clientX;
        point.y = event.clientY;
        
        return point.matrixTransform(
          svg.getScreenCTM().inverse()
        );
      }
      
      
      // --------------------------------------------------------
      // Find nearest physical standard position
      //
      // We compare against the first 16 positions.
      // 2pi and 0 occupy the same physical location.
      // Dragging to that location selects 0.
      // --------------------------------------------------------
      
      function closestAngleIndex(point) {
        
        const dx = point.x - cx;
        const dy = cy - point.y;
        
        let theta = Math.atan2(dy, dx);
        
        if (theta < 0) {
          theta += 2 * Math.PI;
        }
        
        
        let bestIndex = 1;
        let bestDistance = Infinity;
        
        
        for (let i = 0; i < 16; i++) {
          
          const a = standardAngles[i];
          
          let difference = Math.abs(theta - a);
          
          difference = Math.min(
            difference,
            2 * Math.PI - difference
          );
          
          
          if (difference < bestDistance) {
            
            bestDistance = difference;
            bestIndex = i + 1;
          }
        }
        
        
        return bestIndex;
      }
      
      
      // --------------------------------------------------------
      // Choose position from pointer
      // --------------------------------------------------------
      
      function chooseFromPointer(event) {
        
        const point = pointerToSVG(event);
        
        const index = closestAngleIndex(point);
        
        moveSlider(index);
        
        
        Shiny.setInputValue(
          'circle_angle_index',
          index,
          {priority: 'event'}
        );
      }
      
      
      // --------------------------------------------------------
      // Begin dragging
      // --------------------------------------------------------
      
      slider.addEventListener(
        'pointerdown',
        
        function(event) {
          
          dragging = true;
          
          slider.setPointerCapture(event.pointerId);
          
          chooseFromPointer(event);
          
          event.preventDefault();
        }
      );
      
      
      // --------------------------------------------------------
      // Continue dragging
      // --------------------------------------------------------
      
      slider.addEventListener(
        'pointermove',
        
        function(event) {
          
          if (!dragging) {
            return;
          }
          
          chooseFromPointer(event);
          
          event.preventDefault();
        }
      );
      
      
      // --------------------------------------------------------
      // Finish dragging
      // --------------------------------------------------------
      
      slider.addEventListener(
        'pointerup',
        
        function(event) {
          
          dragging = false;
          
          if (slider.hasPointerCapture(event.pointerId)) {
            
            slider.releasePointerCapture(event.pointerId);
          }
        }
      );
      
      
      slider.addEventListener(
        'pointercancel',
        
        function() {
          
          dragging = false;
        }
      );
      
      
      // --------------------------------------------------------
      // Clicking one of the small stopping points
      // --------------------------------------------------------
      
      document.querySelectorAll('.stop-point').forEach(
        
        function(point) {
          
          point.addEventListener(
            'click',
            
            function() {
              
              const index = Number(
                this.getAttribute('data-index')
              );
              
              moveSlider(index);
              
              Shiny.setInputValue(
                'circle_angle_index',
                index,
                {priority: 'event'}
              );
            }
          );
          
        }
      );
      
      
      // --------------------------------------------------------
      // Receive radio-button changes from Shiny
      // --------------------------------------------------------
      
      Shiny.addCustomMessageHandler(
        'moveCircleSlider',
        
        function(message) {
          
          moveSlider(message.index);
        }
      );
      
      
      // Starting position = pi/4
      
      moveSlider(3);
      
    });
    
    ")
  )
)


# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Current selected position
  # ----------------------------------------------------------
  
  selected_angle <- reactive({
    
    angles[
      angles$id == input$angle,
    ]
    
  })
  
  
  # ----------------------------------------------------------
  # Dragging changes radio-button selection
  # ----------------------------------------------------------
  
  observeEvent(
    input$circle_angle_index,
    
    {
      
      i <- input$circle_angle_index
      
      req(i)
      
      updateRadioButtons(
        session = session,
        inputId = "angle",
        selected = angles$id[i]
      )
      
    }
  )
  
  
  # ----------------------------------------------------------
  # Radio-button selection moves slider
  # ----------------------------------------------------------
  
  observeEvent(
    input$angle,
    
    {
      
      i <- which(
        angles$id == input$angle
      )
      
      req(length(i) == 1)
      
      session$sendCustomMessage(
        type = "moveCircleSlider",
        
        message = list(
          index = i
        )
      )
      
    },
    
    ignoreInit = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Selected standard angle
  # ----------------------------------------------------------
  
  output$angleText <- renderUI({
    
    a <- selected_angle()
    
    div(
      class = "angle-display",
      
      paste0(
        "Standard angle: ",
        a$standard
      )
    )
    
  })
  
  
  # ----------------------------------------------------------
  # Exact coordinates
  # ----------------------------------------------------------
  
  output$coordinateText <- renderUI({
    
    a <- selected_angle()
    
    div(
      class = "coordinate-display",
      
      paste0(
        "Point on the unit circle: (",
        a$x_label,
        ", ",
        a$y_label,
        ")"
      )
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
