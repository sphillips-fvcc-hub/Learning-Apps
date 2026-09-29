library(shiny)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# Convert polar coordinates to rectangular coordinates
# ------------------------------------------------------------

polar_to_xy <- function(r, theta) {
  
  list(
    x = r * cos(theta),
    y = r * sin(theta)
  )
}


# ------------------------------------------------------------
# Format theta using familiar multiples of pi
# ------------------------------------------------------------

format_theta <- function(theta) {
  
  fractions <- c(
    0,
    pi / 6,
    pi / 4,
    pi / 3,
    pi / 2,
    2 * pi / 3,
    3 * pi / 4,
    5 * pi / 6,
    pi,
    7 * pi / 6,
    5 * pi / 4,
    4 * pi / 3,
    3 * pi / 2,
    5 * pi / 3,
    7 * pi / 4,
    11 * pi / 6,
    2 * pi
  )
  
  labels <- c(
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
  )
  
  nearest <- which.min(
    abs(theta - fractions)
  )
  
  if (
    abs(theta - fractions[nearest]) < 0.015
  ) {
    
    return(
      labels[nearest]
    )
  }
  
  paste0(
    round(theta / pi, 2),
    "π"
  )
}


# ------------------------------------------------------------
# Draw polar grid
# ------------------------------------------------------------

draw_polar_grid <- function(limit = 4) {
  
  angles <- seq(
    0,
    2 * pi,
    length.out = 361
  )
  
  # Concentric circles
  
  for (
    radius in seq(
      1,
      limit,
      by = 1
    )
  ) {
    
    lines(
      radius * cos(angles),
      radius * sin(angles),
      col = "gray85",
      lwd = 1
    )
  }
  
  # Radial lines every 30 degrees
  
  radial_angles <- seq(
    0,
    11 * pi / 6,
    by = pi / 6
  )
  
  for (
    angle in radial_angles
  ) {
    
    segments(
      0,
      0,
      limit * cos(angle),
      limit * sin(angle),
      col = "gray90",
      lwd = 1
    )
  }
  
  abline(
    h = 0,
    v = 0,
    col = "gray55"
  )
  
  # Angle labels
  
  label_radius <-
    limit * 1.06
  
  angle_labels <- c(
    "0",
    "π/6",
    "π/3",
    "π/2",
    "2π/3",
    "5π/6",
    "π",
    "7π/6",
    "4π/3",
    "3π/2",
    "5π/3",
    "11π/6"
  )
  
  for (
    i in seq_along(
      radial_angles
    )
  ) {
    
    text(
      label_radius *
        cos(
          radial_angles[i]
        ),
      label_radius *
        sin(
          radial_angles[i]
        ),
      labels =
        angle_labels[i],
      cex = 0.68
    )
  }
}


# ============================================================
# USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$title(
      "Building a Polar Graph"
    ),
    
    tags$style(
      HTML("

        body {
          font-size: 15px;
        }

        .container-fluid {
          padding-left: 24px;
          padding-right: 24px;
          padding-top: 8px;
        }

        h1.app-title {
          font-size: 30px;
          margin-top: 0;
          margin-bottom: 3px;
        }

        .intro-text {
          font-size: 13px;
          line-height: 1.2;
          margin-bottom: 5px;
        }

        .intro-text p {
          margin-top: 1px;
          margin-bottom: 1px;
        }

        .polar-layout {
          display: grid;
          grid-template-columns: 350px minmax(0, 1fr);
          column-gap: 20px;
          align-items: start;
          width: 100%;
        }

        .polar-controls {
          min-width: 0;
        }

        .polar-graph {
          min-width: 0;
          align-self: start;
        }

        .polar-graph .shiny-plot-output {
          margin-top: 0;
          margin-bottom: 0;
        }

        .form-group {
          margin-bottom: 6px;
        }

        .control-label {
          margin-bottom: 2px;
        }

        .btn {
          margin-bottom: 3px;
        }

        .investigation-box {
          border-top: 1px solid #dddddd;
          margin-top: 5px;
          padding-top: 4px;
          font-size: 13px;
          line-height: 1.25;
        }

        .investigation-box p {
          margin: 1px 0;
        }

        .lower-layout {
          display: grid;
          grid-template-columns: 1.15fr 0.85fr 1fr;
          column-gap: 18px;
          align-items: start;
          margin-top: -8px;
        }

        .current-box {
          border: 1px solid #dddddd;
          border-radius: 5px;
          padding: 7px 10px;
          background-color: #f8f8f8;
        }

        .current-box h4 {
          margin-top: 0;
          margin-bottom: 4px;
        }

        .useful-box {
          min-width: 0;
        }

        .useful-box h4 {
          margin-top: 0;
          margin-bottom: 3px;
        }

        .table {
          font-size: 13px;
          margin-bottom: 0;
        }

        .table > thead > tr > th,
        .table > tbody > tr > td {
          padding: 2px 6px;
        }

        .investigate-box {
          border: 1px solid #dddddd;
          border-radius: 5px;
          padding: 7px 10px;
          background-color: #f8f8f8;
          font-size: 13px;
          line-height: 1.25;
        }

        .investigate-box h4 {
          margin-top: 0;
          margin-bottom: 4px;
        }

        .investigate-box p {
          margin: 2px 0;
        }

        @media (max-width: 900px) {

          .polar-layout {
            grid-template-columns: 1fr;
          }

          .polar-graph {
            margin-top: 8px;
          }

          .lower-layout {
            grid-template-columns: 1fr;
            row-gap: 8px;
            margin-top: 5px;
          }
        }

      ")
    )
  ),
  
  
  # ----------------------------------------------------------
  # Title
  # ----------------------------------------------------------
  
  h1(
    class = "app-title",
    "Building a Polar Graph"
  ),
  
  
  # ----------------------------------------------------------
  # Introduction
  # ----------------------------------------------------------
  
  div(
    class = "intro-text",
    
    p(
      "A polar equation gives ",
      strong("r"),
      " as a function of ",
      strong("theta"),
      ". As ",
      strong("theta"),
      " changes, ",
      strong("r"),
      " changes, and each pair ",
      strong("(r, theta)"),
      " gives a point on the polar graph."
    ),
    
    p(
      "Move ",
      strong("theta"),
      " and watch the graph form. Pay special attention to what happens when ",
      strong("r is negative"),
      "."
    )
  ),
  
  
  # ==========================================================
  # MAIN LAYOUT
  # ==========================================================
  
  div(
    class = "polar-layout",
    
    
    # --------------------------------------------------------
    # LEFT SIDE
    # --------------------------------------------------------
    
    div(
      class = "polar-controls",
      
      selectInput(
        "trace_equation",
        "Choose a polar equation:",
        choices = c(
          
          "Circle: r = 2 cos(θ)" =
            "circle_cos",
          
          "Circle: r = 2 sin(θ)" =
            "circle_sin",
          
          "Cardioid: r = 2 + 2 cos(θ)" =
            "cardioid",
          
          "3-petal rose: r = 2 cos(3θ)" =
            "rose3",
          
          "4-petal rose: r = 2 sin(2θ)" =
            "rose4",
          
          "Spiral: r = θ/2" =
            "spiral"
        ),
        selected = "circle_cos"
      ),
      
      
      # ------------------------------------------------------
      # Theta slider
      #
      # Slider stores an integer from 0 to 120.
      # Each step represents pi/60.
      # ------------------------------------------------------
      
      sliderInput(
        "theta_index",
        "θ:",
        min = 0,
        max = 120,
        value = 0,
        step = 1,
        ticks = FALSE,
        animate = animationOptions(
          interval = 60,
          loop = FALSE
        )
      ),
      
      
      # ------------------------------------------------------
      # Pi-based slider formatter
      # ------------------------------------------------------
      
      tags$script(
        HTML("

          function formatThetaSlider() {

            var slider =
              $('#theta_index').data('ionRangeSlider');

            if (!slider) {
              return;
            }

            slider.update({

              prettify: function(value) {

                value =
                  Math.round(
                    Number(value)
                  );

                var numerator = value;
                var denominator = 60;

                if (numerator === 0) {
                  return '0';
                }

                function gcd(a, b) {

                  while (b !== 0) {

                    var temp = b;

                    b = a % b;

                    a = temp;
                  }

                  return a;
                }

                var divisor =
                  gcd(
                    numerator,
                    denominator
                  );

                numerator =
                  numerator / divisor;

                denominator =
                  denominator / divisor;

                if (
                  denominator === 1
                ) {

                  if (
                    numerator === 1
                  ) {

                    return 'π';
                  }

                  return (
                    numerator +
                    'π'
                  );
                }

                if (
                  numerator === 1
                ) {

                  return (
                    'π/' +
                    denominator
                  );
                }

                return (
                  numerator +
                  'π/' +
                  denominator
                );
              }
            });
          }


          $(document).on(
            'shiny:connected',
            function() {

              setTimeout(
                formatThetaSlider,
                50
              );
            }
          );


          $(document).on(
            'shiny:value',
            function(event) {

              if (
                event.name ===
                'theta_index'
              ) {

                setTimeout(
                  formatThetaSlider,
                  10
                );
              }
            }
          );

        ")
      ),
      
      
      actionButton(
        "reset_theta",
        "Reset θ"
      ),
      
      
      div(
        class = "investigation-box",
        
        strong(
          "Watch the three steps:"
        ),
        
        p(
          "1. θ determines a direction."
        ),
        
        p(
          "2. The equation determines r."
        ),
        
        p(
          "3. (r, θ) determines the point."
        )
      )
    ),
    
    
    # --------------------------------------------------------
    # RIGHT SIDE
    # --------------------------------------------------------
    
    div(
      class = "polar-graph",
      
      plotOutput(
        "trace_plot",
        height = "410px",
        width = "100%"
      )
    )
  ),
  
  
  # ==========================================================
  # LOWER LAYOUT
  # ==========================================================
  
  div(
    class = "lower-layout",
    
    
    # --------------------------------------------------------
    # CURRENT POINT
    # --------------------------------------------------------
    
    div(
      class = "current-box",
      
      h4(
        "Current Point"
      ),
      
      uiOutput(
        "current_values"
      )
    ),
    
    
    # --------------------------------------------------------
    # USEFUL VALUES
    # --------------------------------------------------------
    
    div(
      class = "useful-box",
      
      h4(
        "Useful Values"
      ),
      
      tableOutput(
        "polar_table"
      )
    ),
    
    
    # --------------------------------------------------------
    # INVESTIGATE
    # --------------------------------------------------------
    
    div(
      class = "investigate-box",
      
      h4(
        "Investigate"
      ),
      
      p(
        "When is r = 0?"
      ),
      
      p(
        "When is r negative?"
      ),
      
      p(
        "Where does the point go when r < 0?"
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Convert slider index to theta
  #
  # index 0   -> 0
  # index 10  -> pi/6
  # index 15  -> pi/4
  # index 20  -> pi/3
  # index 30  -> pi/2
  # index 60  -> pi
  # index 120 -> 2pi
  # ----------------------------------------------------------
  
  theta_current <- reactive({
    
    input$theta_index *
      pi / 60
  })
  
  
  # ----------------------------------------------------------
  # Polar equation
  # ----------------------------------------------------------
  
  trace_r <- reactive({
    
    function(theta) {
      
      if (
        input$trace_equation ==
        "circle_cos"
      ) {
        
        return(
          2 * cos(theta)
        )
      }
      
      if (
        input$trace_equation ==
        "circle_sin"
      ) {
        
        return(
          2 * sin(theta)
        )
      }
      
      if (
        input$trace_equation ==
        "cardioid"
      ) {
        
        return(
          2 +
            2 * cos(theta)
        )
      }
      
      if (
        input$trace_equation ==
        "rose3"
      ) {
        
        return(
          2 *
            cos(
              3 * theta
            )
        )
      }
      
      if (
        input$trace_equation ==
        "rose4"
      ) {
        
        return(
          2 *
            sin(
              2 * theta
            )
        )
      }
      
      if (
        input$trace_equation ==
        "spiral"
      ) {
        
        return(
          theta / 2
        )
      }
    }
  })
  
  
  # ----------------------------------------------------------
  # Equation text
  # ----------------------------------------------------------
  
  trace_equation_text <- reactive({
    
    switch(
      input$trace_equation,
      
      circle_cos =
        "r = 2 cos(θ)",
      
      circle_sin =
        "r = 2 sin(θ)",
      
      cardioid =
        "r = 2 + 2 cos(θ)",
      
      rose3 =
        "r = 2 cos(3θ)",
      
      rose4 =
        "r = 2 sin(2θ)",
      
      spiral =
        "r = θ/2"
    )
  })
  
  
  # ----------------------------------------------------------
  # Reset theta
  # ----------------------------------------------------------
  
  observeEvent(
    input$reset_theta,
    {
      
      updateSliderInput(
        session,
        "theta_index",
        value = 0
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # Current point information
  # ----------------------------------------------------------
  
  output$current_values <- renderUI({
    
    theta_value <-
      theta_current()
    
    r_function <-
      trace_r()
    
    r_current <-
      r_function(
        theta_value
      )
    
    point <-
      polar_to_xy(
        r_current,
        theta_value
      )
    
    theta_text <-
      format_theta(
        theta_value
      )
    
    HTML(
      paste0(
        
        "<div style='font-size:14px; line-height:1.3;'>",
        
        "<b>Equation:</b> ",
        trace_equation_text(),
        
        "<br>",
        
        "<b>θ = </b>",
        theta_text,
        
        "<br>",
        
        "<b>r = </b>",
        round(
          r_current,
          3
        ),
        
        "<br>",
        
        "<b>Polar point:</b> (",
        round(
          r_current,
          3
        ),
        ", ",
        theta_text,
        ")",
        
        "<br>",
        
        "<b>Rectangular point:</b> (",
        round(
          point$x,
          3
        ),
        ", ",
        round(
          point$y,
          3
        ),
        ")",
        
        "</div>"
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # Trace plot
  # ----------------------------------------------------------
  
  output$trace_plot <- renderPlot({
    
    theta_value <-
      theta_current()
    
    r_function <-
      trace_r()
    
    
    # --------------------------------------------------------
    # Complete curve
    # --------------------------------------------------------
    
    theta_full <-
      seq(
        0,
        2 * pi,
        length.out = 1600
      )
    
    r_full <-
      r_function(
        theta_full
      )
    
    x_full <-
      r_full *
      cos(theta_full)
    
    y_full <-
      r_full *
      sin(theta_full)
    
    
    # --------------------------------------------------------
    # Curve traced so far
    # --------------------------------------------------------
    
    if (
      theta_value <= 0
    ) {
      
      theta_trace <- 0
      
    } else {
      
      number_of_points <-
        max(
          2,
          ceiling(
            1600 *
              theta_value /
              (2 * pi)
          )
        )
      
      theta_trace <-
        seq(
          0,
          theta_value,
          length.out =
            number_of_points
        )
    }
    
    r_trace <-
      r_function(
        theta_trace
      )
    
    x_trace <-
      r_trace *
      cos(theta_trace)
    
    y_trace <-
      r_trace *
      sin(theta_trace)
    
    
    # --------------------------------------------------------
    # Current point
    # --------------------------------------------------------
    
    r_current <-
      r_function(
        theta_value
      )
    
    current_point <-
      polar_to_xy(
        r_current,
        theta_value
      )
    
    
    # --------------------------------------------------------
    # Plot limits
    # --------------------------------------------------------
    
    max_distance <-
      max(
        abs(
          r_full
        ),
        na.rm = TRUE
      )
    
    if (
      !is.finite(
        max_distance
      ) ||
      max_distance < 2
    ) {
      
      max_distance <- 2
    }
    
    limit <-
      ceiling(
        max_distance +
          0.5
      )
    
    
    # --------------------------------------------------------
    # Draw plot
    # --------------------------------------------------------
    
    par(
      mar = c(
        2.4,
        2.4,
        1.6,
        0.8
      )
    )
    
    plot(
      NA,
      xlim = c(
        -limit,
        limit
      ),
      ylim = c(
        -limit,
        limit
      ),
      asp = 1,
      xlab = "x",
      ylab = "y",
      main =
        trace_equation_text()
    )
    
    draw_polar_grid(
      limit
    )
    
    
    # Complete curve shown lightly
    
    lines(
      x_full,
      y_full,
      col = "gray82",
      lwd = 2
    )
    
    
    # Traced curve
    
    lines(
      x_trace,
      y_trace,
      col = "blue",
      lwd = 4
    )
    
    
    # Direction determined by theta
    
    ray_length <-
      limit * 0.92
    
    segments(
      0,
      0,
      
      ray_length *
        cos(theta_value),
      
      ray_length *
        sin(theta_value),
      
      col = "darkorange",
      lwd = 2,
      lty = 2
    )
    
    
    # Segment from pole to current point
    
    if (
      r_current >= 0
    ) {
      
      segment_color <-
        "darkgreen"
      
    } else {
      
      segment_color <-
        "red"
    }
    
    segments(
      0,
      0,
      current_point$x,
      current_point$y,
      col = segment_color,
      lwd = 4
    )
    
    
    # Current point
    
    points(
      current_point$x,
      current_point$y,
      pch = 19,
      cex = 1.6,
      col = "red"
    )
    
    
    # Pole
    
    points(
      0,
      0,
      pch = 19,
      cex = 0.9
    )
    
    
    legend(
      "topright",
      
      legend = c(
        "Traced curve",
        "Direction of θ",
        "Current point"
      ),
      
      col = c(
        "blue",
        "darkorange",
        "red"
      ),
      
      lwd = c(
        4,
        2,
        NA
      ),
      
      lty = c(
        1,
        2,
        NA
      ),
      
      pch = c(
        NA,
        NA,
        19
      ),
      
      bty = "n",
      cex = 0.72
    )
  })
  
  
  # ----------------------------------------------------------
  # Useful-value table
  # ----------------------------------------------------------
  
  output$polar_table <- renderTable({
    
    theta_values <-
      c(
        0,
        pi / 2,
        pi,
        3 * pi / 2,
        2 * pi
      )
    
    theta_labels <-
      c(
        "0",
        "π/2",
        "π",
        "3π/2",
        "2π"
      )
    
    r_function <-
      trace_r()
    
    r_values <-
      r_function(
        theta_values
      )
    
    data.frame(
      "θ" =
        theta_labels,
      
      "r" =
        round(
          r_values,
          3
        ),
      
      check.names = FALSE
    )
    
  },
  rownames = FALSE)
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
