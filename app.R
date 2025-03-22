######### Libraries ############
library(shiny)
library(data.table)
library(signal)
library(ggplot2)
library(plotly)
library(pracma)
library(WaveletComp)
library(tidyr)

# Version: 1.2.0
# Author: T. Vredeveld (GitHub: https://github.com/tomvredeveld)
# Version management can be found here: https://github.com/tomvredeveld/center-of-pressure-analysis-tool


########### UI #################
ui <- fluidPage(
  ## Set labels of input fields to non-bold
  tags$head(tags$style(HTML("label {font-weight:normal;}"))),
  
  ## Create side-panels
  titlePanel("Center of Pressure Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      
      # Data input panel
      tabsetPanel(h4("File Upload"),
                  fileInput(inputId = "file", 
                            label = HTML("<i>Choose a CSV or TXT file.</i>"),
                            accept = c(".csv", ".txt"))),
      
      # Data filter set panel.
      tabsetPanel(h4("Filter Frequency"),
                  p("If you wish to filter the data, you may want to specify the sampling frequency of your measurement, otherwise, leave the slider at 20 Hz."),
                  sliderInput(inputId = "get_filter_frequency", label = HTML("<i>Sampling frequency in Hz.</i>"),
                              min = 20, max = 1000,
                              value = 20)),
      
      # Data timestamps for segment & COP calculation
      tabsetPanel(h4("Segment selection"),
                  p("If you want to analyze a segment for specific CoP parameters,
                    please enter two values that show when hovering over the plot in the 'Plotly' tab."),
                  numericInput(inputId = "input_timestamp_one",
                               label = HTML("<i>First timestamp to calculate CoP Parameters.</i>"),
                               value = NA, min = 0),
                  numericInput(inputId = "input_timestamp_two",
                               label = HTML("<i>Second timestamp to calculate CoP Parameters.</i>"),
                               value = NA, min = 0),
                  br(),
                  actionButton("getsegment", 
                               label = "Calculate COP Parameters!", 
                               class = "btn-primary"))),
    
    ## Create mainPanel with tabs and instructions.
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions",
                 br(),
                 h2("Instructions"),
                 hr(),
                 p("Here, you will find some instructions to calculate Center of
                 Pressure parameters using this calculator."),
                 h3("Step 1: Upload data"),
                 hr(),
                 p("First, upload your data as a", code(".txt"), "or", code(".csv"),
                   "file on the left. 
                   Make sure the first three columns in the file are:"),
                 p(code("time"),
                   "containing a time series"),
                 p(code("copx"),
                   "medio-lateral direction COP data column and"),
                 p(code("copy"), "antero-posterior direction COP data column."),
                 p("The names of the columns do not need to match. Other 
                      columns can be within the file but will be ignored for the calculations. 
                      There is no need to clean up the data yourself."), 
                 h3("Step 2: Look at the signals"), 
                 hr(),
                 p("Use the tabs above: table, to have a quick look at the data and see if the data was uploaded correctly,
                      plotly, to inspect a medio-lateral (upper panel) or antero-posterior (lower pannel) signal."), 
                 h3("Step 3: Segment selection"),
                 hr(),
                 p("Hover over either the upper or lower panel of the plots and fill in the values as a 
                    timestamp the sidebar to the left to select a segment of the total signal. The app also
                   calculates the COP parameters if a non-existing timestamp was entered, for example due to a typing mistake.
                   It then selects the nearest available point in time"),
                 h3("Step 4: Press Calculate"),
                 hr(),
                 p("Now you will find a range of COP parameters, reported at the COP parameters tab. 
                   At the tab Sway Area you may find plots that show the postural sway of the individual during the 
                   selected segment. You may adjust the timestamp values at any time"),
        ),
        tabPanel("Table", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("Here you can find a table with the first 6 rows of your data, to see if the upload was succesful"),
                 h3("Table"), 
                 tableOutput("table")),
        tabPanel("Plotly", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("Beneath you will find a plot of your data, consisting of two panels,
                   the upper being the COP on the x-axis, or medio-lateral direction,
                   while the second panel shows the COP on the y-axis, or antero-posterior direction."), 
                 h3("Plot"),
                 hr(),
                 plotlyOutput("plot")),
        tabPanel("CoP Parameters", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("The table below shows the output of generic COP parameters of the selected segment from the two
                   timestamps you entered on the left. If no table is shown, please check your timestamps and make sure
                   these are values taken from the Plotly tab, using your mouse to hover over the signals."), 
                 h3("Table of COP parameters of the selected segment"),
                 tableOutput("cop_table"),
                 downloadButton("download_cop", "Download CoP Parameters as CSV")),
        tabPanel("Sway Area", 
                 br(), 
                 h2("Instructions"),
                 hr(),
                 p("Below, you may find two plots, the left being a 95% Predicted Ellipse Area, layed on top of the data
                   from the segment you selected using the timestamps taken from the Plotly tab you entered on the left.
                   The right plot shows pathlength on a wider coordinated figure including centered x and y lines.
                   If no figures are shown, please check your timestamps and make sure
                   these are values taken from the Plotly tab, using your mouse to hover over the signals."),
                 h3("Plot of Sway Area"),
                 hr(),
                 splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("sway_area_1"), plotlyOutput("sway_area_2"))),
        tabPanel("Wavelet Analysis",
                 h2("Continuous Wavelet Transform (CWT)"),
                 hr(),
                 p("This tab provides a time-frequency analysis of the CoP signals using the Continuous Wavelet Transform (CWT). Below, you can explore the scalograms, global wavelet spectrum, and quantitative metrics for both ML (medio-lateral) and AP (antero-posterior) directions."),
                 
                 h3("Scalograms"),
                 p("Scalograms show how the power (or energy) of the CoP signal varies across time (x-axis) and frequency (y-axis)."),
                 p("- Bright colors indicate high power (strong activity) at a specific time and frequency."),
                 p("- Dark colors indicate low power (weak activity)."),
                 p("- Low frequencies (bottom of the plot) correspond to slow, large postural adjustments (e.g., sway)."),
                 p("- High frequencies (top of the plot) correspond to rapid, small adjustments (e.g., corrective movements)."),
                 plotlyOutput("scalogram_ml"),
                 plotlyOutput("scalogram_ap"),
                 
                 h3("Global Wavelet Spectrum"),
                 p("The global wavelet spectrum shows the average power across all time points for each frequency (or scale)."),
                 p("- Peaks indicate dominant frequencies in the CoP signal."),
                 p("- A broad spectrum suggests a wide range of frequencies contribute to the signal."),
                 p("- A narrow spectrum suggests the signal is dominated by a few specific frequencies."),
                 plotlyOutput("global_spectrum"),
                 
                 h3("Wavelet Metrics"),
                 p("This table provides high-level metrics summarizing the wavelet analysis:"),
                 p("- Total Variance: Total energy in the CoP signal."),
                 p("- Dominant Scale: The frequency (or scale) with the highest power."),
                 p("- Mean Power: Average power across all frequencies."),
                 tableOutput("wavelet_metrics"),
                 
                 h3("Frequency Band Analysis"),
                 p("This table shows the power and sample entropy for specific frequency bands:"),
                 p("- Power: Represents the energy in each frequency band."),
                 p("- Sample Entropy: Measures the complexity or irregularity of the signal in each frequency band."),
                 p("- Frequency Bands: 0–0.1 Hz (low-frequency sway), 0.1–0.3 Hz (medium-frequency adjustments), 0.3–1 Hz (high-frequency corrections)."),
                 tableOutput("frequency_band_analysis"),
                 
                 h3("Combined Power and Entropy Plot"),
                 p("This plot compares the power and sample entropy across frequency bands for both ML and AP directions."),
                 p("- Power: Higher values indicate greater energy in that frequency band."),
                 p("- Sample Entropy: Higher values indicate greater complexity or irregularity in that frequency band."),
                 p("- Colors: Blue = ML Power, Green = AP Power, Red = ML Entropy, Orange = AP Entropy."),
                 plotlyOutput("combined_plot")),
        tabPanel("About", 
                 br(),
                 h2("About"),
                 hr(),
                 p("Author: Tom Vredeveld"),
                 h3("Version"),
                 hr(),
                 p("1.1.1 - Changed colours to match RColorBrewer distinctive colours and fixed some grammatical errors"),
                 p("1.1.0 - Extended filter options based on sampling frequency"),
                 p("1.0.0 - First public version"),
                 p("0.1.0 - Private beta"),
                 h3("COP Data filter"),
                 hr(),
                 p("The data are filtered at 10Hz, using a low-pass zero-lag 4th order Butterworth filter."),
                 h3("Disclaimer"),
                 hr(),
                 p("The author makes no representations or warranties of any kind,
                   express or implied, regarding the accuracy, reliability, 
                   or completeness of any information provided through this 
                   Center of Pressure Analysis Tool."),
                 h3("Privacy"),
                 hr(),
                 p("Please be aware that your data is sent to shinyapps.io as calculations are performed server-side.
                    Be careful with sensitive data! Otherwise, run the app locally from within RStudio
                   Head for instructions to:"),
                 a(href = "https://github.com/tomvredeveld/center-of-pressure-analysis-tool",
                   "https://github.com/tomvredeveld/center-of-pressure-analysis-tool"),
                 h3("Licence"),
                 hr(),
                 p("The code of this app is registered under a GPL-3.0 licence at GitHub
                    and can be downloaded to run locally from a computer with R and RStudio. 
                    It is provided as a RShiny app, which is hosted for free at shinyapps.io,
                    the downside here being a limited bandwith. 
                    If you wish to frequently use this app, please be adviced to run it locally on your computer.
                    More info can be found here: "),
                 a(href = "https://github.com/tomvredeveld/center-of-pressure-analysis-tool",
                   "https://github.com/tomvredeveld/center-of-pressure-analysis-tool")),
        hr()
      )
    )
  )
)

########### SERVER #################
server <- function(input, output) {
  
  ## 95% Predicted Area Ellipse calculation Function
  # Based off the works from P. Schuber and M. Kirchner 2014 (with permission translated to R)
  # Find code here: https://github.com/tomvredeveld/predicted-ellipse-area 
  # Source: http://dx.doi.org/10.1016/j.gaitpost.2013.09.001
  pea <- function(copx, copy, probability = 0.95){
    
    # Set up libraries.
    library(PEIP)
    library(ggplot2)
    library(pracma)
    library(wavelets)
    
    # Create list to export calculations to.
    pea <- list(area = NULL, eigenvectors = NULL, eigenvalues = NULL, plot = NULL)
    
    # Calculate inverse of chi-square cumulative distribution function, eigenvalues and area.
    chisquare <- chi2inv(probability, 2) 
    x <- copx[is.finite(copx)]
    y <- copy[is.finite(copy)]
    mx <- mean(x)
    my <- mean(y) 
    vec_val <- eigen(cov(matrix(c(x, y), ncol = 2)))
    val <- matrix(0, nrow = 2, ncol = 2)
    val[1,1] <- vec_val$values[2]
    val[2,2] <- vec_val$values[1] # place values at the right place.
    rotate  <- function(x, clockwise = TRUE) {
      if (clockwise) { t( apply(x, 2, rev))
      } else {apply( t(x),2, rev)} 
    } # Took this function from user: "bud.dugong" @ website: https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r-by-90-degrees-clockwise
    vec <- rotate(vec_val$vectors, clockwise = FALSE) #rotate to match matlab vectors matrix
    pea$area <- pi * chisquare * prod(sqrt(svd(val)$d))
    pea$eigenvectors <- vec
    pea$eigenvalues <- val
    
    # Create Plot: dataframe
    df_xy <- data.frame(cbind(copx = copx, copy = copy))
    
    # Create Plot: ellipse
    N <- 100 # fixed number, higher creates a smoother ellipse.
    t <- seq(from = 0, to = 2*pi, length.out = N)
    ellipse <- sqrt(chisquare) * vec %*% sqrt(val) %*% 
      t(as.matrix(data.frame(cos_t = cos(t), sin_t = sin(t)))) + 
      kronecker(matrix(1, 1, N), c(mx, my))
    df_ellipse <- data.frame(t(ellipse))
    names(df_ellipse) <- c("x", "y")
    
    # Create Plot: minor and major axis
    ax1 <- sqrt(chisquare) * vec %*% sqrt(val) %*% as.matrix(rbind(c(-1, 1), c(0, 0))) + kronecker(matrix(1, 1, 2), c(mx, my))
    ax2 <- sqrt(chisquare) * vec %*% sqrt(val) %*% as.matrix(rbind(c(0,0), c(-1, 1))) + kronecker(matrix(1, 1, 2), c(mx, my))
    df_axis <- as.data.frame(rbind(t(ax1), c(NA, NA), t(ax2)))
    names(df_axis) <- c("x", "y")
    
    # Draw plot using ggplot2
    pea$plot1 <- ggplot()+
      geom_point(data = df_xy, aes(x = copx, y = copy), colour = "#2c7bb6", shape = 3)+
      geom_path(data = df_ellipse, aes(x = x, y = y), colour = "#d7191c", linewidth = 0.2)+
      geom_path(data = df_axis, aes(x = x, y = y), colour = "#d7191c", linewidth = 0.2)+
      theme_classic()
    
    pea$plot2 <- ggplot()+
      geom_path(data = df_xy, aes(x = copx, y = copy), colour = "#2c7bb6", linewidth = 0.2)+
      geom_path(data = df_ellipse, aes(x = x, y = y), colour = "#d7191c", linewidth = 0.1)+
      geom_path(data = df_axis, aes(x = x, y = y), colour = "#d7191c", linewidth = 0.1)+
      geom_hline(yintercept = 0, colour = "black", linewidth = 0.2)+
      geom_vline(xintercept = 0, colour = "black", linewidth = 0.2)+
      coord_cartesian(xlim = c(-30, 30), ylim = c(-30, 30))+
      theme_classic()
    
    # Return list with 4 elements.
    return(pea)
  } 
  
  
  ## Load the data by user input.
  data <- reactive({
    req(input$file)
    # Load data.
    df <- as.data.frame(fread(input$file$datapath, 
                              col.names = c("time", "copx", "copy"), 
                              skip = 1, 
                              select = c(1:3)))
    # Add names to table headers
    names(df) <- c("time", "copx", "copy")
    return(df)
  })
  
  ## Filter the data uploaded given the filter frequency
  filtered_data <- reactive({
    if (req(input$get_filter_frequency)){ 
      # Set data
      df <- data()
      
      # Get the filter frequency from the input field.
      filter_frequency <- input$get_filter_frequency
      
      # Set Butterworth filtering.
      # Overall, a 4th order low pass bandwith filter with zero-phase at 10 Hz is employed
      fs_cut <- 10
      fs <- filter_frequency
      bf <- butter(2, (fs_cut / (fs / 2)), type = "low")
      df[, 2] <- filtfilt(bf, df[, 2])
      df[, 3] <- filtfilt(bf, df[, 3])
      return(df)
    } else { 
      # return unfiltered data if no filter frequency is added
      df <- data()
      return(df)}
  })
  
  ## Quicklook table: provides a quick overview if upload/fread was successfully completed.
  output$table <- renderTable({
    quicklook <- head(filtered_data())
    return(quicklook)
  })
  
  ## Create a plot to inspect signal. 
  output$plot <- renderPlotly({
    # Plotly settings
    plot1 <- plot_ly(filtered_data(), x = ~time, y = ~copx, 
                     type = "scatter", 
                     mode = "lines",
                     hoverinfo = "x",
                     line = list(width = 1, color = "#2c7bb6"),
                     name = "Medio-Lateral COP") %>%
      layout(title = "Center of Pressure Signals & Selection Tool",
             xaxis = list(title = "Time"), #, hoverformat = ".2f"), 
             yaxis = list(title = "COP Medio-Lateral", range = c(-25, 25)),
             showlegend = TRUE)
    plot2 <- plot_ly(filtered_data(), x = ~time, y = ~copy, 
                     type = "scatter",
                     mode = "lines",
                     hoverinfo = "x",
                     line = list(width = 1, color = "#d7191c"),
                     name = "Antero-Posterior COP") %>%
      layout(title = "Center of Pressure Signals & Selection Tool",
             xaxis = list(title = "Time"), #, hoverformat = ".2f"), 
             yaxis = list(title = "COP Anterior-Posterior", range = c(-25, 25)),
             showlegend = TRUE)
    plot <- subplot(plot1, plot2, nrows = 2, titleX = FALSE)
    return(plot)
  })
  
  ## Set timestamp, based on data provided by user.
  timestamp_one <- reactive({
    df <- filtered_data()
    return(which.min(abs(df[, 1] - input$input_timestamp_one)))})

  ## Set timestamp, based on data provided by user.
  timestamp_two <- reactive({
    df <- filtered_data()
    return(which.min(abs(df[, 1] - input$input_timestamp_two)))})

  ## Set analysis, based on input. 
  analysis <- reactive({
    # Set analysis pre-allocation list
    analysis <- list(segment_pea = NULL, cop_table = NULL, df_segment = NULL)
    
    # Take filtered data
    df <- filtered_data()
    
    # Create segment of the filtered data based on timestamps 
    df_segment <- df[c(timestamp_one():timestamp_two()), ]

    # Calculate segment duration (end timestamp - start timestamp)
    duration <- input$input_timestamp_two - input$input_timestamp_one
    
    # Calculate mean of displacement
    mean_copx <- mean(df_segment[, 2], na.rm = TRUE)
    mean_copy <- mean(df_segment[, 3], na.rm = TRUE)
    
    # Calculate STD's of displacement
    std_copx <- sd(df_segment[, 2], na.rm = TRUE)
    std_copy <- sd(df_segment[, 3], na.rm = TRUE)
    
    # Calculate velocity of displacement
    mcopx <- df_segment[, 2] - mean(df_segment[, 2], na.rm = TRUE)
    mcopy <- df_segment[, 3] - mean(df_segment[, 3], na.rm = TRUE)
    time_diff <- diff(df_segment[, 1])
    mvelo_copx <- mean(abs(diff(mcopx)) / time_diff)
    mvelo_copy <- mean(abs(diff(mcopy)) / time_diff)
    
    # Calculate resultant velocity
    resultant_velocity <- mean(sqrt(diff(df_segment[, 2])^2 + diff(df_segment[, 3])^2) 
                               / diff(df_segment[, 1]))
    
    # Calculate COP pathlength by summation of Euclidian distances
    pathlength <- sum(sqrt((diff(mcopx)^2 + diff(mcopy)^2)))
    
    # Calculate 95% PEA
    segment_pea <- pea(df_segment[, 2], df_segment[, 3], probability = 0.95)
    
    # Calculate sample entropy for ML and AP directions
    sampen_copx <- sample_entropy(df_segment[, 2], edim = 2, r = 0.2 * sd(df_segment[, 2]))
    sampen_copy <- sample_entropy(df_segment[, 3], edim = 2, r = 0.2 * sd(df_segment[, 3]))
    
    # Put values in one table to return.
    cop_table <- as.data.frame(matrix(NA, nrow = 12, ncol = 3))
    names(cop_table) <- c("COP Parameter", "Value", "Units")
    cop_parameters <- c("Segment Duration",
                        "mean displacement - COP medio-lateral", 
                        "mean displacement - COP antero-posterior",
                        "standard deviation of displacement - COP medio-lateral", 
                        "standard deviation of displacement - COP antero-posterior",
                        "mean velocity COP medio-lateral", 
                        "mean velocity COP antero-posterior",
                        "resultant velocity",
                        "CoP Pathlength", "CoP 95% PEA",
                        "sample entropy - COP medio-lateral",
                        "sample entropy - COP antero-posterior")
    cop_table[, 1] <- cop_parameters
    cop_table[, 2] <- c(duration, mean_copx, mean_copy, std_copx, std_copy, 
                        mvelo_copx, mvelo_copy, resultant_velocity, 
                        pathlength, segment_pea$area, sampen_copx, sampen_copy)
    cop_table[, 3] <- c("in seconds", "in centimeter", "in centimeter", 
                        "in centimeter", "in centimeter", 
                        "in centimeter per second", "in centimeter per second",
                        "in centimeter per second", "in centimeters", "in cm2",
                        "unitless", "unitless")
    
    # Add objects to a list.
    analysis$cop_table <- cop_table
    analysis$segment_pea <- segment_pea
    analysis$df_segment <- df_segment
    
    # Return element.
    return(analysis)})
  
  ## Output COP parameters table
  output$cop_table <- renderTable({
    analysis_data <- analysis()
    return(analysis_data$cop_table)
  })
  
  ## Download option for COP parameters
  output$download_cop <- downloadHandler(
    filename = function() {
      paste("cop_parameters_", Sys.Date(), ".csv", sep = "")  # File name with today's date
    },
    content = function(file) {
      # Get the COP parameters table
      cop_data <- analysis()$cop_table
      # Save the table to a CSV file
      write.csv(cop_data, file, row.names = FALSE)
    })
  
  ## Wavelet Analysis Stuff
  
  # Function to perform CWT and calculate metrics
  analyze_cop <- function(cop_signal, Fs) {
    # Perform continuous wavelet transform
    wavelet_data <- analyze.wavelet(
      data.frame(cop = cop_signal),
      loess.span = 0,
      dt = 1/Fs,  # Time resolution (1/sampling frequency)
      lowerPeriod = 1/Fs,  # Smallest scale (highest frequency)
      upperPeriod = length(cop_signal)/Fs,  # Largest scale (lowest frequency)
      make.pval = FALSE
    )
    
    # Extract scales (frequencies) and power matrix
    scales <- wavelet_data$Period / Fs  # Use Period for column scales
    power <- wavelet_data$Power  # Power matrix (time x scales)
    
    # Compute global wavelet spectrum (mean power across time for each scale)
    global_power <- colMeans(power, na.rm = TRUE)
    
    # Align scales with global_power
    if (length(scales) > length(global_power)) {
      scales <- scales[1:length(global_power)]  # Truncate scales
    } else if (length(scales) < length(global_power)) {
      scales <- c(scales, rep(scales[length(scales)], length(global_power) - length(scales)))  # Extend scales
    }
    
    # Calculate total wavelet variance (sum of global power)
    total_variance <- sum(global_power)
    
    # Calculate dominant frequency (scale with maximum power)
    dominant_scale <- scales[which.max(global_power)]
    
    # Debug: Print dimensions and key metrics
    cat("Scales length:", length(scales), "\n")
    cat("Power dimensions:", dim(power), "\n")
    cat("Global power length:", length(global_power), "\n")
    cat("Total variance:", total_variance, "\n")
    cat("Dominant scale:", dominant_scale, "\n")
    
    return(list(
      wavelet_data = wavelet_data,
      global_power = global_power,
      scales = scales,
      total_variance = total_variance,
      dominant_scale = dominant_scale
    ))
  }
  
  output$scalogram_ml <- renderPlotly({
    df_segment <- analysis()$df_segment
    Fs <- input$get_filter_frequency
    
    # Perform CWT on ML CoP data
    ml_results <- analyze_cop(df_segment$copx, Fs)
    wavelet_data <- ml_results$wavelet_data
    
    # Create scalogram
    plot_ly(
      x = wavelet_data$axis.1,  # Time axis
      y = log2(wavelet_data$Period),  # Log2(scale) for better visualization
      z = t(wavelet_data$Power),
      type = "contour",
      colorscale = "Viridis",
      contours = list(showlabels = TRUE)
    ) %>%
      layout(
        title = "Wavelet Power Spectrum (ML CoP)",
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Scale (log2)", autorange = "reversed")
      )
  })
  
  output$scalogram_ap <- renderPlotly({
    df_segment <- analysis()$df_segment
    Fs <- input$get_filter_frequency
    
    # Perform CWT on AP CoP data
    ap_results <- analyze_cop(df_segment$copy, Fs)
    wavelet_data <- ap_results$wavelet_data
    
    # Create scalogram
    plot_ly(
      x = wavelet_data$axis.1,
      y = log2(wavelet_data$Period),
      z = t(wavelet_data$Power),
      type = "contour",
      colorscale = "Viridis",
      contours = list(showlabels = TRUE)
    ) %>%
      layout(
        title = "Wavelet Power Spectrum (AP CoP)",
        xaxis = list(title = "Time (s)"),
        yaxis = list(title = "Scale (log2)", autorange = "reversed")
      )
  })
  
  output$global_spectrum <- renderPlotly({
    df_segment <- analysis()$df_segment
    Fs <- input$get_filter_frequency
    
    # Perform CWT on ML and AP CoP data
    ml_results <- analyze_cop(df_segment$copx, Fs)
    ap_results <- analyze_cop(df_segment$copy, Fs)
    
    # Extract global power spectra and scales
    ml_power <- ml_results$global_power
    ap_power <- ap_results$global_power
    scales <- ml_results$scales
    
    # Debug: Print lengths
    cat("ML Power length:", length(ml_power), "\n")
    cat("AP Power length:", length(ap_power), "\n")
    cat("Scales length:", length(scales), "\n")
    
    # Create global wavelet spectrum plot
    plot_ly() %>%
      add_lines(x = scales, y = ml_power, name = "ML CoP") %>%
      add_lines(x = scales, y = ap_power, name = "AP CoP") %>%
      layout(
        title = "Global Wavelet Spectrum",
        xaxis = list(title = "Scale (s)", type = "log"),
        yaxis = list(title = "Average Power")
      )
  })
  
  output$wavelet_metrics <- renderTable({
    df_segment <- analysis()$df_segment
    Fs <- input$get_filter_frequency
    
    # Perform CWT on ML and AP CoP data
    ml_results <- analyze_cop(df_segment$copx, Fs)
    ap_results <- analyze_cop(df_segment$copy, Fs)
    
    # Create table of metrics
    data.frame(
      Direction = c("ML", "AP"),
      Total_Variance = c(ml_results$total_variance, ap_results$total_variance),
      Dominant_Scale = c(ml_results$dominant_scale, ap_results$dominant_scale),
      Mean_Power = c(mean(ml_results$global_power), mean(ap_results$global_power))
    )
  })
  
  # Define frequency bands
  freq_bands <- list(
    "0-0.1 Hz" = c(0, 0.1),
    "0.1-0.3 Hz" = c(0.1, 0.3),
    "0.3-1 Hz" = c(0.3, 1)
  )
  
  # Function to compute power and sample entropy for a frequency band
  analyze_frequency_band <- function(wavelet_data, band, Fs) {
    scales <- wavelet_data$Scale  # Use Scale attribute for column scales
    idx <- scales >= band[1] & scales <= band[2]
    
    # Compute power in the band
    band_power <- sum(wavelet_data$Power[, idx], na.rm = TRUE)
    
    # Extract time series for the band
    band_series <- rowSums(wavelet_data$Power[, idx], na.rm = TRUE)
    
    # Compute sample entropy for the band
    band_entropy <- sample_entropy(band_series, edim = 2, r = 0.2 * sd(band_series))
    
    # Return a list with named elements
    return(list(power = band_power, entropy = band_entropy))
  }
  
  # Add combined frequency band analysis to the wavelet metrics table
  output$frequency_band_analysis <- renderTable({
    df_segment <- analysis()$df_segment
    Fs <- input$get_filter_frequency
    
    # Perform CWT on ML and AP CoP data
    ml_results <- analyze_cop(df_segment$copx, Fs)
    ap_results <- analyze_cop(df_segment$copy, Fs)
    
    # Compute power and entropy for each frequency band
    ml_analysis <- lapply(freq_bands, function(band) {
      analyze_frequency_band(ml_results$wavelet_data, band, Fs)
    })
    ap_analysis <- lapply(freq_bands, function(band) {
      analyze_frequency_band(ap_results$wavelet_data, band, Fs)
    })
    
    # Create table of frequency band analysis
    data.frame(
      Frequency_Band = names(freq_bands),
      ML_Power = sapply(ml_analysis, function(x) x$power),
      ML_Entropy = sapply(ml_analysis, function(x) x$entropy),
      AP_Power = sapply(ap_analysis, function(x) x$power),
      AP_Entropy = sapply(ap_analysis, function(x) x$entropy)
    )
  })
  
  output$combined_plot <- renderPlotly({
    df_segment <- analysis()$df_segment
    Fs <- input$get_filter_frequency
    
    # Perform CWT on ML and AP CoP data
    ml_results <- analyze_cop(df_segment$copx, Fs)
    ap_results <- analyze_cop(df_segment$copy, Fs)
    
    # Compute power and entropy for each frequency band
    ml_analysis <- lapply(freq_bands, function(band) {
      analyze_frequency_band(ml_results$wavelet_data, band, Fs)
    })
    ap_analysis <- lapply(freq_bands, function(band) {
      analyze_frequency_band(ap_results$wavelet_data, band, Fs)
    })
    
    # Create data frame for plotting
    analysis_df <- data.frame(
      Frequency_Band = rep(names(freq_bands), each = 2),
      Metric = rep(c("Power", "Entropy"), times = length(freq_bands)),
      ML = c(sapply(ml_analysis, function(x) x$power), sapply(ml_analysis, function(x) x$entropy)),
      AP = c(sapply(ap_analysis, function(x) x$power), sapply(ap_analysis, function(x) x$entropy))
    )
    
    # Reshape the data for plotting
    analysis_long <- analysis_df %>%
      pivot_longer(cols = c(ML, AP), names_to = "Direction", values_to = "Value") %>%
      mutate(Metric_Direction = paste(Metric, Direction, sep = " "))
    
    # Define accessible colors
    colors <- c(
      "Power ML" = "#1f78b4",  # Blue
      "Power AP" = "#33a02c",  # Green
      "Entropy ML" = "#e31a1c",  # Red
      "Entropy AP" = "#ff7f00"  # Orange
    )
    
    # Create grouped bar plot
    plot_ly(analysis_long, x = ~Frequency_Band, y = ~Value, color = ~Metric_Direction, 
            colors = colors, type = "bar") %>%
      layout(
        title = "Power and Sample Entropy Across Frequency Bands",
        xaxis = list(title = "Frequency Band"),
        yaxis = list(title = "Value"),
        barmode = "group",
        legend = list(title = list(text = "<b>Metric and Direction</b>")))
  })
  
  ## Output COP Sway plot 1
  output$sway_area_1 <- renderPlotly({
    analysis_data <- analysis()
    segment_pea <- analysis_data$segment_pea
    plot1 <- ggplotly(
      segment_pea$plot1 +
        labs(x = "COP Medio-Lateral direction", y = "COP Antero-Posterior direction ") +
        ggtitle("COP 95% Predicted Ellipse Area"))
    return(plot1)
  })
  
  ## Output COP Sway plot 2
  output$sway_area_2 <- renderPlotly({
    analysis_data <- analysis()
    segment_pea <- analysis_data$segment_pea
    plot2 <- ggplotly(
      segment_pea$plot2 +
        labs(x = "COP Medio-Lateral direction", y = "COP Antero-Posterior direction ") +
        ggtitle("COP Pathlength"))
    return(plot2)
  })
}

shinyApp(ui, server)
