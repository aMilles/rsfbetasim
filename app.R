shinyApp(
  ui =shinyUI(fluidPage(
    
    # Sidebar with a slider input for number of bins
    sidebarPanel(
      sliderInput("samples", label = "number of selections (1=20, 5=400)",min = 1, max = 5, value = match(steps, c(20,50,100,200,400))),
      checkboxInput("variation", "Variation among individuals", value = ifelse(ind.sd > 0, T, F)),
      checkboxGroupInput("sampling", "Type of sampling", c("availability", "absence"), selected= absence.sampling),
      checkboxGroupInput("model", "Model complexity", c("simple", "complex"), selected= mode)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel('Bayes', shiny::plotOutput("leaflet_map")),
                  tabPanel('GLMM', shiny::plotOutput("circular_map"))
      )))),
  server = shinyServer(function(input, output) {
    output$leaflet_map <- renderPlot({
      var <- ifelse(input$variation, 0.33, 0)
      sample.size <- c(20,50,100,200,400)[input$samples]
      maptab <- get(paste0("betaplots_", input$model, "_", input$sampling, "_sd_", var, "_samples_", sample.size))
      plot(x = factor(maptab[[5]], level = maptab[[5]]), y = rep(-100, ncol(maptab[[3]])), pch = "+", ylim = c(-maptab[[4]] * 1.3, maptab[[4]] * 1.3))
      abline(h = 0, lty = "dotted", col = "grey")
      boxplot(axes = F, x = maptab[[1]], add = T, col = "lightgreen", pch = "", border = "darkgreen")
      boxplot(axes = F, maptab[[2]], ylim = c(-1, 1), add = T, col = rgb(1,1,1, .5), main = "Distribution of Bayesian betas vs. Truth (green)", pch = "")}
    )
    # ### Top Doctors circular map
    output$circular_map <- renderPlot({
      var <- ifelse(input$variation, 0.33, 0)
      sample.size <- c(20,50,100,200,400)[input$samples]
      maptab <- get(paste0("betaplots_", input$model, "_", input$sampling, "_sd_", var, "_samples_", sample.size))
      plot(x = factor(maptab[[5]], level = maptab[[5]]), y = rep(-100, ncol(maptab[[3]])), pch = "+", ylim = c(-maptab[[4]] * 1.3, maptab[[4]] * 1.3))
      abline(h = 0, lty = "dotted", col = "grey")
      boxplot(axes = F, x = maptab[[1]], add = T, col = "lightgreen", pch = "", border = "darkgreen")
      boxplot(axes = F, maptab[[3]], ylim = c(-1, 1), add = T, col = rgb(1,1,1, .5), main = "Distribution of betas of a GLMM vs. Truth (green)", pch = "")
    })
  }) # ,options = list(height = 480, width = 1050, dpi=200)
  
)