shinyServer(function(input, output) {
  # restrict upload to 50 mb
  options(shiny.maxRequestSize = 50*1024^2, shiny.usecairo = FALSE)

  # Using reactiveValues to store inside a reactive object
  #values <- reactiveValues(myobj = NULL)

  # source files that are stored inside a src folder to maintain clean coding
  #source('src/srv_load.R', local = TRUE)

  data <- reactive({
    CombineModes(tune = input$tune, nfrets = 22, basemode = input$basemode, modes = input$othermode)
  })

  chords <- reactive({
    #as.data.frame(Degree = c(1:7), Chord = unique(data()[,3]))
    unique(data()$chords)
  })
  output$view <- renderTable({
    chords()
  })

  output$plot <- renderPlot({
    GuitarPlot(data(), labsize = 3)
  }, height = 250)


# The end ---------------------------------------------------------------------
})

