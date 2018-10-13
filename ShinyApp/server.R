shinyServer(function(input, output) {
  # restrict upload to 50 mb
  options(shiny.maxRequestSize = 50*1024^2, shiny.usecairo = FALSE)

  # Using reactiveValues to store inside a reactive object
  #values <- reactiveValues(myobj = NULL)

  # source files that are stored inside a src folder to maintain clean coding
  #source('src/srv_load.R', local = TRUE)

  data <- reactive({
    CombineModes(tune = input$tune, nfrets = input$nfrets, basemode = input$basemode, modes = input$othermode)
  })

  chords <- reactive({
    #as.data.frame(Degree = c(1:7), Chord = unique(data()[,3]))
    data.frame(unique(data()$chords))
  })

  output$chordtable = DT::renderDataTable({
    t(chords())},
  class = 'cell-border stripe',
  rownames = FALSE,
  colnames = c('I', 'II', 'III', 'IV', 'V', 'VI','VII'),
  selection=list(mode="single", target="cell"),
  options = list(searching = FALSE, paging = FALSE))

  output$selectedCells <- renderPrint(sum(input$chordtable_cells_selected))

  output$plot <- renderPlot({
    GuitarPlot(data(),
               nfrets = input$nfrets,
               firstfret = input$neck[1],
               lastfret = input$neck[2],
               labsize = input$textsize,
               target = input$targets,
               targetstart = sum(input$chordtable_cells_selected))
  }, height = 400)


# The end ---------------------------------------------------------------------
})

