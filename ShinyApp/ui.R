shinyUI(

  navbarPage(title = "",
             windowTitle = "GuitarModes", theme = shinytheme("flatly"),
             # Exampe tabPanel (copy for multiple tabs) -------------------------
             tabPanel(div(h4("GuitarModes")),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("tune", label = h4("Tune"),
                                      choices = c("Ab","A","A#","Bb","B","B#",
                                                  "Cb","C","C#","Db","D","D#",
                                                  "Eb","E","E#","Fb","F","F#",
                                                  "Gb","G","G#"),
                                      selected = "C"),
                          br(),
                          selectInput("basemode", label = h4("Mode"),
                                      choices = c("Ionian3", "Dorian3", "Phrygian3", "Lydian3",
                                                  "MixoLydian3", "Aeolian3", "Locrian3"),
                                      selected = "Ionian3"),
                          selectInput("othermode", label = h4("Show other ModePositions"),
                                      choices = c("Ionian3", "Dorian3", "Phrygian3", "Lydian3",
                                                  "MixoLydian3", "Aeolian3", "Locrian3"),
                                      multiple = TRUE, selectize = TRUE),
                          # indicator that the server is busy (copy in every sidebar on each tab)
                          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                           tags$div(id="loadmessage",
                                                    img(src="spin.svg", width = 30, height = 30))),
                          width = 2),

                        mainPanel(
                          plotOutput('plot'),
                          tableOutput("view"),
                          tagList(
                            tags$head(
                              tags$link(rel="stylesheet", type="text/css",href="style.css"),
                              tags$script(type="text/javascript", src = "busy.js")
                            )
                          )
                        )))

# The end ---------------------------------------------------------------------
)
)

