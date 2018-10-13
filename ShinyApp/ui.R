shinyUI(

  navbarPage(title = "",
             windowTitle = "GuitarModes", theme = shinytheme("flatly"),
             # Exampe tabPanel (copy for multiple tabs) -------------------------
             tabPanel(div(h4("GuitarModes")),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("nfrets", label = h4("Number of frets"), min = 19,
                                      max = 27, value = 22),
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
                          radioButtons("targets", label = h4("Show target notes"),
                                       choices = list("1-3-5" = 1, "1-3-5-7" = 2),
                                       selected = 1),
                          sliderInput("textsize", label = h4("Text size"), min = 0,
                                      max = 10, value = 5),
                          sliderInput("neck", label = h4("show frets:"), min = 0,
                                      max = 27, value = c(0, 22)),


                          # indicator that the server is busy (copy in every sidebar on each tab)
                          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                           tags$div(id="loadmessage",
                                                    img(src="spin.svg", width = 30, height = 30))),
                          width = 2),

                        mainPanel(
                          h4("Church modes for Guitar"),
                          p("In order to better understand the possibilities of using the church modes I created this application together with my teacher",
                          a("Mario Miracolo.", href="https://gitaarschoolmariobreda.com/"),"- Ronny Joosen"),

                          plotOutput('plot'),
                          h4("Chords that belong to the selected basemode and tune:"),
                          DT::dataTableOutput("chordtable"),
                          h4("Click to show target notes. Green = basenote (1), Orange = third (3), Blue = fifth (5), Pink = seventh (7)"),
                          #verbatimTextOutput("selectedCells"),
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

