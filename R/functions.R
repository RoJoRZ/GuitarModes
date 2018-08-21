#' GetNote
#'
#' Return note at position
#'
#' @param string string number
#' @param position position number
#' @param tuning Character vector (Null for standard tuning)
#'
#' @return note
#'
#' @examples
#' GetNote(string = 1,position = 1,tuning = c("E","A","D","G","B","E"))
#'
#' @export

GetNote <- function(string, position, tuning = NULL) {
  # Standard tuning is used if no tuning is indicated
  if (is.null(tuning)) tuning <- c("E","A","D","G","B","E")
  # Note notation
  notes <- rep(c("A", "A#/Bb",
             "B",
             "C","C#/Db",
             "D","D#/Eb",
             "E",
             "F","F#/Gb",
             "G", "G#/Ab"),4)
  note <- notes[which(tuning[string] == notes)[1] + position]
  return(note)
}

#' GetPosition
#'
#' Return positions of a note
#'
#' @param note
#' @param nfrets number of frets (Null for 22 standard)
#' @param tuning Character vector (Null for standard tuning)
#'
#' @return positions df as string,position
#'
#' @examples
#' GetPosition(note = "A",tuning = c("E","A","D","G","B","E"))
#'
#' @export

GetPosition <- function(note, nfrets = NULL, tuning = NULL) {
  # Standard tuning is used if no tuning is indicated
  if (is.null(tuning)) tuning <- c("E","A","D","G","B","E")
  # nfrets = 22 used if no nfrets is indicated
  if (is.null(nfrets)) nfrets <- 22
  # Note notation
  notes <- rep(c("A", "A#/Bb",
                 "B",
                 "C","C#/Db",
                 "D","D#/Eb",
                 "E",
                 "F","F#/Gb",
                 "G", "G#/Ab"),4)
  # create a df with all notes on every position
  allnotes <- data.frame(string = rep(c(1:6),nfrets + 1), position = c(0:nfrets)) %>%
              rowwise() %>% mutate(note = GuitarModes::GetNote(string, position))
  # filter the df to the required note
  positions <- allnotes[which(note == allnotes$note),]
  return(positions)
}

#' GetMode
#'
#' Return dataframe with string, position, note
#'
#' @param tune base note
#' @param mode mode
#'
#' @return position of the mode
#'
#' @examples
#' GetMode("A", "Ionian3")
#'
#' @export

GetMode <- function(tune, mode = c("Ionian3")) {

  notes <- rep(c("A", "A#/Bb",
                 "B",
                 "C","C#/Db",
                 "D","D#/Eb",
                 "E",
                 "F","F#/Gb",
                 "G", "G#/Ab"),2)

  if (mode == "Ionian3") {
    shift <- which(notes == tune)[2]-8
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,1,1,2,2,2,2,2,2,4,4,4,4,4,4,5,5)) %>%
    mutate(position = position + shift) %>%
    rowwise() %>% mutate(note = GuitarModes::GetNote(string, position))

  }
  return(modepos)
}



#' GuitarPlot
#'
#' Plot guitar using ggplot
#'
#' @param data dataframe with string, position, note
#' @param nfrets number of frets (Null for 22 standard)
#' @param firstfret first fret to show (default to 0)
#' @param lastfret last fret to show (default to 22)
#' @param tuning Character vector (Null for standard tuning)
#' @param labsize labelsize
#'
#' @return guitarplot
#'
#' @examples
#' GuitarPlot(data = data.frame(string = c(1,2,3,4), position = c(0,2,2,1), note = c("E", "B", "E", "G#/Ab")))
#'
#' @export

GuitarPlot <- function(data, nfrets = NULL, firstfret = NULL, lastfret = NULL, tuning = NULL, labsize = 4) {

  # nfrets = 22 used if no nfrets is indicated
  if (is.null(nfrets)) nfrets <- 22
  # nfrets = 22 used if no nfrets is indicated
  if (is.null(firstfret)) firstfret <- 0
  # nfrets = 22 used if no nfrets is indicated
  if (is.null(lastfret)) lastfret <- nfrets
  # Standard tuning is used if no tuning is indicated
  if (is.null(tuning)) tuning <- c("E","A","D","G","B","E")

ggplot(data, aes(x=string, y=position, label = note)) +
  geom_label(size = labsize, colour = "black") +
  scale_x_continuous(limits=c(1,6), breaks=seq(1, 6, 1), labels = tuning) +
  scale_y_continuous(limits= c(firstfret,lastfret), breaks=seq(0, 22, 1)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_line(colour = "black")) +
  theme(panel.grid.major.y = element_line(colour = "black")) +
  theme(panel.grid.minor.y = element_blank())
}


