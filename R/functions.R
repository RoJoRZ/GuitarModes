#' DefNotes
#'
#' Define note notation
#'
#' @param notes note notation, NULL will get the default
#'
#' @return notes
#'
#' @examples
#' DefNotes()
#'
#' @export

DefNotes <- function(notes = NULL) {
  if (is.null(notes)) {
  notes <- data.frame(equiv1 =
                         c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B"),
                      equiv2 =
                         c("B#","Bx","Cx","Eb","Dx","E#","Ex","Fx","G#","Gx","Bb","Ax"),
                      equiv3 =
                         c("Dbb","Db","Ebb","Fbb","Fb","Gbb","Gb","Abb","Ab","Bbb","Cbb","Cb"))
  notes <- as.data.frame(sapply(notes, rep.int, times = 3), stringsAsFactors = F)
  }
  return(notes)
}

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
  notes <- DefNotes()
  note <- notes[which(tuning[string] == notes)[1] + position,]
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
  notes <- DefNotes()
  # create a df with all notes on every position
  allnotes <- data.frame(string = rep(c(1:6),nfrets + 1), position = c(0:nfrets)) %>%
              rowwise() %>%
              mutate(equiv1 = GuitarModes::GetNote(string, position)[1,1]) %>%
              mutate(equiv2 = GuitarModes::GetNote(string, position)[1,2]) %>%
              mutate(equiv3 = GuitarModes::GetNote(string, position)[1,3])
  # filter the df to the required note
  positions <- allnotes[which(note == allnotes, arr.ind = T)[,1],] %>% arrange(string, position)
  positions <- positions[,c(1,2,which(note == allnotes, arr.ind = T)[1,2])]
  colnames(positions)[3] <- "note"
  return(positions)
}

#' GetMode
#'
#' Return dataframe with string, position, note from modes:
#' make sure mode is "Ionian3", "Dorian3", "Phrygian3", "Lydian3",
#' "MixoLydian3", "Aeolian3" or "Locrian3"
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

GetMode <- function(tune, nfrets = NULL, mode = NULL) {
  if (is.null(mode)) mode = "Ionian3"

  # nfrets = 22 used if no nfrets is indicated
  if (is.null(nfrets)) nfrets <- 22

  notes <- DefNotes()
  shift <- which(notes == tune, arr.ind = T)[1,1]+7
  if (shift > 12) shift <- shift - 12

  if (mode == "Ionian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,1,1,2,2,2,2,2,2,4,4,4,4,4,4,5,5)) %>%
                arrange(string, position) %>%  mutate(chords = rep(c("","m","m","","","m","dim"),3)[1:18])
  }

  if (mode == "Dorian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,0,0,2,2,2,2,2,2,3,3,3,4,4,4,5,5)) %>%
      arrange(string, position) %>%  mutate(chords = rep(c("m","m","","","m","dim",""),3)[1:18])
  }

  if (mode == "Phrygian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,0,0,1,1,1,2,2,2,3,3,3,3,3,4,5,5)) %>%
      arrange(string, position) %>%  mutate(chords = rep(c("m","","","m","dim","","m"),3)[1:18])
  }

  if (mode == "Lydian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,1,1,1,2,2,2,2,2,3,4,4,4,4,4,4,5,6)) %>%
      arrange(string, position) %>%  mutate(chords = rep(c("","","m","dim","","m","m"),3)[1:18])
  }

  if (mode == "MixoLydian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,0,1,2,2,2,2,2,2,3,4,4,4,4,4,5,5)) %>%
      arrange(string, position) %>%  mutate(chords = rep(c("","m","dim","","m","m",""),3)[1:18])
  }

  if (mode == "Aeolian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,0,0,1,2,2,2,2,2,3,3,3,3,4,4,5,5)) %>%
      arrange(string, position) %>%  mutate(chords = rep(c("m","dim","","m","m","",""),3)[1:18])
  }

  if (mode == "Locrian3") {
    modepos <- data.frame(string = rep(1:6,3), position = c(0,0,0,0,1,1,1,1,2,2,3,3,3,3,3,3,5,5)) %>%
      arrange(string, position) %>%  mutate(chords = rep(c("dim","","m","m","","","m"),3)[1:18])
  }

  modepos <- modepos %>% mutate(position = position + shift) %>%
    rowwise() %>%
    mutate(equiv1 = GuitarModes::GetNote(string, position)[1,1]) %>%
    mutate(equiv2 = GuitarModes::GetNote(string, position)[1,2]) %>%
    mutate(equiv3 = GuitarModes::GetNote(string, position)[1,3])

  alphabet <- rep(toupper(letters[1:7]),4)
  first <- grep(substr(tune, 1,1), alphabet)[1]-1
  for (i in 1:nrow(modepos)) {
    if (i == 1) modepos$note <- tune else {
    enharm <- alphabet[first + i ]
    modepos[i,7] <- as.matrix(modepos[i,grep(enharm, substr(as.matrix(modepos[i,]),1,1))[1]])
    }
  }
  modepos <- modepos[,c(1,2,3,7)]
  modepos2 <- mutate(modepos, position = position + 12) %>% filter(position <= nfrets)
  modepos <- rbind(modepos,modepos2)
  modepos <- modepos %>% mutate(chords = paste0(note, chords))

  return(modepos)
}

#' CombineModes
#'
#' Combine Modes using their intervals
#'
#' @param tune base tune
#' @param basemode base mode
#' @param modes modes to combine using the intervals (NULL is all modes)
#'
#' @return df with string,position, note and mode
#'
#' @examples
#' CombineModes(tune = "A",basemode = "Ionian3", modes = c("Ionian3", "Dorian3", "Phrygian3"))
#'
#' @export

CombineModes <- function(tune, nfrets = NULL, basemode, modes = NULL) {
  # Standard modes if modes = NULL


  modeintervals <- data.frame(mode = c("Ionian3", "Dorian3", "Phrygian3", "Lydian3",
                                       "MixoLydian3", "Aeolian3", "Locrian3"),
                              interval = c(2,4,6,7,9,11,13))
  # nfrets = 22 used if no nfrets is indicated
  if (is.null(nfrets)) nfrets <- 22

  notes <- DefNotes()

  # get base mode data
  modecomb <- GetMode(tune, nfrets, basemode)
  modecomb$mode <- basemode
  basemodepos <- which(basemode == modeintervals$mode)
  # now combine with other modes
  if (!is.null(modes)) {
  for (i in 1:length(modes)) {
    #modestart <- which(basemode == modeintervals$mode)
    nextmode <- which(modes[i] == modeintervals$mode)
    if (basemodepos < nextmode) {
      nextinterval <- nextmode - basemodepos
    } else {
      nextinterval <- (7- basemodepos) + nextmode
    }
    nexttune <- modecomb$note[1+nextinterval]
    nextcomb <- GetMode(nexttune, nfrets, modes[i])
    nextcomb$mode <- modes[i]
    modecomb <- rbind(modecomb, nextcomb)
  }
  }

  return(modecomb)
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

GuitarPlot <- function(data, nfrets = NULL,
                       firstfret = NULL, lastfret = NULL,
                       tuning = NULL,
                       labsize = 4,
                       target = 1,
                       targetstart = 1) {

  # nfrets = 22 used if no nfrets is indicated
  if (is.null(nfrets)) nfrets <- 22
  # nfrets = 22 used if no nfrets is indicated
  if (is.null(firstfret)) firstfret <- 0
  # nfrets = 22 used if no nfrets is indicated
  if (is.null(lastfret)) lastfret <- nfrets
  # Standard tuning is used if no tuning is indicated
  if (is.null(tuning)) tuning <- c("E","A","D","G","B","E")
data$targets <- rep(c(1,0,1,0,1,0,1), times=ceiling(nrow(data)/7))[1:nrow(data)]

ggplot(data, aes(x=string, y=position, label = note, fill = targets)) +
  geom_label(size = labsize, color = "black") +
  scale_x_continuous(limits=c(1,6), breaks=seq(1, 6, 1), labels = tuning) +
  scale_y_continuous(limits= c(firstfret,lastfret), breaks=seq(0, 22, 1)) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_line(colour = "black")) +
  theme(panel.grid.major.y = element_line(colour = "black")) +
  theme(panel.grid.minor.y = element_blank())
}


