#' Position2Note
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
#' Postion2note(string = 1,position = 1,tuning = c("E","A","D","G","B","E"))
#'
#' @export

Postion2Note <- function(string, position, tuning = NULL) {
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
