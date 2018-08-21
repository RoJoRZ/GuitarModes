GetNote(1,3)
AllG <- GetPosition("A")
GuitarPlot(AllG, labsize = 2)

Ecord <- data.frame(string = c(1,2,3,4,5,6), position = c(0,2,2,1,0,0), note = c("E", "B", "E", "G#/Ab","B", "E"))
GuitarPlot(Ecord, nfrets = 4, labsize = 4)

Ionian <- GetMode("C", "Ionian3")
GuitarPlot(Ionian, labsize = 3)
