GuitarModes
================

Introduction
------------

Find the positioning of different modes on your guitar. Indicate which chords can be played in the choosen mode/tune and indicate the position of ground notes. Functions defined as R package complemented with a Shiny app for interactive usage.

Package functionality
---------------------

The package consist out of several core functions:

Done:

1.  GetNote: Get the note from a given posion (with flexible tuning)
2.  GetPosition: Get all positions of a certain note (with flexible tuning)
3.  GetMode: Get mode positions and tones (at standard tuning)
4.  CombineModes: Start with a mode in a tune and get the positions of the other modes

<!-- -->

1.  GuitarPlot: Plot Guitar with flexible label size, number of frets, first and last fret to show

Below we give some examples of the core functions

Get a note from a given position:

``` r
GetNote(string = 1,position = 3)
```

    ##   equiv1 equiv2 equiv3
    ## 8      G     Fx    Abb

Get positions of a given note:

``` r
Apositions <- GetPosition("A")
kable(Apositions)
```

|  string|  position| note |
|-------:|---------:|:-----|
|       1|         5| A    |
|       1|        17| A    |
|       2|         0| A    |
|       2|        12| A    |
|       3|         7| A    |
|       3|        19| A    |
|       4|         2| A    |
|       4|        14| A    |
|       5|        10| A    |
|       5|        22| A    |
|       6|         5| A    |
|       6|        17| A    |

Plot on a GuitarPlot

``` r
GuitarPlot(Apositions, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

Get and plot the Ionian mode in C

``` r
Ionian3 <- GetMode(tune = "C",nfrets = 22,mode = "Ionian3")
GuitarPlot(Ionian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

Get and plot the Ionian mode in A\#

``` r
Ionian3 <- GetMode(tune = "A#",nfrets = 22,mode = "Ionian3")
GuitarPlot(Ionian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

Get and plot the Dorian3 mode in D

``` r
Dorian3 <- GetMode(tune = "D",nfrets = 22,mode = "Dorian3")
GuitarPlot(Dorian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

Get and plot the Phrygian3 mode in E

``` r
Phrygian3 <- GetMode(tune = "E",nfrets = 22,mode = "Phrygian3")
GuitarPlot(Phrygian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-7-1.png)

Get and plot the Lydian3 mode in F

``` r
Lydian3 <- GetMode(tune = "F",nfrets = 22,mode = "Lydian3")
GuitarPlot(Lydian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

Get and plot the MixoLydian3 mode in G

``` r
MixoLydian3 <- GetMode(tune = "G",nfrets = 22,mode = "MixoLydian3")
GuitarPlot(MixoLydian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

Get and plot the Aeolian3 mode in A

``` r
Aeolian3 <- GetMode(tune = "A",nfrets = 22,mode = "Aeolian3")
GuitarPlot(Aeolian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

Get and plot the Locrian3 mode in B

``` r
Locrian3 <- GetMode(tune = "B",nfrets = 22,mode = "Locrian3")
GuitarPlot(Locrian3, labsize = 3)
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

Get positions of all modes starting from a centain mode and tune

``` r
# Combine <- CombineModes(tune = "F",nfrets = 22,basemode = "Lydian3", modes = c("Ionian3"))
Combine <- CombineModes(tune = "C",basemode = "Ionian3", modes = NULL)
GuitarPlot(Combine)
```

![](README_files/figure-markdown_github/unnamed-chunk-12-1.png)

Get positions of all modes starting from a centain mode and tune

``` r
Combine <- CombineModes(tune = "F",nfrets = 22, basemode = "Lydian3", modes = c("Ionian3"))
GuitarPlot(Combine)
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

Or completely flexible generate any chord or setting in the plot

``` r
Ecord <- data.frame(string = c(1,2,3,4,5,6), 
                    position = c(0,2,2,1,0,0), 
                    note = c("E", "B", "E", "G#/Ab","B", "E"))
GuitarPlot(Ecord, nfrets = 4, labsize = 4)
```

![](README_files/figure-markdown_github/unnamed-chunk-14-1.png)
