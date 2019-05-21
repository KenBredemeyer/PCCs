# PCCs

Plot Person Characteristic Curves using pairwise comparison data.  Person Characteristic Curves (diagnostic plots) are analagous
to Item Characteristic Curves in Item Response Theory.  Observed proportions for a given number of class intervals are plotted 
against a theoretical probability curve using the Bradley-Terry-Luce model (Bradley & Terry, 1952; Luce, 1959).

## Installation
``` r
# Install from GitHub
devtools::install_github("KenBredemeyer/PCCs")
```

## Usage
The PCCs package contains just one function, `plot_PCCs`, and uses a pairwise comparison data matrix of proportions, and location
estimates as arguments.

Using the built in data set as an example:
``` r
plot_PCCs(gtpa2018_proportions_dm, gtpa2018_locations,
          class_intervals = 3,
          performances = 21:22)
```
Access documentation using
``` r
?plot_PCCs
```
