# Resources for nonlinear analyses

Tools and tutorials to help with recurrence quantification analysis (RQA),
cross-recurrence quantification analysis (CRQA), and detrended fluctuation
analysis (DFA).

## Tutorials

Here are the tutorials that are currently available:

+ `./tutorials/tutorial-categorical_recurrence.R`: Tutorial for categorical
  recurrence quantification analysis and cross-recurrence quantification
  analysis in R.
+ `./tutorials/tutorial-continous_recurrence.R`: Tutorial for continuous
  recurrence quantification analysis and cross-recurrence quantification
  analysis in R, including conducting manual parameter search.
+ `./tutorials/tutorial-dfa.R`: Tutorial for detrended fluctuation analysis.

Data for the tutorials are included in the `./tutorials/data/` directory.

### Acknowledgements for example data

The informative text on chickens comes from the "Chicken" entry from Wikipedia
(https://en.wikipedia.org/wiki/Chicken). The poetic text on chickens comes
from "Last Night I Dreamed of Chickens" by Jack Prelutsky
(https://poets.org/poem/last-night-i-dreamed-chickens).

The converted versions of the text files come thanks to [Rick Dale](https://co-mind.org/ati/)'s
[free "Text Converter" tool](https://co-mind.org/ati/converter.html).

The data for the fractal analysis are courtesy of the 
[U.S. National Oceanic and Atmospheric Association](https://www.noaa.gov/)'s
[Climate Data Online API](https://www.ncdc.noaa.gov/cdo-web/search) from the
[National Centers for Environmental Information](https://www.ncei.noaa.gov/).

## Tools

Here are the tools that are currently available:

+ Visualizations
  + `./tools/create-recurrence-plots-ggplot.r`: Create recurrence plot with
    `qplot` function from `ggplot2` package
  + `./tools/create-recurrence-plots-standard.R`: Create recurrence plot with
    standard `plot` function

## Dependencies

To run these, you will need the following R packages:

+ `crqa` (Coco & Dale, 2014, *Frontiers in Psychology*)
+ `tidyr`
+ `nonlinearTseries`
+ `tseriesChaos`
+ `ggplot2`

## Contributing

Contributions are always welcome! Just submit a pull request to get started.
