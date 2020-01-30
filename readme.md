# Resources for recurrence quantification analysis

Tools and tutorials to help with RQA and CRQA.

## Tutorials

Here are the tutorials that are currently available:

+ `./tutorials/tutorial-categorical_recurrence.R`: Tutorial for categorical
  recurrence quantification analysis and cross-recurrence quantification
  analysis in R.
+ `./tutorials/tutorial-continous_recurrence.R`: Tutorial for continuous
  recurrence quantification analysis and cross-recurrence quantification
  analysis in R, including conducting manual parameter search.

Data for the tutorials are included in the `./tutorials/data/` directory.

### Acknowledgements for example data

The informative text on chickens comes from the "Chicken" entry from Wikipedia
(https://en.wikipedia.org/wiki/Chicken). The poetic text on chickens comes
from "Last Night I Dreamed of Chickens" by Jack Prelutsky
(https://poets.org/poem/last-night-i-dreamed-chickens).

The converted versions of all files come thanks to [Rick Dale](https://co-mind.org/ati/)'s
[free "Text Converter" tool](https://co-mind.org/ati/converter.html).

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
+ `dplyr`
+ `nonlinearTseries`
+ `tseriesChaos`
+ `ggplot2`

## Contributing

Contributions are always welcome! Just submit a pull request to get started.
