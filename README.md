
# epicdata

The goal of epicdata is to summarize data processing in epidemiology in
a central package. It includes functions to set up corresponding R
projects and is based on the R package
[dataquieR](https://dataquality.qihs.uni-greifswald.de/IntroductoryTutorial.html)
and its concept of data quality in obervational health data. However,
while dataquieR focuses on quality reporting, this package focuses on
data processing.

## Installation

You can install the development version of epicdata from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("forsterepi/epicdata")
```

## Workflow

The `epicdata` workflow consists of several stages:

- `meta`: Setup of meta data based on the meta data concept in
  `dataquieR`, but with some additions and changes.
- `raw`: Raw data is pre-processed data and should continue to be
  available to re-do data processing. However, some steps need to be
  taken to be able to save raw data, e.g., removing participans who did
  not provide informed consent.
- `prc`: Data processing, i.e., preparing data for analysis, is the main
  part of this workflow and uses meta data and raw data to create
  processed data, which can be analyzed. Important parts of data
  processing include comparing multiple data entries of paper
  questionnaires, handling missing data, aa well as handling of range
  violations and contradictions. Based on the provided information, a
  data dictionary is created automatically.
- `pre`: Pre-analysis describes the process of creating analysis data
  from processed data. This usually includes joining of multiple data
  sources, variable selection, and processing of variables that contain
  too much information about participants, e.g., creating age groups
  from dates of birth. Ideally, if data confidentiality allows it, the
  resulting analysis data is published alongside the analysis code (see
  next bullet point) to enable reproducibility.
- `anl`: An R project which uses the data created in pre-analysis and
  analyzes it to derive scientific results, which are then published.

## Example

TBC
