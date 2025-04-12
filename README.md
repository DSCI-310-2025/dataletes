
<!-- badges: start -->

[![Tests](https://github.com/DSCI-310-2025/dataletes/actions/workflows/test.yml/badge.svg)](https://github.com/DSCI-310-2025/dataletes/actions/workflows/test.yml)
[![Codecov test
coverage](https://codecov.io/gh/DSCI-310-2025/dataletes/graph/badge.svg)](https://app.codecov.io/gh/DSCI-310-2025/dataletes)
<!-- badges: end -->

## Package Summary

This package is used to download and visualize data. It contains basic
functions for downloading and saving data into file locations while also
removing unwanted columns to clean it. It also contains a function for
plotting a list of variables against a target variable to analyze trends
in a dataset. There also exists a function to convert tables into PNG
files for usage in reports.

## Related Packages

This package is very related to the ggplot2 library and uses it
extensively for the creation of visualizations. It differs from the
library since it can be used to visualize tables as well as plots. This
allows for tables to be more easily incorporated into reports as PNGs.
The package is also similar to the dplyr package since they both allow
for users to filter unwanted data out of the datasets. However, the data
cleaning function in our package has less flexibility than the filter
function in the dyplr package.
