[Last update: 24 September 2020]

R version `4.0.2` and above is required to run this example.

The `sdmbench` package can be downloaded with the following code (it requires the package 'devtools'):

```r
library(devtools)
install_github("boyanangelov/sdmbench")
```

If you are working in a Windows environment, to install `sdmbench` is probably required to install `Rtools40` as well. It is available at https://cran.r-project.org/bin/windows/Rtools/. For reproducibility purposes we do not obtain the dataset of African Elephant GBIF and raster data dynamically in this example, but read the datasets locally instead (stored as`RDS` files).

[![DOI](https://zenodo.org/badge/254860266.svg)](https://zenodo.org/badge/latestdoi/254860266)
