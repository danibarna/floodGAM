
<h1 align="center">
The gamfelt dataset
</h1>

This folder contains the annual maxima and catchment descriptors for the
`gamfelt` dataset. We summarize some properties of the dataset below.

## File structure

- `gamfelt_annual_maxima` - the annual maxima, in $m^3/s$.
- `gamfelt_catchment_descriptors` - catchment descriptors for each
  gamfelt station.
- `README-catchment-descriptors.txt` - description of each variable in
  the catchment descriptor dataset.

## Describing the dataset

The `gamfelt` dataset contains annual maxima for 249 stations in Norway.
Each station has at least 20 years of total data and 10 years of
sub-daily (fine) data. Each year of data has been assessed using the
process described in the [data README](/data/README.md).

The stations represent a reasonably diverse collection of catchment
sizes and hydroclimatic regimes:

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

| Catchment area ($km^2$) | 0 - 2 $km^2$ | 2 - 10 $km^2$ | 10 - 60 $km^2$ | 60 - 100 $km^2$ | 100 - 500 $km^2$ | 500+ $km^2$ |
|:-----------------------:|:------------:|:-------------:|:--------------:|:---------------:|:----------------:|:-----------:|
| **Number of stations**  |      2       |      18       |       67       |       27        |        94        |     39      |

``` r
par(mfrow=c(1,2),mar = c(5,5,1,1))

hist(percentfin$N,breaks=20,
     xlab="Record length (years)",ylab="Stations",main="",
     col="white")
H2 <- hist(percentfin$perct,xaxt="n",
           xlab="Percent of record that is subdaily data",ylab="Stations",
           main="",
           col="white")
axis(side=1, at=H2$breaks, labels=paste0(H2$breaks, "%"))
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# saved manually as data_details_histograms.pdf, landscape, 11.5 
```

## How fine is “fine data”?
