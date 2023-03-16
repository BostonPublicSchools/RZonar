
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RZonar

<!-- badges: start -->
<!-- badges: end -->

RZonar makes it easy to retrieve and cleanup data from the Zonar API.

## Installation

The development version of RZonar is available from GitHub. Install
with:

``` r
# install.packages("remotes")
remotes::install_github("BostonPublicSchools/RZonar")
```

## Example

This is a basic example which shows you how to retrieve all schedule
events for a specified time period:

``` r
library(RZonar)
sched_data <- zonar_get_schedules(
  start = paste(Sys.Date(), "07:00:00"),
  end = paste(Sys.Date(), "07:15:00"))
dplyr::glimpse(sched_data[0,])
#> Rows: 0
#> Columns: 11
#> $ zoneID             <int> 
#> $ Zone               <chr> 
#> $ category           <chr> 
#> $ `Asset ID`         <chr> 
#> $ Asset              <chr> 
#> $ Time               <dttm> 
#> $ `IN/OUT`           <chr> 
#> $ `Time In Zone`     <chr> 
#> $ Duration           <chr> 
#> $ `Duration Total`   <chr> 
#> $ `Distance (Miles)` <chr>
```

The data returned from the Zonar API is not always formatted
conveniently. RZonar also provides convenience functions to post-process
data returned from the API:

``` r
dplyr::glimpse(zonar_cleanup(sched_data)[0,])
#> Rows: 0
#> Columns: 9
#> $ zoneID     <int> 
#> $ Zone       <chr> 
#> $ category   <chr> 
#> $ `Asset ID` <chr> 
#> $ Asset      <chr> 
#> $ tgroup     <int> 
#> $ time_in    <dttm> 
#> $ time_out   <dttm> 
#> $ duration   <drtn>  secs
```

## Documentation

All exported functions are documented and the package includes vignettes
to help you understand how the package is structured and what it is good
for. On-line documentation can be found at
<https://bostonpublicschools.github.io/RZonar/>.
