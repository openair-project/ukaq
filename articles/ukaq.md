# Importing UK Air Quality Data

``` r

library(ukaq)
```

## Metadata

The first step when importing air quality data is to consult
[`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md).
Let’s have a look at the AURN metadata now:

``` r

import_ukaq_meta("aurn")
#> # A tibble: 340 × 14
#>    source code  site    site_type latitude longitude start_date end_date   zone 
#>    <chr>  <chr> <chr>   <chr>        <dbl>     <dbl> <date>     <date>     <chr>
#>  1 aurn   ABD   Aberde… Urban Ba…     57.2    -2.09  1999-09-18 2021-09-20 Nort…
#>  2 aurn   ABD9  Aberde… Urban Ba…     57.2    -2.09  2021-10-01 NA         Nort…
#>  3 aurn   ABD7  Aberde… Urban Tr…     57.1    -2.11  2008-01-01 2024-12-31 Nort…
#>  4 aurn   ABD8  Aberde… Urban Tr…     57.1    -2.09  2016-02-09 NA         Nort…
#>  5 aurn   ARM6  Armagh… Urban Tr…     54.4    -6.65  2009-01-01 NA         Nort…
#>  6 aurn   ASHC  Ashing… Urban Ba…     55.2    -1.57  2025-12-22 NA         Nort…
#>  7 aurn   AH    Aston … Rural Ba…     52.5    -3.03  1986-06-26 NA         Nort…
#>  8 aurn   ACTH  Auchen… Rural Ba…     55.8    -3.24  2006-01-01 NA         Cent…
#>  9 aurn   AYLA  Aylesb… Urban Tr…     51.8    -0.794 2025-08-11 NA         Sout…
#> 10 aurn   BAAR  Ballym… Urban Tr…     54.9    -6.27  2017-04-01 NA         Nort…
#> # ℹ 330 more rows
#> # ℹ 5 more variables: agglomeration <chr>, zagglom <chr>,
#> #   local_authority <chr>, lmam_provider <chr>, lmam_code <chr>
```

This output can be customised using different function arguments. For
example, lets find AURN sites which measured O₃ in 2020.

``` r

meta <- import_ukaq_meta("aurn", year = 2020, by_pollutant = TRUE)
meta[meta$pollutant == "o3",]
#> # A tibble: 78 × 16
#>    source code  site           site_type latitude longitude pollutant start_date
#>    <chr>  <chr> <chr>          <chr>        <dbl>     <dbl> <chr>     <date>    
#>  1 aurn   ABD   Aberdeen       Urban Ba…     57.2     -2.09 o3        2003-08-01
#>  2 aurn   AH    Aston Hill     Rural Ba…     52.5     -3.03 o3        1986-06-26
#>  3 aurn   ACTH  Auchencorth M… Rural Ba…     55.8     -3.24 o3        2006-11-01
#>  4 aurn   BAR3  Barnsley Gawb… Urban Ba…     53.6     -1.51 o3        1997-07-07
#>  5 aurn   BEL2  Belfast Centre Urban Ba…     54.6     -5.93 o3        1992-03-08
#>  6 aurn   BIRR  Birmingham A4… Urban Tr…     52.5     -1.87 o3        2016-09-09
#>  7 aurn   AGRN  Birmingham Ac… Urban Ba…     52.4     -1.83 o3        2011-03-18
#>  8 aurn   BMLD  Birmingham La… Urban Ba…     52.5     -1.92 o3        2019-10-09
#>  9 aurn   BLC2  Blackpool Mar… Urban Ba…     53.8     -3.01 o3        2005-06-14
#> 10 aurn   BORN  Bournemouth    Urban Ba…     50.7     -1.83 o3        2003-02-27
#> # ℹ 68 more rows
#> # ℹ 8 more variables: end_date <date>, ratified_to <date>, zone <chr>,
#> #   agglomeration <chr>, zagglom <chr>, local_authority <chr>,
#> #   lmam_provider <chr>, lmam_code <chr>
```

To import data, please make a note of the relevant site codes in the
“code” column (and, if appropriate, the “source” network of the data).

## Continuous Monitoring

Arguably the most useful data made available by
[ukaq](https://github.com/openair-project/ukaq) could be termed
‘continuous monitoring data’ - most commonly hourly data. To access
this, you may use
[`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md)
which requires two key pieces of information - a site `code` from the
metadata table and a `year` (or years) to import.

``` r

import_ukaq_measurements(c("my1", "kc1"), year = 2024L)
#> Warning: Metadata could not be imported for the following source(s), which have
#> been skipped: aqe.
#> # A tibble: 17,568 × 44
#>    source date                code  site        o3    no   no2   nox   so2    co
#>    <chr>  <dttm>              <fct> <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 aurn   2024-01-01 00:00:00 MY1   London …  39.7  22.1  30.4  64.3  1.33 0.244
#>  2 aurn   2024-01-01 01:00:00 MY1   London …  23.7  43.0  48.4 114.   2.39 0.373
#>  3 aurn   2024-01-01 02:00:00 MY1   London …  31.3  28.9  41.1  85.5  1.86 0.268
#>  4 aurn   2024-01-01 03:00:00 MY1   London …  34.1  23.0  37.5  72.7  1.60 0.314
#>  5 aurn   2024-01-01 04:00:00 MY1   London …  37.1  23.2  37.9  73.4  1.33 0.256
#>  6 aurn   2024-01-01 05:00:00 MY1   London …  39.5  14.6  28.5  50.9  1.06 0.244
#>  7 aurn   2024-01-01 06:00:00 MY1   London …  35.5  17.7  34.2  61.2  1.06 0.221
#>  8 aurn   2024-01-01 07:00:00 MY1   London …  37.7  13.0  31.2  50.9  1.06 0.186
#>  9 aurn   2024-01-01 08:00:00 MY1   London …  33.1  20.1  37.7  68.7  1.06 0.186
#> 10 aurn   2024-01-01 09:00:00 MY1   London …  32.7  17.0  37.3  63.5  1.06 0.163
#> # ℹ 17,558 more rows
#> # ℹ 34 more variables: pm10 <dbl>, pm2.5 <dbl>, ethane <dbl>, ethene <dbl>,
#> #   ethyne <dbl>, propane <dbl>, propene <dbl>, ibutane <dbl>, nbutane <dbl>,
#> #   `1butene` <dbl>, t2butene <dbl>, c2butene <dbl>, ipentane <dbl>,
#> #   npentane <dbl>, `13bdiene` <dbl>, t2penten <dbl>, `1penten` <dbl>,
#> #   `2mepent` <dbl>, isoprene <dbl>, nhexane <dbl>, nheptane <dbl>,
#> #   ioctane <dbl>, noctane <dbl>, benzene <dbl>, toluene <dbl>, …
```

[`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md)
is clever enough to work out the specific monitoring network each site
is a member of, but sometimes there can be ambiguity. The `source`
argument allows this to be specified. Consider `source` as defining the
pool of one or more networks
[ukaq](https://github.com/openair-project/ukaq) will use to align each
`code` with an actual monitoring station. In reality, this should only
be an issue for “locally managed” English sites which share site codes
with WSP-managed sites (e.g., “AD1” which is the ‘Aberdeen King Street’
AURN site and the ‘Adur - Shoreham-by-Sea’ Sussex AQ site). Consider the
difference between the two outputs below.

``` r

import_ukaq_measurements("ad1", year = 2020L)
#> Warning: Metadata could not be imported for the following source(s), which have
#> been skipped: aqe.
#> Warning: Ambiguous codes detected: AD1.
#> Importing sites using the following order of preference: aurn, saqn, waqn, niaqn, lmam.
#> # A tibble: 8,784 × 13
#>    source date                code  site        no   no2   nox  pm10 pm2.5   pm1
#>    <chr>  <dttm>              <fct> <chr>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 saqn   2020-01-01 00:00:00 AD1   Aberdee… 4.22  17.8  24.3  15.6  13.1  12.4 
#>  2 saqn   2020-01-01 01:00:00 AD1   Aberdee… 6.04  19.5  28.8   7.24  5.92  5.33
#>  3 saqn   2020-01-01 02:00:00 AD1   Aberdee… 5.07  21.1  28.9   9.75  8.35  7.93
#>  4 saqn   2020-01-01 03:00:00 AD1   Aberdee… 1.53   9.97 12.3   5.00  3.83  3.37
#>  5 saqn   2020-01-01 04:00:00 AD1   Aberdee… 1.02   9.83 11.4   5.00  3.81  3.32
#>  6 saqn   2020-01-01 05:00:00 AD1   Aberdee… 0.716  5.16  6.25  3.86  2.94  2.59
#>  7 saqn   2020-01-01 06:00:00 AD1   Aberdee… 0.575  5.79  6.67  4.24  3.31  2.97
#>  8 saqn   2020-01-01 07:00:00 AD1   Aberdee… 1.03   7.99  9.57  3.88  3.19  2.96
#>  9 saqn   2020-01-01 08:00:00 AD1   Aberdee… 8.26  23.6  36.2   4.49  3.71  3.48
#> 10 saqn   2020-01-01 09:00:00 AD1   Aberdee… 6.07  26.6  35.9   6.32  5.04  4.80
#> # ℹ 8,774 more rows
#> # ℹ 3 more variables: wd <dbl>, ws <dbl>, temp <dbl>

import_ukaq_measurements("ad1", year = 2020L, source = "lmam")
#> # A tibble: 8,784 × 8
#>    source date                code  site                    no   no2   nox  pm10
#>    <chr>  <dttm>              <fct> <chr>                <dbl> <dbl> <dbl> <dbl>
#>  1 lmam   2020-01-01 00:00:00 AD1   Adur - Shoreham-by-… NA     NA    NA      39
#>  2 lmam   2020-01-01 01:00:00 AD1   Adur - Shoreham-by-… 23.7   52.2  88.0    38
#>  3 lmam   2020-01-01 02:00:00 AD1   Adur - Shoreham-by-… 12.5   45.9  65.2    36
#>  4 lmam   2020-01-01 03:00:00 AD1   Adur - Shoreham-by-… 12.5   47.2  65.6    34
#>  5 lmam   2020-01-01 04:00:00 AD1   Adur - Shoreham-by-… 26.2   52.4  92.2    38
#>  6 lmam   2020-01-01 05:00:00 AD1   Adur - Shoreham-by-…  6.24  30.8  39.8    37
#>  7 lmam   2020-01-01 06:00:00 AD1   Adur - Shoreham-by-…  7.48  27.9  39.6    37
#>  8 lmam   2020-01-01 07:00:00 AD1   Adur - Shoreham-by-… 15.0   30.6  52.8    34
#>  9 lmam   2020-01-01 08:00:00 AD1   Adur - Shoreham-by-… 15.0   30.6  52.8    38
#> 10 lmam   2020-01-01 09:00:00 AD1   Adur - Shoreham-by-… 36.2   37.1  91.8    33
#> # ℹ 8,774 more rows
```

Data can be augmented with three kinds of extra information to make the
functions more useful:

1.  `append_meteorology` will add modelled wind speed (`ws`), wind
    direction (`wd`), and air temperature (`temp`) for networks where it
    exists. This defaults to `TRUE`.

2.  `append_quality_flag` will add a column (or columns) to indicate
    whether each pollutant has been ratified. This defaults to `FALSE`.

3.  `append_metadata` will add the metadata columns defined in
    `metadata_columns`. This is useful to append information like
    latitude/longitude for mapping, for example. This defaults to
    `FALSE`, with chosen metadata being site type, latitude and
    longitude.

To demonstrate we’ll just grab a couple of pollutants from Marylebone
Road (`"my1"`) using the `pollutant` argument, to keep the output small.

``` r

import_ukaq_measurements(
  "my1", 
  2020L,  
  pollutant = c("no2", "o3"),
  append_metadata = TRUE, 
  append_meteorology = FALSE, 
  append_quality_flag = TRUE, 
  metadata_columns = "zagglom"
)
#> Warning: Metadata could not be imported for the following source(s), which have
#> been skipped: aqe.
#> # A tibble: 8,784 × 9
#>    source date                code  site          no2    o3 o3_qc no2_qc zagglom
#>    <chr>  <dttm>              <fct> <chr>       <dbl> <dbl> <lgl> <lgl>  <chr>  
#>  1 aurn   2020-01-01 00:00:00 MY1   London Mar…  45.8  1.73 TRUE  TRUE   Greate…
#>  2 aurn   2020-01-01 01:00:00 MY1   London Mar…  52.6  1.93 TRUE  TRUE   Greate…
#>  3 aurn   2020-01-01 02:00:00 MY1   London Mar…  44.8  2.00 TRUE  TRUE   Greate…
#>  4 aurn   2020-01-01 03:00:00 MY1   London Mar…  40.2  2.05 TRUE  TRUE   Greate…
#>  5 aurn   2020-01-01 04:00:00 MY1   London Mar…  47.3  2.99 TRUE  TRUE   Greate…
#>  6 aurn   2020-01-01 05:00:00 MY1   London Mar…  40.4  2.89 TRUE  TRUE   Greate…
#>  7 aurn   2020-01-01 06:00:00 MY1   London Mar…  42.7  2.99 TRUE  TRUE   Greate…
#>  8 aurn   2020-01-01 07:00:00 MY1   London Mar…  40.7  1.90 TRUE  TRUE   Greate…
#>  9 aurn   2020-01-01 08:00:00 MY1   London Mar…  42.6  1.85 TRUE  TRUE   Greate…
#> 10 aurn   2020-01-01 09:00:00 MY1   London Mar…  38.3  1.95 TRUE  TRUE   Greate…
#> # ℹ 8,774 more rows
```

By default, the data is put into a ‘wide’ format, with different
pollutants in different columns. For many applications in R, we may want
the data to be in a “long” format. For this data structure, simply set
`pivot = "long"`. Note that this will interact with the other ‘append’
arguments - meteorological data and site metadata aren’t pivoted with
the pollutants, and there will only be a single quality flag alongside
the ‘value’ column.

``` r

import_ukaq_measurements(
  "my1",
  2020L,
  pivot = "long"
)
#> Warning: Metadata could not be imported for the following source(s), which have
#> been skipped: aqe.
#> # A tibble: 377,712 × 9
#>    source date                code  site          wd    ws  temp pollutant value
#>    <chr>  <dttm>              <fct> <chr>      <dbl> <dbl> <dbl> <chr>     <dbl>
#>  1 aurn   2020-01-01 00:00:00 MY1   London Ma…  92.7   2.1   2.3 o3         1.73
#>  2 aurn   2020-01-01 01:00:00 MY1   London Ma…  98.3   2.1   1.4 o3         1.93
#>  3 aurn   2020-01-01 02:00:00 MY1   London Ma… 117.    2.3   1   o3         2.00
#>  4 aurn   2020-01-01 03:00:00 MY1   London Ma… 131.    1.8   0.8 o3         2.05
#>  5 aurn   2020-01-01 04:00:00 MY1   London Ma… 109.    1.7   0.8 o3         2.99
#>  6 aurn   2020-01-01 05:00:00 MY1   London Ma…  84.3   1.1   0   o3         2.89
#>  7 aurn   2020-01-01 06:00:00 MY1   London Ma…  86.9   1.2  -0.4 o3         2.99
#>  8 aurn   2020-01-01 07:00:00 MY1   London Ma… 143     1.3   0.9 o3         1.90
#>  9 aurn   2020-01-01 08:00:00 MY1   London Ma… 168.    1.1   1.5 o3         1.85
#> 10 aurn   2020-01-01 09:00:00 MY1   London Ma… 186.    0.6   1.5 o3         1.95
#> # ℹ 377,702 more rows
```

Finally, there are other data types available if they are of interest,
which can be used with the `data_type` argument of
[`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md).
These include:

- `"hourly"`: Hourly data (the default).

- `"daily"`: Daily average data.

- `"15_min"`: 15-minute average SO2 concentrations.

- `"8_hour"`: 8-hour rolling mean concentrations for O3 and CO.

- `"24_hour"`: 24-hour rolling mean concentrations for particulates.

- `"daily_max_8"`: Maximum daily rolling 8-hour maximum for O3 and CO.

On top of these data types, there are three additional `data_type`s that
can be accessed through other functions, detailed in the sections below.

## Monthly & Annual Statistics

When examining entire networks, it may be useful to examine aggregated
data.
[`import_ukaq_summaries()`](https://openair-project.github.io/ukaq/reference/import_ukaq_summaries.md)
allows for `"monthly"` and `"annual"` data types to be imported. This
function works differently to
[`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md)
in a few key ways.

1.  A pre-calculated monthly or annual data capture is also returned
    with the data.

2.  `code` is optional (defaulting to `NULL`) which will make the
    function return all data available for the given `source` and
    `year`.

Many of the arguments mentioned in the above section are also available
for this function, including `pollutant`, `append_metadata`,
`metadata_columns`, and `pivot`.

``` r

import_ukaq_summaries(year = 2024, source = "aurn")
#> # A tibble: 187 × 89
#>    source date        year code  site            o3 o3.capture o3.summer.capture
#>    <chr>  <date>     <int> <chr> <chr>        <dbl>      <dbl>             <dbl>
#>  1 aurn   2024-01-01  2024 ABD7  Aberdeen Un…  NA       NA               NA     
#>  2 aurn   2024-01-01  2024 ABD8  Aberdeen We…  NA       NA               NA     
#>  3 aurn   2024-01-01  2024 ABD9  Aberdeen Er…  56.0      0.878            0.959 
#>  4 aurn   2024-01-01  2024 ACTH  Auchencorth…  59.2      0.998            0.999 
#>  5 aurn   2024-01-01  2024 AH    Aston Hill    63.4      0.991            0.991 
#>  6 aurn   2024-01-01  2024 ARM6  Armagh Road…  NA       NA               NA     
#>  7 aurn   2024-01-01  2024 BAAR  Ballymena A…  NA       NA               NA     
#>  8 aurn   2024-01-01  2024 BALM  Ballymena B…  NA       NA               NA     
#>  9 aurn   2024-01-01  2024 BAR3  Barnsley Ga…  56.9      0.292            0.0947
#> 10 aurn   2024-01-01  2024 BBRD  Birkenhead …  NA       NA               NA     
#> # ℹ 177 more rows
#> # ℹ 81 more variables: o3.daily.max.8hour <dbl>, o3.aot40v <int>,
#> #   o3.aot40f <int>, somo35 <dbl>, somo35.capture <dbl>, no <dbl>,
#> #   no.capture <dbl>, no2 <dbl>, no2.capture <dbl>, nox <dbl>,
#> #   nox.capture <dbl>, so2 <dbl>, so2.capture <dbl>, co <dbl>,
#> #   co.capture <dbl>, pm10 <dbl>, pm10.capture <dbl>, pm2.5 <dbl>,
#> #   pm2.5.capture <dbl>, gr10 <dbl>, gr10.capture <dbl>, gr2.5 <dbl>, …
```

``` r

import_ukaq_summaries(year = 2024, source = "aurn", pivot = "long")
#> # A tibble: 8,228 × 8
#>    source date        year code  site                    pollutant  mean capture
#>    <chr>  <date>     <int> <chr> <chr>                   <chr>     <dbl>   <dbl>
#>  1 aurn   2024-01-01  2024 ABD7  Aberdeen Union Street … o3         NA    NA    
#>  2 aurn   2024-01-01  2024 ABD8  Aberdeen Wellington Ro… o3         NA    NA    
#>  3 aurn   2024-01-01  2024 ABD9  Aberdeen Erroll Park    o3         56.0   0.878
#>  4 aurn   2024-01-01  2024 ACTH  Auchencorth Moss        o3         59.2   0.998
#>  5 aurn   2024-01-01  2024 AH    Aston Hill              o3         63.4   0.991
#>  6 aurn   2024-01-01  2024 ARM6  Armagh Roadside         o3         NA    NA    
#>  7 aurn   2024-01-01  2024 BAAR  Ballymena Antrim Road   o3         NA    NA    
#>  8 aurn   2024-01-01  2024 BALM  Ballymena Ballykeel     o3         NA    NA    
#>  9 aurn   2024-01-01  2024 BAR3  Barnsley Gawber         o3         56.9   0.292
#> 10 aurn   2024-01-01  2024 BBRD  Birkenhead Borough Road o3         NA    NA    
#> # ℹ 8,218 more rows
```

``` r

import_ukaq_summaries("my1", 2020, data_type = "monthly", pivot = "long")
#> # A tibble: 516 × 10
#>    source date        year month month_label code  site  pollutant  mean capture
#>    <chr>  <date>     <int> <int> <fct>       <chr> <chr> <chr>     <dbl>   <dbl>
#>  1 aurn   2020-01-01  2020     1 Jan         MY1   Lond… o3         15.5   0.960
#>  2 aurn   2020-02-01  2020     2 Feb         MY1   Lond… o3         21.8   0.978
#>  3 aurn   2020-03-01  2020     3 Mar         MY1   Lond… o3         35.4   0.997
#>  4 aurn   2020-04-01  2020     4 Apr         MY1   Lond… o3         54.7   0.976
#>  5 aurn   2020-05-01  2020     5 May         MY1   Lond… o3         57.1   0.976
#>  6 aurn   2020-06-01  2020     6 Jun         MY1   Lond… o3         41.0   0.993
#>  7 aurn   2020-07-01  2020     7 Jul         MY1   Lond… o3         27.0   0.978
#>  8 aurn   2020-08-01  2020     8 Aug         MY1   Lond… o3         41.8   0.660
#>  9 aurn   2020-09-01  2020     9 Sep         MY1   Lond… o3         31.3   0.781
#> 10 aurn   2020-10-01  2020    10 Oct         MY1   Lond… o3         18.5   0.933
#> # ℹ 506 more rows
```

## Daily Air Quality Index (DAQI)

Pre-calculated Daily Air Quality Indices (the ‘DAQI’, see
<https://www.gov.uk/government/publications/health-effects-of-air-pollution>)
are also available through
[`import_ukaq_daqi()`](https://openair-project.github.io/ukaq/reference/import_ukaq_daqi.md).
This function is similar to
[`import_ukaq_summaries()`](https://openair-project.github.io/ukaq/reference/import_ukaq_summaries.md),
with some additional nuances:

1.  The default `pivot` is `"long"`. This is due to the amount of data
    presented - the daily statistic, the corresponding index, the
    corresponding band, and the measurement period. Not all of this
    information is carried to the ‘wide’ format if the alternative is
    selected.

2.  `pollutant` ensures only one of the five DAQI pollutants are given -
    any combination of NO2, O3, PM10, PM2.5 or SO2.

``` r

import_ukaq_daqi("my1", 2020)
#> # A tibble: 1,624 × 8
#>    source date       code  site     pollutant concentration poll_index poll_band
#>    <chr>  <date>     <chr> <chr>    <chr>             <dbl>      <int> <fct>    
#>  1 aurn   2020-01-01 MY1   London … no2               72.4           2 Low      
#>  2 aurn   2020-01-01 MY1   London … o3                 6             1 Low      
#>  3 aurn   2020-01-01 MY1   London … pm10              39             3 Low      
#>  4 aurn   2020-01-01 MY1   London … pm2.5             36             4 Moderate 
#>  5 aurn   2020-01-01 MY1   London … so2                9.15          1 Low      
#>  6 aurn   2020-01-02 MY1   London … no2               66.2           1 Low      
#>  7 aurn   2020-01-02 MY1   London … o3                25             1 Low      
#>  8 aurn   2020-01-02 MY1   London … pm10              15             1 Low      
#>  9 aurn   2020-01-02 MY1   London … pm2.5              9             1 Low      
#> 10 aurn   2020-01-02 MY1   London … so2                6.46          1 Low      
#> # ℹ 1,614 more rows
```
