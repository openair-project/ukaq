# Import UK Air Quality Metadata

`import_ukaq_meta()` imports metadata for UK air quality monitoring
sites. By default this function returns one row per monitoring site,
including the site opening and closing dates and other useful meta-data.
The `by_pollutant` option will return one row per monitoring site *and
pollutant*; see `import_ukaq_pollutants()` for definitions. The `"site"`
column can be used in other functions to import data.

## Usage

``` r
import_ukaq_meta(
  source = "ukaq",
  year = NA,
  by_pollutant = FALSE,
  ...,
  .class = NULL
)

import_ukaq_pollutants(source = "ukaq", ..., .class = NULL)
```

## Arguments

- source:

  *One or more UK Monitoring networks from which to import metadata*

  *default:* `"ukaq"`

  The default, `"ukaq"`, will import metadata from the AURN, the four
  'devolved' networks, and locally managed English monitoring networks.
  Any combination of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, `"niaqn"`
  and , `"lmam"` will *only* import data from those specific monitoring
  networks.

- year:

  *A year, or range of years, with which to filter data.*

  *default:* `NA`

  By default all monitoring stations are returned regardless of their
  open/closed status. `year` allows users to filter sites open in a
  specific year, or over a range of years. For example,
  `year = c(2010, 2020)` (or `year = 2010:2020`) will return sites that
  were open at any point between 2010 and 2020.

- by_pollutant:

  *Return site-pollutant combinations?*

  *default:* `FALSE`

  If `TRUE`, the rows of the resulting `data.frame` will each represent
  a monitoring site-pollutant pair, with the start, end, and
  ratification date reflecting each specific pollutant. When `FALSE`,
  the default, each row is a unique monitoring site, and the start and
  end dates represent the opening and closing dates of the site as a
  whole.

- ...:

  *Not used.*

- .class:

  *Signifier for the dataframe class.*

  *default:* `NULL`

  [ukaq](https://openair-project.github.io/ukaq/reference/ukaq-package.md)
  functions, by default, will return `tbl_df`s if the
  [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  package is installed, but will otherwise return `data.frame`s.
  `.class` can override this behaviour, and takes either `"tbl"` or
  `"df"`, which sets the return class to be `tbl_df` or `data.frame`
  respectively.

## Value

a `data.frame`

## Details

The following metadata columns are returned:

- **source** : The network with which the site is associated. Note that
  some monitoring sites are part of multiple networks (e.g., the AURN &
  SAQN) so the same site may feature twice under different sources.

- **code** : The site code, used to import data from specific sites of
  interest.

- **site** : The site name, which is more human-readable than the site
  code.

- **site_type** : A description of the site environment.

- **latitude** and **longitude** : The coordinates of the monitoring
  station, using the World Geodetic System (<https://epsg.io/4326>).

- **start_date** and **end_date** : The opening and closing dates of the
  monitoring station. If `by_pollutant = TRUE`, these dates are instead
  the first and last dates at which specific pollutants were measured. A
  missing value, `NA`, indicates that monitoring is ongoing.

- **ratified_to** : The date to which data has been ratified (i.e.,
  'quality checked'). Data after this date is subject to change.

- **zone**, **agglomeration** and **zagglom** : The UK is divided into
  agglomeration zones (large urban areas) and non-agglomeration zones
  for air quality assessment. The *zagglom* column coalesces the two,
  listing the agglomeration if the site is located within one and the
  zone otherwise.

- **local_authority** : The local authority in which the monitoring
  station is found.

- **lmam_provider** and **lmam_code** : The specific provider of the
  locally managed dataset (e.g., `"londonair"`).

## References

With thanks to Trevor Davies and [WSP UK&I](https://www.wsp.com/en-gb)
for preparing and hosting the data

## Author

Jack Davison, David Carslaw
