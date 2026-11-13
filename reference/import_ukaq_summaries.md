# Import Monthly and Annual Summaries of UK Air Quality Data

Imports either annual or monthly summaries matching UK AIR. Unlike the
data available in
[`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md),
this data is appended with a monthly/annual data capture and additional
relevant columns (e.g., the month as a character for monthly data).

## Usage

``` r
import_ukaq_summaries(
  code = NULL,
  year,
  source = "aurn",
  data_type = "annual",
  pollutant = NULL,
  append_metadata = FALSE,
  metadata_columns = c("site_type", "latitude", "longitude"),
  pivot = "wide",
  progress = rlang::is_interactive(),
  ...,
  .class = NULL
)
```

## Arguments

- code:

  *Specific site codes to import.*

  *default:* `NULL`

  `code` expects a vector of character values. It defines the specific
  sites to import will filter the result for specific sites based on
  their site codes, available through
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md).
  Each code can be paired with a specific network using `source`. If
  `NULL`, the default, all available data for the `year` and `source`
  will be returned.

- year:

  *A year, or range of years, from which to import data.*

  **required**

  `year` expects a vector of integer values. It can be a single year
  (`2020`), a selection of specific years (c(`2020, 2021, 2022`)), or a
  range of years (`2020:2025`).

- source:

  *The UK monitoring network(s) from which to import data.*

  *default:* `"aurn"`

  Each value should be one of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`,
  `"niaqn"`, `"lmam"`, or `"ukaq"`.

  If `code = NULL`, `source` can be any combination of networks, and all
  sites in those networks are returned. `"ukaq"` returns all networks.

  If `code` is provided, `source` can be either:

  - **A single value**, which applies to every `code`. `"ukaq"` will
    look for each site in all of the available networks; any other value
    will *only* import data from that network.

  - **A vector the same length as `code`**, pairing each site code with
    a network. For example, `code = c("MY1", "KC1", "ABD")` and
    `source = c("aurn", "aurn", "saqn")`. `"ukaq"` can be used for any
    code whose network should be worked out automatically.

  When a site is found in multiple networks under `"ukaq"`, the first of
  `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, `"niaqn"` and `"lmam"` is used
  and a warning is given.

- data_type:

  *What type of summary should be returned?*

  *default:* `"annual"`

  `import_ukaq_summaries()` can return either `"annual"` (the default)
  or `"monthly"` summary data.

- pollutant:

  *One or more pollutants for which to import data.*

  *default:* `NULL`

  By default, all available pollutants are imported. `pollutant` allows
  any specific combination of the pollutants to be returned instead.

- append_metadata:

  *Append site metadata to the dataframe?*

  *default:* `FALSE`

  When `TRUE`, the resulting `data.frame` will have site metadata
  appended. The specific columns are selected using `metadata_columns`.

- metadata_columns:

  *Specific metadata columns to append to the data.frame.*

  *default:* `c("site_type", "latitude", "longitude")`

  When `append_metadata = TRUE`, the columns selected here will be
  appended to the data. Columns names should match those in
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md),
  excluding pollutant-specific columns (e.g., ratification date).

- pivot:

  *Should the dataframe be 'wide' or 'long'?*

  *default:* `"wide"`

  There are two main ways to store air quality data; "long" (with
  'pollutant' and 'value' columns) or "wide" (with each pollutant value
  being stored in its own column). `pivot` allows users to define which
  format they would prefer their data in.

- progress:

  *Show a progress bar?*

  *default:*
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html)

  When `TRUE`, this function will print a progress bar to track
  individual files being imported. If `FALSE` this is suppressed. By
  default, a progress bar is shown in interactive sessions only.

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

## References

With thanks to Trevor Davies and [WSP UK&I](https://www.wsp.com/en-gb)
for preparing and hosting the data

## Author

Jack Davison, David Carslaw
