
# ukaq (development version)

## Breaking changes

- `import_ukaq_daqi(pivot = "wide")` now retains the `source` column. Previously this was dropped, which collapsed sites reported by more than one network and prevented `append_metadata = TRUE` from working.

- The `year` argument of `import_ukaq_meta()` now returns sites open at *any point* within the given range, as documented. Previously only sites open for the entire range were returned.

- `import_ukaq_measurements(pivot = "long", append_quality_flag = TRUE)` no longer drops pollutants without a ratification date (e.g., all `"lmam"` data); these are now returned with `qc = NA`.

## Bug fixes

- `import_ukaq_meta()` and `import_ukaq_measurements()` no longer fail when a network's metadata is unavailable (e.g., if Air Quality England is offline). Unavailable networks are skipped with a warning.

- `import_ukaq_measurements()` now correctly returns `"15_min"`, `"8_hour"`, `"24_hour"` and `"daily_max_8"` data, which previously always errored with "No data has been returned."

- `import_ukaq_measurements(data_type = "daily")` now includes additional daily statistics (e.g., gravimetric PM, `gr10` and `gr2.5`) where available.

- `import_ukaq_meta(by_pollutant = FALSE)` no longer drops sites which share a name with a site in another network (e.g., ABD7 and ABD3, both "Aberdeen Union Street Roadside"). This also affected `import_ukaq_measurements()` and `append_metadata = TRUE`.

- `import_ukaq_daqi(append_metadata = TRUE)` no longer errors.

- `import_ukaq_meta()` no longer errors when `"lmam"` is listed before other networks in `source`.

- Missing DAQI indices now result in missing bands and colours, rather than a "Very High" band. Requesting `"colour_band"` without `"poll_band"` now returns band colours.

- Ratification flags (`append_quality_flag = TRUE`) now treat data as ratified up to the *end* of the ratification date, and are no longer incorrect when a site lists a pollutant more than once.

- `import_ukaq_summaries(pivot = "wide")` now matches `pollutant` exactly (e.g., `"co"` no longer also selects the `code` column).

- Unknown site codes or pollutants passed to `import_ukaq_measurements()`, and summary requests which return no data, now give informative errors (or warnings, if only some values are unmatched).

- Ambiguous site codes are now reported in a single, readable warning.

- Removed R >= 4.1 syntax so that the package works with R >= 3.5, as declared in `DESCRIPTION`.

## Documentation

- The documented default `source` for `import_ukaq_daqi()` and `import_ukaq_summaries()` is now correctly given as `"aurn"`.

# ukaq 0.1.0

This is a lightweight package for importing UK air quality data. 

It has a single import (`{rlang}`) and two suggested packages (`{tibble}` and `{cli}`).
