metadata_column_names <-
  c(
    "site_type",
    "latitude",
    "longitude",
    "zone",
    "agglomeration",
    "zagglom",
    "local_authority",
    "lmam_provider",
    "lmam_code"
  )

daqi_pollutant_names <-
  c("no2", "o3", "pm10", "pm2.5", "so2")

ukaq_network_names <-
  c("aurn", "aqe", "saqn", "waqn", "niaqn", "lmam")

ukaq_network_names_nolocal <-
  ukaq_network_names[ukaq_network_names != "lmam"]
