## code to prepare `testing` dataset goes here

rawMeta <- importMeta(c("aurn", "saqn", "lmam"))
rawMeta <- dplyr::slice_head(rawMeta, n = 5, by = source)

usethis::use_data(rawMeta, overwrite = TRUE, internal = TRUE)
