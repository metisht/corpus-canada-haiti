packages <- c("yaml", "httr2", "rvest", "xml2", "jsonlite", "pdftools", "digest", "ggplot2", "knitr")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message(if (length(missing)) "Package installation complete." else "All required CRAN packages are already installed.")
