# Generates the pending spatial-coding inventory from retained corpus text.
# Run from the repository root with `Rscript scripts/build_spatial_review.R`.
# Inputs are data/corpus.csv and config/spatial_terms.csv; the sole output is
# data/spatial_review.csv. Existing manual spatial decisions are preserved.

source("R/spatial_coding.R", encoding = "UTF-8")

spatial_review <- write_spatial_review()
spatial_review_queue <- write_spatial_queue(spatial_review)

message(
  "Spatial review inventory: ", nrow(spatial_review), " occurrences across ",
  dplyr::n_distinct(spatial_review$document_id), " logical documents; ",
  nrow(spatial_review_queue), " document-code decisions in the review queue."
)
