# Build comment IDs representing the nested structure of a thread
build_comment_ids <- function(json) seq_along(json) |>
  lapply(function(x) generate_flat_comment_ids(json[[x]], x)) |>
  unlist()


# Generate flat comment IDs
generate_flat_comment_ids <- function(json, depth="0") {
  if(is.null(json)) return(list())
  replies <- get_replies_or_null(json)
  list(
    ifelse(is.null(json$data$author), NA, depth),
    lapply(seq_along(replies), function(x) generate_flat_comment_ids(replies[[x]], paste0(depth, "_", x)))
  ) |> remove_na()
}
