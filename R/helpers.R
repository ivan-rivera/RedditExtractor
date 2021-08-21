# Sleep before returning identity. Use this when calling the Reddit API
throttle <- function(x) {
  Sys.sleep(1)
  x
}


# Either apply a data builder function or return NA if no data exists to build
maybe_build_df <- function(json, index, builder) {
  if(sum(index) > 0) builder(json[index]) else NA
}


# Row-bind (stack) a list of data frames
rbind_list <- function(data_list) do.call("rbind", data_list)


# Deduplicate a data frame
dedup_df <- function(df) df[!duplicated(df),]


# Remove NA instances from a collection
remove_na <- function(x) x[!is.na(x)]


# Infix string concatenator
`%+%` <- function(a, b) paste0(a, b)


# Infix ternary operator
`%||%` <- function(a, b) ifelse(is.na(a), b, a)
