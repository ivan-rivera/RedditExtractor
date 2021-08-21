# Validate that a given parameter belongs to a list
validate_one_of <- function(param, one_of) {
  param_name <- deparse(substitute(param))
  error_message <- param_name %+% " must be one of: " %+% paste(one_of, collapse = ", ")
  if(!param %in% one_of) stop(error_message)
}
