check_inputs <- function(x, n) {
  if (is.null(n)) {
    if (is.vector(x)) {
      stop("without n, x must be a 2x2 matrix or table")
    } else {
      if (!is.table(x) & !is.array(x) & !is.matrix(x) & !is.data.frame(x)) {
        stop("x must be a 2x2 matrix, array, table, or data.frame")
      }
      if (!all(dim(x) == c(2, 2))) {
        stop("x must be 2 rows by 2 columns")
      } else {
        data <- x
      }
    }
  } else {
    if (!is.vector(x) | !is.vector(n)) {
      stop("x and n must both be numeric vectors")
    } else {
      if (length(x) != 2 | length(n) != 2) {
        stop("only 2-group categorical analysis is supported")
      } else {
        data <- matrix(c(x, n-x), nrow = 2, byrow = FALSE)
      }
    }
  }
  return(data)
}

format_confint <- function(est, ci = NULL, digits = 2, units = "") {
  if (units == "%") {
    units <- paste0(units, units)
  }
  type <- switch(typeof(est), "character" = "s", "double" = "f", "integer" = "i")
  x <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), est)
  if (!is.null(ci)) {
    y <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), ci[1])
    z <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), ci[2])
    return(paste0(x, " (", y, ", ", z, ")"))
  }
  return(x)
}

if_else <- function(test, yes, no) {
  if (test) {
    return(yes)
  }
  return(no)
}
