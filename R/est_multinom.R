#' @title Estimate multinomial cell probabilities in a 2x2 table
#'
#' @description Simulatenously estimates of multinomial cell probabilities.
#'
#' @param x A numeric vector of success counts OR a 2x2 table.
#' @param n A numeric vector of totals.
#' @param prior A matrix of hyper-parameters.
#'
#' @details The model assumes no margins are fixed. Two random binary outcomes
#'   are measured on N individuals, resulting in four possible combined
#'   outcomes with probabilities \eqn{p_{11}, p_{12}, p_{21}, p_{22}}.
#'
#' @examples
#' fake_data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(fake_data) <- c('Safe' ,'Dangerous')
#' rownames(fake_data) <- c('Animals', 'Plants')
#' est_multinom(fake_data)
#'
#' est_multinom(x = c(200, 250), n = c(350, 550))
#'
#' hyperparams <- matrix(c(0.25, 0.2, 0.15, 0.4), nrow = 2, byrow = TRUE)
#' est_multinom(fake_data, prior = hyperparams)
#'
#' @references Fienberg, S.E. and Holland, P.W. (1973) Simultaneous estimation
#'   of multinomial cell probabilities. \emph{Journal of the American Statistical
#'   Association}, \strong{68}, 683-691. doi:10.1080/01621459.1973.10481405
#'
#' @export
est_multinom <- function(x, n = NULL, prior = NULL, groups = c("test", "control")) {
  data <- check_inputs(x, n)
  p <- prop.table(data)
  if (is.null(prior)) {
    prior <- matrix(0, nrow = nrow(data), ncol = ncol(data))
    p_iplus <- prop.table(margin.table(data, 2))
    p_plusj <- prop.table(margin.table(data, 1))
    for ( i in 1:nrow(prior) ) {
      for ( j in 1:ncol(prior) ) {
        prior[i, j] <- p_iplus[i] * p_plusj[j]
      }
    }
  } else {
    if (any(dim(prior) != c(2, 2)) | !is.matrix(x)) {
      stop("an explicit prior must be a 2x2 matrix")
    }
  }
  if (length(groups) != 2) {
    stop("only 2 group labels are allowed")
  } else {
    if (is.vector(x)) {
      if (!is.null(names(x))) {
        groups <- names(x)
      }
    } else {
      if (!is.null(rownames(x))) {
        groups <- rownames(x)
      }
    }
  }
  K <- (1 - sum(as.vector(p)^2)) / sum((as.vector(prior) - as.vector(p))^2)
  n <- sum(data)
  output <- p*(n/(n+K)) + prior*(K/(n+K))
  rownames(output) <- groups
  if (!is.vector(x)) {
    if (!is.null(colnames(x))) {
      colnames(output) <- colnames(x)
    } else {
      colnames(output) <- c("Outcome 1", "Outcome 2")
    }
  } else {
    colnames(output) <- c("Outcome 1", "Outcome 2")
  }
  return(output)
}
