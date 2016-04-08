#' @title Interpret Bayes Factor
#' @param bf A Bayes factor computed using \code{LearnBayes::ctable()}, for
#'   example.
#' @param interpreter A character vector indicating whether to interpret
#'   \code{bf} through the lens of Kass and Raftery or Harold Jeffreys.
#' @examples \dontrun{
#' data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(data) <- c('Safe' ,'Dangerous')
#' rownames(data) <- c('Animals', 'Plants')
#'
#' bf <- LearnBayes::ctable(data, matrix(rep(1, 4), 2))
#' interpret_bf(bf, interpreter = 'Kass and Raftery')
#' }

interpret_bf <- function(bf, interpreter = c('Kass and Raftery', 'Harold Jeffreys')) {
  if (interpreter[1] == 'Kass and Raftery') {
    bf_transformed <- 2 * log(bf)
    if ( bf_transformed <= 2 ) {
      return("Not worth more than a bare mention.")
    } else if ( bf_transformed > 2 && bf_transformed <= 6 ) {
      return("Positive evidence against H0.")
    } else if ( bf_transformed > 6 && bf_transformed <= 10 ) {
      return("Strong evidence against H0.")
    } else { # bf_transformed > 10
      return("Very strong evidence against H0.")
    }
  } else { # interpreter == 'Harold Jeffreys'
    bf_transformed <- log10(bf)
    if ( bf_transformed <= 1/2 ) {
      return("Not worth more than a bare mention.")
    } else if ( bf_transformed > 1/2 && bf_transformed <= 1 ) {
      return("Substantial evidence against H0.")
    } else if ( bf_transformed > 1 && bf_transformed <= 2 ) {
      return("Strong evidence against H0.")
    } else { # bf_transformed > 2
      return("Decisive evidence against H0.")
    }
  }
}

#' @title Compute the Bayes factor (for testing independence)
bayes_factor <- function(x) {
  # ...
}

#' @title Test of independence for contigency tables using Bayes factor
#' @param x A numeric vector of success counts OR a 2x2 table or matrix.
#' @param n A numeric vector of totals. (Optional if x is a table or matrix.)
#' @param interpreter A character vector indicating whether to interpret
#'   \code{bf} through the lens of Kass and Raftery or Harold Jeffreys.
#' @examples \dontrun{
#' data <- matrix(c(200, 150, 250, 300), nrow = 2, byrow = TRUE)
#' colnames(data) <- c('Safe' ,'Dangerous')
#' rownames(data) <- c('Animals', 'Plants')
#'
#' test_independence(data)
#' test_independence(data, "Harold Jeffreys")
#' }
#'
#' @references Albert, J. (2009) Bayesian Computation with R.
#' Springer-Verlag New York, New York. doi:10.1007/978-0-387-92298-0
#'
#' Kass, R.E. and Raftery, A.E. (1995) Bayes Factors. \emph{Journal of
#'   the American Statistical Association}, \strong{90}, 773-795.
#'
#' @seealso Irony, T.Z. and Pereira, C.A.B. (1986) Exact tests for equality
#'   of two proportions: fisher v. bayes. \emph{Journal of Statistical
#'   Computation and Simulation}, \strong{25}, 93-114.
#'   doi:10.1080/00949658608810926
#'
#' Altham, P.M.E. (1963) Exact Bayesian analysis of a 2x2 contingency table,
#'   and Fisher's "exact" significance test. \emph{Journal of the Royal
#'   Statistical Society}, Series B, Methodological, \strong{31}, 261-269.
#'
#' Altham, P.M.E. (1971) The analysis of matched proportions. \emph{Biometrika},
#' \strong{58}, 561-576.
#'
#' @rdname test_indepen
#' @export

test_independence <- function(x, n = NULL,
                              interpreter = c('Kass and Raftery', 'Harold Jeffreys')) {
  data <- check_inputs(x, n); bf <- bayes_factor(data)
  # return(list(BF = bf, Interpretation = interpret_bf(bf, interpreter[1])))
}
