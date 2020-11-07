#' Compute Functional Diversity FDiv
#'
#' @param data The matrix dataset for which you want to compute the index
#'
#' @examples
#' data(traits_birds)
#' fd_fdiv(traits_birds)
#'
#' @return The value of FDiv (numeric of length 1)
#'
#' @references
#' Villéger S., Mason N. W. H., Mouillot D. (2008), New multidimensional
#' functional diversity indices for a multifaceted framework in functional
#' ecology, Ecology 89(8), \doi{10.1890/07-1206.1}
#'
#' @export
fd_fdiv <- function(data) {

  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  if (is.vector(data)) {
    data <- as.matrix(data)
  }

  G <- colMeans(data, na.rm = TRUE)

  dG <- sqrt(colSums((t(data) - G)^2, na.rm = TRUE))

  mean_dG <- mean(dG)

  deltaD <- sum(dG - mean_dG)

  deltaD_abs <- sum(abs(dG - mean_dG))

  FDiv <- (deltaD + mean_dG) / (deltaD_abs + mean_dG)

  return(FDiv)

}