#' A function discretizes the x vector and then summarizes over 
#' the y vector based on the discretization result.
#'
#' @param x A numeric vector.
#' @param y A numeric vector with 0/1 binary values.
#' @param c A numeric vector of cut points for discretize x.
#'
#' @return A data frame to summarize the binning outcome
#'
#' @examples
#' manual_bin(df$rev_util, df$bad, c(25, 50, 75))
#' # bin freq bads minx maxx
#' #   1 2703  423    0   25
#' #   2 1111  200   26   50
#' #   3  868  191   51   75
#' #   4 1155  382   76  100

manual_bin <- function(x, y, c) {
  df1 <- data.frame(x = x, y = y, cut = findInterval(x, sort(c(c, -Inf, Inf)), left.open = T))
  return(
    Reduce(rbind, 
           lapply(split(df1, df1$cut), 
                  function(x) 
                    data.frame(bin  = x$cut[1],
                               freq = nrow(x),
                               bads = sum(x$y),
                               minx = min(x$x),
                               maxx = max(x$x)))))
}
