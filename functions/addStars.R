################################################################################
# File:         addStars.R
#
# Description:  function reads in vector of one or more numeric pvalues and
#               formats to indicated digit length (_digit_) and assigns sig. 
#               stars
#               for each threshold specified in _lvls_
#
# Created by:	  Alex
# requires:     numeric vector of pvalues
# provides:     character vector of original values plus sig stars
################################################################################

addStars <- function(pval, digits = 3, lvls = c(.05, .01, .001)) {
  if (sum(is.na(pval)) == 0) {
    sapply(pval, function(x) {
      x <- ifelse(x > 1, 1.000, x)
      y <- as.numeric(x)
      x <- format(round(x, digits), nsmall = digits)
      lvls <- sort(lvls, decreasing = TRUE)
      for (i in 1:length(lvls)) {
        if (y < lvls[i]) {
          x <- paste0(x, "*")
        }
      }
      return(x)
    })
  }
  else {
    return(pval)
  }
}
