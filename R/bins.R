###############################################################################
# get_bin_ids
###############################################################################

# Divides `y` into `n_bins` bins and maps each element of
# `x` into a bin.
#
# @param x a vector of any type (likely integer)
# @param y a numeric vector, of equal length to x
# @param n_bins the number of bins
# @return a vector of length x, with elements of form "val, n" where val is the
# value of x and n is the bin it falls into (based on the value of the
# corresponding value of y)
get_bin_ids <- function(x, y, n_bins) {
   if (length(x) != length(y)) {
      stop("x and y must be of equal length")
   }

   # create bins
   bins <- get_bins(y, x, n_bins)

   # create a bin descriptor for each (x, y) point, so all of the points with the
   # same x and in the same y bin will have the same bin descriptor
   paste(x, bins, sep = ", ")
}

# Creates a numeric vector of length n_bins from min(x) to max(x),
# inclusive.
#
# @param x a numeric vector
# @param n_bins the number of bins (must be > 0)
# @return a vector of length n_bins with equally-spaced elements from
#min(x) to max(x), inclusive.
get_bins <- function(x, grouping, n_bins) {

   calculate <- function(x, grouping) {
     d <- data.frame(xx = x, grouping = grouping, order = 1:length(x))

     d <- d[order(d$xx), ]

     d$diff <- c(0, diff(d$xx))

     # calculate resolution
     MAX_BINS <- 100
     resolution <- diff(range(d$xx)) / MAX_BINS

     # mark the ones that are very close together with NA
     d$bin <- ifelse(d$diff < resolution, NA, d$xx)

     # first is always NA (need to do this before filling in the NAs
     # in case the second one needs to fill from here)
     d$bin[1] <- d$xx[1]

     # fill in NAs with previous value
     d <- zoo::na.locf(d)

     d$bin[order(d$order)]
   }

   d <- data.frame(xx = x, order = 1:length(x), grouping = grouping) %>%
      group_by(grouping) %>%
      mutate(n = calculate(xx, grouping))

   d$n
}
