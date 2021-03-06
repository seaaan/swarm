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
   bins <- get_bins(y, n_bins)

   # map y-values to bin number
   y_mapped <- map_to_bin(y, bins)

   # create a bin descriptor for each (x, y) point, so all of the points with the
   # same x and in the same y bin will have the same bin descriptor
   paste(x, y_mapped, sep = ", ")
}

# Creates a numeric vector of length n_bins from min(x) to max(x),
# inclusive.
#
# @param x a numeric vector
# @param n_bins the number of bins (must be > 0)
# @return a vector of length n_bins with equally-spaced elements from
#min(x) to max(x), inclusive.
get_bins <- function(x, n_bins) {
   if(n_bins <= 0) stop("n_bins must be > 0")

   seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE),
      length.out = n_bins)
}

# Returns a numeric vector mapping each element of x to a bin.
#
# @param x a numeric vector
# @param bins a sorted (increasing), numeric vector from min(x) to max(x),
# inclusive
# @return a vector of the same length as x, with each element indicating which
# bin the corresponding element of x belongs in. Elements go into the largest
# bin that is less than or equal to their value.
map_to_bin <- function(x, bins) {

   #validate parameters
   if ( min(x, na.rm = TRUE) != min(bins) ||
         (max(x, na.rm = TRUE) != max(bins) && length(bins) != 1)) {
      stop(paste("x and bins must have the same minimum and maximum. min(x) =",
         min(x, na.rm = TRUE), "min(bins) =", min(bins), "max(x) =",
         max(x, na.rm = TRUE), "max(bins) =", max(bins, na.rm = TRUE)))
   }
   if (is.unsorted(bins)) stop("bins must be sorted, but it isn't")

   result <- sapply(x, FUN = function(i) ifelse(is.na(i), 0,
      max(which(i >= bins))))
   # if i is NA, it will be removed later in the ggplot2 code, so return value
   # doesn't matter. max(integer(0)), which is what happens if i == NA, generates
   # a warning, so this avoids that

   result
}
