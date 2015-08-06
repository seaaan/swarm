###############################################################################
# get_offsets
###############################################################################

# Generates a numeric vector the same length as \code{values} from
# \code{-amount} to \code{+amount}, sorted according to
# \code{arrange_function}.
#
# @param values a numeric vector of values to generate offsets for
# @param amount the maximum amount of offset
# @param arrange_function the name of function to sort the offsets by
# @return a numeric vector the same length as \code{values} from
# \code{-amount} to \code{+amount}, sorted according to
# \code{arrange_function}.
get_offsets <- function(values, amount, arrange_function) {

   # 0 if only one element
   offsets <- 0

   if(length(values) > 1) {
      offsets <- seq(from = -amount, to = amount, length.out = length(values))

      offsets <- do.call(arrange_function, list(offsets, values))
   }
   offsets
}

# Scales \code{offsets}.
#
# @param list list with each element containing indices and offsets
# @param points_per_full_amount the minimum number of points necessary to
# maintain the current level of offset (> 0)
# @return list with offsets scaled by a number between 0 and 1. Scaled by 1 if
# \code{length(offsets) >= points_per_full_amount}, else by the fraction
# \code{length(offsets) / points_per_full_amount}. If null, it is replaced by
# the maximum number of indices in an element of \code{list}.
scale_offsets <- function(list, points_per_full_amount) {

   # if points_per_full_amount is null, scale by bin with most points
   # else use user specification
   if(is.null(points_per_full_amount)) {
      points_per_full_amount <- max(sapply(list,
         FUN = function(elem) length(elem[["offsets"]])))
   }

   list <- lapply(list, FUN = function(elem) {
      elem[["offsets"]] <- apply_scale(elem[["offsets"]], points_per_full_amount)
      elem
   })
   list
}

# Scales \code{offsets}.
#
# @param offsets a numeric vector of offsets to scale
# @param points_per_full_amount the minimum number of points necessary to
# maintain the current level of offset (> 0)
# @return offsets scaled by a number between 0 and 1. Scaled by 1 if
# \code{length(offsets) >= points_per_full_amount}, else by the fraction
# \code{length(offsets) / points_per_full_amount}.
apply_scale <- function(offsets, points_per_full_amount) {
   if(points_per_full_amount < 1) stop("points_per_full_amount must be > 0")
   # scale the fraction of jitter amount used by number of points
   # should be 1 if number of points >= points_per_full_amount, else
   # between 0 and 1
   scaleFactor <- min(c(1, length(offsets) / points_per_full_amount))

   offsets <- offsets * scaleFactor
}

# Reorders \code{offsets} by \code{indices}.
#
# @param list list with each element containing indices and offsets
# @return a numeric vector of all the offsets in the list ordered by the
# indices in the list
map_offsets_to_indices <- function(list) {
   indices <- unlist(sapply(list, FUN = function(elem) elem[["indices"]]))
   offsets <- unlist(sapply(list, FUN = function(elem) elem[["offsets"]]))
   offsets <- offsets[order(indices)]
   offsets
}
