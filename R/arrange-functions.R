###############################################################################
# arrange functions
# for arranging the points within a bin
###############################################################################
# Sorts offsets by values, such that the largest (absolute value) offsets map
# to the largest values.
#
# @param offsets numeric vector
# @param values numeric vector of equal length to offsets
# @return offsets sorted such that the largest (absolute value) offsets map
# to the largest values.
v_shape <- function(offsets, values) {
   # order offsets with lowest values in the middle
   # giving a sort of V shape to each bin
   offsets <- offsets[order(abs(offsets))] # 0, -1, 1, -2, 2...
   rankOrder <- rank(values, ties.method = "random")
   stopifnot(length(levels(factor(rankOrder))) == length(offsets))
   offsets[rankOrder]
}

# Sorts offsets by values, such that the largest (absolute value) offsets map
# to the smallest values.
#
# @param offsets numeric vector
# @param values numeric vector of equal length to offsets
# @return offsets sorted such that the largest (absolute value) offsets map
# to the smallest values.
inverted_v_shape <- function(offsets, values) {
   # order offsets with lowest values in the middle
   # giving an inverted V shape to each bin
   offsets <- offsets[order(abs(offsets))] # 0, -1, 1, -2, 2...
   offsets <- rev(offsets)
   rankOrder <- rank(values, ties.method = "random")
   stopifnot(length(levels(factor(rankOrder))) == length(offsets))
   offsets[rankOrder]
}

# Preserves the input order of the offsets.
#
# @param offsets numeric vector
# @param values numeric vector of equal length to offsets
# @return offsets without changing the ordering
preserve_order <- function(offsets, values) {
   offsets
}
