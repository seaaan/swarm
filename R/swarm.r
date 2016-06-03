swarm <- function(to_jitter, amount, matched_var,
  points_per_full_amount = NULL, threshold = 0.01,
  arrange_function = "v_shape") {

  # Needs:
  # 4. Remove points_per_full_amount, n_bins
  # 5. Update width argument to smaller default
  # 6. Tweak points_per_full_width defaults

  # check if integer
  if (any(to_jitter %% 1 != 0)) {
    stop(paste("The axis to be jittered must be only integers/whole numbers"))
  }

  bin_id <- get_bin_ids(to_jitter, matched_var, threshold)

  # create list where each element (a level of bin_id) has two components:
  #   indices = indices of to_jitter in that bin_id group
  #   offsets = amount to offset based on number of elements in group
  elements <- as.list(levels(factor(bin_id)))
  result <- lapply(elements, FUN = function(elem) {
    indices <- which(elem == bin_id)
    values <- matched_var[indices]
    offsets <- get_offsets(values, amount, arrange_function)

    list(indices = indices, offsets = offsets)
  })

  result <- scale_offsets(result, points_per_full_amount)

  # order offsets by indices
  offsets <- map_offsets_to_indices(result)

  # return variable transformed by offsets
  to_jitter + offsets
}
