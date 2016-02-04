#' position_sym_jitter() is deprecated. Use swarm::position_swarm().
#' @export
position_sym_jitter <- function (width = NULL, height = NULL,
  points_per_full_amount = NULL, n_bins = NULL,
  arrange_function = NULL) {
   warning("position_sym_jitter() is deprecated. Use swarm::position_swarm().",
      call. = FALSE)
  swarm::position_swarm(width = width,
    height = height,
    points_per_full_amount = points_per_full_amount,
    n_bins = n_bins,
    arrange_function = arrange_function)
}
