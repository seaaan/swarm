#' Symmetric, conditional, and proportional jitter to avoid overplotting.
#'
#' @description \code{position_sym_jitter} is an upgraded
#' \code{position-jitter()} for data with a categorical variable on one axis.
#'
#' @param width degree of jitter in x direction. Defaults to 40\% of the
#'   resolution of the data, or 0 if height is non-zero. Only one of height or
#'   width can be non-zero.
#' @param height degree of jitter in y direction. Defaults to 0.
#' @param points_per_full_amount the minimum number of overplotted points for
#' jittering to spread across the full width or height. This controls the
#' proportional nature of the jitter. Defaults to the number of points in the
#' most populous bin.
#' @param n_bins the number of bins to divide the data into (per column
#' or row). Points that fall into the same bin and share the same column or row
#' will be jittered. Defaults to 25.
#' @param arrange_function the name of a function defining how the points are
#' arranged within a bin. Defaults to "v_shape". Other possibilities are
#' "inverted_v_shape" and "preserve_order". See "Arrangement of jittered points"
#' section below.
#'
#' @section Symmetic, conditional, and proportional:
#' \code{sym_jitter} is symmetric, conditional, and proportional.
#' \strong{Symmetric}
#' means that points are jittered approximately equally around the relevant
#' axis. If three points overlap and all have x = 1, they will be adjusted to
#' something like x = 1.1, x = 1, and x = 0.9. \strong{Conditional} means that
#' points are only jittered if they overlap with other points. Otherwise they
#' will remain unchanged on the relevant axis. \strong{Proportional} means that
#' the magnitude of the jitter depends on the degree of overlap. If many points
#' overlap, the jitter will span up to the maximum defined by the parameter
#' \code{width} or \code{height}. If only a few points overlap, the jitter will
#' span a fraction of that maximum. The proportionality is defined by the
#' parameter \code{points_per_full_amount}.
#'
#' @section Arrangement of jittered points:
#' Overlapping points can be arranged several ways, as defined by the choice of
#' \code{arrange_function}. The default, "v_shape", sorts overlapping points
#' by size and jitters them such that the greatest and second-greatest values
#' are jittered most (but in opposite directions) and the smallest and
#' second-smallest values are jittered least. This creates a subtle V-shape. At
#' lower values of \code{n_bins}, the V-shape can be overly pronounced,
#' which can be remedied by increasing the value of this parameter.
#'
#' The \code{arrange_function} "inverted_v_shape" simply flips the V-shape 180
#' degrees.
#'
#' Finally, "preserve_order" arranges the points left to right or bottom to top
#' based on their order in the data.
#' @export
position_sym_jitter <- function (width = NULL, height = NULL,
  points_per_full_amount = NULL, n_bins = NULL,
  arrange_function = NULL) {
  ggplot2::ggproto(NULL, PositionSymJitter,
    width = width,
    height = height,
    points_per_full_amount = points_per_full_amount,
    n_bins = n_bins,
    arrange_function = arrange_function)
}

PositionSymJitter <- ggplot2::ggproto("PositionSymJitter", ggplot2::Position,
  compute_defaults = function(self, data) {
    ggplot2:::check_required_aesthetics(c("x", "y"), names(data), "position_sym_jitter")
    if (ggplot2:::empty(data)) return(data.frame())

    nullZeroWidth <- is.null(self$width) || self$width == 0
    nullZeroHeight <- is.null(self$height) || self$height == 0

    # shouldn't jitter BOTH x and y
    if ((!nullZeroWidth) & (!nullZeroHeight)) {
      stop("width or height should be 0.")
    }

    tempWidth <- self$width %||% 0
    tempHeight <- self$height %||% 0

    # default: jitter x but not y
    if (nullZeroWidth & nullZeroHeight) {
      tempWidth <- resolution(data$x, zero = FALSE) * 0.4
      tempHeight <- 0
    }

    # insert parameter defaults
    list(width = tempWidth,
      height = tempHeight,
      points_per_full_amount = self$points_per_full_amount %||% NULL,
      n_bins = self$n_bins %||% 25,
      arrange_function = self$arrange_function %||% "v_shape"
    )
  },

  adjust = function(data, params) {
    trans_x <- NULL
    trans_y <- NULL

    if(params$width > 0) {
      trans_x <- function(x, width, y) {
        sym_jitter(x, amount = params$width, matched_var = data$y,
          points_per_full_amount = params$points_per_full_amount,
          n_bins = params$n_bins,
          arrange_function = params$arrange_function)
      }
    }

    if(params$height > 0) {
      trans_y <- function(x, amount, y) {
        sym_jitter(x, amount = params$height, matched_var = data$x,
          points_per_full_amount = params$points_per_full_amount,
          n_bins = params$n_bins,
          arrange_function = params$arrange_function)
      }
    }

    ggplot2:::transform_position(data, trans_x, trans_y)
  }

)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
