#' Simultaneously dodge and jitter
#'
#' This is primarily used for aligning points generated through
#' `geom_point()` with dodged boxplots (e.g., a `geom_boxplot()` with
#' a fill aesthetic supplied).
#'
#' @family position adjustments
#' @param jitter.width degree of jitter in x direction. Defaults to 40% of the
#'   resolution of the data.
#' @param jitter.height degree of jitter in y direction. Defaults to 0.
#' @param dodge.width the amount to dodge in the x direction. Defaults to 0.75,
#'   the default `position_dodge()` width.
#' @inheritParams position_jitter
#' @export
#' @examples
#' dsub <- diamonds[sample(nrow(diamonds), 1000), ]
#' ggplot(dsub, aes(x = cut, y = carat, fill = clarity)) +
#'   geom_boxplot(outlier.size = 0) +
#'   geom_point(pch = 21, position = position_jitterdodge())
position_jitterdodgepres <- function(jitter.width = NULL, jitter.height = 0,
                                     dodge.width = 0.75, preserve = c("total", "single"), seed = NA) {
  if (!is.null(seed) && is.na(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)
  }

  ggproto(NULL, PositionJitterdodgepres,
    jitter.width = jitter.width,
    jitter.height = jitter.height,
    dodge.width = dodge.width,
    preserve = match.arg(preserve),
    seed = seed
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionJitterdodgepres <- ggproto("PositionJitterdodgepres", Position,
  jitter.width = NULL,
  jitter.height = NULL,
  dodge.width = NULL,
  preserve = "total",

  required_aes = c("x", "y"),

  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    width <- self$jitter.width %||% (resolution(data$x, zero = FALSE) * 0.4)
    # Adjust the x transformation based on the number of 'dodge' variables
    dodgecols <- intersect(c("fill", "colour", "linetype", "shape", "size", "alpha"), colnames(data))
    if (length(dodgecols) == 0) {
      abort("`position_jitterdodge()` requires at least one aesthetic to dodge by")
    }
    ndodge    <- lapply(data[dodgecols], levels)  # returns NULL for numeric, i.e. non-dodge layers
    ndodge    <- length(unique(unlist(ndodge)))

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      ns <- vapply(panels, function(panel) max(table(panel$xmin)), double(1))
      n <- max(ns)
    }

    list(
      dodge.width = self$dodge.width,
      jitter.height = self$jitter.height,
      jitter.width = width / (ndodge + 2),
      seed = self$seed,
      n = n,
      flipped_aes = flipped_aes
    )
  },

  compute_panel = function(data, params, scales) {
    data <- flip_data(data, params$flipped_aes)
    data <- ggplot2:::collide(data, params$dodge.width, "position_jitterdodge", pos_dodge,
      check.width = FALSE)

    trans_x <- if (params$jitter.width > 0) function(x) jitter(x, amount = params$jitter.width)
    trans_y <- if (params$jitter.height > 0) function(x) jitter(x, amount = params$jitter.height)

    data <- ggplot2:::with_seed_null(params$seed, transform_position(data, trans_x, trans_y))

    collided <- ggplot2:::collide(
      data,
      params$width,
      name = "position_dodge",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE
    )

    ## flip_data(data, params$flipped_aes)
    flip_data(collided, params$flipped_aes)
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodge <- function(df, width, n = NULL) {
  if (is.null(n)) {
    n <- length(unique(df$group))
  }

  if (n == 1)
    return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  d_width <- max(df$xmax - df$xmin)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2

  df
}

