# Power analysis plots ---------------------------------------------------------
#
# Drawing uses base graphics rather than ggplot2. The panels are simple shaded
# normal curves, and ggplot2 spends roughly 100 ms of fixed overhead on any plot
# at all before drawing anything, which dominated the render time and made the
# sliders feel sluggish. Base graphics produce the same picture in a few
# milliseconds. The theme below reproduces the grey panel and white gridlines the
# app previously got from theme_grey(), so the look is unchanged.

# The plot window is fixed so both panels stay on a common scale as inputs move.
X_MIN <- 10
X_MAX <- 70
MU0 <- 40

# One x grid, resolved once, reused by every shaded region on every redraw.
X_GRID <- seq(X_MIN, X_MAX, length.out = 257)
X_BREAKS <- seq(X_MIN, X_MAX, by = 5)
X_MINOR <- seq(X_MIN, X_MAX, by = 2.5)

FILL_ACCEPT <- "cadetblue1"
FILL_REJECT <- "firebrick"
FILL_BETA <- "skyblue"
FILL_POWER <- "plum2"
PANEL_BG <- "grey92"
GRID_COL <- "white"
AXIS_COL <- "grey30"

# Function: test_quantities
# Description: Computes every critical value, z score, and error rate for one set
#   of inputs. Both panels read from this, so the work happens once per change
#   instead of being repeated inside each plot.
test_quantities <- function(alpha, mu1, sigma, n, direction) {
  sdxbar <- sigma / sqrt(n)
  out <- list(alpha = alpha, mu0 = MU0, mu1 = mu1, sdxbar = sdxbar, direction = direction)

  if (identical(direction, "LTail")) {
    out$xcrit <- qnorm(alpha, MU0, sdxbar)
    out$z0 <- round((out$xcrit - MU0) / sdxbar, 3)
    out$z1 <- round((out$xcrit - mu1) / sdxbar, 3)
    out$type_ii <- round(pnorm(out$z1, lower.tail = FALSE), 3)
  } else if (identical(direction, "UTail")) {
    out$xcrit <- qnorm(1 - alpha, MU0, sdxbar)
    out$z0 <- round((out$xcrit - MU0) / sdxbar, 3)
    out$z1 <- round((out$xcrit - mu1) / sdxbar, 3)
    out$type_ii <- round(pnorm(out$z1, lower.tail = TRUE), 3)
  } else {
    out$xcrit_l <- qnorm(alpha / 2, MU0, sdxbar)
    out$xcrit_u <- qnorm(1 - alpha / 2, MU0, sdxbar)
    out$z0_l <- round((out$xcrit_l - MU0) / sdxbar, 3)
    out$z0_u <- round((out$xcrit_u - MU0) / sdxbar, 3)

    out$z1_l <- round((out$xcrit_l - mu1) / sdxbar, 3)
    out$z1_u <- round((out$xcrit_u - mu1) / sdxbar, 3)

    # A two-tailed test rejects below the lower cut or above the upper one, so
    # power is the sum of both tail probabilities under the alternative. Counting
    # only the nearer tail understates it, most severely when mu1 sits close to
    # mu0, where the two tails contribute equally.
    out$tail_lower <- pnorm(out$xcrit_l, mu1, sdxbar)
    out$tail_upper <- pnorm(out$xcrit_u, mu1, sdxbar, lower.tail = FALSE)
    out$type_ii <- round(1 - (out$tail_lower + out$tail_upper), 3)

    # Kept for the marker line: the cut the alternative mean sits nearest.
    out$xcrit <- if (mu1 < MU0) out$xcrit_l else out$xcrit_u
    out$z1 <- if (mu1 < MU0) out$z1_l else out$z1_u
  }

  out
}

# Function: shade_region
# Description: Fills the area under the normal curve between two bounds.
shade_region <- function(lo, hi, mu, sd, fill) {
  lo <- max(lo, X_MIN)
  hi <- min(hi, X_MAX)
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) {
    return(invisible(NULL))
  }

  xs <- c(lo, X_GRID[X_GRID > lo & X_GRID < hi], hi)
  polygon(c(xs, hi, lo), c(dnorm(xs, mu, sd), 0, 0), col = fill, border = "black", lwd = 1.2)
  invisible(NULL)
}

# Function: open_panel
# Description: Sets up the drawing surface and paints the grey backdrop and white
#   gridlines that reproduce the previous ggplot theme.
open_panel <- function(ylim, scale = 1) {
  par(mar = c(4.4 * scale, 0.8, 1.4, 0.8), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(X_MIN, X_MAX), ylim = ylim)

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col = PANEL_BG, border = NA)
  abline(v = X_MINOR, col = GRID_COL, lwd = 0.8)
  abline(v = X_BREAKS, col = GRID_COL, lwd = 1.7)
  abline(h = pretty(ylim, 5), col = GRID_COL, lwd = 1.7)
  invisible(NULL)
}

# Function: close_panel
# Description: Draws the x axis and its label in the muted grey the theme used.
close_panel <- function(scale = 1) {
  # Fewer ticks on a narrow screen, where every label would otherwise collide.
  breaks <- if (scale < 0.8) X_BREAKS[seq(1, length(X_BREAKS), by = 2)] else X_BREAKS
  axis(1, at = breaks, col = NA, col.ticks = NA, col.axis = AXIS_COL,
       cex.axis = 1.2 * scale, line = -0.5)
  mtext("x", side = 1, line = 2.6 * scale, cex = 1.4 * scale, col = AXIS_COL)
  invisible(NULL)
}

# Function: mark_cut
# Description: Draws the dashed critical-value line with its two direction arrows.
mark_cut <- function(x, ymax, left_colour, right_colour, span = 5, scale = 1) {
  segments(x, 0, x, ymax * 1.14, lty = "twodash", lwd = 1.6)
  arrows(x, ymax * 1.1, x - span, ymax * 1.1, length = 0.09 * scale, lwd = 2.2, col = left_colour)
  arrows(x, ymax * 1.1, x + span, ymax * 1.1, length = 0.09 * scale, lwd = 2.2, col = right_colour)
  invisible(NULL)
}

# Function: null_plot
# Description: The null (comparison) distribution panel, centred at mu0.
null_plot <- function(q, scale = 1) {
  mu <- q$mu0
  sd <- q$sdxbar
  ymax <- dnorm(mu, mu, sd)
  a <- q$alpha
  # Narrow panels omit the wide rate labels; the readout above the plots carries
  # the same numbers, and at this size they would overlap each other.
  compact <- scale < 0.75
  open_panel(c(0, ymax * 1.34), scale)

  if (identical(q$direction, "LTail")) {
    shade_region(q$xcrit, X_MAX, mu, sd, FILL_ACCEPT)
    shade_region(X_MIN, q$xcrit, mu, sd, FILL_REJECT)
    mark_cut(q$xcrit, ymax, FILL_REJECT, "blue", scale = scale)
    text(q$xcrit, ymax * 1.18, bquote(bar(x) == .(round(q$xcrit, 3))), cex = 1.25 * scale)
    text(q$xcrit, ymax * 1.25, paste("z =", q$z0), cex = 1.25 * scale)
    if (!compact) {
      text(q$xcrit - 7, ymax * 1.06, bquote(alpha == .(a)), cex = 1.75 * scale)
      text(q$xcrit + 7, ymax * 1.06, bquote(1 - alpha == .(1 - a)), cex = 1.75 * scale)
    }
  } else if (identical(q$direction, "UTail")) {
    shade_region(X_MIN, q$xcrit, mu, sd, FILL_ACCEPT)
    shade_region(q$xcrit, X_MAX, mu, sd, FILL_REJECT)
    mark_cut(q$xcrit, ymax, "blue", FILL_REJECT, scale = scale)
    text(q$xcrit, ymax * 1.18, bquote(bar(x) == .(round(q$xcrit, 3))), cex = 1.25 * scale)
    text(q$xcrit, ymax * 1.25, paste("z =", q$z0), cex = 1.25 * scale)
    if (!compact) {
      text(q$xcrit - 7, ymax * 1.06, bquote(1 - alpha == .(1 - a)), cex = 1.75 * scale)
      text(q$xcrit + 7, ymax * 1.06, bquote(alpha == .(a)), cex = 1.75 * scale)
    }
  } else {
    shade_region(q$xcrit_l, q$xcrit_u, mu, sd, FILL_ACCEPT)
    shade_region(X_MIN, q$xcrit_l, mu, sd, FILL_REJECT)
    shade_region(q$xcrit_u, X_MAX, mu, sd, FILL_REJECT)

    segments(c(q$xcrit_l, q$xcrit_u), 0, c(q$xcrit_l, q$xcrit_u), ymax * 1.14,
             lty = "twodash", lwd = 1.6)
    arrows(q$xcrit_l, ymax * 1.1, q$xcrit_l - 5, ymax * 1.1,
           length = 0.09 * scale, lwd = 2.2, col = FILL_REJECT)
    arrows(q$xcrit_u, ymax * 1.1, q$xcrit_u + 5, ymax * 1.1,
           length = 0.09 * scale, lwd = 2.2, col = FILL_REJECT)
    arrows(q$xcrit_l, ymax * 1.1, q$xcrit_u, ymax * 1.1,
           length = 0.09 * scale, lwd = 2.2, col = "blue", code = 3)

    text(q$xcrit_l, ymax * 1.18, bquote(bar(x) == .(round(q$xcrit_l, 3))), cex = 1.25 * scale)
    text(q$xcrit_u, ymax * 1.18, bquote(bar(x) == .(round(q$xcrit_u, 3))), cex = 1.25 * scale)
    text(q$xcrit_l, ymax * 1.25, paste("z =", q$z0_l), cex = 1.25 * scale)
    text(q$xcrit_u, ymax * 1.25, paste("z =", q$z0_u), cex = 1.25 * scale)
    if (!compact) {
      text(q$xcrit_l - 7, ymax * 1.06, bquote(alpha / 2 == .(a / 2)), cex = 1.75 * scale)
      text(q$xcrit_u + 7, ymax * 1.06, bquote(alpha / 2 == .(a / 2)), cex = 1.75 * scale)
      text(mu, ymax * 1.06, bquote(1 - alpha == .(1 - a)), cex = 1.75 * scale)
    }
  }

  close_panel(scale)
  invisible(NULL)
}

# Function: alternative_plot
# Description: The alternative distribution panel, centred at mu1, showing how the
#   same critical value splits power from the Type II error rate.
alternative_plot <- function(q, scale = 1) {
  mu <- q$mu1
  sd <- q$sdxbar
  ymax <- dnorm(mu, mu, sd)
  power <- 1 - q$type_ii
  compact <- scale < 0.75
  open_panel(c(0, ymax * 1.34), scale)

  if (identical(q$direction, "TwoTail")) {
    # Both tails reject, so both are shaded as power and the middle is beta.
    shade_region(q$xcrit_l, q$xcrit_u, mu, sd, FILL_BETA)
    shade_region(X_MIN, q$xcrit_l, mu, sd, FILL_POWER)
    shade_region(q$xcrit_u, X_MAX, mu, sd, FILL_POWER)

    segments(c(q$xcrit_l, q$xcrit_u), 0, c(q$xcrit_l, q$xcrit_u), ymax * 1.14,
             lty = "twodash", lwd = 1.6)
    arrows(q$xcrit_l, ymax * 1.1, q$xcrit_l - 5, ymax * 1.1,
           length = 0.09 * scale, lwd = 2.2, col = FILL_REJECT)
    arrows(q$xcrit_u, ymax * 1.1, q$xcrit_u + 5, ymax * 1.1,
           length = 0.09 * scale, lwd = 2.2, col = FILL_REJECT)
    arrows(q$xcrit_l, ymax * 1.1, q$xcrit_u, ymax * 1.1,
           length = 0.09 * scale, lwd = 2.2, col = "blue", code = 3)

    text(q$xcrit_l, ymax * 1.22, paste("z =", q$z1_l), cex = 1.25 * scale)
    text(q$xcrit_u, ymax * 1.22, paste("z =", q$z1_u), cex = 1.25 * scale)

    if (!compact) {
      # Power is the sum of the two shaded tails, so the total is centred above the
      # panel rather than placed beside one tail, where it read as that tail's own
      # probability. Each tail carries its own share underneath.
      text(q$mu0, ymax * 1.06, bquote(beta == .(q$type_ii)), cex = 1.75 * scale)
      text(q$mu0, ymax * 1.29, bquote(1 - beta == .(power)), cex = 1.6 * scale)
      text(q$xcrit_l - 7, ymax * 1.06, sprintf("%.3f", q$tail_lower), cex = 1.25 * scale)
      text(q$xcrit_u + 7, ymax * 1.06, sprintf("%.3f", q$tail_upper), cex = 1.25 * scale)
    }

    close_panel(scale)
    return(invisible(NULL))
  }

  # A lower-tail rejection region puts the power area left of the cut; an
  # upper-tail one puts it to the right.
  lower_rejects <- identical(q$direction, "LTail")

  if (lower_rejects) {
    shade_region(q$xcrit, X_MAX, mu, sd, FILL_BETA)
    shade_region(X_MIN, q$xcrit, mu, sd, FILL_POWER)
    mark_cut(q$xcrit, ymax, FILL_REJECT, "blue", scale = scale)
    if (!compact) {
      text(q$xcrit - 7, ymax * 1.06, bquote(1 - beta == .(power)), cex = 1.75 * scale)
      text(q$xcrit + 7, ymax * 1.06, bquote(beta == .(q$type_ii)), cex = 1.75 * scale)
    }
  } else {
    shade_region(X_MIN, q$xcrit, mu, sd, FILL_BETA)
    shade_region(q$xcrit, X_MAX, mu, sd, FILL_POWER)
    mark_cut(q$xcrit, ymax, "blue", FILL_REJECT, scale = scale)
    if (!compact) {
      text(q$xcrit + 7, ymax * 1.06, bquote(1 - beta == .(power)), cex = 1.75 * scale)
      text(q$xcrit - 7, ymax * 1.06, bquote(beta == .(q$type_ii)), cex = 1.75 * scale)
    }
  }

  text(q$xcrit, ymax * 1.15, paste("z =", q$z1), cex = 1.25 * scale)
  close_panel(scale)
  invisible(NULL)
}
