#' Helper functions to convert from degree to radian
#'
#' @param deg vector of numeric in degree
deg2rad <- function (deg) {
  stopifnot(is.numeric(deg))
  (rad <- (pi / 180) * deg)
}

#' Helper functions to convert from radian to degree
#'
#' @param rad vector of numeric in degree
rad2deg <- function (rad) {
  stopifnot(is.numeric(rad))
  (deg <- rad / (pi / 180))
}

#' Calculate the points on an arc between two points
#'
#' @param x0 first point coordinate on x-axis
#' @param y0 first point coordinate on y-axis
#' @param x1 second point coordinate on x-axis
#' @param y1 second point coordinate on y-axis
#' @param arcdeg degrees of the arc connecting the points (positive is
#'  counter-clockwise, negative is clockwise)
#' @param n number of points in resulting arc line
calcArc <- function(x0 = 1,
                    y0 = 1,
                    x1 = 4,
                    y1 = 6,
                    arcdeg = 30,
                    n = 50) {

  if (abs(arcdeg) >- 359.9 & abs(arcdeg) > 359.9) {
    stop("angle of arc (arcdeg) must be between -359.9 and 359.9")
  }

  anglerad <- atan2(y = y1 - y0, x = x1 - x0) # angle between points
  midpt <- list(x = mean(c(x0, x1)), y = mean(c(y0, y1))) # midpoint coordinates
  # of chord
  arcrad <- deg2rad(deg = arcdeg) # angle of arc in radian
  chordlength <- sqrt((x1 - x0)^2 + (y1 - y0)^2) # length between points
  r <- abs((chordlength / 2) / sin(arcrad / 2)) # radius of circle

  # angle from midpoint to circle center
  lut <- data.frame(
    lessthan180 = c(TRUE, TRUE, FALSE, FALSE),
    sign = c(1, -1, 1, -1),
    rotation = c(90, -90, -90, 90)
  )
  hit <- which(lut$lessthan180 == (abs(arcdeg) < 180) & lut$sign == sign(arcdeg))
  anglecen <- anglerad + deg2rad(lut$rotation[hit])

  # length of midpoint to circle center
  midpt2cenpt <- sqrt(r^2 - (chordlength / 2)^2)

  # calculate center point
  cenpt <- list(
    x = midpt$x + midpt2cenpt*cos(anglecen),
    y = midpt$y + midpt2cenpt*sin(anglecen)
  )

  # angle from circle center to arc
  anglecen2arc <- anglecen + ifelse(abs(arcdeg) < 180, deg2rad(180), 0)

  # produce vector of arc with n points
  arc <- data.frame(
    rad = seq(
      from = anglecen2arc - arcrad/2,
      to = anglecen2arc + arcrad/2,
      length.out = n
    )
  )
  arc$x <- cenpt$x + r * cos(arc$rad)
  arc$y <- cenpt$y + r * sin(arc$rad)

  arc
}

#' Draw the results of calcArc as a line or arrow.
#' makes a conversion in plotting region units to maintain a circular arc
addArc <- function(x0 = 1,
                   y0 = 1,
                   x1 = 4,
                   y1 = 6,
                   arcdeg = 30,
                   n = 50,
                   t = "l",
                   col = 1,
                   lty = 1,
                   lwd = 1,
                   arrowlength = NULL,
                   arrowangle = 30,
                   arrowcode = 2,
                   result = FALSE,
                   ...) {

  # calculate arc
  arc <- calcArc(x0 = x0, y0 = y0, x1 = x1, y1 = y1, arcdeg = arcdeg, n = n)

  # calculate arc in device units
  FROM = "user"
  TO = "chars"

  x0 <- grconvertX(x0, from = FROM, to = TO)
  x1 <- grconvertX(x1, from = FROM, to = TO)
  y0 <- grconvertY(y0, from = FROM, to = TO)
  y1 <- grconvertY(y1, from = FROM, to = TO)
  arc2 <- calcArc(x0 = x0, y0 = y0, x1 = x1, y1 = y1, arcdeg = arcdeg, n = n)
  names(arc2) <- c("rad", "xusr", "yusr")

  arc <- cbind(arc, arc2[,c("xusr", "yusr")])

  # convert back to user coordinates
  arc$xusr <- grconvertX(arc$xusr, from = TO, to = FROM)
  arc$yusr <- grconvertY(arc$yusr, from = TO, to = FROM)

  lines(yusr ~ xusr, data = arc, t = t, col = col, lty = lty, lwd = lwd, ...)
  if (!is.null(arrowlength)) {
    arrows(
      x0 = arc$xusr[n-1], x1 = arc$xusr[n], y0 = arc$yusr[n-1], y1 = arc$yusr[n],
      length = arrowlength, code = arrowcode, angle = arrowangle,
      col = col, lty = lty, lwd = lwd, ...
    )
  }

  if (result == TRUE) { return(arc) }
}
