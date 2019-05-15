
Rzeka <- function (params = list(dist = "t", df = 99), bg_color = "gold") {
  old_bg <- par("bg")
  old_mar <- par("mar")
  par(bg = bg_color)
  par(mar = c(0, 0, 0, 0))
  bg_hue <- rgb2hsv(col2rgb(bg_color))[1]
  plot(seq(-4, 4, 0.01), pnorm(seq(-4, 4, 0.01)), type = "s", 
       col = bg_color, axes = FALSE, ann = FALSE)
  for (i in rep(100:500, 3)) {
    if (params$dist == "t") {
      lines(sort(rt(i, df = params$df, ncp = ifelse(i%%2, -1, 1) * (i - 300)/2000)),
            (1:i)/i, type = "s", col = rainbow(600, start = bg_hue)[i - 100])
    } else if (params$dist == "norm") {
      lines(sort(rnorm(i, mean = ifelse(i%%2, -1, 1) * (i - 300)/2000)),
            (1:i)/i, type = "s", col = rainbow(600, start = bg_hue)[i - 100])
    } else if (params$dist == "lnorm") {
      lines(sort(rlnorm(i, meanlog = ifelse(i%%2, -1, 1) * (i - 300)/2000)),
            (1:i)/i, type = "s", col = rainbow(600, start = bg_hue)[i - 100])
    } else if (params$dist == "binom") {
      lines(sort(rbinom(i, size = params$size, prob = params$prob) + ifelse(i%%2, -1, 1) * (i - 300)/2000),
            (1:i)/i, type = "s", col = rainbow(600, start = bg_hue)[i - 100])
    }
  }
  par(bg = old_bg)
  par(mar = old_mar)
}
