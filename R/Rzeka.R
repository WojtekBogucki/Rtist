#' @title Rzeka
#'
#' @description Funkcja rysująca obraz pt. "Rzeka"
#'
#' @param bg_color - kolor tła podany jako tekst
#'
#' @usage Rzeka(bg_color="red")
#'
#' @export
Rzeka <- function(bg_color="gold"){
  norm20 <- rnorm(20)
  x <- sort(norm20)
  plot(x, (1:20)/20, type = "s")
  old_bg <- par("bg")
  old_mar <- par("mar")

  par(bg=bg_color)
  par(mar=c(0,0,0,0))
  bg_hue <- rgb2hsv(col2rgb(bg_color))[1]

  plot(seq(-2,2,0.01),pnorm(seq(-2,2,0.01)), type="s", col=3, axes=FALSE, ann = FALSE)
  for(i in rep(100:500, 3)){
    lines(sort(rnorm(i, mean=ifelse(i%%2,-1,1)*(i-300)/2000)),(1:i)/i,type="s", col=rainbow(600, start = bg_hue)[i-100])
  }
  par(bg=old_bg)
  par(mar=old_mar)

}
