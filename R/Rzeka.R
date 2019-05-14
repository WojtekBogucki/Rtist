#' @name Rzeka
#'
#' @author Wojciech Bogucki
#' @author Karol Pysiak
#'
#'
#' @title Plotting river
#'
#' @description This function helps us when we want to see an image of a river. It creates that by plotting
#' many cumulative normal distributions. We can change the color of a background to any colour we want.
#'
#' @param bg_color - background color in form of a text
#'
#' @usage Rzeka(bg_color = "gold")
#'
#' @examples
#' Rzeka()
#' Rzeka(bg_color = "red")
#' Rzeka(bg_color = "blue")
#'
#' @export
Rzeka <- function(bg_color="gold"){

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
