maPalette <- function(low = "white", high = c("green", "red"), mid=NULL, k =50)
{
  low <- col2rgb(low)/255
  high <- col2rgb(high)/255
  
  if(is.null(mid)){
    r <- seq(low[1], high[1], len = k)
    g <- seq(low[2], high[2], len = k)
    b <- seq(low[3], high[3], len = k)
  }
  if(!is.null(mid)){
    k2 <- round(k/2)
    mid <- col2rgb(mid)/255
    r <- c(seq(low[1], mid[1], len = k2),
           seq(mid[1], high[1], len = k2))
    g <- c(seq(low[2], mid[2], len = k2),
           seq(mid[2], high[2], len = k2))
    b <- c(seq(low[3], mid[3], len = k2),
           seq(mid[3], high[3], len = k2))
  }
  rgb(r, g, b)
}
