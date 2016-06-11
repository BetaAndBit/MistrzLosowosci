#
# Plot ECDF with ggplot2

library(ggplot2)
library(ggthemes)

plotECDF <- function(x, y, name=NA) {
  df <- data.frame(x,y)
  pl <- ggplot(df, aes(x,y)) + geom_point() +
    scale_x_continuous(breaks=unique(x), name="K-10") +
    scale_y_continuous(breaks=unique(y), name="") +
    theme_minimal()
  if (!is.na(name)) ggsave(pl, file=paste0(name,".pdf"))
  pl
}


#
# Generate Distributions

x <- 1:10
y <- round(qnorm((1:10 -0.5)/10, sd = 6))
plotECDF(x,y,"norm")

y <- round(4*qcauchy((1:10 -0.5)/10))
plotECDF(x,y,"cauchy")

y <- round(qunif((1:10 -0.5)/10, -10, 10))
plotECDF(x,y,"unif")

y <- round(4*qlnorm((1:10 -0.5)/10))
plotECDF(x,y,"lnorm")


