require(ggplot2)
require(plyr)
#install.packages("perm")
require(perm)

ddss <- function(nsamp = 50){
     z1samp <- rnorm(nsamp, 10)
     zsamp <- rnorm(nsamp)
     ff <- data.frame(dat = c(zsamp, z1samp), trt = c(rep("y", nsamp), rep("x", nsamp)))
     return(ff)
}


sp <- ddss(50000)

sp$PctChange <- sp$dat
sp$SpeedLimit <- sp$trt

grid <- with(sp, seq(min(PctChange), max(PctChange), length = 100))

normaldens <- ddply(sp, "SpeedLimit", 
                    function(df) {
                         data.frame( 
                              predicted = grid,
                              density = dnorm(grid, mean(df$PctChange), sd(df$PctChange)))
                    })

# look at distributions of data
ggplot(sp, aes(x = PctChange)) + 
     # histogram
     geom_histogram(aes(y = ..density.., fill = "Histogram"), color = "grey40", 
                    alpha = 0.2) + 
     # kernel density line
     geom_line(aes(y = ..density..,  lty = "Density"), stat = 'density')+
     # normal line
     geom_line(aes(y = density, x = predicted, lty = "Normal"), data = normaldens) + 
     # facet
     facet_grid(~SpeedLimit)  +
     
     # labels and theme
     #xlim(c(-3.5, 3.5))+
     labs(x = "Value", title = "Distributions for #3d, from 50000 samples") + 
     theme_bw() + 
     theme(legend.background = element_rect(colour = "black"),
           plot.background = element_blank()
           ,panel.grid.major = element_blank()
           ,panel.grid.minor = element_blank()
           ,panel.border = element_blank()
           ,axis.line = element_line(color = 'black')) + 
     # Names for the legend
     scale_linetype(name = "Line")+
     scale_fill_manual(name = "Histogram", values = c("black"))

t.test(sp$dat~sp$trt)
require(perm)
x <- rnorm(100)
y <- rnorm(100, 10)
permTS(x,y,
       exact = TRUE,
       alternative = "two.sided", 
       method = "exact.mc",
       control = permControl(nmc=10000))

permTS(sp$dat~sp$trt, method = "exact.mc")
