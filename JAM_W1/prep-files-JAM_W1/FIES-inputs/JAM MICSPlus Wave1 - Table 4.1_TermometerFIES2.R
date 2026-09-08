TermometerFIES <- function(rr){
  library(reshape2)
  items <- ncol(rr$XX)
  rawscores <- length(rr$a)
  
  ## Dataset density functions
  mydata <- data.frame("RS0" = rnorm(25000, rr$a[1],rr$se.a[1]),
                       "RS1" = rnorm(25000, rr$a[2],rr$se.a[2]),
                       "RS2" = rnorm(25000, rr$a[3],rr$se.a[3]),
                       "RS3" = rnorm(25000, rr$a[4],rr$se.a[4]),
                       "RS4" = rnorm(25000, rr$a[5],rr$se.a[5]),
                       "RS5" = rnorm(25000, rr$a[6],rr$se.a[6]))
  for (i in 6:(rawscores-1)) {
    mydata[,i+1] <- rnorm(25000, rr$a[i+1],rr$se.a[i+1])
    colnames(mydata)[i+1] <- paste0("RS", i)
  }
  
  mydata <- melt(mydata)
  mydata <- mydata[mydata$value>-10 & mydata$value<10,]
  tmp.plot <- 
    ggplot(mydata, aes(x = value, group = variable)) + 
    geom_density(aes(colour = variable)) +
    scale_color_manual(values = rev(heat.colors(rawscores))) +
    geom_rect(aes(xmin = value-0.075, xmax = value+0.075, ymin = -0.05, ymax = 0, fill = value)) +
    scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                         midpoint = 0) +
    xlab("Log-odds") + ylab("Density") +
    annotate("text", x = -9.5, y = -0.025, label = "Mild") +
    annotate("text", x = 9.3, y = -0.025, label = "Severe") +
    guides(fill = FALSE) + labs(colour = "Raw score\ndistribution") +
    annotate("text", y = -0.2, x = 0, label = "Item severity parameters") +
    annotate("text", y = -0.1, x = rr$b, label = names(rr$b), angle = 90) +
    annotate("text", x = 0, y = 0.6, label = "Raw scores") +
    annotate("text", x = rr$a, y = 0.575, label = 0:(length(rr$a)-1)) + 
    annotate("text", x = 0, y = 0.5, label = "Respondent severity parameters") +
    annotate("text", x = rr$a, y = 0.475, label = round(rr$a, 1)) 
  
  pdf("./Termometer.pdf", width = 10, height = 8)
  print(tmp.plot)
  dev.off()
  tmp.plot
}