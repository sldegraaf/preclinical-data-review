data<- read.csv("Bishayee Colony Counts 10.27.97-3.8.01.csv")
cols<- as.matrix(data[,c("col1", "col2", "col3")])

midratio<- function(row){
  low<- min(cols[row,"col1"], cols[row, "col2"], cols[row, "col3"])
  high<- max(cols[row,"col1"], cols[row, "col2"], cols[row, "col3"])
  mid<- setdiff(c(cols[row,"col1"], cols[row, "col2"], cols[row, "col3"]), c(low, high))
  midratio<- (mid-low)/(high-low)
  return(midratio)
}

data$midratio<- sapply(1:dim(cols)[1], midratio)
# histogram of RTS midratio values: looks the same as in the paper
hist(as.numeric(as.character(data$midratio)), breaks = 30, 
     xlab = "Mid-ratio values", main = "Histogram of RTS Mid-ratio Values")


others<- read.csv("Other Investigators in Lab.Colony Counts.4.23.92-11.27.02.csv")
cols<- as.matrix(others[, c("col1", "col2", "col3")])
others$midratio<- sapply(1:dim(cols)[1], midratio)
# histogram of Others midratio values
hist(as.numeric(as.character(others$midratio)), breaks = 30,
     xlab = "Mid-ratio values", main = "Histogram of Others Mid-ratio Values")
