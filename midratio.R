data<- read.csv("Bishayee Colony Counts 10.27.97-3.8.01.csv")

midratio<- function(row){
  low<- min(cols[row,"col1"], cols[row, "col2"], cols[row, "col3"])
  high<- max(cols[row,"col1"], cols[row, "col2"], cols[row, "col3"])
  mid<- setdiff(c(cols[row,"col1"], cols[row, "col2"], cols[row, "col3"]), c(low, high))
  
  midratio<- (mid-low)/(high-low)
  return(midratio)
}


cols<- as.matrix(data[,c("col1", "col2", "col3")])
data$midratio<- sapply(1:dim(cols)[1], midratio)
