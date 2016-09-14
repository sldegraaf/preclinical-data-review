data<- read.csv("preclinical-data-review\\Bishayee Colony Counts 10.27.97-3.8.01.csv")
cols<- as.matrix(data[,c("col1", "col2", "col3")])

midratio<- function(row){
  low<- min(cols[row,1], cols[row, 2], cols[row, 3])
  high<- max(cols[row,1], cols[row, 2], cols[row, 3])
  mid<- setdiff(c(cols[row,1], cols[row, 2], cols[row, 3]), c(low, high))
  midratio<- (mid-low)/(high-low)
  return(midratio)
}
data$midratio<- as.numeric(as.character(sapply(1:dim(cols)[1], midratio)))
# histogram of RTS midratio values: looks the same as in the paper
hist(as.numeric(as.character(data$midratio)), breaks = 30, 
     xlab = "Mid-ratio values", main = "Histogram of RTS Mid-ratio Values")

# conduct significance test based on binomial 
# how many midratios are between [0.4, 0.6]
n<- length(na.omit(data$midratio))
p<- 0.26
mr<- length(which(data$midratio <= 0.6 & data$midratio >= 0.4))
prob<- 1-pbinom(mr, size=n, prob=p)


others<- read.csv("Other Investigators in Lab.Colony Counts.4.23.92-11.27.02.csv")
cols<- as.matrix(others[, c("col1", "col2", "col3")])
others$midratio<- sapply(1:dim(cols)[1], midratio)
# histogram of Others midratio values
hist(as.numeric(as.character(others$midratio)), breaks = 30,
     xlab = "Mid-ratio values", main = "Histogram of Others Mid-ratio Values")


outside<- read.csv("Outside Lab 3.Colony Counts.2.4.10-5.21.12.csv")
cols<- as.matrix(outside[, c("c1", "c2", "c3")])
outside$midratio<- sapply(1:dim(cols)[1], midratio)
# histogram of Outside midratio values
hist(as.numeric(as.character(outside$midratio)), breaks = 20,
     xlab = "Mid-ratio values", main = "Histogram of Others Mid-ratio Values")


#look at Coulter counts as well:
coult<- read.csv("Bishayee Coulter Counts.10.20.97-7.16.01.csv")
cols<- as.matrix(coult[, c("Count.1", "Count.2", "Count.3")])
coult$midratio<- as.numeric(as.character(sapply(1:dim(cols)[1], midratio)))
# histogram of Outside midratio values
hist(as.numeric(as.character(coult$midratio)), breaks = 20,
     xlab = "Mid-ratio values", main = "Histogram of Coulter Count Mid-ratio Values")
# binomial significance test for coulters:
n<- length(na.omit(coult$midratio))
p<- 0.26
mr<- length(which(coult$midratio <= 0.6 & coult$midratio >= 0.4))
prob<- 1-pbinom(mr, size=n, prob=p)
