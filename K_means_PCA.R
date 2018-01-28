setwd("/Users/paragmehta/Desktop/R_Coursera")

data_k = read.csv("chin.csv")
data_k = data_k[,-11]
d1 = c()
# Find optimim K value
for (i in 2:10) {
  d1 = c(d1, kmeans(data_k,i)$tot.withinss)
}
plot(d1)

d1 = kmeans(data_k,2)
d2 = kmeans(data_k,3)
d3 = kmeans(data_k,4)
d4 = kmeans(data_k,5)
d5 = kmeans(data_k,6)
d6 = kmeans(data_k,7)

##########################################
# Scale = TRUE- WIll divide by SD, Center will subtract data from mean to make it mean center
#retx, to calculate transform matrix from original matrix

pca1 = prcomp(data_k, retx = TRUE, center = TRUE)
pca1$sdev
pca1$rotation
pca1$x # Transformed matrix

# find optimum K value
d=c()
for (i in 1:10) {
  
  d[i] = pca1$sdev[i]/sum(pca1$sdev)
}
plot(d*100)



