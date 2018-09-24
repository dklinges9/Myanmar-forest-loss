
setwd('Z:/Interns/David/Myanmar/ArcMap_working_folder/Myanmar_townships')
getwd()
install.packages('SDMTools')
library(SDMTools)  


#create a simple object of class 'asc'
tasc = as.asc(matrix(rep(x=1:10, times=1000),nr=100)); print(tasc)
str(tasc)

#convert to SpatialGridDataFrame
tgrid = sp.from.asc(tasc)
str(tgrid)
