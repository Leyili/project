# project for CSE 847
# working in Rstudio
setwd("C:\\Users\\lly\\Desktop\\") 
hr=read.csv('hr.csv')  # the data is about an Human Rresource.
id=seq(1,14999,by=1)
whole=cbind(id,hr)  # give each subject an id to analysis

 
