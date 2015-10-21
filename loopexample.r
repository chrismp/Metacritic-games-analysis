install.packages('Rcpp') # Makes installation of 'dyplr' work
install.packages('dplyr')
install.packages('ggplot2')

library(Rcpp)
library(dplyr)
library(ggplot2)

#toy data
system<-sample(c("a","b","c"),100, replace=TRUE)
system2<-sample(c("d","e","f"),100, replace=TRUE)
system3<-sample(c("g","h","i"),100, replace=TRUE)
var1<-rnorm(100)
# var2<-rnorm(100)
# var3<-rnorm(100)
data<-data.frame(system,system2,system3,var1)

plot_list<-list()
for (i in 1:3){     
  #save plots as single objects
  # assign(paste0("plot",i),ggplot()+geom_boxplot(aes(x=reorder(system,get(names(data)[i+1])),y=get(names(data)[i+1])))) 
  #or all together in a list
  z <- reorder(
    get(names(data)[i]),
    var1,
    median
  )
  print(levels(z))
  print(ggplot() + 
    geom_boxplot(
      aes(
        x=z,
        y=var1
      )
    ))
}