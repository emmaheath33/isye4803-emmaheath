## preparation 
rm(list=ls())				# clearing

## load data 
## data sets are available at 
## https://people.orie.cornell.edu/davidr/SDAFE2/index.html
dat = read.csv("Stock_FX_Bond.csv")
stocks_ac = dat[,c(3,5,7,9,11,13,15,17)]
n = length(stocks_ac[,1])
stocks_returns = log(stocks_ac[-1,] / stocks_ac[-n,])
fact = factanal(stocks_returns,factors=2,rotation="none")
print(fact)
print(fact,cutoff = 0.01)
print(fact,cutoff = 0)

loadings = matrix(as.numeric(loadings(fact)),ncol=2)
unique = as.numeric(fact$unique)
loadings
unique
