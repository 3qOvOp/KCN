#install.packages("quantmod")
library(quantmod)
library(zoo)
library(xts)
library(TTR)
library(caret)
#install.packages("ggpubr")
library("ggpubr")
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("psych")
library(psych)
#install.packages("corrplot")
library(corrplot)
#install.packages("fBasics")
library(fBasics)
library(tseries)
library(ggplot2) # alternative plot with ggplot

set.seed(19)

## Trend Analysis - green means up bar and the red means down bar
#PFE (Pfizer) trend

getSymbols(c("AMD","PFE","JNJ","MRK","BSX","DVA","VTRS",
             "NVDA","ASML","MSFT","TSM","AVGO","ADBE","ORCL",
             "CSCO","ACN","INTC","TXN","CRM","IBM"),
           from = '2021-05-25', to ='2022-5-25')
#chartSeries(PFE,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#JNJ (Johnson & Johnson) trend

## data preprocessing
prices.df <- cbind(PFE$PFE.Close,JNJ$JNJ.Close,MRK$MRK.Close,
                   BSX$BSX.Close,DVA$DVA.Close,VTRS$VTRS.Close,AMD$AMD.Close,
                   NVDA$NVDA.Close,ASML$ASML.Close,MSFT$MSFT.Close,TSM$TSM.Close,
                   AVGO$AVGO.Close,ADBE$ADBE.Close,ORCL$ORCL.Close,CSCO$CSCO.Close,
                   ACN$ACN.Close,INTC$INTC.Close,TXN$TXN.Close,CRM$CRM.Close,IBM$IBM.Close
)
prices.df <- as.data.frame(prices.df)

library(PerformanceAnalytics)
prices.df <- Return.calculate(prices.df, method="log")
prices.df <- prices.df[-1,]

standard_sp500 <- Return.calculate(GSPC$GSPC.Close, method="log")
standard_sp500 <- standard_sp500[-1,]
standard_sp500.df <- as.data.frame(standard_sp500)
for (i in 1:dim(prices.df)[2]) {
  prices.df[,i] <- prices.df[,i] - standard_sp500
}


#norm.values <- preProcess(prices.df, method=c("center", "scale"))
#prices.df <- predict(norm.values, prices.df)
#rollmean(prices.df[,2:4],5)


## heatmap with values
#install.packages('gplots')
strsplit(colnames(prices.df)[1],split='\\.')[[1]][1]
prices.df_pic <- prices.df
for(i in 1:dim(prices.df_pic)[2] ){
  colnames(prices.df_pic)[i] <- strsplit(colnames(prices.df_pic)[i],split='\\.')[[1]][1]
}
library(gplots)
heatmap.2(cor(prices.df_pic), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(prices.df),2),
          notecol = "black", key = FALSE, trace = 'none',cexRow = 1,cexCol = 1)
prices.df <- prices.df_pic

## auto-initial algerithm
# get rid of repetitive coefficiency
library(reshape) # to generate input for the plot

cor.mat <- round(cor(prices.df),2) # rounded correlation matrix
dim(cor.mat)
tem <- cor.mat
for(i in 1:dim(cor.mat)[1]){
  for(j in 1:dim(cor.mat)[2]){
    if(j>i){
      tem[i,j] <- 1
    }
  }
}# repetitive digital mark 1
melted.cor.mat <- melt(tem)
melted.cor.mat <- melted.cor.mat[which(melted.cor.mat$value != 1),]
#melted.cor.mat$value <- abs(melted.cor.mat$value)



#Select the stock with the lowest correlation coefficient
#In the covariance matrix, all correlation coefficients are classified 
#and the class with the lowest average covariance coefficient is selected
#In this category, each coefficient has two stocks, 
#which are the subject matter of the initialization category

#install.packages("meanShiftR")
#norm.values <- preProcess(as.data.frame(melted.cor.mat$value), method=c("center", "scale"))
#melted.cor.mat.df <- predict(norm.values, as.data.frame(melted.cor.mat$value))
library(meanShiftR)
classification <- meanShift(melted.cor.mat$value,
                            melted.cor.mat$value,
                            bandwidth=c(0.02),
                            alpha=0,
                            iterations = 1000)
assignment <- classification$assignment
melted.cor.mat_ast <- cbind(melted.cor.mat,assignment)

min_classification <- melted.cor.mat_ast[which.min(melted.cor.mat_ast$value),]
min_classification <- min_classification$assignment
init_class <- melted.cor.mat_ast[which(melted.cor.mat_ast$assignment== min_classification),]
init_class <- unique(init_class)
d <- dim(init_class)[1]
melted.cor.mat_tem1 <- melted.cor.mat_ast
while (d<11) {
  melted.cor.mat_tem1 <- melted.cor.mat_tem1[which(
    melted.cor.mat_tem1$assignment != min_classification),]
  
  min_classification <- melted.cor.mat_tem1[which.min(melted.cor.mat_ast$value),]
  min_classification <- min_classification$assignment
  init_class <- rbind(init_class,
                      melted.cor.mat_ast[
                        which(melted.cor.mat_ast$assignment== min_classification),])
  init_class <- unique(init_class)
  
  d <- dim(init_class)[1]
}
# set max 10 classsifications
init_class <- init_class[order(init_class$value),]
init_stock <- c(init_class$X1,init_class$X2)
init_stock <- unique(init_stock)
init_stock

#tem_cor_num <- melted.cor.mat$value
k <- length(init_stock)
melted.cor.mat_tem <- melted.cor.mat_ast
init_stock_tem <- init_stock

while (k>10) {
  max_classification <- melted.cor.mat_tem[which.max(melted.cor.mat_tem$value),]
  max_classification <- max_classification$assignment
  merge_stocks <-  melted.cor.mat_tem[which(melted.cor.mat_tem$assignment==max_classification),]
  merge_stocks <- merge_stocks[order(merge_stocks$value, decreasing = TRUE),]
  init_marks <- matrix(0,nrow=1,ncol=k)
  # get rid of very relevant initial classification
  q <- 0
  for (i in 1:dim(merge_stocks)[1]) {
    p1 <- which(init_stock_tem == merge_stocks[i,1])
    p2 <- which(init_stock_tem == merge_stocks[i,2])
    if(length(p1)>0 & length(p2)>0){
      if(p1 > 0 & p2 > 0){
        #print(i)
        if(init_marks[p1] > 0 | init_marks[p2] > 0 ){
          tem_max <- max(init_marks[p2],init_marks[p1])
          
          init_marks[p2] <- tem_max
          init_marks[p1] <- tem_max
        }else{
          q <- q + 1
          init_marks[p2] <- q
          init_marks[p1] <- q
        }
      }
    }
  }
  #init_marks
  for (r in 1:max(init_marks)) {
    init_marks[which(init_marks==r)[1]]<-0
  }
  init_stock_tem <- init_stock_tem[which(init_marks == 0)]
  k <- length(init_stock_tem)
  melted.cor.mat_tem <- melted.cor.mat_tem[which(
    melted.cor.mat_tem$assignment != max_classification),]
}
init_stock_tem

init_stock <- init_stock_tem
melted.cor.mat_reorder <- melted.cor.mat[order(melted.cor.mat$value,decreasing = TRUE),]
l <- dim(melted.cor.mat_reorder)[1]
stocks_tem <- c()
for (i in 1:l) {
  stocks_tem[2*i-1] <- melted.cor.mat_reorder[i,1]
  stocks_tem[2*i] <- melted.cor.mat_reorder[i,2]
}
stocks_tem <- unique(stocks_tem)
prices.df_tem <- prices.df[,stocks_tem]
prices.df_tem[1]

# Each initialized stock has a very low correlation with another stock, but these stocks may have a high correlation
# Merge initialization classes with high correlation: find the class with the largest average correlation coefficient, 
# and merge the very relevant classes into the same class in the initialization stock

## Train
KCN <- function(prices.df, init_stock){
  init_stock.df <- data.frame(t(as.matrix(init_stock)))
  k <- length(init_stock)
  indexs <- which(colnames(prices.df)%in%c(as.character(init_stock[1:k])))
  stocks <- prices.df[,-c(indexs)]
  marks <- matrix(2,nrow=1,ncol=k)
  for(j in 1:dim(stocks)[2]){ # j
    mean_cors <- matrix(c(1:k),ncol=k)
    for( i in 1:k ){   # k
      mean_cor <- 0
      n <- 0
      for(m in 1:dim(init_stock.df[i])[1]){# m
        if(!is.na(init_stock.df[m,i])){
          n <- n + 1
          cor_tem <- cor(stocks[,j],prices.df[
            ,which(colnames(prices.df)==init_stock.df[m,i])])
          mean_cor <- cor_tem + mean_cor
        }
      }
      mean_cor <- mean_cor / n
      mean_cors[,i] <- mean_cor
    }
    index <- which.max(mean_cors)
    init_stock.df[marks[1,index],index] <-  colnames(stocks)[j]
    marks[1,index] <- marks[1,index] + 1
  }
  return(init_stock.df)
}
cls <- KCN(prices.df_tem,init_stock)
#          X1        X2         X3         X4        X5         X6        X7        X8         X9       X10
# 1 NVDA.Close IBM.Close MSFT.Close AVGO.Close TSM.Close CSCO.Calose JNJ.Close DVA.Close VTRS.Close BSX.Close
# 2  AMD.Close      <NA> ADBE.Close INTC.Close      <NA> ORCL.Close PFE.Close      <NA>       <NA>      <NA>
# 3 ASML.Close      <NA>  ACN.Close  TXN.Close      <NA>       <NA> MRK.Close      <NA>       <NA>      <NA>
# 4  CRM.Close      <NA>       <NA>       <NA>      <NA>       <NA>      <NA>      <NA>       <NA>      <NA>

# std of classication for KCN
KCN_test <- function(prices.df,cls,m = k){
  
  rand_cls <- sample(1:k, m, replace = FALSE, prob = NULL)
  cls <- cls[,rand_cls]
  k <- dim(cls)[2]
  stocks_po <- c()
  for(i in 1:k){
    n <- 0
    for (j in 1:length(cls[,i])) {
      if(is.na(cls[j,i])){
        break
      }
      n <- n + 1
    }  
    ramdom <- round(runif(1,1,n),0)
    stocks_po[i] <- cls[ramdom,i]
  }
  mean_sum_std <- 0
  sum_price <- 0
  for (m in stocks_po) {
    sum_price <- sum_price + prices.df[,m]
  }
  std <- sd(sum_price / k)
  result <- c(std,stocks_po)
  return(result)
}
KCN_test(prices.df,cls)

Mean_KCN_test <- function(k,n){
  li <- data.frame()
  s <- 0
  for(i in 1 : n){
    tem <- as.numeric(KCN_test(prices.df,cls,k )[1])
    li[i,1] <- tem
    s <- s + tem
  }
  m <- s / 1000
  #return(m)# output risk
  return(li)# output stock
}
li <- Mean_KCN_test(5,1000)

ggplot(li, aes(x = li[,1])) +
  geom_histogram(binwidth = 0.0001, fill = "lightblue", colour = "black")

# std of classication at random
k <- length(init_stock)
Rand_risk <- function(prices.df,k,ilter=1000){
  rstd <- data.frame()
  sum_std <- 0
  for(i in 1:ilter){
    sp <- sample(dim(prices.df)[2],k,replace = FALSE)
    sum_price <- 0
    for (j in sp) {
      sum_price <- sum_price + prices.df[,j]
    }
    std <- sd(sum_price / k)
    sum_std <- sum_std + std
    rstd[i,1] <- std
  }
  mean_std <- sum_std / ilter
  #return(mean_std)
  return(rstd) 
}
li2 <- Rand_risk(prices.df,5,ilter)
sd(as.numeric(li$V1))
sd(as.numeric(li2$V1))
li$V2 <- "KCN"
li2$V2 <- "Rand"
li_all <- rbind(li,li2) 
names(li_all) <- c("fluctuation","stategy")
li_all$stategy = factor(li_all$stategy)
ggplot(li_all, aes(x = fluctuation, fill = stategy)) +
  geom_histogram(position = "identity", alpha = 0.4)



##############################################################################

## Moving average with std 
# window_size is 3 5 7 ...
Ma_std <- function(ts,window_size=21){
  margin <- (window_size-1)/2
  l <- length(ts)
  ma <- rollmean(ts,window_size)
  ts <- ts[-c(1:margin,l-margin+1:l)]
  tem <- (ts - ma)^2
  sum_std <- sum(tem)
  mean_std <- sum_std / l
  ma_std <- sqrt(mean_std) 
  return(ma_std)
}
#Ma_std(prices.df$PFE.Close)