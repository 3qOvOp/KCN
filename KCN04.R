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
getSymbols(c("AMD","PFE","JNJ","MRK","BSX","DVA","VTRS"),from = '2021-01-01', to ='2022-5-20')
chartSeries(PFE,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#JNJ (Johnson & Johnson) trend
chartSeries(JNJ,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#MRK (Merck) trend
chartSeries(MRK,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#BSX (Boston Scientifc Corp) trend
chartSeries(BSX,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
addBBands(n=20,sd=2,draw = 'bands')
#DVA (DaVita) trend
chartSeries(DVA,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
addBBands(n=20,sd=2,draw = 'bands')
#VTRS (VTRS) trend
chartSeries(VTRS,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
addBBands(n=20,sd=2,draw = 'bands')

## data preprocessing
prices.df <- cbind(PFE$PFE.Close,JNJ$JNJ.Close,MRK$MRK.Close,
                   BSX$BSX.Close,DVA$DVA.Close,VTRS$VTRS.Close,AMD$AMD.Close)
prices.df <- as.data.frame(prices.df)
#norm.values <- preProcess(prices.df, method=c("center", "scale"))
#prices.df <- predict(norm.values, prices.df)
library(PerformanceAnalytics)
prices.df <- Return.calculate(prices.df, method="log")
prices.df <- prices.df[-1,]
#rollmean(prices.df[,2:4],5)

## simple heatmap of correlations (without values)
heatmap(cor(prices.df), Rowv = NA, Colv = NA)
## heatmap with values
#install.packages('gplots')
library(gplots)
heatmap.2(cor(prices.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(prices.df),2),
          notecol = "black", key = FALSE, trace = 'none',cexRow = 1,cexCol = 1)

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
melted.cor.mat$value <- abs(melted.cor.mat$value)

# 选出相关系数最小的股票
#Select the stock with the lowest correlation coefficient
# 在协相关矩阵中，选把所有相关系数分类，选出平均协相关系数最小的一类
#In the covariance matrix, all correlation coefficients are classified 
#and the class with the lowest average covariance coefficient is selected
# 这类中，每个系数都有两支股票，这些股票为初始化类别的标的物
#In this category, each coefficient has two stocks, 
#which are the subject matter of the initialization category
#install.packages("meanShiftR")
library(meanShiftR)
classification <- meanShift(melted.cor.mat$value,melted.cor.mat$value,
                            bandwidth=c(0.03),
                            alpha=0,
                            iterations = 1000)
melted.cor.mat <- cbind(melted.cor.mat,classification$assignment)
min_classification <- melted.cor.mat[which.min(melted.cor.mat$value),]
min_classification <- min_classification$`classification$assignment`
init_class <- melted.cor.mat[which(melted.cor.mat$`classification$assignment`== min_classification),]
init_stock <- c(init_class$X1,init_class$X2)
init_stock <- unique(init_stock)
# [1] MRK.Close  DVA.Close  AMD.Close  VTRS.Close JNJ.Close  BSX.Close 
# 7 Levels: PFE.Close JNJ.Close MRK.Close BSX.Close DVA.Close ... AMD.Close

# 每个初始化的股票都和另外某个股票具有非常低的相关性，但这些股票之间可能具有较高的相关性
# Each initialized stock has a very low correlation with another stock, but these stocks may have a high correlation
# 合并相关性高的初始化类：找出平均相关系数最大的类，在初始化股票中合并非常相关的类为同一类
# Merge initialization classes with high correlation: find the class with the largest average correlation coefficient, 
# and merge the very relevant classes into the same class in the initialization stock
max_classification <- melted.cor.mat[which.max(melted.cor.mat$value),]
max_classification <- max_classification$`classification$assignment`
merge_stocks <-  melted.cor.mat[which(melted.cor.mat$`classification$assignment`== max_classification),]
merge_stocks <- merge_stocks[order(merge_stocks$value, decreasing = TRUE),]

# 去除非常相关的初始化类
# get rid of very relevant initial classification
for (i in 1:dim(merge_stocks)[1]) {
  if(i > length(merge_stocks)){
    break
  }
  #print(init_stock[which(init_stock == merge_stocks[i,1])])
  p1 <- which(init_stock == merge_stocks[i,1])
  p2 <- which(init_stock == merge_stocks[i,2])
  #print(p1)
  #print(p2)
  if(length(p1)>0 & length(p2)>0){
    if(p1 > 0 & p2 > 0){
      init_stock <- init_stock[-p2]
    }
  }
}
#[1] MRK.Close  AMD.Close  VTRS.Close BSX.Close 
#Levels: PFE.Close JNJ.Close MRK.Close BSX.Close DVA.Close VTRS.Close AMD.Close

k <- length(init_stock)
## Moving average with std 
# window_size is 3 5 7 ...
Ma_std <- function(ts,window_size=21){
  margin <- (window_size-1)/2
  l <- length(prices.df$PFE.Close)
  ma <- rollmean(ts,window_size)
  ts <- ts[-c(1:margin,l-margin+1:l)]
  tem <- (ts - ma)^2
  sum_std <- sum(tem)
  mean_std <- sum_std / l
  ma_std <- sqrt(mean_std) 
  return(ma_std)
}
#Ma_std(prices.df$PFE.Close)

## Train
KCN <- function(prices.df, init_stock){
  init_stock.df <- data.frame(t(as.matrix(init_stock)))
  k <- length(init_stock)
  indexs <- which(colnames(prices.df)%in%c(as.character(init_stock[1:k])))
  stocks <- prices.df[,-c(indexs)]
  marks <- matrix(2,nrow=1,ncol=k)
  for(j in 1:dim(stocks)[2]){ 
    # j个剩余股票
    mean_cors <- matrix(c(1:k),ncol=k)
    
    for( i in 1:k ){                  # k类
      mean_cor <- 0
      n <- 0
      for(m in 1:dim(init_stock.df[i])[1]){# m个归类股票
        if(!is.na(init_stock.df[m,i])){
          n <- n + 1
          cor_tem <- abs(cor(stocks[,j],prices.df[,which(colnames(prices.df)==init_stock.df[m,i])]))
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
cls <- KCN(prices.df,init_stock)
# X1        X2         X3        X4
# 1 MRK.Close AMD.Close VTRS.Close BSX.Close
# 2      <NA> PFE.Close       <NA>      <NA>
# 3      <NA> JNJ.Close       <NA>      <NA>
# 4      <NA> DVA.Close       <NA>      <NA>

# std of classication for KCN
KCN_test <- function(prices.df,cls){
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
# [1] "0.658869846703084" 
#"MRK.Close"         "PFE.Close"         "VTRS.Close"       "BSX.Close"      

# std of classication at random
k <- length(init_stock)
Rand_risk <- function(prices.df,k,ilter=1000){
  sum_std <- 0
  for(i in 1:ilter){
    sp <- sample(dim(prices.df)[2],k,replace = FALSE)
    sum_price <- 0
    for (j in sp) {
      sum_price <- sum_price + prices.df[,j]
    }
    std <- sd(sum_price / k)
    sum_std <- sum_std + std
  }
  mean_std <- sum_std / ilter
  return(mean_std)
}
Rand_risk(prices.df,k,ilter)
# 0.6959148




#ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
#  geom_tile() +
#  geom_text(aes(x = X1, y = X2, label = value))

#heatmap.2(cor(initlist1.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#          cellnote = round(cor(initlist1.df),2),
#          notecol = "black", key = FALSE, trace = 'none',
#          ,cexRow = 1,cexCol = 1)


