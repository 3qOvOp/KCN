# This is not the final version.
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
getSymbols("^GSPC",
           from = '2021-05-25', to ='2022-5-25')

getSymbols(c("AMD","PFE","JNJ","MRK","BSX","DVA","VTRS",
             "NVDA","ASML","MSFT","TSM","AVGO","ADBE","ORCL",
             "CSCO","ACN","INTC","TXN","CRM","IBM"),
           from = '2021-05-25', to ='2022-5-25')
#chartSeries(PFE,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#JNJ (Johnson & Johnson) trend
#chartSeries(JNJ,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#MRK (Merck) trend
#chartSeries(MRK,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#BSX (Boston Scientifc Corp) trend
#chartSeries(BSX,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#addBBands(n=20,sd=2,draw = 'bands')
#DVA (DaVita) trend
#chartSeries(DVA,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#addBBands(n=20,sd=2,draw = 'bands')
#VTRS (VTRS) trend
#chartSeries(VTRS,subset='2020-01-01::2021-10-31',up.col='green',dn.col='red',theme="white")
#addBBands(n=20,sd=2,draw = 'bands')

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
#prices.df<- prices.df * 100
#dim(standard_sp500)
#norm.values <- preProcess(prices.df, method=c("center", "scale"))
#prices.df <- predict(norm.values, prices.df)
#rollmean(prices.df[,2:4],5)

## simple heatmap of correlations (without values)
#heatmap(cor(prices.df), Rowv = 20, Colv = 20)
#heatmap(cor(prices.df))
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


# 选出相关系数最小的股票
#Select the stock with the lowest correlation coefficient
# 在协相关矩阵中，选把所有相关系数分类，选出平均协相关系数最小的一???
#In the covariance matrix, all correlation coefficients are classified 
#and the class with the lowest average covariance coefficient is selected
# 这类中，每个系数都有两支股票，这些股票为初始化类别的标的???
#In this category, each coefficient has two stocks, 
#which are the subject matter of the initialization category
#install.packages("meanShiftR")
#class(melted.cor.mat$value)
#norm.values <- preProcess(as.data.frame(melted.cor.mat$value), method=c("center", "scale"))
#melted.cor.mat.df <- predict(norm.values, as.data.frame(melted.cor.mat$value))
library(meanShiftR)
#melted.cor.mat.df$`melted.cor.mat$value`
#classification <- meanShift(melted.cor.mat.df$`melted.cor.mat$value`,
#                            melted.cor.mat.df$`melted.cor.mat$value`,
#                            bandwidth=c(0.1),
#                            alpha=0,
#                            iterations = 1000)
classification <- meanShift(melted.cor.mat$value,
                            melted.cor.mat$value,
                            bandwidth=c(0.02),
                            alpha=0,
                            iterations = 1000)
#classification$assignment
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
#init_class <- init_class[1:10,]
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
  merge_stocks <-  melted.cor.mat_tem[which(melted.cor.mat_tem$assignment== max_classification),]
  merge_stocks <- merge_stocks[order(merge_stocks$value, decreasing = TRUE),]
  init_marks <- matrix(0,nrow=1,ncol=k)
  # 去除非常相关的初始化???
  # get rid of very relevant initial classification
  q <- 0
  for (i in 1:dim(merge_stocks)[1]) {
    
    #print(init_stock[which(init_stock == merge_stocks[i,1])])
    p1 <- which(init_stock_tem == merge_stocks[i,1])
    p2 <- which(init_stock_tem == merge_stocks[i,2])
    #print(p1)
    #print(p2)
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
  #[1] MRK.Close  AMD.Close  VTRS.Close BSX.Close 
  #Levels: PFE.Close JNJ.Close MRK.Close BSX.Close DVA.Close VTRS.Close AMD.Close
  
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
# 每个初始化的股票都和另外某个股票具有非常低的相关性，但这些股票之间可能具有较高的相关???
# Each initialized stock has a very low correlation with another stock, but these stocks may have a high correlation
# 合并相关性高的初始化类：找出平均相关系数最大的类，在初始化股票中合并非常相关的类为同一???
# Merge initialization classes with high correlation: find the class with the largest average correlation coefficient, 
# and merge the very relevant classes into the same class in the initialization stock

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

## Train
KCN <- function(prices.df, init_stock){
  init_stock.df <- data.frame(t(as.matrix(init_stock)))
  k <- length(init_stock)
  indexs <- which(colnames(prices.df)%in%c(as.character(init_stock[1:k])))
  stocks <- prices.df[,-c(indexs)]
  marks <- matrix(2,nrow=1,ncol=k)
  for(j in 1:dim(stocks)[2]){ 
    # j个剩余股???
    mean_cors <- matrix(c(1:k),ncol=k)
    
    for( i in 1:k ){                  # k???
      mean_cor <- 0
      n <- 0
      for(m in 1:dim(init_stock.df[i])[1]){# m个归类股???
        if(!is.na(init_stock.df[m,i])){
          n <- n + 1
          #cor_tem <- abs(cor(stocks[,j],prices.df[,which(colnames(prices.df)==init_stock.df[m,i])]))
          cor_tem <- cor(stocks[,j],prices.df[,which(colnames(prices.df)==init_stock.df[m,i])])
          mean_cor <- cor_tem + mean_cor
        }
      }
      mean_cor <- mean_cor / n
      mean_cors[,i] <- mean_cor
    }
    #print(mean_cors)
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
  s <- 0
  for(i in 1 : n){
    tem <- as.numeric(KCN_test(prices.df,cls,k )[1])
    s <- s + tem
  }
  m <- s / 1000
  return(m)
}
Mean_KCN_test(10,1000)
# 2  0.01203829
# 3  0.00979374
# 4  0.00856183
# 5  0.007571107
# 6  0.00702611
# 7  0.00646038
# 8  0.00604471
# 9  0.00570155
# 10 0.00542160
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
Rand_risk(prices.df,6,ilter)
# 2  0.01196652
# 3  0.01000668
# 4  0.00876331
# 5  0.007892457
# 6  0.00723697
# 7  0.00680076
# 8  0.00640069
# 9  0.00609278
# 10 0.00580009



#ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value)) +
#  geom_tile() +
#  geom_text(aes(x = X1, y = X2, label = value))

#heatmap.2(cor(initlist1.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
#          cellnote = round(cor(initlist1.df),2),
#          notecol = "black", key = FALSE, trace = 'none',
#          ,cexRow = 1,cexCol = 1)
d <- data.frame(k = c(10,9,8,7,6,5,4,3,2),
                kcn = c(0.00542160,0.00570155,0.00604471,0.00646038,0.00702611,0.00759972,0.00856183,0.00979374,0.01203829),
                random = c(0.00580009,0.00609278,0.00640069,0.00680076,0.00723697,0.00795139,0.00876331,0.01000668,0.01196652),
                diff_per = c(0.065255884,0.064212067,0.055615879,0.050050288,0.029136503,0.040716091,0.022991313,0.021279785,-0.005997567)
                )

ggplot(data = d, aes(x = k, y = diff_per)) +
  geom_line()
