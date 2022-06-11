# KCN
stock polifolio

This article applies statistics and machine learning knowledge to reduce the risk of stock portfolio investing. 
Here I developed a stock portfolio algorithm called K-Correlated Neighbours (KCN). 
The algorithm will group related stocks into the same category. 
This allows investors to combine uncorrelated stocks by simply selecting at least one stock in each class. 
This reduces portfolio risk. KCN is a semi-supervised algorithm, 
which means that users need to manually or automatically select some irrelevant stocks. 
This paper details a strategy for initializing KCN labels that selects the least correlated stocks as acceptable to investors. 
In general, investors need to set two parameters before using the investment strategy in this article, 
the first is the maximum affordable number of stocks, and the second is the optimal number of stocks selected according to performance. 
In this way, investors can select the optimal stock portfolio plan within the range of the maximum acceptable number of stocks.

Algorithm 1. Initial stocks portfolio selection using correlation coefficient

1	Meanshift classification for correlation coefficient	

2	Repeat					

3------select the minimum class as initial class	
 
4------find the relative stocks as initial stocks	

5------unique the stocks			

6------compute the length of the stocks		

7------remove the minimum class		

8------(if the length is less than 11, return to repeat)	

9	Compute the length of the stocks			

10	Repeat		

11------select the maximum class as emerged class	

12------find the relationship between stocks in emerged class

13------emerge the initial stocks according to relationship

14------compute the length of the stocks		

15------remove the maximum class		

16------(if the length is more than 10, return to repeat)



Algorithm 2. Classification for all stocks using K-correlated neighbours

1---Reorder the stocks by correlation coefficient in descending			

2---Remove stocks being classified			

3---For each stock				

4------for each class	

5---------compute the correlation coefficiency

6---------add correlation coefficiency up to the sum of correlation coefficiency

7------compute the mean of correlation coefficiency

8---Select the class of the maximum mean		

9---Add the stock to the class	　	　	　
