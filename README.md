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


Scheme abstraction


The stock market has many stocks, which are scattered in a certain dimension of space like nodes. These stocks have more or less some correlation, and this close correlation is like distance. The greater the correlation, the closer the two stocks are in space. It is difficult for investors to categorize all stocks by correlation. This paper considers the correlation between stocks as distance, so that the abstract stock market comes to life. There are many algorithms that study classification in traditional machine learning, so classifying stocks becomes simple. But there are still some differences. Traditional classification methods need to know the spatial vector of each data point to perform classification or clustering. For stock market data, we can only know the relative position of each stock. Therefore, it is necessary to mark several underlying stocks first. The role of these stocks is to determine the node space, that is to say, all other stocks can find their relative positions. The detailed process follows.
The stock has been simulated as the node in Figure 3.1 below, and the line is the correlation between the two stocks. The gray lines are randomly presented, while the red lines show very small correlations. In theory there is a line between any two stocks. Thus, with the help of the red lines, we can easily spot the five extreme stocks. However, we found that there are two stocks that are very close to each other, which have been connected by a blue line on the right in the image below. The votes of the five stocks that are very close should be in the same class, so keep one of these stocks of the same class.
 ![image](https://github.com/3qOvOp/KCN/blob/main/351.png)
Figure 3.5.1 The first and second steps of the origin of an idea

The algorithm then classifies the remaining stocks. The stocks that are classified are also ordered by order. The algorithm should first classify stocks that are closer to the initial stock. The advantage of this classification is that it has less impact on subsequent classifications. The yellow node on the left of Figure 3.2 below is the first batch of stocks that should be classified. The image on the right shows what happens after the first batch of stocks has been classified, and the algorithm can then classify the second batch of stocks. The subsequent classification is based on the correlation between nodes and node categories, that is, the yellow nodes are summarized into the red lines.
 
Figure 3.5.2 Step 3 and Step 4 of the Origin of Ideas
In the end, the scheme obtained a few classification results, and different classifications have been distinguished by color. As can be seen from Figure 3.3, there are almost no nodes that violate the sum, and the algorithm can better cluster adjacent nodes together.
 
Figure 3.5.3 Classification result
This innovative idea has many benefits. First, it does not require manual labeling of data, so it is suitable for classification of a large number of stocks. Several extreme stocks can be automatically marked. Second, it solves the classification problem by applying correlation without the need for spatial coordinates. The last point is that the algorithm does not need to be trained, so it is extremely efficient, which is very important in short-term trading of stocks.


Performance

As can be clearly seen in Figure 4.5.2 below, as the number of stocks selected, the greater the risk reduction ratio of KCN, KCN can reduce the risk reduction effect by up to about 6.5%. When k = 2, KCN has almost no household effect. It is easy to find from the figure that the slope of the line is getting smaller and smaller and has an upward trend, indicating that the effect of the model has an upper limit, and the upper limit will be close to 6.5%. Because when k=10, the slope of the line is 6.53%-6.42%=0.11%, it is estimated that if k=11, the risk reduction effect will not be greater than 6.53%*(1+0.11)=7.24%. There is a special point in the figure, when k=6, the risk reduction effect becomes worse. So for investors who don't have much time and energy, the algorithm recommends them to pick 5 stocks. For yield-conscious investors, 7 to 9 stocks are their best bet for balancing risk-reward.

 ![image](https://github.com/3qOvOp/KCN/blob/main/performance.png)
 
 
Risk distribution for k=7 stocks

Then we continue to explore the case when k is equal to 7, the standard deviation of KCN distribution is 0.006, and the standard deviation of random portfolio investment is 0.0012, which is about twice that of KCN. Obviously, the effect of KCN investment strategy at this time is between K=5 and 10. It has both tail docking and concentration effects. Not only that, we can find from Figure 4.5.5 that the concentration effect of KCN is not excessive, because the left sides of the two distributions are almost coincident. This means that KCN's portfolio investment strategy does not filter out extremely low-risk portfolios. Therefore, when k=7, KCN's portfolio performs the best overall.

 ![image](https://github.com/3qOvOp/KCN/blob/main/distribution.png)




