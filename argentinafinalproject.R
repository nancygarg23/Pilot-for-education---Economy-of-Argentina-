#Project on Argentina 

#Step 1: Loading the dataset and tidyverse package. 
argentina<-read.csv("D:/Nancy/R Projects/Public Planning/datasets/argentina.csv")
View(argentina)
library(tidyverse)



# Step 2: Check the number of rows and top 6 headings of dataset. 
head(Data)
nrow(Data)



# Step 3: We add Gdp per capita to our datset as GDP per capita is considered to be a better measure of standard of living
# compared t only gdp. 
library(dplyr) #we import package dplyr for using the select command. 
argentina<-argentina%>%mutate(gdp_per_capita=gdp/pop)



#Step 4 : We then sort the dataset into descending order according to gdp per capita to extract the richest provinces 
#in the state. 
library(dplyr)
rich_provines<-argentina%>%arrange(desc(gdp_per_capita))%>%select(province,gdp_per_capita)%>%top_n(4)
rich_provines

#we then extract the 4 richest provinces to have an idea of which all states are richest which will help us at a later 
#stage to evaluate how these satates are different from others.

#we similarly then sort the data based n the population to compare the states which are highest in terms of population.
#as only it occurs in top 6 of both indicators. So this is the example of a city which is utilising its resources carefully and effeciently. 
bigger_popn<-argentina%>%arrange(desc(pop))%>%select(province,pop)%>%filter(pop>1000000)
#we then filter for the population greater than 1 million to have a better idea of the states which are having greater than 
#a threshold population as population in India is larger. 
bigger_popn
#here we will see that there are 9 cities. further,we will witness that none of the country comes in top of gdp_per_capita
#and popn top both. 



# Step 5: for a better preparation of PCA we extract only the numeric columns and remove the province column.This is like the
#data pre processing part in PCA as PCA takes into data the numeric value part only so that it can study the correlation
#among specified variables. 
num_matrix<-as.matrix(argentina[-1])
View(num_matrix)
class(num_matrix)
head(num_matrix)



#Step 6:we will first understand the meaning and purpose and why we use PCA itself for this pupose.
#PCA is an unsupervised learning technique for dimensionaliy reduction which we are using in these database because 
#certain social indicators and economic variables can be highly correlated and this will help redundancy for this pupose.
#the principal components extracted will be a linear combination of original variables which will help us retain
#as much variation as possible.
#typically the PCA is used for multivariate datasets to eract two components which will explain the variation 
#of all the 11 datapoints. And have the highest possible variance (variation explained). 
#for start applying the PCA technique we first load the facto mine R package and scale all the variables before applying
#PCA as so that variances are not over represented and all the variables are in same scale. 
install.packages("FactoMineR")
library(FactoMineR)
argentina.pca<-PCA(num_matrix, scale.unit = TRUE)
summary(argentina.pca)
str(argentina.pca)
#using str$rotation we will get the correlation between initial variables and principal components. 
#we can see that the first two PC's will have the highest variation expalined no matter even if it is positive or negative. 
#now from PCA we obtain 11 PC's where like PC1 explains 45% of variation in our dataset and similarly for other PC's.
#the maximum variation is explained by PC1 and least by PC11.





#Step 7 : now we will plot the PCA components to analyse the PCA results by way of plotting through factoextra package. 
#now to understand the relationship between original variables and among original and new variables, we will 
#formulate correlation plot which will check for the relationship among these variables among ll variables as they are
#plotted among first two dimensions. 
install.packages("factoextra")
library(factoextra)
pca_var_plot<-fviz_pca_var(argentina.pca)
pca_var_plot
#certain key rules which will help us analyse the rules or correlation are as follows :
# 1)Positively correlated vectors have similar vectors.
# 2) The vectors of negatively correlated variables are on opposite sides of the plot origin (opposite quadrants).
# 3) Each axis represents a principal component. Vectors pointing in the direction of the component are correlated with that component.
# 4) The percentage of the original variance explained by each component (dimension) is given in parentheses in the axes labels.
# certain observations we can make from the plot are :-
# 1)GDP and POP are positively correlated as increase in gdp will result in increase in population. 
# 2)Movie theatre per capita and no_healthcare are negatively correlated because as people become more conscious of health,
# number of movie theatres will decrease.
# 3)Further defecient infra and gdp and pop are both negatively correlated. As deficientinfra increases, gdp and pop will 
# decrease.
# 4)Further illetracy and poverty are negatively correlated with doctors per capita because as illetracy and poverty increases
# doctors per capita will decrease. 
# 5) Further variables like deficient infra ,birth mortality no healthcare etc that is those that are in direction of 
# first dimension are correlated with it and those in dim 2 are with it. 
# 6) gdp and popn, movie theatres and gdp per capita, no healhcare and school mortality and deficient infra and illetracy and
#school mortaliy are all positively correlaed to each other pairs ha is they all move in same direction.
variance_first_two_pca<-argentina.pca$eig[1,2]+argentina.pca$eig[2,2]
variance_first_two_pca
# we will further calculate the variances of the two PCs.
#we can further observe from cumulative poportion that PC1 and PC2 together observe 63.54% of the variation in data 
#which is a good indicator as it helps to explain more than 50 % of variation in data. 




#Step 8:
#now the question is how do these clusters match with provinces, for that we need to dig deeper .For that we need to have 
#plot the individual principal components for each province and formulate clusters. 
plot_provinces_pca=fviz_pca_ind(argentina.pca)+labs(title="Provinces_PCA",x="PC1",y="PC2")
plot_provinces_pca
#from the graph we can see that only one province which is province no 1 stands out of the region whereas the other ones are
#somewhat gradient or in the directio towards 2nd PCA.
#we will further use kmeans clustering to identify if there are variables or correlations missing or how can we formulate cluster among 
#these variables. 




#Step 9: Now we begin with forming clusters to identify the exact relationships/clusters of the provinces.
#first we will se the seed for same reproducability.
set.seed(1234)
#now we will first form a dataeframe of only first two PC's as these are the ones we need. 
argentina_comps<-tibble(PC1=argentina.pca$ind$coord[ ,1],PC2=argentina.pca$ind$coord[ ,2])
argentina_comps
#next we make a cluster analysis by using k means clustering in R where we set clusters as 4 with center as an argument.
argentina_km <- kmeans(argentina_comps, centers=4, nstart=20, iter.max=50)
argentina_km
#now we can observe from result that we have clusters of sizes 6, 1, 7 and 8 and we can furher form clusters using plot
#to have a better picture of which provice ends within which cluster. 





#step 10: Now we will formulate a cluster analysis plotting of the above clusters. 
cluser_as_factor<-factor(argentina_km$cluster)
#after converting the clusters into factoors we will now plot these clusters and color these clusters.
fviz_pca_ind(argentina.pca,title="Clustered_by_provinces",habillage=cluser_as_factor)
#this will then color all the clusters by colors. 
#here we can witness that the province which was outside the factors of other provinces actually has only one color,green.
#we can witness that Bueno Aires is in a league of its own.




#step 11: Now we will analyse furher how these clusers are connected wih gdp and gdp per capita.
#for this we first add these clusters factors to our dataset. 
argentina<-argentina%>%mutate(cluster=cluser_as_factor)
library(ggrepel)
ggplot(argentina, aes(cluster, gdp, color = cluser_as_factor)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP")
#from the graph we can witness that Beuno aires is assosciated with highes GDP.
ggplot(argentina, aes(cluster, gdp_per_capita, color = cluser_as_factor)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "GDP PER CAPITA")
#however when we witness the same relation with respect to gdp per capita. Bueno Aires lies somehwere below, that is 
#even thogugh GDP for it might be higher gdp per capita is not. That is the higher GDP is not
#eniugh to suffice the higher populaion. This is particularly representative of case of India as a whole. 
#therefore, we can make predicions that it is the ciies in cluster 4 which are richest provinces .
#these will include Santa Cruz, Neuquen, Chubut, Santa Fe, Mendoza, Cardoba. 
#there is only one province which is San Luis which will lie within its range. which is a par of cluster 3. 
#now we will also see which are the provinces which have high povery rate. 
ggplot(argentina, aes(cluster, poverty, color = cluser_as_factor)) +
  geom_point()+
  geom_text_repel(aes(label = province), show.legend = FALSE) +
  labs(x = "Cluster", y = "Poverty")
#on the basis of poverty we can see that it is provices in cluster 1 which are most poor. 
#these provinces include, Farmosa, Salta, Chaco, Misiones, Corrientos and Santiago del Estero.
#further we can see that provinces in categoty 4 are in lowest regions. 
#this allowed us to review the category of each of the provines in terms of correlation of one another. 


#step 12: Now our final step is to assign the provinces for the pilot study of which all provinces would acually make sense.
#to sudy for the education program impementation. 
#On the basis if our results we should be picking atleast one province from each of the category of pairs of correlaions 
#studied above.





#step13: we can further analyse the relationships between these variables by way of regressions.
reg_gdp_pop_linear<-lm(formula=gdp~pop,argentina)
reg_gdp_pop_linear
#we can witness that there is a positive correation. 
reg_gdp_pop_multiple<-lm(formula=gdp~.,argentina)
reg_gdp_pop_multiple
reg_gdppercapita_pop_linear<-lm(formula=gdp_per_capita~pop,argentina)
reg_gdppercapita_pop_linear
#as a result of gdp per capita when pop increases, gdp per capita decreases by a higher amount.
summary(reg_gdppercapita_pop_linear)
cor(argentina$pop,argentina$gdp_per_capita)
cor(argentina$pop,argentina$gdp)
#pop and gdp are highly correlated. with correlation coeffecient of 0.99%.
#pop and gdp per capita is negatively correlated however the coeffecient effect is only 0.011.
cor(argentina$pop,argentina$no_healthcare)
cor(argentina$gdp,argentina$no_healthcare)
#this depicts that yes gdp and no_healthcare are negatively correlated with 0.14 %. 
cor(argentina$deficient_infra,argentina$gdp)
#deficient infrastructure and gdp are negatively correlated. as 0.26%is explained by it.
cor (argentina$illiteracy)