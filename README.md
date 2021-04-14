# Data Warehouse, OLAP, Visualization, CART and C5.0 evaluation

Results: <br />

Looking in the evaluation tool results it is fair to say that CART decision tree is better for number of reasons: accuracy , area under the curve(AUC) and F1 Score, along with sensitivity scores are higher. According to these scores CART DT is better in predicting the “defaulted” class, with lower probabil-ity of missqualification. Especially F1 Score, which means test accuracy is considerably higher than c5.0 algorithm (0.8242812 opposed to 0.7957746). 
To improve the models I have manually forced the model to split according to Gini index, but it did not make any difference to the evaluation measurements. However, pruning the tree to avoid over-fitting the data, by following cross validated error achieved the best results. 

### Process 
1.	MySQL/R Queries/Visualization <br/>
1.1.	Import and inspect	<br/>
1.2.	Query R using RMySQL/DBI	<br/>
1.3.	DPLYR/DBPLYR methodology with pipe operations	<br/>
1.4.	Visualisation	<br/>
2.	OLAP Operations in R <br/>
2.1.	Create data	<br/>
2.2.	Generate sale table	<br/>
2.3.	Revenue cube / Multi-dimentional cube	<br/>
2.4.	OLAP Operations	<br/>
3.	Decision Support Systems in BI <br/>
3.1.	Data import from MySQL to R Studio	<br/>
3.2.	Explore and prepare the data	<br/>
•	Frequency, proportion and average <br/>
•	Visualisation	<br/>
•	Shuffle and re-order the provided data, then split into training and testing sets	<br/>
3.3.	C5.0 algorithm decision tree model	<br/>
3.4.	Decision tree model based on CART <br/>
3.5.	Improvement of current models

### Example of some rendered visualizations
![data plot](https://github.com/gretaivan/bi-decision-support-with-data-warehouse/blob/main/newplot%20(1).png)
