# DS_Challenge
Data Science Challenge
Discussion Questions with Answers:
--------------------------------------------------------------
Briefly describe the conceptual approach you chose! 
1. I have used target variable as dependent and other variables as independent.
2. Then I have identified numeric variables and categorical variables separately and did outlier and missing value treatment.
3. For numerical vars reduction ,factor analysis  and anova (between target and each numerical variable) and identified important numerical variables based on F-value and loading from respective tests. 
4. For categorical vars reduction,I have used chi-square test (between target and each categorical variable) and identified important categorical variables based on chi-square values.
5. As a best practice,I restricted number of variables as less than 15 variables and applied glm with binomial logit function on those selected vars.once I ensured, no multi collinierity, good concordance(in this case ~62%),AUC(60%)
6. Using the final model equation from step5, predicted the values for test data set.  
7. Importance of variables can be calculated using  betas and its significance through p values.

-------------------------------------------------------------

What are the trade-offs?
My model is too simple and has very few parameters then it may have high bias and low variance.
Such cases can be sorted out by taking reasonable no of predictors to reduce bias 
and more training data to reduce variance.

--------------------------------------------------------------
What's the model performance? 

1. AIC :182408
2. Null Deviance(186486) and Residual Deviance(182096)  
3. Confusion Matrix:Accuracy:67.8%

       0     1	
394377	11128
179907	10588

4.AUC=0.6048(Cutoff value=0.03927)

--------------------------------------------------------------
What is the complexity?

Limited to Linear relationships between variables.
Large Sample Size(here testing data set is larger than training set).
Independent Observations Required.

--------------------------------------------------------------

Where are the bottlenecks?
Had this been operationa problem,Logistic regression wont be a better option to choose here.Assuming it to strategical problem and then going ahead.
Also accuracy is not good.
Null Deviance(186486) and Residual Deviance(182096)  has not much differnce.

--------------------------------------------------------------
If you had more time, what improvements would you make, and in what order of priority?

I could have gone for forward or backward feature selections.
I would have used some advance algorithm for classifying.


----------------------------------------
Important variables as per model are "cat3","cat7","cat9","cat4","num14","num21","num20","num17","num4"

-------------------------------------------
