# project for CSE 847
# working in Rstudio
setwd("C:\\Users\\lly\\Desktop\\") 
hr=read.csv('hr.csv')  # the data is about an Human Rresource.
id=seq(1,14999,by=1)
whole=cbind(id,hr)  # give each subject an id to analysis

 
project title ： add the confusion matrix into the loss function and try to get the min loss for the company

team members: just one-- Leyi li

Description of the problem:  the data set is about the HR. The company want to keep those high qualify people stay at the company and pay less. So first of all, we would do a regression which consider the different loss for those people leave. Because the different people leaving have different effect for the company. Here is the  confusion matrix.
	             Predict leave	 Predict stay
True leave		right   1          wrong 1
True stay		wrong 2         right 2

	For the company, if they get right 1 they may make some decision to stay the high qualify staff and just let the low qualify stuff leave.  For the wrong 1, they would loss those people due to their wrong prediction. Wrong 2, they may give extra bouns to those people who would just stay when you do nothing. And right 2 they could ignore those people instead of giving bouns. In general, we always treat them the same effect to get the estimate. But for this question, it should be consider the different effect of the different result. 
	After we using the new loss function to get the estimate, we could start our second steps. 
min E(loss of the company)=sum(pi(the person leave the company)*(the bouns we should give to keep those person+if he leave what we loss + the salary they get ))
the beta is fixed but when we give some bouns the pi would also change and the total loss part depend on the bouns and this person qualify. According to this function, we can get a estimate of the alpha(how much bouns we should give ). This result may give a better advise to the company. Of course, we should also try other technique such as random forest , svm and NNs to get the prediction and then we would compare the those result.

reference list：

L.Breiman. Random Forests[J].Machine Learning,2001,45(1):5-32. 
Breiman, L. 2001b. Statistical modeling: The two cultures. Statistical Science 16:199-215.
Breiman, L., J. H. Friedman, R. A. Olshen, and C. J. Stone 1984. Classification and Regression Trees. Chapman and Hall, New York.
TONG ZHANG,Institute of Mathematical Statistics, 2004,Statistical behavior and consistency of classification methods based on convex risk minimization.
TONG ZHANG,Journal of Machine Learning Research 5 (2004) ,Statistical analysis of some multi-category large margin classification methods
