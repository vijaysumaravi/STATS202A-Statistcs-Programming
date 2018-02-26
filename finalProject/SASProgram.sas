proc import out= work.data
datafile= "/folders/myfolders/Stats/regression_auto.csv"
dbms=csv replace; getnames=yes; datarow=2;
run;

* Compute the correlation between car length and mpg;
proc corr data = work.data;
title "Correlation between length and MPG";
var length mpg;
run;

* Make a scatterplot of price (x-axis) and mpg (y-axis);
title "Price vs MPG";
proc sgplot data=work.data;
  scatter x=price y=mpg;
run;

* Make a box plot of mpg for foreign vs domestic cars;
title "Boxplot MPG for Foreign vs Domestic";
proc boxplot data=work.data;
plot mpg*foreign;
run;

* Perform simple linear regression, y = mpg, x = price1; 
* Do NOT include the intercept term;
proc reg;
title "Linear regression X = Price1 Y = MPG";
  model mpg=price1 / NOINT;
run;

* Perform linear regression, y = mpg, x1 = length, x2 = length^2; 
* Include the intercept term;
proc glm;
title "Linear regression Y= MPG X1 = Length X2 = Length^2";
 model mpg=length length*length;
run;