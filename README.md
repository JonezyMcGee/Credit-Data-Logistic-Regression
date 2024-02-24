Cross Validation was used to test whether a logistic regression model would be useful for such data.
False positives, points classified as good applicants when their true response value is bad are five
times worse than false negatives, points classified as bad applicants when their true response value
is good. The goal was to find a model that minimized the costs of errors while maximizing accuracy. 
Using a threshold of 0.87 on the predictions from the logistic regression model, meaning probabilities
of being good of 87% or greater being classified as good were classified as good and any less were
classified as bad. This resulted in an accuracy of 76.5% with the last model.
