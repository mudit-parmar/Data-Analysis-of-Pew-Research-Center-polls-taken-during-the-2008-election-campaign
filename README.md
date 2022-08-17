# Data-Analysis-of-Pew-Research-Center-polls-taken-during-the-2008-election-campaign

Language: R
Steps:-
(a) Take the first data file (Q1Data1.csv). 1) Subset the data so that you have all states but Hawaii, Alaska, andWashington D.C and have only four columns “state,” “marital,” “heat2,” and “heat4.” 2) If no data is available in “heat2,” replace na for the corresponding value in “heat4.” If neither of “heat2” and “heat4” has data, erase the corresponding row. 3) Subset the data so that you only have “dem/lean dem” and “rep/lean rep” in the “heat2” column. 4) Change the label of all the variables but ‘married’ (married people) in the “marital” column to ‘other’ (which indicates non-married people).
(b) For each state, calculate 1) the proportion of the democratic supporters, 2) the proportion of the married people, 3) the ratio of the married people among the
democratic supporters to the total married people, 4) the ratio of non-married among the democratic to the total non-married people, 5) the difference of 3) and 4). Multiply all values by 100 to convert to percentage. Show the first 5observations of these new variables.
(c) Take the second data file (Q1Data2.csv). Subset the data so that 1) you have all but three states, Hawaii, Alaska, and Washington D.C, and 2) only two columns
“state,” and “vote_Obama_pct” (Obama’s actual vote share). Show the first 5 lines of the data set.
(d) Use a logistic regression predicting vote intention given state, using the indicator for being married as a predictor. Set up a proper link function. Try three different assumptions as to the state-level heterogeneity
       Assumption 1: No state-level heterogeneity. All states have the same intercept and slope.
       Assumption 2: Complete state-level heterogeneity. All states have completely independent intercepts and slopes. No outlying coefficient is penalized.
       Assumption 3: State-level heterogeneity is unknown a priori. States have partially pooled intercepts and slopes. Outlying coefficients are penalized.
(e) Using the estimation result from the model with Assumption 3, plot your inference for the predicted vote share by state, along with the actual vote intention, plotting them vs. Obama’s actual vote share. Annotate each dot with the corresponding state name.
(f) The marriage gap is defined as the difference of Obama’s vote share among married and non-married people (“other”). Figure out how to infer this marriage gap from
your model. Using the estimation result from the model with Assumption 3, plot your inference for the marriage gap, along with the raw marriage gaps from the data, plotting them vs. Obama’s vote share.
(g) Repeat (e)-(f) for the model with Assumption 1
