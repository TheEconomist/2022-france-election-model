# The _Economist_ French Presidential Election Model

This is a polls-only forecasting model for the 2022 France presidential election. This model predicts the percentages won for each candidate in round 1 and, conditional on their performance, round 2.


For daily-updated estimates, please visit our interactive website [here.](https://www.economist.com/interactive/france-2022) 

For full methodological details please see our Graphic Detail [article](https://www.economist.com/graphic-detail/how-we-forecast-the-french-election/21807484). An outlined version follows.

---


The forecasting model is called via the `scripts/make_forecasts_2022.R` script and works like so:

1. Polling averages. We draw a trend line through the polls using an optimized GAM with splines to calculate an aggregate on every day of the campaign. 
    - The knots parameter on the splines are optimized to produce the most historically accurate averages on every day the campaign.
    - We repeat this process for round 2 polling data.
2. Historical uncertainty. We rely on a dataset of all the polls taken in French elections since 1965 (from Jennings and Wlezien 2018), as well as the vote shares for those parties in those elections.
    - We use the Bayesian programming language Stan to fit another model with splines to predict the historical error of our polling averages across every day of the campaign. We take the root mean squared error of the posterier draws for every day of the campaign (0 to 200 days before polling day) as our target standard deviation for the polling average in step 1.
3. Simulate the uncertainty in translating the polls to votes for round one. Polling errors for each candidate are drawn with a `correlated multivariate t distribution` with 4 degrees of freedom. The covariance between parties is derived from the correlation in the evolutions of party vote share in the polls over the campaign. We use 4 degrees of freedom because we only have 4 elections with a significant number of polls.
4. For each simulated matchup, simulate an additional 1000 elections. 
  1. (Again) Use a GAM with splines to fit a curve to the historical error of our round 2 polling averages across every day of the campaign.
  2. Simulate t-distributed errors (df = 4) again. This allows for additional error in the polls that may be distributed with fat tails.



---
# References

- Jennings, Will; Wlezien, Christopher, 2018, "Replication Data for: Election Polling Errors across Time and Space", https://doi.org/10.7910/DVN/8421DX, Harvard Dataverse, V3, UNF:6:fNeyt86bIJtgIJnhkzydLA== [fileUNF]

