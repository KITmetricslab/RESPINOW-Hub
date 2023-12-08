### Explanation of control elements

- **Data version:** Choose a date from the past to see nowcasts and forecasts as they were made on that date. This enables comparisons between past predictions and eventually observed data.
- **Disease / indicator:** Various epidemiological indicators can be displayed. Currently we are only generating nowcasts for a small subset of them, but we are planning to provide more comprehensive results in the future.
- **Stratification**: Nowcasts can be shown for the whole of Germany, by Bundesland (federal state) or age group. Which stratification levels are available depends on the respective indicator.
<!---
- **Graphical display**: Nowcasts can be displayed in two different ways: Either in an interactive plot which can overlay nowcasts from different models; here zooming etc is possible. Or in an overview plot which shows the nowcasts of one selected model for all states or age groups.
- **Show summary table**: Shows a table summarizing the nowcasts made by one model for a specific Meldedatum (target date) and all federal states or age groups. The table shows the seven-day hospitalization incidence according to the most recent data version and the data version from the time of nowcasting, the nowcast, the resulting correction factor and the precentage change relative to the previous week (according to the nowcast).
--->


**Other options:**

- **Maximum reporting delay**: All nowcasts and predictions refer to values of the respective indicators as available four weeks after the respective end of week. Strongly delayed corrections are thus ignored. In order to show data including these corrections, the maximum reporting delay can be set to "any".
- **Show as**: Data and nowcasts can be shown on a logarithmic or a natural scale. Exponential growth or decline corresponds to a straight line on the logarithmic scale. Moreover, you can choose between absolute numbers and numbers per 100.000 population. The latter makes values more comparable e.g. across federal states.
- **Time series of frozen values**: An alternative to nowcasting is to display for each week the value as it was available on the following Thursday. This way, all values are similarly incomplete and comparable across time.
- **Point estimate**: Either the mean or the median can be shown (the median being the value which is smaller than the true value in 50% of the cases and larger in the other 50%).
- **Prediction interval**: Should a prediction interval be shown? 95% prediction intervals are supposed to contain the true value in 95% of the cases, 50% prediction intervals in 50% of the cases. Note that in practice the true values are often contained less frequently than intended.
<!---
- **Time series by appearance in RKI data**: An alternative to the nowcast of hospitalization incidences by *Meldedatum* (date at which the first positive test of a person was reported to the local health authorities) is to aggregate hospitalization incidences by the date when they first appeared in the RKI data set. These numbers do not change over the following days, meaning that recent trends are easier to interpret.
- **Show retrospective nowcasts**: For research purposes we also collect nowcasts which have been created retrospectively, but based on the data version of the respective date. To avoid confusion with nowcasts generated in real time they are not displayed by default.
- **Uniform y-axis ranges in overview**: If "Overview for one model" is chosen, this switch decides if the same y-axis ranges are to be used for all plots. This facilitates comparisons between states or age groups, but may render some plots difficult to read.
--->
