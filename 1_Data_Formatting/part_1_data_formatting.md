Part 1: Data Formatting
================

The COVID-19 loan & grant data is conveniently provided in .csv and .xml
format. When using R with small-medium, .csv format is often ideal, so
we will use that file. To download the data that is updated regularly,
please visit [this
link](https://data.covidstimuluswatch.org/prog.php?detail=opening).

# Data setup

First we want to load the packages and take a quick peek at the data to
see what needs to be done.

``` r
library(dplyr) # for data manipulation
library(stringr) # for string formatting

df <- read.csv('../data/loan_data_6_17.csv', header = TRUE)

glimpse(df)
```

    ## Observations: 28,466
    ## Variables: 32
    ## $ Recipient.Company                                                                <fct> ...
    ## $ Parent.Company                                                                   <fct> ...
    ## $ Award.Date                                                                       <int> ...
    ## $ Award.Type                                                                       <fct> ...
    ## $ Grant.Amount                                                                     <fct> ...
    ## $ Face.Loan.Amount                                                                 <fct> ...
    ## $ Awarding.Agency                                                                  <fct> ...
    ## $ Program.Name                                                                     <fct> ...
    ## $ Award.Details                                                                    <fct> ...
    ## $ Data.Source.for.Award                                                            <fct> ...
    ## $ Facility.Name                                                                    <lgl> ...
    ## $ Facility.State                                                                   <fct> ...
    ## $ Facility.City                                                                    <fct> ...
    ## $ Notes                                                                            <fct> ...
    ## $ Ownership.Structure                                                              <fct> ...
    ## $ Stock.Ticker.Symbol                                                              <fct> ...
    ## $ Parent.Headquarters.State                                                        <fct> ...
    ## $ Parent.Headquarters.Country                                                      <fct> ...
    ## $ Parent.Sector                                                                    <fct> ...
    ## $ Parent.Industry                                                                  <fct> ...
    ## $ Parent.Total.Workforce.at.end.of.2019                                            <int> ...
    ## $ Parent.Latest.Workforce.Size                                                     <int> ...
    ## $ Parent.Employment.related.Penalties.Since.2010                                   <int> ...
    ## $ Parent.Federal.Corporate.Income.Tax.Rate                                         <fct> ...
    ## $ Parent.Total.Federal..State..and.Local.Subsidies.Since.2010                      <int> ...
    ## $ Parent.Government.contracting.related.Penalties.Since.2010                       <int> ...
    ## $ Parent.Environmental...Healthcare...Safety.Penalties.Since.2010                  <int> ...
    ## $ Parent.Consumer.Protection...Financial...Competition.related.Penaties.Since.2010 <int> ...
    ## $ Parent.Ratio.of.CEO.Pay.to.that.of.Median.Worker                                 <fct> ...
    ## $ CEO.Pay                                                                          <fct> ...
    ## $ Median.Worker.Pay                                                                <fct> ...
    ## $ Parent.TARP.Loans.Received.During.Financial.Crisis                               <int> ...

The columns with monetary values are in terms of dollar signs.
Additionally, one of the variables uses percentage signs. We need to
change these variable classes from string to numeric.

``` r
change.cols <- c('Grant.Amount', 'Face.Loan.Amount', 'CEO.Pay', 'Median.Worker.Pay')

df <- df %>%
  mutate_at(change.cols, function(x){ as.numeric(str_remove_all(x, '[^[:digit:]]')) }) %>% # remove non-digit characters and coerce to numeric
  mutate(Parent.Federal.Corporate.Income.Tax.Rate = as.numeric( str_replace(Parent.Federal.Corporate.Income.Tax.Rate, '%', '')) ) # remove % and coerce to numeric
```

After that first step, I’m not sure how I feel about typing these column
names out every time. So now I’m going to manually edit them. This makes
more sense to me - but ideal shortening of words varies from person to
person.

``` r
colnames(df) <- c('recipient_company', 'p_company', 'award_date', 'award_type', 'grant_amount',
                  'face_loan_amount', 'awarding_agency', 'program_name', 'award_details', 'award_data_source',
                  'facility_name', 'facility_state', 'facility_city', 'notes', 'ownership_structure',
                  'stock_ticker', 'p_hq_state', 'p_hq_country', 'p_sector', 'p_industry',
                  'p_2019_workforce', 'p_latest_workforce', 'p_employee_penalties_s2010', 'p_fed_income_tax_rate', 'p_total_subsidies_s2010',
                  'p_gov_contracting_penalties_s2010', 'p_env_health_safety_penalties_s2010', 'p_consum_protect_finan_compet_penalties_s2010',
                  'p_CEO_median_worker_pay_ratio', 'ceo_pay', 'median_worker_pay', 'p_crisis_tarp_loans')
```

Another potential issue for visualization and analyses is that the grant
and loan columns are separate. We can move the variables into one column
if none of the rows have both `grant_loan` and `face_loan_amount` above
0.

``` r
any(df$grant_amount > 0 & df$face_loan_amount > 0)
```

    ## [1] FALSE

It looks like we’re good to go.

``` r
df$award_amount <- ifelse(df$grant_amount > 0, df$grant_amount, df$face_loan_amount)
```

Now we’re ready to write our newly-formatted data and answer some
questions about the U.S. COVID loans in part 2\!

``` r
write.csv(df, '../data/formatted_loan_data_6_17.csv')
```
