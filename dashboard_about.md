## Information on U.S. CARES Act Spending Dashboard

### Intro

U.S. citizens were not provided with much transparency on CARES Act spending. Curious taxpayers have the right to know who their money is being distributed to. Because of this, Good Jobs First and Grassroot Connection [released a project](https://covidstimuluswatch.org/sources) providing CARES Act loan & grant recipient data gathered through SEC filings. They also include a good bit of information on company accountability. I decided to use this data to help people get a better idea of what industries were either impacted most by COVID loans, or favored in priority by the current administration.

Since I first released this project, much more information has been released - with some being directly from the U.S. government. Unfortunately, the government-provided data only provides ranges of monetary amounts and a [below acceptable](https://www.nbcnews.com/business/business-news/judge-orders-trump-administration-reveal-ppp-loan-data-it-sought-n1246792) amount of information on the businesses was released. The data from the Good Jobs First' project gives more exact figures based on filings for these companies.

### [Dashboard](https://atamalu.shinyapps.io/covid_loans/)

#### About

The dashboard provides tables and graphs of grant, loan, and contract totals awarded by each U.S. agency with options to group by company industry, sector, program, or ownership structure.

#### Notes and (maybe) future fixes

* scaling for the graphs tends to cut off beginning or end text on mobile devices
* graphs are not available for "Overall" as a grouping variable because there is only one piece of data (resulting in one bar)
* since COVID Stimulus Watch only allows downloading 5000 entries at a time, the data was gathered by manually going through each company's parent sector to get a more complete picture
* `Grouping Variable = Industry` `Award Type = Loan` returns an error, which is from a failure to render on shinyapps.io. This is most likely because the very large number of rows
* if you open a graph where `Grouping Variable` is not "Overall", the next time you move to `Grouping Variable = Overall`, the option to render a graph appears (it is usually hidden when the user selects the `Grouping Variable`)

### Resources

* [Dashboard](https://atamalu.shinyapps.io/covid_loans/)
* [Detailed information on data](https://covidstimuluswatch.org/sources)
