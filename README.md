This is to compare the number of visas that the different US embassies, consulates, etc. process
each month.  Our interest is to compare the 2021 numbers to pre-COVID numbers for students so we
can approximately estimate how many new and continuing students will be able to get visas to come to
the US for Fall 2021.

The data are available from
[here](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics/monthly-nonimmigrant-visa-issuances.html)
as PDF documents. 

+ The file [fetch.R](fetch.R) downloads the relevant Post-visa issuance documents.
+ The [read.R](read.R) 
+ [cityCountry.R](cityCountry.R) maps cities to countries.  We use this in read.R to create a
  country column for each record for Post, Visa Class, number, country.

This uses the [ReadPDF](https://github.com/dsidavis/ReadPDF) package to read the XML that we
generate from the PDF documents. We convert these to XML using our version of [pdftohtml](https://github.com/dsidavis/pdftohtml).



