library(XML)
library(RCurl)
u = "https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics/monthly-nonimmigrant-visa-issuances.html"

doc = htmlParse(getURLContent(u))
ll = getHTMLLinks(doc)


d = grep("MonthlyNIVIssuances.*Post", ll, value = TRUE)
du = getRelativeURL(d, u)

# make PDF directory
mapply(download.file, du, file.path("PDF", gsub("%20", "_", basename(d))))
# Then make the XML
status = lapply(sprintf("pdftohtml -xml %s", list.files("PDF", pattern = "\\.pdf$", full = TRUE)), system)

