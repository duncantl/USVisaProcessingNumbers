# https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics/monthly-nonimmigrant-visa-issuances.html


library(ReadPDF)
library(XML)

if(FALSE) {
doc = readPDFXML("JUNE 2021 - NIV Issuances by Post and Visa Class.pdf")
p = doc[[74]]

z18 = readYear("2018", skip = 1:2)
z19 = readYear("2019", skip = 1:2)
z21 = readYear("2021")

z21 = readYear(xml = "JUNE 2021 - NIV Issuances by Post and Visa Class.xml")

z19.F1 = subset(z19, `Visa Class` == "F1") # drop NAs from GRAND TOTAL
z21.F1 = subset(z21, `Visa Class` == "F1") 

countryMap = readRDS("cityCountryMap.rds")

#z19.F1$country = countryMap[ z19.F1$Post ]
#z21.F1$country = countryMap[ z21.F1$Post ]


years = table(gsub("^[A-Za-z]+[-_]([0-9]+)[-_].*", "\\1", list.files("PDF", pattern = "Post.*\\.pdf$", )))

zz = lapply(names(years), function(y) if(y == "2021")readYear(y) else readYear(y, skip = 1:2))
names(zz) = names(years)

ay = do.call(rbind, zz)
ay$year = rep(as.integer(names(zz)), sapply(zz, nrow))
         
head(z21.F1[order(z21.F1$Issuances, decreasing = TRUE), ], 50)
}

readYear =
function(year, skip = 1:2, dir = "PDF", xml = list.files(dir, pattern = sprintf(".*%s.*\\.xml$", year), full.names = TRUE), ...)    
{
    d = lapply(xml, readDoc, skip = skip, ...)
    month = gsub("[-_ ].*", "", basename(xml))
    d = mapply(function(d, month) {
              names(d) = c("Post", "Visa Class", "Issuances", if(length(d) > 3) "page")
              d$month = month
              d
          }, d, month, SIMPLIFY = FALSE)    
    d = do.call(rbind, d)
    d$Issuances = as.integer(gsub(",", "", d$Issuances))

    d$country = countryMap[d$Post]
    
    d
}

readDoc =
function(f, ..., doc = readPDFXML(f), combine = TRUE, addPage = FALSE)    
{
    tbls = lapply(getPages(doc), readPage, addPage = addPage, ...)

    if(nrow(tbls[[length(tbls)]]) == 1 && grepl("total", tbls[[length(tbls)]][1, 1], ignore.case = TRUE))
        tbls = tbls[ - length(tbls) ]
    
    if(combine) {
        ans = do.call(rbind, tbls)
        if(ans[1, 3] == "Issuances") {
            names(ans)[1:3] =  as.character(ans[1, 1:3])
            ans = ans[-1,]
        }
        ans
    } else
        tbls
}

pageNum = function(p) as.integer(xmlGetAttr(p, "number"))

readPage =
function(page, skip = if(pageNum(page) == 1) 1L else integer(),
         removeFooter = TRUE,
         addPage = FALSE,
         rects = getBBox(page, TRUE), txt = getBBox2(page, TRUE))
{
    # Find the horizontal lines
    rects = rects[rects$nodeType == "rect",]
    is.horiz = abs(rects$y0 - rects$y1) < 3

    if(removeFooter)
        txt = txt[ txt$top < max(rects$y1, rects$y0), ]

    
    # Split the text into rows between the horizontal lines
    rows = split(txt, cut(txt$top + txt$height, c(0, unique(rects$y0[is.horiz]) + 0.5, Inf)))
    rows = rows[ sapply(rows, nrow) > 0]

    if(length(skip))
        rows = rows[ - skip ]
    
    # Now split by the vertical lines
    vert = unique(rects$x0[abs(rects$x0 - rects$x1) < 3])

    # We have to 
    els = lapply(rows, function(x) unlist(split(x$text, cut(x$left, vert))))
    nels = sapply(els, length)

    # Check if we have the same number of elements in each row.
    if(length(unique(nels)) > 1) {
        # Fix with NAs
        i = which.max(nels)
        ids = names(els[[i]])
        w = nels < max(nels)
        templ = structure(rep(NA, length(ids)), names = ids)
        els[w] = lapply(els[w], function(x) { templ[names(x)] = x ; templ })
    }

    ans = as.data.frame(do.call(rbind, els), row.names = seq(along.with = els))
    
    if(addPage)
        ans$page = as.integer(xmlGetAttr(page, "number"))
    
    ans
}


