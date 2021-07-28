# https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/nonimmigrant-visa-statistics/monthly-nonimmigrant-visa-issuances.html

if(FALSE) {
library(ReadPDF)
doc = readPDFXML("JUNE 2021 - NIV Issuances by Post and Visa Class.pdf")
p = doc[[74]]
}

readDoc =
function(f, ..., doc = readPDFXML(f), combine = TRUE)    
{
    tbls = lapply(getPages(doc), readPage, ...)
    if(combine)
        do.call(rbind, tbls)
    else
        tbls
}

pageNum = function(p) as.integer(xmlGetAttr(p, "number"))

readPage =
function(page, skip = if(pageNum(page) == 1) 1L else integer(),
          rects = getBBox(page, TRUE), txt = getBBox2(page, TRUE))
{
    # Find the horizontal lines
    rects = rects[rects$nodeType == "rect",]
    is.horiz = abs(rects$y0 - rects$y1) < 3
browser()
    # Split the text into rows between the horizontal lines
    rows = split(txt, cut(txt$top + txt$height, c(0, unique(rects$y0[is.horiz]), Inf)))
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

    as.data.frame(do.call(rbind, els), row.names = seq(along.with = els))
}


