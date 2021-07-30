library(XML)
library(RCurl)

if(FALSE) {
u = "https://en.wikipedia.org/wiki/Lists_of_cities_by_country"
tt = getURLContent(u)
doc = htmlParse(tt)
ll = getHTMLLinks(doc)
li= grep("/List_of_cities_in", ll, value = TRUE)
urls = getRelativeURL(li, u)
}



# Database from https://simplemaps.com/data/world-cities
#
# Others:
#  https://www.geodatasource.com/world-cities-database/free  (from SO post https://stackoverflow.com/questions/3845006/of-countries-and-their-cities)
#  https://www.geonames.org/


cityCountry = read.csv("worldcities.csv")

origPosts = posts = unique(c(z19.F1$Post, z21.F1$Post))
origPosts = posts = posts[!is.na(posts)]

# Remove any alternative name in ()
posts = gsub(" \\(.*", "", posts) # Chennai (...)  Madras...
posts = gsub("/.*", "", posts)  # Osaka/Kobe
posts = gsub(" Tpf$", "", posts) # Tijuana

# 1 NA value
w = (posts %in% cityCountry$city)

pcountry = cityCountry$country[match(posts, cityCountry$city)]

w = is.na(pcountry)
pcountry = cityCountry$country[match(tolower(posts), tolower(cityCountry$city))]

w = is.na(pcountry)
pcountry[w] = cityCountry$country[match(posts[w], cityCountry$city_ascii)]

w = is.na(pcountry)
pcountry[w] = cityCountry$country[match(posts[w], cityCountry$admin_name)]

w = is.na(pcountry)
pcountry[w] = cityCountry$country[match(posts[w], cityCountry$country)]

table(is.na(pcountry))
posts[is.na(pcountry)]


pcountry[posts == "Ciudad Juarez"] = "Mexico"
pcountry[posts == "Curacao"] = "Curacao"

#12 didn't match. 1 NA,
# Port-au-Prince matches Port-au-Prince
# Rio De Janeiro match Rio de Janeiro
# N`Djamean matches N'Djamena

D = adist(posts[is.na(pcountry)], cityCountry$city)

m = structure(apply(D, 1, function(x) cityCountry$city[ x == min(x) ]), names = posts[is.na(pcountry)])

w2 = sapply(m, length) > 1
stopifnot(!any(w2))

pcountry[ match(names(w2), posts ) ] = cityCountry$country[ match(unlist(m), cityCountry$city) ]

names(pcountry) = origPosts

saveRDS(pcountry, "cityCountryMap.rds")


# Tijuana Tpf is in the posts, but this should be Tijuana. It is Tijuana in 2019, but Tijuana Tpf in 2021. Only post with Tpf in name!
# adist for Tijuana matches Tujuanagr. But there is a Tijuana.

# Kuwait is a country

###########



if(FALSE) {


# 26 didn't match. One is NA.
#[1] "AIT Taipei"        "Asuncion"          "Brasilia"         
#[4] "Chennai ( Madras)" "Ciudad Juarez"     "Curacao"          
#[7] "Dar Es Salaam"     "Hyderabad"         "Kolkata"          
#[10] "Krakow"            "Kuwait"            "Lome"             
#[13] "Montreal"          "Mumbai (Bombay)"   "N`Djamena"        
#[16] "Osaka/Kobe"        "Port Au Prince"    "Port Of Spain"    
#[19] "Quebec"            "Reykjavik"         "Rio De Janeiro"   
#[22] "Sao Paulo"         "Tel Aviv"          "Yaounde"          
#[25] NA                  "Tijuana Tpf"      

intersect(b,  cityCountry$admin_name)
# [1] "Quebec"   "Tel Aviv"

b2 = setdiff(b,  cityCountry$admin_name)

b2 = b2[!is.na(b2)]

# No matches in $city_ascii
intersect(b2,  cityCountry$city_ascii)
}
