# Based on J's solution to the "Choropleth Challenge"
# http://blog.revolutionanalytics.com/2009/11/choropleth-challenge-result.html

# load data
# unemp includes data for some counties not on the "lower 48 states" county
# map, such as those in Alaska, Hawaii, Puerto Rico, and some tiny Virginia
# cities


#install.packages('RColorBrewer')
#install.packages('maps')
library(RColorBrewer)
library(maps)
data(unemp)
data(county.fips)

# define color buckets

display.brewer.all()
colors = brewer.pal(name='Blues',n=6) # try different color palette
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")

# align data with map definitions by matching FIPS codes
# works much better than trying to match the state, county names
colorsmatched <- unemp$colorBuckets [match(county.fips$fips, unemp$fips)]

# draw unemployment map by county and add state boundaries
map("county", col=colors[colorsmatched], fill=TRUE, resolution=0, lty=0)
map("state", col="black", fill=FALSE, add=TRUE, lty=1, lwd=1)

title("unemployment by county, 2009")
legend("bottomleft", leg.txt, horiz=TRUE, fill=colors, cex=.5)


# Add a point on Berkeley
point.col <- rgb(0.8, 0.1, 0, 0.99) 
symbols(x=-122, y=37, circles=1, add=T, inches=0.1, bg=point.col)



