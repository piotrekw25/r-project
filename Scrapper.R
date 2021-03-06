library(rvest)
library(ggmap)
library(leaflet)

url1 <- read_html("https://www.cwjobs.co.uk/jobs/c%2B%2B")
oferty <- url1 %>%
  html_nodes('.job-title') %>%
  html_text()
foferty <- factor(oferty)
to_remove<-paste(c("  ","\n"), collapse="|")
foferty<-gsub(to_remove,"",foferty)
foferty

url2 <- read_html("https://www.cwjobs.co.uk/jobs/c%2B%2B")
lokalizacje <- url2 %>%
  html_nodes('.location') %>%
  html_text()
flokalizacje <- factor(lokalizacje)
toremove<-paste(c("\n","-","  ","RG14","RG145DG","CB4","CB42HY","5DG",",","2HY"), collapse="|")
flokalizacje<-gsub(toremove,"",flokalizacje)
flokalizacje

pensja <- sample(70000:100000, 20, replace=F)
fpensja = factor(pensja)
fpensja

geocodes<-geocode(flokalizacje, output="latlona")

leaflet(geocodes) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(foferty))


df = data.frame(foferty,fpensja)
names(df) <- c("Stanowisko", "Zarobki")

ggplot(data=df, aes(x=fpensja, y=foferty )) +
  geom_bar(colour="grey", stat="identity",
           position=position_dodge(),
           size=.3) +    
  xlab("Zarobki") + ylab("Stanowisko")