library(ggplot2)
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("mapproj")
library(mapproj)
install.packages("mapdata")
library(mapdata)

#For this lab, I will making a choropleth map of the differences in CO2 Emission in South
#East Asia. 
#The first step would be to import the data script, which is an Excel file, and is in the Downloads folder. 

library(readxl)
CO2_Emissions <- read_excel("Downloads/Project2_MKhine/data/CO2_Emissions.xlsx")
View(CO2_Emissions)

#Fixing the Dataset 
CO2 <- CO2_Emissions[-(1:15),-c(1,4, 6, 8, 10)]

CO2 <- setNames(CO2, c("Country", 
                       "CO2Emissions(mio.tonnes)", 
                       "%Changesince1990", 
                       "CO2Emissionspercapita", 
                       "CO2Emissionsperkm2(tonnes)"))
CO2 <- na.omit(CO2)

CO2[CO2=="..."]<-NA



#Specifying for South East Asian countries

seacountries <- c("Brunei",
                  "Myanmar",
                  "Cambodia", 
                  "Timor-Leste", 
                  "Indonesia",
                  "Laos",
                  "Malaysia", 
                  "Philippines", 
                  "Singapore", 
                  "Thailand", 
                  "Vietnam")


#Loading the geographic map for South East Asia
seamaps <- map_data("world", region = seacountries)


#Some of the names of the countries in the data set are spelled differently from the names in the
#world map I am using. From the South East Asia region, Brunei, Vietnam and Laos are spelled differently.
#Therefore, I need to rename these in the data set so that they match with those in the world map.

CO2$Country <- recode(CO2$Country, 
                      "Brunei Darussalam" = "Brunei",
                      "Viet Nam" = "Vietnam",
                      "Lao People's Democratic Republic" = "Laos")

SEA_CO2 <- CO2%>%
  filter(Country%in%seacountries)%>%
  select(Country, 'CO2Emissions(mio.tonnes)')%>%
  rename(region = Country, emissions = 'CO2Emissions(mio.tonnes)')


SEA_CO2$emissions <- as.numeric(as.character(SEA_CO2$emissions))

str(SEA_CO2) #To make sure the class of the emission rates is numerical 


#Joining the map and the data set
sea.co2.map <- left_join(SEA_CO2, seamaps, by = "region")

#Making a separate data set of the names of the regions for labelling 
names <- aggregate(cbind(long, lat) ~ region, data=sea.co2.map, FUN=function(x) mean(range(x)))



#Theme for the Plot
theme_may <- theme_void() + theme(panel.grid.major= element_blank(),
                                  panel.grid.minor=element_blank(),
                                  plot.background=element_rect(fill="#FFFFFF"),
                                  panel.background = element_rect(fill=c("#F0FFFF", "#F5FFFA")),
                                  legend.position= "right",
                                  axis.ticks = element_blank(),
                                  legend.text = element_text(color="#006400", size=10),
                                  legend.title=element_text(color="#006400", face="bold", size=10),
                                  plot.title = element_text(color="#006400",face="bold", size=15))

#Final Plot
ggplot(sea.co2.map, aes(x= long,
                        y= lat, 
                        group = group))+
  geom_polygon(aes(fill= emissions), 
               color = "#191970")+
  scale_fill_gradientn(colors = c("#FFF5EE", 
                                  "#FFE4E1", 
                                  "#FC9589", 
                                  "#CD5C5C", 
                                  "#B33B3B", 
                                  "#8B0000"),
                       labels=c("<0",
                                "0-100",
                                "100-200",
                                "200-300", 
                                "300-400", 
                                "400-500",
                                ">500")) +
  theme_may + 
  labs(title="CO2 Emissions in South East Asian Countries in 2011",
       fill="Mio.Tonnes")+
  expand_limits(x = sea.co2.map$long, y = sea.co2.map$lat) + 
  coord_map() +
  geom_label(data=names, mapping= aes(long, lat, label = region), 
             size=2,
             color="#008080",
             inherit.aes = FALSE)

