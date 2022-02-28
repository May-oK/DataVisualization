#Final Lab
#November 8, 2019
#Name: May Oo Khine
#Class: GOVT16/QSS17 Data Visualization 

#For this final lab, I will make a cartogram and a choropleth map of the population of Asian
#countries. I will also make a plot of the population growth rates of the same countries
#in 2019 to predit the direction of population increase/decrease. 

library(dplyr)        
install.packages("cartogram")
library(cartogram)    
library(ggplot2)      
library(broom)       
library(maptools)     
library(viridis)     
install.packages("rvest")
library(rvest)
library(readr)
library(maps)


#Loading the world data set and specifiying Asian countries 
data(wrld_simpl)
Asia=wrld_simpl[wrld_simpl$REGION==142,]
plot(Asia)

Asia_cartogram <- cartogram_cont(Asia, "POP2005", itermax=7)
plot(Asia_cartogram)


#To make a choropleth map, I need to combine the values of the geospatial object and the numeric
#population value. 
Tidy_Asia <- tidy(Asia_cartogram)
Tidy_Asia = Tidy_Asia %>%
  left_join(. , asia_cartogram@data, by=c("id"="ISO3")) 


#I also want to add some information about big cities in Asia. The world.cities dataset
#contains a list of the biggest cities in the world. I will filter out five of the most 
#populated countries from the dataset. 

install.packages("ggrepel")
library(ggrepel)
cities <- world.cities%>%
  filter(country.etc==c("China","India", "Indonesia", "Pakistan", "Bangladesh"))

?guide_legend

#Graphing the Cartogram Map 
ggplot() +
  geom_polygon(data = Tidy_Asia, mapping= aes(x=long, y=lat, fill = POP2005, group = group),size=0.5) +
  scale_fill_viridis_c(name="Population in millions",
                       guide=guide_legend(label.position="right",
                                          title.position="top",
                                          keyheight=unit(4, units="mm")))+
  coord_map()+
  geom_point(data=cities, mapping=aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel(data=cities %>% 
                    arrange(pop) %>% 
                    tail(10), 
                  aes(x=long, y=lat, label=name), 
                  size=3,
                  alpha=1, 
                  color="#CD5C5C")+
  geom_point(data=cities %>% 
               arrange(pop) %>% 
               tail(10), 
             aes(x=long, y=lat),
             color="red", 
             alpha=0.8,
             size=5) +
  theme(plot.background = element_rect(fill="#F5FFFA"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background=element_rect(fill="#F5FFFA"),
        text=element_text(color="#4B0082"),
        legend.text = element_text(size=10),
        legend.background = element_blank(),
        plot.title=element_text(size=20, color="#000000"))+
  labs(title = "Cartogram of the populations of Asian Countries") 

  

#Making a bar plot of the growth rates of Asian countries

#I am going to scrape data from this website of World Population Review.

popdat <- read_html("http://worldpopulationreview.com/countries/")
popdat2 <- popdat%>%
  html_nodes(css = "table")%>%
  html_table(fill=TRUE)

popdat2019 <- popdat2[[1]]
popdat2019 <- popdat2019[-c(1),-c(1, 3, 4, 6, 7)]
colnames(popdat2019)[colnames(popdat2019)=="X2"] <- "Country"
colnames(popdat2019)[colnames(popdat2019)=="X5"] <- "Growth Rate"
str(popdat2019)
popdat2019$`Growth Rate` <- parse_number(as.character(popdat2019$`Growth Rate`))
colnames(popdat2019)[colnames(popdat2019)=="Growth Rate"] <- "Growth_Rate"
str(popdat2019)

#I want to change the names of some countries to match that of the Cartogram. 
popdat2019[26,1] <- "Burma"
popdat2019[19,1] <- "Iran (Islamic Republic of)"
popdat2019[54,1] <- "Korea, Democratic People's Republic of"
popdat2019[28,1] <- "Korea, Republic of"
popdat2019[105,1] <- "Lao People's Democratic Republic"
popdat2019[69,1] <- "Syrian Arab Republic"
popdat2019[15,1] <- "Viet Nam"
popdat2019[175,1] <- "Brunei Darussalam"

Countries <- c("Azerbaijan", 
               "Armenia",
               "Bahrain",
               "Bangladesh",
               "Burma",
               "Brunei Darussalam",
               "Cambodia",
               "Sri Lanka",
               "China",
               "Afghanistan",
               "Bhutan",
               "Cyprus",
               "Georgia",
               "India",
               "Iran (Islamic Republic of)",
               "Israel",
               "Iraq",
               "Japan",
               "Jordan",
               "Kyrgyzstan",
               "Korea, Democratic People's Republic of",
               "Korea, Republic of",
               "Kuwait",
               "Kazakhstan",
               "Lao People's Democratic Republic",
               "Lebanon",
               "Mongolia",
               "Oman",
               "Maldives",
               "Malaysia",
               "Hong Kong",
               "Macau",
               "Palestine",
               "Nepal",
               "Pakistan",
               "Qatar",
               "Philippines",
               "Saudi Arabia",
               "Singapore",
               "Syrian Arab Republic",
               "Thailand",
               "Tajikistan",
               "Turkey",
               "Turkmenistan",
               "Uzbekistan",
               "Viet Nam",
               "Yemen",
               "Indonesia",
               "United Arab Emirates",
               "Timor-Leste")

#Creating my own theme 
theme_may <- theme( panel.grid.major= element_blank(),
                    panel.grid.minor=element_blank(),
                    plot.background=element_rect(fill="#F5F5F5"),
                    panel.background = element_rect(fill=c("#FFFAFA", "#F5FFFA")),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.line.x = element_line(size=0.1),
                    plot.title = element_text(color="#800080", size=15))

popdat2019_final <- popdat2019%>%
  filter(Country%in%Countries)

popdat2019_final <- popdat2019_final%>%
  arrange(Country)

ggplot(popdat2019_final, aes(x=Country, y=Growth_Rate)) + 
  geom_col() + ylim(-2, 5) +
  geom_text(data= popdat2019_final[popdat2019_final$Country == "Oman",], 
                                      mapping=aes(label="Oman(3.01)"), 
                                      hjust=0.4, vjust=-0.3, size=2.3, color="#228B22") +
  geom_text(data= popdat2019_final[popdat2019_final$Country == "Bahrain",], 
            mapping=aes(label="Bahrain(4.57)"),
            hjust=0.4, vjust=-0.1, size=2.3, color="#228B22")+
  geom_text(data= popdat2019_final[popdat2019_final$Country == "Japan",], 
            mapping=aes(label="Japan(-0.27)"),
            hjust=0.4, vjust=1, size=2.3, color="#DC143C")+
  geom_text(data= popdat2019_final[popdat2019_final$Country == "Georgia",], 
            mapping=aes(label="Georgia(-0.15)"),
            hjust=0.4, vjust=1, size=2.3, color="#DC143C") +
  theme_may+
  labs(title="Population Growth Rate(%) of Asian Countries",
       y="Growth Rate(%)")

 


