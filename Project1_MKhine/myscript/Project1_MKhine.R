library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)

##For this project, I am using the data from the 35th Congress (1857-1859) and the 116th Congress (2019-2021).
## According to VoteView, the 35th Congress was the first time the Republican Party was part of the Congress.
## I want to compare the number of democrats and republicans from the 35th Congress and 116th Congress.
## I also want to see how the First Dimension Estimates (nominate_dim1) compare within the two parties among the different periods.
#The first step would be to import the two data scripts, which is in the data folder, to R. 

library(readr)
HS035_members <- read_csv("~/Downloads/Project1_MKhine/data/HS035_members.csv")
View(HS035_members)

library(readr)
HS035_members <- read_csv("~/Downloads/Project1_MKhine/data/HS116_members.csv")
View(HS116_members)

Thirtyfifth <- HS035_members%>%
  select(party_code, bioname, nominate_dim1)
Thirtyfifth$Congress <- "35th Congress"; Thirtyfifth

Onehundredthsixteenth <- HS116_members%>%
  select(party_code, bioname, nominate_dim1)
Onehundredthsixteenth$Congress <- "116th Congress"; Onehundredthsixteenth

Combined <- rbind(Thirtyfifth, Onehundredthsixteenth); Combined

#Creating my own theme
theme_may <- theme( panel.grid.major= element_blank(),
                    panel.grid.minor=element_blank(),
                    plot.background=element_rect(fill="#FFFFFF"),
                    panel.background = element_rect(fill=c("#FFFAFA", "#F5FFFA")),
                    legend.position= "bottom",
                    axis.text= element_text(color="#191970", size=10),
                    axis.line.x = element_line(size=0.1),
                    legend.text = element_text(color="black", size=10),
                    legend.title=element_text(color="black", size=10),
                    plot.title = element_text(color="#191970", size=15))

Combined%>%
  filter(party_code==c(100, 200))%>%
  ggplot(., aes(x=bioname, y=nominate_dim1, color=factor(party_code))) +
  geom_point(position="jitter", alpha=0.7) +
  facet_wrap(~factor(Congress, ordered=TRUE, levels=c("35th Congress", "116th Congress")), scales="free") + 
  ylim(-1,1) +
  theme(axis.text.x = element_blank()) +
  theme_may +
  geom_text(data= Combined[Combined$bioname == "McQUEEN, John",], 
            mapping=aes(label="Mcqueen, John"), 
            hjust=1.1, vjust=0.4, size=2.3) + 
  geom_text(data= Combined[Combined$bioname == "MURPHY, Gregory Francis",], 
            mapping=aes(label="Murphy, Gregory"), 
            hjust=1.3, vjust=2, size=2.3) +
  #Adding labels for two specific points in the plot. hjust and vjust aligns the position of the text.
  labs(title="Distribution of Democrats and Republicans vs First Dimension Estimates",
       x="Party Members", 
       y="First Dimension Estimate") +
  scale_color_manual(name="Party", values=c("#FF7F50", "#4682B4"), 
                     labels=c("Democratic Party", "Republican Party")) +
  geom_smooth() + 
  geom_segment(aes(x=-Inf, y=0, xend=Inf, yend=0), color="black", alpha=0.2)	

##Obviously, there were more democrats and republicans in the 116th Congress. In this period, all of the demo-
##crats have First Dimension Estimates below 0 while the republicans have First Dimension Estimates of over 0.
