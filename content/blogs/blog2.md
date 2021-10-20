---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
image: pic09.jpg
keywords: ""
slug: magna
title: Magna
---

We have three data sources as below:

Vaccination by county from the CDC
2020 Election Votes from County Presidential Election Returns 2000-2020
Estimate of the population of each county

There are 277 counties, mainly from Texas, that show a 0% vaccination at the latest point in time. Looking at them in more detail (see above), we see that there are no values for the other variables. This indicates that there is no vaccination data for these counties available, which is why we will remove them from the dataset.

First we need to process the original dataset a little bit.

vaccinations_latest <- vaccinations %>% 
  filter(date == "09/20/2021") %>% #filter for most recent data entries and those for which vaccination rate available
  mutate(pct_vaccinated = case_when(
    recip_state %in% c("CA", "GA", "IA", "MI", "TX") ~ 
      administered_dose1_pop_pct, #use data for doses administered for certain states
    T ~ series_complete_pop_pct)) %>% 
  filter(pct_vaccinated != 0) %>% #drop observations with 0% vaccination rates
  select(fips, pct_vaccinated) #select only those columns that necessary for graph

election2020_results_Trump <- election2020_results %>% #create new dataframe that sums Trump votes from duplicates
  filter(candidate == "DONALD J TRUMP", mode=="TOTAL") %>% 
  mutate(Trump_perc = candidatevotes/totalvotes) %>% #calculate %age of Trump votes per county
  select(state_po, county_name, fips, candidate, Trump_perc) #last variable to calculate share of Trump votes

chart_data <- election2020_results_Trump %>% #join dataframes
  left_join(population,
            by = "fips") %>% 
  left_join(vaccinations_latest) %>% 
  #mutate(trump_maj = ifelse(Trump_perc >= 0.5, "Yes","No")) #create variable to color graph later
  mutate(pct_vaccinated = pct_vaccinated/100) %>% 
  distinct(fips, .keep_all = TRUE) #remove 1 duplicate


We attempt to calculate the total percentage of people vaccinated. However, our result for September shows a number that is less than the one shown from the graph in April which is why we will not add the horizontal line showing the actual ratio of people vaccinated in the graph.

#calculate %age of entire population vaccinated for horizontal line in chart
get_total_pct <- chart_data %>% 
  mutate(pop_vaccinated = pct_vaccinated*pop_estimate_2019) %>% 
  select(pop_vaccinated, pop_estimate_2019) %>% 
  colSums(na.rm = TRUE)

get_total_pct[1]/get_total_pct[2] #<50.8% shown in the graph from April
## pop_vaccinated 
##          0.451
Then we can reproduce the graph.

ggplot(chart_data,
       aes(x = Trump_perc,
           y = pct_vaccinated,
           size = pop_estimate_2019)) + #draw points based on size of population of county
  geom_point(color = "darkblue", 
             alpha = 0.4,
             na.rm = TRUE) +
  scale_size_continuous(range = c(1, 40)) + #specify scale for size of population-based points
  geom_point(size = 0.75) + #add points to show vaccination level per county
  geom_smooth(method = "lm", #add regression line
              se = FALSE,
              size=0.5) +
  annotate("rect", #change background color
           xmin = 0,
           xmax = 0.55,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.3,
           fill = "blue") +
  annotate("rect", #change background color
           xmin = 0.45,
           xmax = 1,
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.3,
           fill = "red") +
  labs(title = "COVID-19 VACCINATION LEVELS OUT OF TOTAL POPULATION PER COUNTY",
       subtitle = "(most states based on FULLY vaccinated only; CA, IA, GA, MI & TX, based on total doses administered)",
       y = "% of Total Population Vaccinated",
       x = "2020 Trump Vote %") +
  geom_hline(yintercept = 0.85, #add horizontal line
             linetype = "dashed")+
  annotate("text",
           x = 0.12,
           y = 0.865,
           label = "Herd immunity threshold (?)",
           size = 3,
           color = "blue",
           fontface = 2)+
  #geom_hline(yintercept = 0.508,
  #           linetype = "dashed")+ #no value for actual %age added
  #annotate("text",x=0.0528, y=0.534,label="ACTUAL: 50.8%",size=3, color="blue",fontface=2)+ 
  geom_hline(yintercept = 0.539, #add horizontal line
             linetype = "dashed")+
  annotate("text",
           x = 0.07,
           y = 0.549,
           label = "TARGET: 53.9%",
           size = 3,
           color = "blue",
           fontface = 2)+
  theme_bw() +
  scale_x_continuous(minor_breaks = seq(0, 1, 0.05), #change gridlines
                     breaks = seq(0, 1.05, by = 0.05),
                     expand=c(0,0), limits=c(0,1.01),
                     labels = scales::percent_format(accuracy = 5L)) + #change scale to %
  scale_y_continuous(minor_breaks = seq(0, 1, 0.05), #change gridlines
                     breaks = seq(0, 1.05, by = 0.05),
                     expand=c(0,0), limits=c(0,1.02),
                     labels = scales::percent_format(accuracy = 5L)) + #change scale to %
  theme(legend.position = "none") + #remove legend
  NULL