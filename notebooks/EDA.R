# Load in libraries
library(tidyverse)
library(gridExtra)
library(usmap)
library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r") #for correlation matrix

# Load data, some fnctions use `data', some use `covid'
# We can organize this later, should rename everything to one name
data <- read_rds("../data/processed/main_state_data.RDS")
covid <- data


#### Workplace mobility as stand-in for “mobility” in general ####

# Workplace mobility vs transit
workplace_transit_scatter <- ggplot(data = data, aes(x=workplaces_2020.10.10, y=transit_2020.10.10 )) +
  geom_point()

ggsave(plot = workplace_transit_scatter,  filename = "../reports/figures/workplace_transit_scatter.png")

# Workplace vs retail/recreation
workplace_retrec_scatter <- ggplot(data = data, aes(x=workplaces_2020.10.10, y=retail_rec_2020.10.10 )) +
  geom_point()

ggsave(plot = workplace_retrec_scatter,  filename = "../reports/figures/workplace_retrec_scatter.png")


#### Mobility as Predictor ####

workplace_cases_scatter <- ggplot(data = data, aes(x=workplaces_2020.10.10, y=Case.Rate.per.100000.in.Last.7.Days )) +
  geom_point()

ggsave(plot = workplace_cases_scatter,  filename = "../reports/figures/workplace_cases_scatter.png")

workplace_logcases_scatter <-  ggplot(data = data, aes(x=workplaces_2020.10.10, y=log(Case.Rate.per.100000.in.Last.7.Days) )) +
  geom_point()+
  xlab("Workplace Mobility for the week of 10/10/20") +
  ylab("Log Case Rate from 10/24/20 to 10/30/20")

ggsave(plot = workplace_logcases_scatter,  filename = "../reports/figures/workplace_logcases_scatter.png")

cor(data$workplaces_2020.10.10, log(data$Case.Rate.per.100000.in.Last.7.Days))


# Workplace vs cases, grouped by SIP indicator
workplace_cases_SIP <- ggplot(data = data, aes(x=workplaces_2020.10.10, y = Case.Rate.per.100000.in.Last.7.Days, color = as.factor(SIP), fill = as.factor(SIP))) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  xlab('Workplace Mobility 10/10-10/17') +
  ylab('Case Rates per 100K 10/23-10/30') +
  scale_fill_discrete(name = "Instituted SIP", labels = c("No", "Yes")) +
  guides(color = F)

ggsave(plot = workplace_cases_SIP,  filename = "../reports/figures/Mob_Cases_SIP_plot.png")


#### Maps ####

# No face mask map for EDA

nofacemaskMap <- plot_usmap(data = covid, values = "NoFaceMask_factor", color = "black", labels = F) + 
  scale_fill_manual(values = c(`0` = "grey", `1` = "dimgrey"), name = "NoFaceMask")+ 
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))

ggsave(filename = "../reports/figures/NoFaceMaskMap.png", width = 6.5, height = 4.5, units = "in", plot = nofacemaskMap)


# SIP map for EDA

SIPMap <- plot_usmap(data = covid, values = "SIP_factor", color = "black", labels = F) + 
  scale_fill_manual(values = c(`0` = "dimgrey", `1` = "grey"), name = "SIP") + 
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
SIPMap
ggsave(filename = "../reports/figures/SIP_map.png", width = 6.5, height = 4.5, units = "in", plot = SIPMap)

class(covid$NoFaceMask)

##### Mobility ######
mobility <- data[,c('workplaces_2020.10.10', 'transit_2020.10.10', 'parks_2020.10.10', 'residential_2020.10.10', 'groc_pharm_2020.10.10', 'retail_rec_2020.10.10')]

mobility <- mobility %>% 
  rename(
    'workplaces' = 'workplaces_2020.10.10',
    'transit' = 'transit_2020.10.10',
    'parks' = 'parks_2020.10.10',
    'residential' = 'residential_2020.10.10',
    'groc_pharm' = 'groc_pharm_2020.10.10',
    'retail_rec'= 'retail_rec_2020.10.10' ,
  )

png(filename = "../reports/figures/mobility_corr.png",  units="in", width=5, height=5, res=200)
rquery.cormat(mobility)
# dev.off() to save the file and return to control screen
dev.off()

# I don't think ggsave knows how to save this type of image? -Josh
#ggsave(filename = "../reports/figures/mobility_corr", width = 6.5, height = 4.5, units = "in", plot = mobility_corr)


# No employee face mask map for EDA
noEmpfacemaskMap <- plot_usmap(data = covid, values = "NoFaceMaskEmploy_factor", color = "black", labels = F) + 
  scale_fill_manual(values = c(`0` = "grey", `1` = "dimgrey"), name = "NoFaceMask")+ 
  theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))

ggsave(filename = "../reports/figures/NoEmpFaceMaskMap.png", width = 6.5, height = 4.5, units = "in", plot = noEmpfacemaskMap)

#### Case Rate per 100K in the last 7 Days: ####

rate_hist <- ggplot(covid, aes(x=`Case.Rate.per.100000.in.Last.7.Days`)) +
  geom_histogram() +
  ggtitle("") +
  xlab("Case Rate Per 100K from Oct. 24th- Oct. 30th")

ggsave(filename = "../reports/figures/case_rate_sevendays_hist.png")

log_rate_hist <- ggplot(covid, aes(x=log(`Case.Rate.per.100000.in.Last.7.Days`))) +
  geom_histogram() +
  ggtitle("") +
  xlab("Log Case Rate Per 100K from Oct. 24th- Oct. 30th")

ggsave(filename = "../reports/figures/log_case_rate_sevendays_hist.png")






