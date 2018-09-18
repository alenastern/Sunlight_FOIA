library(plm)
library(psych)
library(magrittr)
library(plyr)
library(dplyr)
library(Hmisc)
library(corrplot)
library(lmtest)
library(treemap)
library(RColorBrewer)

#remove items, set working directory, read in data
remove(list = ls())
setwd("~/Documents/Sunlight/Sunlight_FOIA/src")
data <- read.csv("data/clean_data.csv", header = TRUE, sep= ",", fill = TRUE)
pbc <- read.csv("analysis/pbc.csv", header = TRUE, sep= ",", fill = TRUE)
pd_gb <- read.csv("analysis/topics/norm_pop_2_dt_sub_pol_Greensboro.csv", header = TRUE, sep = ",")
data$pop_10000 <- data$population/10000
data$count_p10000 <- data$count/data$pop_10000
data$month_year <- as.Date(data$month_year)

data_yr_pol <- data %>% group_by(year, policy)
data_yp <- data_yr_pol %>% summarise(
count = mean(count),
count_p10000 = mean(count_p10000)
)

6.48/((24.995 + 18.332)/2)
7.15/((24.995 + 18.332)/2)
9.23/((24.995 + 18.332)/2)
# 2018 interaction term coefficient over 2018 control group avg
15.3/24.995
# 2017 interaction term coefficient over 2017 control group avg
7.12/18.332

# Difference of Means Analysis
by_city <- data %>% group_by(city_x)
data_city <- by_city %>% summarise(
  policy = mean(policy),
  portal = mean(portal),
  robust_policy = mean(robust_policy),
  robust_portal = mean(robust_portal),
  population = mean(population),
  median_age = mean(median_age),
  median_gross_rent = mean(median_gross_rent),
  median_income = mean(median_income),
  pct_25_34 = mean(pct_25_34),
  pct_male = mean(pct_male),
  pct_black = mean(pct_black),
  pct_white = mean(pct_white),
  pct_bachelor = mean(pct_bachelor),
  mayor_council = mean(mayor_council),
  council_manager = mean(council_manager),
  dem = mean(dem),
  rep = mean(rep)
  )

data_city$treatment <- ifelse(data_city$policy > 0, 1, 0)

by_treat <- data_city %>% group_by(treatment)
data_treat <- by_treat %>% summarise(
  population = mean(population),
  median_age = mean(median_age),
  median_gross_rent = mean(median_gross_rent),
  median_income = mean(median_income),
  pct_25_34 = mean(pct_25_34),
  pct_male = mean(pct_male),
  pct_black = mean(pct_black),
  pct_white = mean(pct_white),
  pct_bachelor = mean(pct_bachelor),
  mayor_council = mean(mayor_council),
  council_manager = mean(council_manager),
  dem = mean(dem),
  rep = mean(rep)
)

data_treat_t = t(data_treat)
write.csv(data_treat_t, file = "diff_means.csv")

by_mc <- data_city %>% group_by(mayor_council)
data_mc <- by_mc %>% summarise(
  population = mean(population),
  median_age = mean(median_age),
  median_gross_rent = mean(median_gross_rent),
  median_income = mean(median_income),
  pct_25_34 = mean(pct_25_34),
  pct_male = mean(pct_male),
  pct_black = mean(pct_black),
  pct_white = mean(pct_white),
  pct_bachelor = mean(pct_bachelor),
  dem = mean(dem),
  rep = mean(rep)
)

data_mc_t = t(data_mc)
write.csv(data_mc_t, file = "diff_means_mayor_council.csv")

##########################
## PRR Volume Over Time ##
##########################


data_treat <- data[which(data$treatment_pol == 1), ]
data_control <- data[which(data$treatment_pol == 0), ]

# trim the largest and the smallest cities for robustness check of trends
data_trim = data[which(data$city_x != 'San Francisco city' & data$city_x != 'Washington city'  & data$city_x != 'Fort Worth city'
                       & data$city_x != 'Cathedral City city'  & data$city_x != 'Winchester city' & data$city_x != 'Arlington city' 
                       & data$city_x != 'Middleborough town' & data$month_year >= as.Date("1jan2011", "%d%b%Y")), ]
data_tt <- data[which(data_trim$treatment_pol == 1), ]
data_tcl <- data[which(data_trim$treatment_pol == 0), ]

data_trim_treat <- data_trim %>% group_by(treatment_pol)
dtt_avg <- data_trim_treat %>% summarise(
  ps = mean(ps),
  population = mean(population),
  count = mean(count),
  count_p10000 = mean(count_p10000),
  months = mean(months)
)


# 1) Overall trend over time plot
by_date_t <- data_treat %>% group_by(month_year)
my_treat <- by_date_t %>% summarise(
  count_t = mean(count),
  count_p10000_t = mean(count_p10000)
)

by_date_c <- data_control %>% group_by(month_year)
my_control <- by_date_c %>% summarise(
  count_c = mean(count),
  count_p10000_c = mean(count_p10000)
)

my = merge(my_control, my_treat, by = "month_year", all = TRUE)
my$month_year <- as.Date(my$month_year)

ggplot(my, aes(month_year, group =1)) + 
  geom_line(aes(y = count_p10000_t, col = "treatment")) + 
  geom_smooth(aes(y = count_p10000_t)) +
  geom_line(aes(y = count_p10000_c, col = "control")) +
  geom_smooth(aes(y = count_p10000_c)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  labs(x= "Month, Year", y= "PRRs per 10,000 residents", title = "Figure 1: Average PRR Volume Over \nTime for Treatment and Control Cities")

ggsave("analysis/plots/volume_trend.png")

#1a) Overall trend over time plot trim

by_date_t_trim <- data_tt %>% group_by(month_year)
my_treat_trim <- by_date_t_trim %>% summarise(
  count_t = mean(count),
  count_p10000_t = mean(count_p10000)
)

by_date_c_trim <- data_tcl %>% group_by(month_year)
my_control_trim <- by_date_c_trim %>% summarise(
  count_c = mean(count),
  count_p10000_c = mean(count_p10000)
)

my_trim = merge(my_control_trim, my_treat_trim, by = "month_year", all = TRUE)
my_trim$month_year <- as.Date(my_trim$month_year)

ggplot(my_trim, aes(month_year, group =1)) + 
  geom_line(aes(y = count_p10000_t, colour = "treatment")) + 
  geom_smooth(aes(y = count_p10000_t)) +
  geom_line(aes(y = count_p10000_c, colour = "control")) +
  geom_smooth(aes(y = count_p10000_c)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x= "Month, Year", y= "PRRs per 10,000 residents", title = "Average PRR Volume Over Time for \nTrimmed Treatment and Control Cities")


ggsave("analysis/plots/volume_trend_trim.png")

## 2. City Plots

pp_cities <- vector(mode="list", length=8)
names(pp_cities) <- c('Fort Collins city', 'Fort Worth city', 'Greensboro city', 'New Orleans city',
                      'Oakland city', 'Riverside city', 'Salt Lake City city')
pp_cities[[1]] <- '07feb2017'; pp_cities[[2]] <- '14oct2016'; pp_cities[[3]] <- '17jan2018'; pp_cities[[4]] <-'07oct2016'; pp_cities[[5]] <- '01sep2016'; pp_cities[[6]] <- '15oct2013'; pp_cities[[7]] <- '07mar2017'; pp_cities[[8]] <-'02jun2014'

for(city in names(pp_cities)){
  city_data <- data[which(data$city_x == city), ]
  
  my_trim = merge(my_control_trim, city_data, by = "month_year", all = TRUE)
  my_trim$month_year <- as.Date(my_trim$month_year)
  
  ggplot(my_trim, aes(x=month_year, group =1)) + 
    geom_line(aes(y = count_p10000, colour = city)) + 
    geom_smooth(aes(y = count_p10000)) +
    geom_line(aes(y = count_p10000_c, colour = "Control Avg")) +
    geom_smooth(aes(y = count_p10000_c)) +
    scale_colour_manual(values=c("goldenrod3","dodgerblue4")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_vline(xintercept= as.Date(pp_cities[[city]], "%d%b%Y")) +
    labs(x= "Year", y= "PRRs per 10,000 residents", title = city)
  
  file <- paste("analysis/plots/trim_", city, ".png", sep = '')
  ggsave(file)
  
  my = merge(my_control, city_data, by = "month_year", all = TRUE)
  my$month_year <- as.Date(my$month_year)
  
  ggplot(my, aes(x=month_year,  group =1)) + 
    geom_line(aes(y = count_p10000, colour = city)) + 
    geom_smooth(aes(y = count_p10000)) +
    geom_line(aes(y = count_p10000_c, col = "Control Avg")) +
    geom_smooth(aes(y = count_p10000_c)) +
    scale_colour_manual(values=c("goldenrod3","dodgerblue4")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_vline(xintercept= as.Date(pp_cities[[city]], "%d%b%Y")) +
    labs(x= "Year", y= "PRRs per 10,000 residents", title = city)
  
  file <- paste("analysis/plots/", city, ".png", sep = '')
  ggsave(file)
  
}

############################
## Treeplot of Data Types ##
############################

topics <- read.csv("analysis/topics/norm_pop_2_dt_prop_final.csv", header = TRUE, sep = ",", fill = TRUE)
topics$data_type[topics$data_type == "Employee Benefits, Payroll"] <- "Employee benefits, payroll"
topics$data_type[topics$data_type == "Crime Photo and Video"] <- "Crime photo and video"
topics$data_category <- "Uncategorized"
topics$data_category[topics$data_type == "Parcel records, permits, plans" | topics$data_type == "Property liens" | topics$data_type == "Building code violations" ] <- "Property"
topics$data_category[topics$data_type == "Police incident report" | topics$data_type == "Criminal record check" | topics$data_type == "Auto collision report" | topics$data_type == "Crime photo and video" | topics$data_type == "911/law enforcement service calls" | topics$data_type == "Witness statements"] <- "Crime and public safety"
topics$data_category[topics$data_type == "Environmental assessment, hazardous materials"] <- "Environment"
topics$data_category[topics$data_type == "Purchasing records, contracts" | topics$data_type == "Employee benefits, payroll" | topics$data_type == "Checks and deposits"] <- "Spending"
topics$data_category[topics$data_type == "City emails, social media posts" | topics$data_type == "City government meeting notes" | topics$data_type == "Complaints to city" ] <- "Government"
topics$data_category[topics$data_type == "Public works, utilities"] <- "Public works"
topics$data_category[topics$data_type == "Human services cases"] <- "Human services"
topics$data_category <- as.factor(topics$data_category)

                     
treemap(topics, #Your data frame object
        index=c("data_category","data_type"),  
        vSize = "total_score", 
        type="index", 
        title="Public Record Requests by Category",
        border.col = "white",
        par(lheight=.1),
        fontsize.title = 14,
        fontsize.labels=c(12,10),                
        fontcolor.labels=c("white","black"),   
        fontface.labels=c(2,1),                  
        bg.labels=c("transparent"),  
        xmod.labels = .007,
        force.print.labels = TRUE, 
        align.labels=list(
          c("left", "top"),
          c("right", "bottom")
        ),                                  
        overlap.labels=0.5,                    
        inflate.labels=F                        
        
)

ggsave("analysis/plots/911_diverge.png")

##########################
## Diverging Bar Charts ##
##########################

## Difference from average - Accident Report for Insurance Claims ##

# Data Prep
pbc_41 <- pbc[which(pbc$topic == 41), ]
pbc_41 <- pbc_41[complete.cases(pbc_41), ]
pbc_41$pct_z <- round((pbc_41$pct - mean(pbc_41$pct))/sd(pbc_41$pct), 2)  # compute normalized mpg
pbc_41$pct_type <- ifelse(pbc_41$pct_z < 0, "below", "above")  # above / below avg flag
pbc_41 <- pbc_41[order(pbc_41$pct_z), ]  # sort
pbc_41$city <- factor(pbc_41$city, levels = pbc_41$city)

# Diverging Barcharts
ggplot(pbc_41, aes(x=city, y=pct_z, label=pct_z)) + 
  geom_bar(stat='identity', aes(fill= pct_type), width=.5)  +
  labs( x = "City", y = "Standard Deviations from Mean Percentage", 
        title ="Figure 3: Accident Reports for Insurance Claims") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(name = "", labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d"))  + 
  coord_flip()
ggsave("analysis/plots/insurance_claim_diverge.png")

# Data Prep- Parcel
pbc_gp <- pbc %>% group_by(data_type, city)
pbc_group <- pbc_gp %>% summarise(
  pct = sum(pct),
  total_pop = sum(total_pop)
)
pbc_parcel <- pbc_group[which(pbc_group$data_type == "Parcel records, permits, plans"), ]
pbc_parcel <- pbc_parcel[complete.cases(pbc_parcel), ]
pbc_parcel$pct_z <- round((pbc_parcel$pct - mean(pbc_parcel$pct))/sd(pbc_parcel$pct), 2)  # compute normalized mpg
pbc_parcel$pct_type <- ifelse(pbc_parcel$pct_z < 0, "below", "above")  # above / below avg flag
pbc_parcel <- pbc_parcel[order(pbc_parcel$pct_z), ]  # sort
pbc_parcel$city <- factor(pbc_parcel$city, levels = pbc_parcel$city)

# Diverging Barcharts
ggplot(pbc_parcel, aes(x=city, y=pct_z, label=pct_z)) + 
  geom_bar(stat='identity', aes(fill= pct_type), width=.5)  +
  scale_fill_manual(name="Parcel Records, Permits, Plans", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d"))  + 
  coord_flip()

ggsave("analysis/plots/parcel_diverge.png")

# Data Prep- Police Incident Report
pbc_police <- pbc_group[which(pbc_group$data_type == "Police incident report"), ]
pbc_police <- pbc_police[complete.cases(pbc_police), ]
pbc_police$pct_z <- round((pbc_police$pct - mean(pbc_police$pct))/sd(pbc_police$pct), 2)  # compute normalized mpg
pbc_police$pct_type <- ifelse(pbc_police$pct_z < 0, "below", "above")  # above / below avg flag
pbc_police <- pbc_police[order(pbc_police$pct_z), ]  # sort
pbc_police$city <- factor(pbc_police$city, levels = pbc_police$city)

# Diverging Barcharts
ggplot(pbc_police, aes(x=city, y=pct_z, label=pct_z)) + 
  geom_bar(stat='identity', aes(fill= pct_type), width=.5)  +
  scale_fill_manual(name="Police Incident Report", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d"))  + 
  coord_flip()

ggsave("analysis/plots/police_report_diverge.png")

# Data Prep- City Emails
pbc_email <- pbc_group[which(pbc_group$data_type == "City emails, social media posts"), ]
pbc_email <- pbc_email[complete.cases(pbc_email), ]
pbc_email$pct_z <- round((pbc_email$pct - mean(pbc_email$pct))/sd(pbc_email$pct), 2)  # compute normalized mpg
pbc_email$pct_type <- ifelse(pbc_email$pct_z < 0, "below", "above")  # above / below avg flag
pbc_email <- pbc_email[order(pbc_email$pct_z), ]  # sort
pbc_email$city <- factor(pbc_email$city, levels = pbc_email$city)

# Diverging Barcharts
ggplot(pbc_email, aes(x=city, y=pct_z, label=pct_z)) + 
  geom_bar(stat='identity', aes(fill= pct_type), width=.5)  +
  scale_fill_manual(name="City Email, Social Media", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d"))  + 
  coord_flip()

ggsave("analysis/plots/emails_diverge.png")

# Data Prep- Purchasing Records
pbc_purchasing <- pbc_group[which(pbc_group$data_type == "Purchasing records, contracts"), ]
pbc_purchasing <- pbc_purchasing[complete.cases(pbc_purchasing), ]
pbc_purchasing$pct_z <- round((pbc_purchasing$pct - mean(pbc_purchasing$pct))/sd(pbc_purchasing$pct), 2)  # compute normalized mpg
pbc_purchasing$pct_type <- ifelse(pbc_purchasing$pct_z < 0, "below", "above")  # above / below avg flag
pbc_purchasing <- pbc_purchasing[order(pbc_purchasing$pct_z), ]  # sort
pbc_purchasing$city <- factor(pbc_purchasing$city, levels = pbc_purchasing$city)

# Diverging Barcharts
ggplot(pbc_purchasing, aes(x=city, y=pct_z, label=pct_z)) + 
  geom_bar(stat='identity', aes(fill= pct_type), width=.5)  +
  scale_fill_manual(name="Purchasing Records, Contracts", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d"))  + 
  coord_flip()

ggsave("analysis/plots/purchasing_diverge.png")

# Data Prep- 911 Calls
pbc_911 <- pbc_group[which(pbc_group$data_type == "911/law enforcement service calls"), ]
pbc_911 <- pbc_911[complete.cases(pbc_911), ]
pbc_911$pct_z <- round((pbc_911$pct - mean(pbc_911$pct))/sd(pbc_911$pct), 2)  # compute normalized mpg
pbc_911$pct_type <- ifelse(pbc_911$pct_z < 0, "below", "above")  # above / below avg flag
pbc_911 <- pbc_911[order(pbc_911$pct_z), ]  # sort
pbc_911$city <- factor(pbc_911$city, levels = pbc_911$city)

# Diverging Barcharts
ggplot(pbc_911, aes(x=city, y=pct_z, label=pct_z)) + 
  geom_bar(stat='identity', aes(fill= pct_type), width=.5)  +
  scale_fill_manual(name="911/Law Enforcement Service Calls", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d"))  + 
  coord_flip()

ggsave("analysis/plots/911_diverge.png")

#########################
## Greensboro Analysis ##
#########################

## Top Topics In Policy and Non-Policy Months - Greensboro ##

topic_0 <- pd_gb[ which(pd_gb$policy == 0), ]
topic_1 <- pd_gb[ which(pd_gb$policy == 1), ]

topic_0$pct_0 <- (topic_0$total_score/sum(topic_0$total_score))*100
topic_1$pct_1 <- (topic_1$total_score/sum(topic_1$total_score))*100
top_all_gb <- merge(topic_1, topic_0, by = "data_type", all= TRUE)
top_all_gb$difference <- top_all_gb$pct_1 - top_all_gb$pct_0
write.csv(top_all_gb, file = "analysis/topics/difference_in_topics_gb.csv")

## Line plot of City Emails, Social Media ##
gb <- final_data[ which(final_data$city == 'Greensboro'),]
gb$policy <- ifelse(gb$month_year < ymd('2016-10-01'), 0, 1)
gb$quarter <- quarter(gb$month_year, with_year = TRUE)
gb <- gb[which(gb$month_year != ymd('2018-06-01')),]

gb_email <- gb[ which(gb$data_type == 'City emails, social media posts'), ]

gb_email_q <- gb_email %>% group_by(quarter)
gb_eq <- gb_email_q %>% summarise(
  top_topic_comp = sum(top_topic_comp)
)

gb_q <- gb %>% group_by(quarter)
gb_quarter<- gb_q %>% summarise(
  top_topic_comp = sum(top_topic_comp)
)

gb_email_plot <- merge(gb_eq, gb_quarter, by = "quarter", all = TRUE)
gb_email_plot$pct <- (gb_email_plot$top_topic_comp.x/gb_email_plot$top_topic_comp.y)*100

# plot
gb_dates <- data.frame("dates" = c(ymd('2016-10-01'), ymd('2017-01-12')), "text" = c("\nopen data policy launch","\nfirst publish email data"))

ggplot(gb_email_plot, aes(x=quarter), group = 1) + 
  geom_line(aes(y=pct)) + 
  labs(title="Figure 4: Percent of Quarterly Requests for \nCity Emails, Social Media Posts Over Time", x = "Quarter", y = "Percent of Total Requests") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(data = gb_dates, aes(xintercept= quarter(dates, with_year = TRUE))) +
  geom_text(data = gb_dates, mapping = aes(label = text, x = quarter(dates, with_year = TRUE), y = 14), colour="blue", angle=90, size = 3) +
  
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid 

ggsave("analysis/plots/Greensboro_Emails.png")
