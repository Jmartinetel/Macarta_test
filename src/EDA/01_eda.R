# install.packages('pacman')


## Installing packages

pacman::p_load(tidyverse, dplyr, lubridate, ggplot2, scales, ggpubr, patchwork,
               corrplot)




# Import data -------------------------------------------------------------

## We import the data and transform the date column to date
data <- 
  read.csv("cache/prep_data.csv",nrows = F) %>% 
  mutate(date = as.Date(date))



# check -------------------------------------------------------------------

data %>% 
  summary()



# Analysis --------------------------------------------------------


#############################################################3
## Let's start analyzing the behavior of sales against the
## `lead_n` column.
##
## We will focus our main analysis on the `lead_n` variable,
## as it allows us to identify if a campaign is performing well.
#########################################################

data %>% 
  names()


## Create a monthly data, and applied some trasnformations to the variables
monthly_data <- 
  data %>% 
  mutate(date_monthly = as.Date(paste0(year(date), "-", month(date), "-" , "01"))) %>% 
  group_by(date_monthly) %>% 
  summarise(ggsearch = sum(ggsearch), 
            facebook = sum(facebook),
            display = sum(display), 
            promotion = sum(promotion),
            leads_n = sum(leads_n), 
            purchase_leads = sum(purchase_leads),
            purchase_leads_fur = sum(purchase_leads_fur),
            time_to_purchase = mean(ifelse(time_to_purchase== 0, NA, time_to_purchase), na.rm = T),
            days_considered = mean(ifelse(days_considered== 0, NA, days_considered), na.rm = T),
            days_purchase = mean(ifelse(days_purchase== 0, NA, days_purchase), na.rm = T)
            
            ) 

monthly_data %>% summary()



#############################################################3
## First, we are going to plot share-of-market line plots.
## The goal is to identify if there is a clear trend or change.
##
#########################################################
monthly_data %>% 

  mutate(total = ggsearch+ facebook+ display) %>% 
  mutate(share_ggsearch = ggsearch/total, 
         share_facebook = facebook/total, 
         share_display = display/total,
         leads_scale = rescale(leads_n),
         rate_roi= purchase_leads/leads_n
    
  ) %>% 
  ggplot(aes( x = date_monthly)) +
  geom_col(aes( y = leads_scale), alpha = 0.5) +
  geom_col(aes( y = rate_roi), alpha = 1, fill = "#bb33ff") +
  
  geom_line(aes(y =share_ggsearch ,  color = "GGsearch"), linewidth = 2) +
  geom_line(aes(y =share_facebook , color = "Facebook"),linewidth = 2) +
  geom_line(aes(y =share_display, color = "Display"),linewidth = 2)+ 
  
  scale_color_manual(values = c("GGsearch"="#ff5733", 
                                    'Facebook'='#33c0ff',
                                    'Display'= '#fff133'))+
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  labs( title = "Share of market Time series", 
        subtitle = "Leads count",
        x = "", 
        y = "Daily Spending")


ggsave("graphs/principal/share.png", width = 20, height = 13, units = "cm")
## Is not a clear beahivor in this share plot


#############################################################3
## We are going to rescale the data to compare if there is
## a trend in the media against the leads.
##
#########################################################


monthly_data %>% 
  
  mutate(total = ggsearch+ facebook+ display) %>% 
  mutate(rescale_ggsearch = rescale(ggsearch), 
         rescale_facebook = rescale(facebook), 
         rescale_display = rescale(display),
         leads_scale = rescale(leads_n),
         rate_roi= purchase_leads/leads_n
         
  ) %>% 
  ggplot(aes( x = date_monthly)) +
  geom_col(aes( y = leads_scale), alpha = 0.5) +
  geom_col(aes( y = rate_roi), alpha = 1, fill = "#bb33ff") +
  
  geom_line(aes(y =rescale_ggsearch ,  color = "GGsearch"), linewidth = 1.5) +
  geom_line(aes(y =rescale_facebook , color = "Facebook"),linewidth = 1.5) +
  geom_line(aes(y =rescale_display, color = "Display"),linewidth = 1.5)+ 
  
  scale_color_manual(values = c("GGsearch"="#ff5733", 
                                'Facebook'='#33c0ff',
                                'Display'= '#fff133'))+
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  labs( title = "Scale Time series", 
        subtitle = "Leads count",
        x = "", 
        y = "Daily Spending")


ggsave("graphs/principal/scale_series.png", width = 20, height = 13, units = "cm")

#############################################################3
##Now, we are going to compare the time taken to complete a purchase.
#########################################################

monthly_data %>% 
  
  mutate(total = ggsearch+ facebook+ display) %>% 
  mutate(share_ggsearch = ggsearch/total, 
         share_facebook = facebook/total, 
         share_display = display/total,
         days_considered = days_considered/max(days_considered, na.rm = T),
         days_purchase= days_purchase/max(days_purchase, na.rm = T)
         
  ) %>% 
  ggplot(aes( x = date_monthly)) +
  geom_col(aes( y = days_purchase), alpha = 0.5, fill = "#bb33ff") +
  geom_col(aes( y = days_considered)) +
  
  geom_line(aes(y =share_ggsearch ,  color = "GGsearch"), linewidth = 2) +
  geom_line(aes(y =share_facebook , color = "Facebook"),linewidth = 2) +
  geom_line(aes(y =share_display, color = "Display"),linewidth = 2)+ 
  
  scale_color_manual(values = c("GGsearch"="#ff5733", 
                                'Facebook'='#33c0ff',
                                'Display'= '#fff133'))+
  scale_y_continuous(limits = c(0,1)) +
  theme_minimal() +
  labs( title = "Media Time series", 
        subtitle = "Days to Qualified",
        x = "", 
        y = "Daily Spending")

ggsave("graphs/principal/scale_series_days.png", width = 20, height = 13, units = "cm")

# Scatter plots -----------------------------------------------------------------


#############################################################3
## In this section, we are going to plot leads against media spending.
## We will also plot a linear model (lm) to see if there is a
## 1-to-1 relationship.
## Additionally, we will apply log transformations, as some variables
## have high values, to determine if the log helps achieve a better linear relationship.
#########################################################

variables <- c("ggsearch",
               "facebook",
               "display")
scatter_function <- function( .variable, log = TRUE) {
  # browser()
  
  ## Fucntion to generate scatterplot
  if (log) {
    data %>% 
      mutate(!!sym(.variable):= log(!!sym(.variable))) %>% 
      mutate(leads_n = log(leads_n)) %>% 
      ggplot(aes(x = !!sym(.variable), y = leads_n))+
      geom_point() +
      theme_minimal() +
      geom_smooth(method = "lm")+ 
      stat_regline_equation(
        aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
        formula = y~x
      )+
      labs(title = paste0("LM lead ~ ", .variable),
           x= .variable, 
           y = "Leads Count") 
    
  }else {
  data %>% 
    ggplot(aes(x = !!sym(.variable), y = leads_n))+
    geom_point() +
    theme_minimal() +
    geom_smooth(method = "lm")+ 
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
      formula = y~x
    )+
    labs(title = paste0("LM lead ~ ", .variable),
         x= .variable, 
         y = "Leads Count") 
    }
}

# map(variables, ~scatter_function(.))
  
p1 <- scatter_function("ggsearch", F)



p2 <- scatter_function("facebook", F)

p3 <- scatter_function("display", F)

p1 / p2 / p3
ggsave("graphs/scatters/lead_spending.png", width = 20, height = 15, units = "cm")

## Log plots
p1_log <- scatter_function("ggsearch", T)

p2_log <- scatter_function("facebook", T)

p3_log <- scatter_function("display", T)

p1_log / p2_log / p3_log

ggsave("graphs/scatters/lead_spending_log.png", width = 20, height = 15, units = "cm")




# boxplots ----------------------------------------------------------------

#############################################################3
## For this section, we are going to analyze
## the distribution of our spending
## using boxplots.
#########################################################


boxplot_function <- function(.variable) {
  # browser()
  
  #### Function to generate boxplots
  data %>% 
    filter(!!sym(.variable)!= 0 ) %>% 
    ggplot(aes(x = !!sym(.variable), y =""))+
    geom_boxplot()+
    geom_jitter(alpha = 0.4, color = "#ff5733") +
    theme_minimal()+
    labs(title = paste0("Boxplot ", .variable),
         x= .variable, 
         y = "") 
  
  
}


p1 <- boxplot_function("ggsearch")

p2 <- boxplot_function("facebook")

p3 <- boxplot_function("display")

p1 / p2 / p3
ggsave("graphs/boxplot/distribution_spending.png", width = 20, height = 15, units = "cm")


p1 <- boxplot_function("leads_n")
p1
data %>% names()

p2 <- boxplot_function("days_considered")
p2
p1/p2
ggsave("graphs/boxplot/distribution_leads.png", width = 20, height = 15, units = "cm")


# dates -------------------------------------------------------------------

#############################################################
### Now, we are going to examine seasonal behavior.
### It's clear that GGsearch and leads have a relationship
### through months and days.
###
#############################################################



data_months_lead <- 
  data %>% 
  group_by(month) %>% 
  summarise(ggsearch = mean(ggsearch), 
            facebook = mean(facebook),
            display = mean(display), 
            promotion = sum(promotion),
            leads_n = sum(leads_n), 
            purchase_leads = sum(purchase_leads),
            days_considered = mean(ifelse(days_considered== 0, NA, days_considered), na.rm = T),
            days_purchase = mean(ifelse(days_purchase== 0, NA, days_purchase), na.rm = T)) %>% 
  mutate(month= factor(month, 
                       levels= c('January','February' ,'March',
                                 'April','May', 'June', 'July', 
                                 'August','September', 'October', 'November',
                                 'December'))) %>% 
  arrange(month)




monthly_fcn  <- function(.variable, .data) {
  .data %>% 
    mutate(across(where(is.numeric), ~rescale(.))) %>% 
    # names()
    ggplot(aes(x = month ))+
    geom_col(aes( y =leads_n), fill = "#1a99b8") +
    
    geom_line(aes(y =!!sym(.variable), group = 1) , linewidth = 2, color = "#ff3333")+
    
    theme_minimal()+
    labs(title = paste0("Monthly data ", .variable),
         x= .variable, 
         y = "") 
  
}



p1 <- monthly_fcn("ggsearch", data_months_lead)


p2 <- monthly_fcn("facebook", data_months_lead)

p3 <- monthly_fcn("display", data_months_lead)

p1+p2+p3

ggsave("graphs/monthly/spending.png", width = 20, height = 15, units = "cm")



data_daily_lead <- 
  data %>% 
  group_by(day) %>% 
  summarise(ggsearch = mean(ggsearch), 
            facebook = mean(facebook),
            display = mean(display), 
            promotion = sum(promotion),
            leads_n = sum(leads_n), 
            purchase_leads = sum(purchase_leads),
            days_considered = mean(ifelse(days_considered== 0, NA, days_considered), na.rm = T),
            days_purchase = mean(ifelse(days_purchase== 0, NA, days_purchase), na.rm = T)) %>% 
  mutate(day= factor(day, 
                       levels= c('Monday','Tuesday' ,'Wednesday', 
                                 'Thursday','Friday', 'Saturday', 'Sunday'))) %>% 
  arrange(day)



daily_fcn  <- function(.variable, .data) {
  .data %>% 
    mutate(across(where(is.numeric), ~rescale(.))) %>% 
    # names()
    ggplot(aes(x = day ))+
    geom_col(aes( y =leads_n), fill = "#1a99b8") +
    
    geom_line(aes(y =!!sym(.variable), group = 1) , linewidth = 2, color = "#ff3333")+
    
    theme_minimal()+
    labs(title = paste0("Daily data ", .variable),
         x= .variable, 
         y = "") 
  
}





p1 <- daily_fcn("ggsearch", data_daily_lead)


p2 <- daily_fcn("facebook", data_daily_lead)

p3 <- daily_fcn("display", data_daily_lead)
p1+p2+p3


ggsave("graphs/daily/spending.png", width = 20, height = 15, units = "cm")


data %>% 
  mutate(rate_con = purchase_leads_fur/leads_n) %>% 
  group_by(month) %>% 
  summarise(rate_con = mean(rate_con),
            time_to_purchase = mean(time_to_purchase))

monthly_data %>% 
  mutate(leads_n =rescale(leads_n),
         ggsearch = rescale(ggsearch ),
         display = rescale(display ),
         
         facebook = rescale(facebook),
         year = year(date_monthly)) %>% 
  ggplot(aes(x = date_monthly,group = factor(year))) + 
  geom_line(aes( y = leads_n), color = "red")+
  geom_line(aes( y =ggsearch ), color = "blue")+
  geom_line(aes( y =facebook ), color = "green")+
  geom_line(aes( y =display ), color = "black")+
  
  
  facet_wrap(~factor(year), scales = "free")+
  theme_minimal()


data_yearly<- 
  data %>% 
  mutate(year = year(date)) %>% 
  group_by(year , month) %>% 
  summarise(ggsearch = mean(ggsearch), 
            facebook = mean(facebook),
            display = mean(display), 
            promotion = sum(promotion),
            leads_n = sum(leads_n), 
            purchase_leads = sum(purchase_leads),
            days_considered = mean(ifelse(days_considered== 0, NA, days_considered), na.rm = T),
            days_purchase = mean(ifelse(days_purchase== 0, NA, days_purchase), na.rm = T)) %>% 
  mutate(month= factor(month, 
                       levels= c('January','February' ,'March',
                                 'April','May', 'June', 'July', 
                                 'August','September', 'October', 'November',
                                 'December'))) %>% 
  arrange(month)


data_yearly %>% 
  ggplot(aes(x = month, y = leads_n,group = factor(year), color = factor(year))) + 
  geom_line()+
  theme_minimal()



# corrplot ----------------------------------------------------------------

#############################################################
### Finally, we are going to examine relationships.
### An interesting observation is that, although in daily data
### we see a relationship with days of the week,
### when we aggregate to monthly data, we observe a stronger correlation
### among the variables.
#############################################################


cor(data %>% select(where(is.numeric)) %>% select(-X)) %>% 
  corrplot::corrplot(method = 'number')

monthly_data %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  corrplot(method = 'number')



# CONCLUSIONS -------------------------------------------------------------
#############################################################
#
# CONCLUSIONS: We are going to focus the analysis on 
#`lead_n` (number of leads),
# but we will develop two models: one using daily data 
# and the other using weekly data.
#
####################################################################






