# install.packages('pacman')


pacman::p_load(tidyverse, dplyr, lubridate, ggplot2, scales, ggpubr, patchwork,
               corrplot)



data <- 
  read.csv("cache/prep_data.csv",nrows = F) %>% 
  mutate(date = as.Date(date))



# check -------------------------------------------------------------------

data %>% 
  summary()



# ggsearch analysis --------------------------------------------------------

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
            days_considered = mean(ifelse(days_considered== 0, NA, days_considered), na.rm = T),
            days_purchase = mean(ifelse(days_purchase== 0, NA, days_purchase), na.rm = T)
            
            ) 

monthly_data %>% summary()

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


# Scatter plots -----------------------------------------------------------------

variables <- c("ggsearch",
               "facebook",
               "display")
scatter_function <- function( .variable, log = TRUE) {
  # browser()
  if (log) {
    data %>% 
      mutate(!!sym(.variable):= log(!!sym(.variable))) %>% 
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
p1_log <- scatter_function("ggsearch", T)

p2_log <- scatter_function("facebook", T)

p3_log <- scatter_function("display", T)

p1_log / p2_log / p3_log




# boxplots ----------------------------------------------------------------

boxplot_function <- function(.variable) {
  # browser()
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


p1 <- boxplot_function("leads_n")
p1
data %>% names()

p2 <- boxplot_function("days_considered")
p2
p1/p2


# dates -------------------------------------------------------------------


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




# corrplot ----------------------------------------------------------------

cor(data %>% select(where(is.numeric)) %>% select(-X)) %>% 
  corrplot::corrplot(method = 'number')
