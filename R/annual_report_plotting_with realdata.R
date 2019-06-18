p <- "Z:/DEC/SwanCanningRiversMonitoringProgram_SP2018-072/DATA/Working/nutrient_monitoring"
setwd(p)

library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(ggthemes)
library(grid)
library(gridExtra)

WIN_files <- list.files("./swan_nutrient_WIN", pattern = ".xlsx",
                        full.names = TRUE)

#required fields
#these are the columns to import - need verification
fields <- c("collection_method_name", "sample_type_code", "collect_date", 
           "area_ref", "alkalinity_tot_ca_co3_mg_l", 
           "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
           "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
           "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
           "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l", 
           "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
           "tss_mg_l", "temperature_in_situ_deg_c", "p_h_no_units")

#anonymous function to replace all < values with a slightly smaller real value
conv_less <- function(x){
  ifelse(str_detect(x, "^<"), as.numeric(sapply(str_split(x, "<"), 
                                                function(x) x[2])) - 0.00001, 
         as.numeric(x))
}

#this is reading available data - add more as required
d1 <- read_excel(WIN_files[1], .name_repair = ~ janitor::make_clean_names)
d2 <- read_excel(WIN_files[2], .name_repair = ~ janitor::make_clean_names)
d3 <- read_excel(WIN_files[3], .name_repair = ~ janitor::make_clean_names)


#tidies erroneous character columns for each subset of historical data
tidy_d1 <- d1 %>%
  select(fields) %>%
  mutate(chlorophyll_a_by_vol_mg_l = conv_less(chlorophyll_a_by_vol_mg_l),
         n_sum_sol_ox_n_ox_n_ton_mg_l = conv_less(n_sum_sol_ox_n_ox_n_ton_mg_l),
         nh3_n_nh4_n_sol_mg_l = conv_less(nh3_n_nh4_n_sol_mg_l),
         po4_p_sol_react_srp_frp_mg_l = conv_less(po4_p_sol_react_srp_frp_mg_l))

tidy_d2 <- d2 %>%
  select(fields) %>%
  mutate(chlorophyll_a_by_vol_mg_l = conv_less(chlorophyll_a_by_vol_mg_l),
         n_sum_sol_ox_n_ox_n_ton_mg_l = conv_less(n_sum_sol_ox_n_ox_n_ton_mg_l),
         nh3_n_nh4_n_sol_mg_l = conv_less(nh3_n_nh4_n_sol_mg_l),
         po4_p_sol_react_srp_frp_mg_l = conv_less(po4_p_sol_react_srp_frp_mg_l))

tidy_d3 <- d3 %>%
  select(fields) %>%
  mutate(chlorophyll_a_by_vol_mg_l = conv_less(chlorophyll_a_by_vol_mg_l),
         n_sum_sol_ox_n_ox_n_ton_mg_l = conv_less(n_sum_sol_ox_n_ox_n_ton_mg_l),
         nh3_n_nh4_n_sol_mg_l = conv_less(nh3_n_nh4_n_sol_mg_l),
         po4_p_sol_react_srp_frp_mg_l = conv_less(po4_p_sol_react_srp_frp_mg_l))


#checks
sum(is.na(d3$chlorophyll_a_by_vol_mg_l)) == sum(is.na(tidy_d3$chlorophyll_a_by_vol_mg_l))
sum(is.na(d3$n_sum_sol_ox_n_ox_n_ton_mg_l)) == sum(is.na(tidy_d3$n_sum_sol_ox_n_ox_n_ton_mg_l))
sum(is.na(d3$nh3_n_nh4_n_sol_mg_l)) == sum(is.na(tidy_d3$nh3_n_nh4_n_sol_mg_l))
sum(is.na(d3$po4_p_sol_react_srp_frp_mg_l)) == sum(is.na(tidy_d3$po4_p_sol_react_srp_frp_mg_l))

#make one tidy data set for historical data and add ecological mngt zones
#change as required with additional years

#set up reporting zones for swan dates from 31/05/2013 for 2018-19 report
tidy_all <- bind_rows(tidy_d1, tidy_d2, tidy_d3) %>%
  mutate(emz = case_when(area_ref == "BLA" | area_ref == "ARM" | area_ref == "HEA" | area_ref == "NAR" ~ "lower",
                         area_ref == "NIL" | area_ref == "STJ" | area_ref == "MAY" | area_ref == "RON" ~ "middle",
                         area_ref == "KIN" | area_ref == "SUC" | area_ref == "WMP" | area_ref == "MSB" ~ "upper",
                         TRUE ~ "nrz")) %>%
  filter(collect_date > "2013-05-31") %>%
  mutate(mth = month(collect_date),
         pord = case_when(mth == 1 ~ 8,
                          mth == 2 ~ 9,
                          mth == 3 ~ 10,
                          mth == 4 ~ 11,
                          mth == 5 ~ 12,
                          mth == 6 ~ 1,
                          mth == 7 ~ 2,
                          mth == 8 ~ 3,
                          mth == 9 ~ 4,
                          mth == 10 ~ 5,
                          mth == 11 ~ 6,
                          mth == 12 ~ 7))#hack to reorder mths as must be continous variable




max(unique(tidy_all$collect_date))
min(unique(tidy_all$collect_date))

#main 7 handled the same
seven <- c("n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", 
           "n_sum_sol_ox_n_ox_n_ton_mg_l", "n_sum_sol_org_don_mg_l",
           "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
           "si_o2_si_sol_react_mg_l")


test7 <- tidy_all %>%
  filter(emz != "nrz" & collection_method_name == "Grab sample") %>%
  gather(xvar, value, seven) %>%
  group_by(emz, pord, sample_type_code, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE))


#dissolved organic nitrogen BOTTOM
df <- filter(test7, xvar == "n_sum_sol_org_don_mg_l" & sample_type_code == "ST_BOTT")

#lower
bl <- ggplot(filter(df, emz =="lower"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  labs(y = bquote('DOrgN ('*mgL^-1*')')) +
  scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5),
                     limits = c(0, 2.5)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

#middle
bm <- ggplot(filter(df, emz =="middle"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  labs(y = bquote('DOrgN ('*mgL^-1*')')) +
  scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5),
                     limits = c(0, 2.5)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

#upper - with legend
bu <- ggplot(filter(df, emz =="upper"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "blue"), alpha = 0.1) +
  scale_fill_manual(name = "",
                    values = "blue",
                    labels = "Mthly Background\n10th-90th percentile") +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median, colour = "blue")) +
  scale_colour_manual(name = "",
                      values = "blue",
                      labels = "Mthly Background\nMedian") +
  labs(y = bquote('DOrgN ('*mgL^-1*')')) +
  scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5),
                     limits = c(0, 2.5)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

#dissolved organic nitrogen SURFACE
df2 <- filter(test7, xvar == "n_sum_sol_org_don_mg_l" & sample_type_code == "ST_SURF")

#lower
sl <- ggplot(filter(df2, emz =="lower"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  labs(y = bquote('DOrgN ('*mgL^-1*')')) +
  scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5),
                     limits = c(0, 2.5)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

#middle
sm <- ggplot(filter(df2, emz =="middle"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  labs(y = bquote('DOrgN ('*mgL^-1*')')) +
  scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5),
                     limits = c(0, 2.5)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

#upper - with legend
su <- ggplot(filter(df2, emz =="upper"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "red"), alpha = 0.2) +
  scale_fill_manual(name = "",
                    values = "red",
                    labels = "Mthly Background\n10th-90th percentile") +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median, colour = "red")) +
  scale_colour_manual(name = "",
                      values = "red",
                      labels = "Mthly Background\nMedian") +
  labs(y = bquote('DOrgN ('*mgL^-1*')')) +
  scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5),
                     limits = c(0, 2.5)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"))

grid.arrange(sl, bl, sm, bm, su, bu, ncol = 2)
