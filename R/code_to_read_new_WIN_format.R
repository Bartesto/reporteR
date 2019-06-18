library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(ggthemes)
library(grid)
library(gridExtra)


reportingYear = 2019


#grab vector of spreadsheets to import
path <- "../nutrient_monitoring/swan_nutrient_WIN"
files <- list.files(path = path, pattern = ".xlsx",
                    full.names = TRUE)

#assumes first 24 names in double headed spreadsheet are same for each import
#dynamically assigns text vals to all columns after first 24 - function
#conv_l_g handles conversion to numeric later
temp_names <- names(readxl::read_excel(files[1]))
temp_names24 <- names(readxl::read_excel(files[1], skip = 1))[1:24] #first 24 are consistent
new_names <- c(temp_names24, temp_names[25:length(temp_names)])
ctypes <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", length(temp_names) - 24))

d1 <- readxl::read_excel(files[1], skip = 2, col_names = new_names,
                         col_types = ctypes, .name_repair = ~ janitor::make_clean_names)

#setup info
#these are the fields to import - NEED VERIFICATION
fields <- c("collection_method", "sample_type", "collect_date",
            "project_site_ref", "alkalinity_tot_ca_co3_mg_l",
            "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
            "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
            "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
            "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
            "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
            "tss_mg_l", "temperature_in_situ_deg_c", "p_h_no_units")

# metrics <- c("alkalinity_tot_ca_co3_mg_l",
#             "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
#             "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
#             "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
#             "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
#             "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
#             "tss_mg_l", "temperature_in_situ_deg_c", "p_h_no_units")


#function to convert less than greater than symbols to numeric values
conv_l_or_g <- function(x){
  ifelse(str_detect(x, "^<"), as.numeric(sapply(str_split(x, "<"),
                                                function(x) x[2]))/2,
         ifelse(str_detect(x, "^>"), as.numeric(sapply(str_split(x, ">"),
                                                       function(x) x[2])),
                as.numeric(x)))
  }


report_start <- paste0(reportingYear - 6, "-06-01")
report_fin <- paste0(reportingYear - 1, "-06-01")



# NEED VERIFICATION for greater than handling
tidy_all <- d1 %>%
  select(fields) %>%
  mutate(alkalinity_tot_ca_co3_mg_l = conv_l_or_g(alkalinity_tot_ca_co3_mg_l),
         c_sol_org_doc_doc_as_npoc_mg_l = conv_l_or_g(c_sol_org_doc_doc_as_npoc_mg_l),
         chlorophyll_a_by_vol_mg_l = conv_l_or_g(chlorophyll_a_by_vol_mg_l),
         n_sum_sol_org_don_mg_l = conv_l_or_g(n_sum_sol_org_don_mg_l),
         n_sum_sol_ox_n_ox_n_ton_mg_l = conv_l_or_g(n_sum_sol_ox_n_ox_n_ton_mg_l),
         n_tot_tn_p_tn_mg_l = conv_l_or_g(n_tot_tn_p_tn_mg_l),
         nh3_n_nh4_n_sol_mg_l = conv_l_or_g(nh3_n_nh4_n_sol_mg_l),
         o_do_in_situ_mg_l = conv_l_or_g(o_do_in_situ_mg_l),
         p_tot_tp_p_tp_mg_l = conv_l_or_g(p_tot_tp_p_tp_mg_l),
         po4_p_sol_react_srp_frp_mg_l = conv_l_or_g(po4_p_sol_react_srp_frp_mg_l),
         salinity_mg_l = conv_l_or_g(salinity_mg_l),
         secchi_depth_m = conv_l_or_g(secchi_depth_m),
         si_o2_si_sol_react_mg_l = conv_l_or_g(si_o2_si_sol_react_mg_l),
         tss_mg_l = conv_l_or_g(tss_mg_l),
         temperature_in_situ_deg_c = conv_l_or_g(temperature_in_situ_deg_c),
         p_h_no_units = conv_l_or_g(p_h_no_units))

#tests seeking number of NA's remains consistent
sum(is.na(tidy_all$alkalinity_tot_ca_co3_mg_l)) == sum(is.na(d1$alkalinity_tot_ca_co3_mg_l))
sum(is.na(tidy_all$c_sol_org_doc_doc_as_npoc_mg_l)) == sum(is.na(d1$c_sol_org_doc_doc_as_npoc_mg_l))
sum(is.na(tidy_all$chlorophyll_a_by_vol_mg_l)) == sum(is.na(d1$chlorophyll_a_by_vol_mg_l))
sum(is.na(tidy_all$n_sum_sol_org_don_mg_l)) == sum(is.na(d1$n_sum_sol_org_don_mg_l))
sum(is.na(tidy_all$n_sum_sol_ox_n_ox_n_ton_mg_l)) == sum(is.na(d1$n_sum_sol_ox_n_ox_n_ton_mg_l))
sum(is.na(tidy_all$n_tot_tn_p_tn_mg_l)) == sum(is.na(d1$n_tot_tn_p_tn_mg_l))
sum(is.na(tidy_all$nh3_n_nh4_n_sol_mg_l)) == sum(is.na(d1$nh3_n_nh4_n_sol_mg_l))
sum(is.na(tidy_all$o_do_in_situ_mg_l)) == sum(is.na(d1$o_do_in_situ_mg_l))
sum(is.na(tidy_all$p_tot_tp_p_tp_mg_l)) == sum(is.na(d1$p_tot_tp_p_tp_mg_l))
sum(is.na(tidy_all$po4_p_sol_react_srp_frp_mg_l)) == sum(is.na(d1$po4_p_sol_react_srp_frp_mg_l))
sum(is.na(tidy_all$salinity_mg_l)) == sum(is.na(d1$salinity_mg_l))
sum(is.na(tidy_all$secchi_depth_m)) == sum(is.na(d1$secchi_depth_m))
sum(is.na(tidy_all$si_o2_si_sol_react_mg_l)) == sum(is.na(d1$si_o2_si_sol_react_mg_l))
sum(is.na(tidy_all$tss_mg_l)) == sum(is.na(d1$tss_mg_l))
sum(is.na(tidy_all$temperature_in_situ_deg_c)) == sum(is.na(d1$temperature_in_situ_deg_c))
sum(is.na(tidy_all$p_h_no_units)) == sum(is.na(d1$p_h_no_units))


#loop to get all data
if(length(files) > 1){
  for(i in seq_along(files)[-1]){
    temp_names1 <- names(readxl::read_excel(files[i]))
    new_names1 <- c(temp_names24, temp_names1[25:length(temp_names1)])
    ctypes1 <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", length(temp_names1) - 24))
    d <- readxl::read_excel(files[i], skip = 2, col_names = new_names1,
                            col_types = ctypes1, .name_repair = ~ janitor::make_clean_names)
    t_d <- d %>%
      select(fields) %>%
      mutate(alkalinity_tot_ca_co3_mg_l = conv_l_or_g(alkalinity_tot_ca_co3_mg_l),
             c_sol_org_doc_doc_as_npoc_mg_l = conv_l_or_g(c_sol_org_doc_doc_as_npoc_mg_l),
             chlorophyll_a_by_vol_mg_l = conv_l_or_g(chlorophyll_a_by_vol_mg_l),
             n_sum_sol_org_don_mg_l = conv_l_or_g(n_sum_sol_org_don_mg_l),
             n_sum_sol_ox_n_ox_n_ton_mg_l = conv_l_or_g(n_sum_sol_ox_n_ox_n_ton_mg_l),
             n_tot_tn_p_tn_mg_l = conv_l_or_g(n_tot_tn_p_tn_mg_l),
             nh3_n_nh4_n_sol_mg_l = conv_l_or_g(nh3_n_nh4_n_sol_mg_l),
             o_do_in_situ_mg_l = conv_l_or_g(o_do_in_situ_mg_l),
             p_tot_tp_p_tp_mg_l = conv_l_or_g(p_tot_tp_p_tp_mg_l),
             po4_p_sol_react_srp_frp_mg_l = conv_l_or_g(po4_p_sol_react_srp_frp_mg_l),
             salinity_mg_l = conv_l_or_g(salinity_mg_l),
             secchi_depth_m = conv_l_or_g(secchi_depth_m),
             si_o2_si_sol_react_mg_l = conv_l_or_g(si_o2_si_sol_react_mg_l),
             tss_mg_l = conv_l_or_g(tss_mg_l),
             temperature_in_situ_deg_c = conv_l_or_g(temperature_in_situ_deg_c),
             p_h_no_units = conv_l_or_g(p_h_no_units))
    tidy_all <- dplyr::bind_rows(tidy_all, t_d)
  }
} else {
  tidy_all <- tidy_all
}

#add reporting zones, plot order for months and key for report period
tidy_all2 <- tidy_all %>%
  filter(collect_date >= report_start) %>%
  mutate(emz = case_when(project_site_ref == "BLA" | project_site_ref == "ARM" | project_site_ref == "HEA" | project_site_ref == "NAR" ~ "lower",
                         project_site_ref == "NIL" | project_site_ref == "STJ" | project_site_ref == "MAY" | project_site_ref == "RON" ~ "middle",
                         project_site_ref == "KIN" | project_site_ref == "SUC" | project_site_ref == "WMP" | project_site_ref == "MSB" ~ "upper",
                         project_site_ref == "JBC" | project_site_ref == "POL" ~ "swan", TRUE ~ "nrz")) %>%
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
                          mth == 12 ~ 7),
         rep_per = ifelse(collect_date < report_fin, "background", "present"))

write_csv(tidy_all2, path = paste0("annual_report_data_for_", reportingYear,".csv"))

#setup datasets for different variables
#1 surface and bottom grab samples
seven_vars <- c("n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l",
                "n_sum_sol_ox_n_ox_n_ton_mg_l", "n_sum_sol_org_don_mg_l",
                "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
                "si_o2_si_sol_react_mg_l")

seven <- tidy_all2 %>%
  filter(emz != "nrz" & collection_method == "Grab sample" & sample_type != "Standard") %>%
  gather(xvar, value, seven_vars) %>%
  group_by(emz, pord, sample_type, rep_per, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE))

#2 surface and bottom insitu samples
four_vars <- c("o_do_in_situ_mg_l", "salinity_mg_l",
               "temperature_in_situ_deg_c", "p_h_no_units")

four <- tidy_all2 %>%
  filter(emz != "nrz" & collection_method == "Insitu" & sample_type != "Standard") %>%
  gather(xvar, value, four_vars) %>%
  group_by(emz, pord, sample_type, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE))

#3 surface only grab samples
three_vars <- c("tss_mg_l", "alkalinity_tot_ca_co3_mg_l",
                "c_sol_org_doc_doc_as_npoc_mg_l")

three <- tidy_all2 %>%
  filter(emz != "nrz" & collection_method == "Grab sample" & sample_type == "Surface sample") %>%
  gather(xvar, value, three_vars) %>%
  group_by(emz, pord, sample_type, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE))

#4 chlorophyl a - surface and integrated
chla <- tidy_all2 %>%
  filter(emz != "nrz" & collection_method != "Insitu" & sample_type != "Bottom sample") %>%
  gather(xvar, value, chlorophyll_a_by_vol_mg_l) %>%
  group_by(emz, pord, sample_type, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE))

#5 secchi is what it is
secchi_1 <- tidy_all2 %>%
  filter(emz != "nrz") %>%
  select(emz, pord, collect_date, sample_type,chlorophyll_a_by_vol_mg_l) %>%
  gather(xvar, value, chlorophyll_a_by_vol_mg_l) %>%
  group_by(emz, pord, sample_type, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE))

#dissolved organic nitrogen BOTTOM
dfb <- filter(seven, xvar == "n_sum_sol_org_don_mg_l" & sample_type == "Bottom sample" & rep_per == "background")
dfp <- filter(seven, xvar == "n_sum_sol_org_don_mg_l" & sample_type == "Bottom sample" & rep_per == "present")

#lower
bl <- ggplot(filter(dfb, emz =="lower"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  geom_point(data = filter(dfp, emz =="lower"), aes(x = pord, y = median)) +
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
bm <- ggplot(filter(dfb, emz =="middle"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  geom_point(data = filter(dfp, emz =="middle"), aes(x = pord, y = median)) +
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
bu <- ggplot(filter(dfb, emz =="upper"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "red"), alpha = 0.1) +
  scale_fill_manual(name = "",
                    values = "red",
                    labels = "Mthly Background\n10th-90th percentile") +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_point(data = filter(dfp, emz =="upper"), aes(x = pord, y = median)) +
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

#dissolved organic nitrogen SURFACE
dfb2 <- filter(seven, xvar == "n_sum_sol_org_don_mg_l" & sample_type == "Surface sample" & rep_per == "background")
dfp2 <- filter(seven, xvar == "n_sum_sol_org_don_mg_l" & sample_type == "Surface sample" & rep_per == "present")

#lower
sl <- ggplot(filter(dfb2, emz =="lower"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  geom_point(data = filter(dfp2, emz =="lower"), aes(x = pord, y = median), pch = 1) +
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
sm <- ggplot(filter(dfb2, emz =="middle"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  geom_point(data = filter(dfp2, emz =="middle"), aes(x = pord, y = median), pch = 1) +
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
su <- ggplot(filter(dfb2, emz =="upper"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "blue"), alpha = 0.2) +
  scale_fill_manual(name = "",
                    values = "blue",
                    labels = "Mthly Background\n10th-90th percentile") +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median, colour = "blue")) +
  geom_point(data = filter(dfp2, emz =="upper"), aes(x = pord, y = median), shape = 1) +
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

grid.arrange(sl, bl, sm, bm, su, bu, ncol = 2)

