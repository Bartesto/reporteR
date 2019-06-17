library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)



path <- "../nutrient_monitoring/swan_nutrient_WIN"

files <- list.files(path = path, pattern = ".xlsx",
                    full.names = TRUE)

temp_names <- names(readxl::read_excel(files[1]))
temp_names24 <- names(readxl::read_excel(files[1], skip = 1))[1:24] #first 24 are consistent
new_names <- c(temp_names24, temp_names[25:length(temp_names)])

ctypes <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", 128))

d1 <- readxl::read_excel(files[1], skip = 2, col_names = new_names,
                         col_types = ctypes, .name_repair = ~ janitor::make_clean_names)

#setup info
#these are the fields to import - need verification
fields <- c("collection_method", "sample_type", "collect_date",
            "project_site_ref", "alkalinity_tot_ca_co3_mg_l",
            "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
            "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
            "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
            "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
            "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
            "tss_mg_l", "temperature_in_situ_deg_c", "p_h_no_units")

metrics <- c("alkalinity_tot_ca_co3_mg_l",
            "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
            "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
            "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
            "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
            "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
            "tss_mg_l", "temperature_in_situ_deg_c", "p_h_no_units")

#function to replace all '<' values LOR/2
conv_less <- function(x){
  ifelse(str_detect(x, "^<"), as.numeric(sapply(str_split(x, "<"),
                                                function(x) x[2]))/2,
         as.numeric(x))
}

conv_l_or_g <- function(x){
  ifelse(str_detect(x, "^<"), as.numeric(sapply(str_split(x, "<"),
                                                function(x) x[2]))/2,
         ifelse(str_detect(x, "^>"), as.numeric(sapply(str_split(x, ">"),
                                                       function(x) x[2]))/2,
                as.numeric(x)))
  }

conv_great <- function(x){
  ifelse(str_detect(x, "^>"), as.numeric(sapply(str_split(x, ">"),
                                                function(x) x[2]))/2,
         as.numeric(x))
}

# report_start <- paste0(reportingYear - 6, "-06-01")
# report_fin <- paste0(reportingYear - 1, "-06-01")




tidy_all <- d1 %>%
  select(fields) %>%
  mutate(alkalinity_tot_ca_co3_mg_l = conv_less(alkalinity_tot_ca_co3_mg_l),
         c_sol_org_doc_doc_as_npoc_mg_l = conv_less(c_sol_org_doc_doc_as_npoc_mg_l),
         chlorophyll_a_by_vol_mg_l = conv_less(chlorophyll_a_by_vol_mg_l),
         n_sum_sol_org_don_mg_l = conv_less(n_sum_sol_org_don_mg_l),
         n_sum_sol_ox_n_ox_n_ton_mg_l = conv_less(n_sum_sol_ox_n_ox_n_ton_mg_l),
         n_tot_tn_p_tn_mg_l = conv_less(n_tot_tn_p_tn_mg_l),
         nh3_n_nh4_n_sol_mg_l = conv_less(nh3_n_nh4_n_sol_mg_l),
         o_do_in_situ_mg_l = conv_less(o_do_in_situ_mg_l),
         p_tot_tp_p_tp_mg_l = conv_less(p_tot_tp_p_tp_mg_l),
         po4_p_sol_react_srp_frp_mg_l = conv_less(po4_p_sol_react_srp_frp_mg_l),
         salinity_mg_l = conv_less(salinity_mg_l),
         secchi_depth_m = conv_less(secchi_depth_m),
         si_o2_si_sol_react_mg_l = conv_less(si_o2_si_sol_react_mg_l),
         tss_mg_l = conv_less(tss_mg_l),
         temperature_in_situ_deg_c = conv_less(temperature_in_situ_deg_c),
         p_h_no_units = conv_less(p_h_no_units))

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

# sum(tidy_all$secchi_depth_m, na.rm = TRUE)
# sum(as.numeric(d1$secchi_depth_m), na.rm = TRUE)
#
# t_a <- d1 %>%
#   select(secchi_depth_m) %>%
#   mutate(secchi_depth_m = conv_l_or_g(secchi_depth_m))
# t_b <- d1 %>%
#   select(secchi_depth_m)
# sum(is.na(t_a$secchi_depth_m)) == sum(is.na(t_b$secchi_depth_m))

#loop to get all data
if(length(files) > 1){
  for(i in seq_along(files)[-1]){
    temp_names <- names(readxl::read_excel(files[1]))
    d <- readxl::read_excel(files[2], skip = 2, col_names = new_names,
                            col_types = ctypes, .name_repair = ~ janitor::make_clean_names)
    t_d <- d %>%
      select(fields) %>%
      mutate(alkalinity_tot_ca_co3_mg_l = conv_less(alkalinity_tot_ca_co3_mg_l),
             c_sol_org_doc_doc_as_npoc_mg_l = conv_less(c_sol_org_doc_doc_as_npoc_mg_l),
             chlorophyll_a_by_vol_mg_l = conv_less(chlorophyll_a_by_vol_mg_l),
             n_sum_sol_org_don_mg_l = conv_less(n_sum_sol_org_don_mg_l),
             n_sum_sol_ox_n_ox_n_ton_mg_l = conv_less(n_sum_sol_ox_n_ox_n_ton_mg_l),
             n_tot_tn_p_tn_mg_l = conv_less(n_tot_tn_p_tn_mg_l),
             nh3_n_nh4_n_sol_mg_l = conv_less(nh3_n_nh4_n_sol_mg_l),
             o_do_in_situ_mg_l = conv_less(o_do_in_situ_mg_l),
             p_tot_tp_p_tp_mg_l = conv_less(p_tot_tp_p_tp_mg_l),
             po4_p_sol_react_srp_frp_mg_l = conv_less(po4_p_sol_react_srp_frp_mg_l),
             salinity_mg_l = conv_less(salinity_mg_l),
             secchi_depth_m = conv_less(secchi_depth_m),
             si_o2_si_sol_react_mg_l = conv_less(si_o2_si_sol_react_mg_l),
             tss_mg_l = conv_less(tss_mg_l),
             temperature_in_situ_deg_c = conv_less(temperature_in_situ_deg_c),
             p_h_no_units = conv_less(p_h_no_units))
    tidy_all <- dplyr::bind_rows(tidy_all, t_d)
  }
} else {
  tidy_all <- tidy_all
}
