### Annual Plots


library(tidyverse)
library(lubridate)
library(ggthemes)
library(grid)
library(gridExtra)

## Inputs
path = "Z:/DEC/SwanCanningRiversMonitoringProgram_SP2018-072/DATA/Working/reporteR"


##start
file <- list.files(path = path, pattern = "annual_report", full.names = TRUE)
data <- read_csv(file = file, guess_max = 10000)


##seven
seven <- c("n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l",
           "n_sum_sol_ox_n_ox_n_ton_mg_l", "n_sum_sol_org_don_mg_l",
           "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
           "si_o2_si_sol_react_mg_l")

df7 <- data %>%
  filter(emz != "nrz" & collection_method == "Grab sample" &
           sample_type != "Standard") %>%
  gather(xvar, value, seven) %>%
  group_by(emz, pord, sample_type, rep_per, xvar) %>%
  summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
            upper = quantile(value, probs = 0.90, na.rm = TRUE),
            median = median(value, na.rm = TRUE)) %>%
  mutate(filt = paste(str_sub(emz, 1, 1),
                      str_to_lower(str_sub(sample_type, 1, 1)),
                      str_sub(rep_per, 1, 1),
                      sep = "."))

#maybe loop/function from here
df7b <- filter(df7, xvar == seven[1])


lab7names <- list(c(bquote("TN (mg  "*L^-1*")"), "TN"),
                   c(bquote(NH[3]*"-N (mg  "*L^-1*")"), bquote(NH[3]*"-N")),
                   c(bquote("NOx-N (mg  "*L^-1*")"), "NOx-N"),
                   c(bquote("DOrGN (mg  "*L^-1*")"), "DOrGN"),
                   c(bquote("TP (mg  "*L^-1*")"), "TP"),
                   c(bquote("FRP (mg  "*L^-1*")"), "FRP"),
                   c(bquote(Si0[2]*"-Si (mg  "*L^-1*")"), bquote(Si0[2]*"-Si")))


a <- ggplot(data = filter(df7b, filt == "l.s.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  geom_point(data = filter(df7b, filt == "l.s.p"), aes(x = pord, y = median), pch = 1, size = 2) +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = paste("(a) ", lab7names[[1]][[2]], "Lower Swan Canning Estuary (Surface)"),
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title.x = element_text(face = "bold", hjust = 0, size = 14))

c <- ggplot(data = filter(df7b, filt == "m.s.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  geom_point(data = filter(df7b, filt == "m.s.p"), aes(x = pord, y = median), pch = 1, size = 2) +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = paste("(c) ", lab7names[[1]][[2]], "Middle Swan Canning Estuary (Surface)"),
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title.x = element_text(face = "bold", hjust = 0, size = 14))

e <- ggplot(data = filter(df7b, filt == "u.s.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "blue") +
  geom_point(data = filter(df7b, filt == "u.s.p"), aes(x = pord, y = median), pch = 1, size = 2) +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = paste("(e) ", lab7names[[1]][[2]], "Upper Swan Canning Estuary (Surface)"),
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title.x = element_text(face = "bold", hjust = 0, size = 14))



e_leg <- ggplot(data = filter(df7b, filt == "u.s.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "blue"), alpha = 0.1) +
  scale_fill_manual(name = "",
                    values = "blue",
                    labels = "Monthly background 10th and 90th percentile") +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "blue", alpha = 0.4) +
  geom_line(aes(x = pord, y = median, colour = "blue")) +
  scale_colour_manual(name = "",
                      values = "blue",
                      labels = "Monthly background Median") +
  geom_point(data = filter(df7b, filt == "u.s.p"), aes(x = pord, y = median, shape = filt),
             size = 2) +
  scale_shape_manual(name = "",
                     values = 1,
                     labels = "18/19 monthly Median") +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.box = "vertical", legend.box.just = "left",
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")) +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2),
         shape = guide_legend(order = 3))

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend_surf <- get_legend(e_leg)

#grid.arrange(a, c, e, legend, ncol = 1)

b <- ggplot(data = filter(df7b, filt == "l.b.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  geom_point(data = filter(df7b, filt == "l.b.p"), aes(x = pord, y = median), size = 2) +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = paste("(b) ", lab7names[[1]][[2]], "Lower Swan Canning Estuary (Bottom)"),
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title.x = element_text(face = "bold", hjust = 0, size = 14))

d <- ggplot(data = filter(df7b, filt == "m.b.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  geom_point(data = filter(df7b, filt == "m.b.p"), aes(x = pord, y = median), size = 2) +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = paste("(d) ", lab7names[[1]][[2]], "Middle Swan Canning Estuary (Bottom)"),
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title.x = element_text(face = "bold", hjust = 0, size = 14))

f <- ggplot(data = filter(df7b, filt == "u.b.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha = 0.1) +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median), colour = "red") +
  geom_point(data = filter(df7b, filt == "u.b.p"), aes(x = pord, y = median), size = 2) +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = paste("(f) ", lab7names[[1]][[2]], "Upper Swan Canning Estuary (Bottom)"),
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank"),
        axis.title.x = element_text(face = "bold", hjust = 0, size = 14))


f_leg <- ggplot(data = filter(df7b, filt == "u.b.b"), aes(pord)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "red"), alpha = 0.1) +
  scale_fill_manual(name = "",
                    values = "red",
                    labels = "Monthly background 10th-90th percentile") +
  geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = "red", alpha = 0.4) +
  geom_line(aes(x = pord, y = median, colour = "red")) +
  scale_colour_manual(name = "",
                      values = "red",
                      labels = "Monthly background Median") +
  geom_point(data = filter(df7b, filt == "u.b.p"), aes(x = pord, y = median, shape = filt),
             size = 2) +
  scale_shape_manual(name = "",
                     values = 16,
                     labels = "18/19 monthly Median") +
  labs(y = lab7names[[1]][[1]]) +
  scale_y_continuous(breaks = seq(from = 0, to = 4, by = 1),
                     limits = c(0, 4)) +
  scale_x_continuous(name = "",
                     breaks = c(8:12, 1:7),
                     labels = month.abb[1:12]) +
  theme_bw() +
  theme(legend.position = "bottom", legend.box = "vertical",
        legend.box.just = "left",
        panel.grid.major = element_line(linetype = "blank"),
        panel.grid.minor = element_line(linetype = "blank")) +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2),
         shape = guide_legend(order = 3))

legend_bott <- get_legend(f_leg)

grid.arrange(a, b, c, d, e, f, legend_surf, legend_bott, ncol = 2)


tmp <- ggplot_gtable(ggplot_build(f_leg))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

tmps <- ggplot_gtable(ggplot_build(e_leg))
leg2 <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend2 <- tmp$grobs[[leg2]]

