# Figures for poster at KU Archaeology Day, 2018

library("dplyr")
library("ggplot2")
library("ggridges")
library("magrittr")
library("rcarbon")
library("readr")
library("extrafont")

theme_poster <- theme_minimal() +
  theme(panel.grid.major = element_line(colour = "#666666"),
        panel.grid.minor = element_line(colour = "#666666", linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank())

# Import data, some light cleaning
read_csv("./analysis/data/zagros_radiocarbon.csv") %>%
  filter(error > 0) ->
  dates

sites <- read.csv("./analysis/data/tcec_sites.csv")

# Calibrate
cal_dates <- calibrate(dates$cra, dates$error, ids=dates$lab_id,
                       normalised = FALSE, calCurves="intcal13")

# Bin and sum, see zagros_spd.Rmd for selection of parameters
bins <- binPrep(dates$site_name, dates$cra, 200)
zagros_spd <- spd(cal_dates, timeRange = c(20000, 6000), datenormalised = FALSE,
                  bins = bins, spdnormalised = TRUE)

# TODO: Confidence envelope

# Total SPD plot
ggplot() +
  geom_area(data=zagros_spd$grid, mapping=aes(x=calBP, y=PrDens),
            colour="#e5e5e5", fill="#333333") +
  scale_x_reverse(limits=c(20000, 6000),
                  breaks=seq(from=20000, to=6000, by=-1000),
                  minor_breaks=seq(from=20000, to=6000, by=-100)) +
  theme_poster
ggsave("./analysis/kuarch2018_poster/figures/plot_spd.pdf", width=450, height=90, units="mm")


# SPD plots by chronotype
tcec_ids <- list(EP = sites[sites$period_EP %in% c("x", "?"),]$tcec_id,
                 PPN = sites[sites$period_N0 %in% c("x", "?"),]$tcec_id,
                 PN = sites[sites$period_N1.1 %in% c("x", "?") |
                            sites$period_N1.2 %in% c("x", "?") |
                            sites$period_N2 %in% c("x", "?") |
                            sites$period_N3 %in% c("x", "?"),]$tcec_id,
                 CH = sites[sites$period_C1 %in% c("x", "?") |
                            sites$period_C2 %in% c("x", "?") |
                            sites$period_C3 %in% c("x", "?"),]$tcec_id)

dates %>% filter(site_id %in% paste0("TCEC", tcec_ids$EP)) -> dates_EP
dates %>% filter(site_id %in% paste0("TCEC", tcec_ids$PPN)) -> dates_PPN
dates %>% filter(site_id %in% paste0("TCEC", tcec_ids$PN)) -> dates_PN
dates %>% filter(site_id %in% paste0("TCEC", tcec_ids$CH)) -> dates_CH

cal_dates_EP <- calibrate(dates_EP$cra, dates_EP$error, normalised = FALSE)
cal_dates_PPN <- calibrate(dates_PPN$cra, dates_PPN$error, normalised = FALSE)
cal_dates_PN <- calibrate(dates_PN$cra, dates_PN$error, normalised = FALSE)
cal_dates_CH <- calibrate(dates_CH$cra, dates_CH$error, normalised = FALSE)

bins_EP <- binPrep(dates_EP$site_id, dates_EP$cra, h = 200)
bins_PPN <- binPrep(dates_PPN$site_id, dates_PPN$cra, h = 200)
bins_PN <- binPrep(dates_PN$site_id, dates_PN$cra, h = 200)
bins_CH <- binPrep(dates_CH$site_id, dates_CH$cra, h = 200)

spd_EP <- spd(cal_dates_EP, bins = bins_EP, timeRange=c(20000, 6000),
              spdnormalised = TRUE)
spd_PPN <- spd(cal_dates_PPN, bins = bins_PPN, timeRange=c(20000, 6000),
              spdnormalised = TRUE)
spd_PN <- spd(cal_dates_PN, bins = bins_PN, timeRange=c(20000, 6000),
              spdnormalised = TRUE)
spd_CH <- spd(cal_dates_CH, bins = bins_CH, timeRange=c(20000, 6000),
              spdnormalised = TRUE)

ggplot() +
  geom_ridgeline(data=spd_EP$grid, mapping=aes(x=calBP, height=PrDens,
                                               y=rep(1.0003,length(calBP))),
                 colour="#e5e5e5", fill="#e41a1c") +
  geom_ridgeline(data=spd_PPN$grid, mapping=aes(x=calBP, height=PrDens,
                                               y=rep(1.0002,length(calBP))),
                 colour="#e5e5e5", fill="#984ea3") +
  geom_ridgeline(data=spd_PN$grid, mapping=aes(x=calBP, height=PrDens,
                                               y=rep(1.0001,length(calBP))),
                 colour="#e5e5e5", fill="#377eb8") +
  geom_ridgeline(data=spd_CH$grid, mapping=aes(x=calBP, height=PrDens,
                                               y=rep(1.0000,length(calBP))),
                 colour="#e5e5e5", fill="#4daf4a") +
  scale_x_reverse(limits=c(20000, 6000),
                  breaks=seq(from=20000, to=6000, by=-1000),
                  minor_breaks=seq(from=20000, to=6000, by=-100)) +
  theme_ridges() +
  theme_poster
ggsave("./analysis/kuarch2018_poster/figures/plot_chron.pdf", width=450, height=120, units="mm")

# Aoristic sum plot
period_spans <- list(period_LP   = 3000000:300001,
                     period_MP   = 300000:50001,
                     period_UP   = 50000:20001,
                     period_EP   = 20000:11501,
                     period_N0   = 11500:10001,
                     period_N1.1 = 10000:9201,
                     period_N1.2 = 9200:8501,
                     period_N2   = 8500:8001,
                     period_N3   = 8000:7501,
                     period_C1   = 7500:7001,
                     period_C2   = 7000:6501,
                     period_C3   = 6500:6001,
                     period_B    = 6000:4001,
                     period_H    = 4001:1)

sites %>%
  transmute_at(vars(period_LP, period_MP, period_UP, period_EP, period_N0,
                    period_N1.1, period_N1.2, period_N2, period_N3,
                    period_C1, period_C2, period_C3, period_B, period_H),
               Vectorize(function(x) {
                 if(x == 'x') return(1)
                 else if(x == '?') return(1)
                 else return(0)
               })) %>%
  divide_by(., rowSums(.)) %>%
  divide_by(., sapply(period_spans, length)) ->
  ao

ao %>%
  na.omit() %>%
  summarise_all(sum) %>%
  t(.) ->
  ao_sums

# Do some clipping here to speed things up
ao_abs <- data.frame(bp = unlist(period_spans[3:13]),
                     p =  unlist(sapply(1:length(period_spans[3:13]), function(n, a, b) {
                       rep(a[n], length(b[[n]])) %>% return()
                     }, a = ao_sums[3:13], b = period_spans[3:13])),
                     period = unlist(sapply(1:length(period_spans[3:13]), function(n, b) {
                       rep(names(b)[n], length(b[[n]])) %>% return()
                     }, b = period_spans[3:13])))

ggplot(ao_abs, aes(x=bp, y=p)) +
  geom_area(colour="#e5e5e5", fill="#333333") +
  scale_x_reverse(limits=c(20000, 6000),
                  breaks=seq(from=20000, to=6000, by=-1000),
                  minor_breaks=seq(from=20000, to=6000, by=-100)) +
  scale_y_reverse() +
  theme_poster
ggsave("./analysis/kuarch2018_poster/figures/plot_ao.pdf", width=450, height=30, units="mm")
