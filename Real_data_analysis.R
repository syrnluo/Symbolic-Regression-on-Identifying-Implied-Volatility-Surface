setwd("/Users/jiayiluo/Dropbox/PHD/symbolic/symbolic_regression")

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(plotly)
library(Metrics)

options<-read_rds("SPX_Option.rds")

options %>% mutate(date=date(date),
                   strike=strike/1000,
                   expiration=date(expiration),
                   time_maturity=as.numeric(expiration-date),
                   time_maturity_rate=time_maturity/365)->options_f

options %>% group_by(call_put,date) %>% 
  summarise(n=length(best_bid)) %>% glimpse()


options_f %>% mutate(biderror=as.numeric(best_bid>best_offer)) %>% 
  filter(biderror==0) %>% 
  group_by(call_put,date) %>% 
  summarise(n=length(best_bid)) %>% glimpse()


options_f %>% filter(best_bid<=best_offer)->options_final

options_final %>% filter(volume>5) %>% 
  group_by(call_put,date) %>% 
  summarise(n=length(best_bid)) %>%
  filter(call_put=='C') %>% 
  arrange(desc(n))

options_final %>% filter(volume>5) ->options_final

options_final %>% glimpse()

options_final %>% group_by(call_put,date) %>% 
  summarise(n=length(best_bid)) %>% 
  arrange(desc(n))

###filtering#####

options_final %>% filter(time_maturity>6,volume>=5,
                         open_interest>=5, best_bid>=3/8,
                         best_bid<=best_offer,best_bid>0,
                         best_offer>0)->options_clean

write.csv(options_clean, "./cleaned_real_new/options_clean_raw.csv")

options_clean<-read.csv("./cleaned_real_new/options_clean_raw.csv")[,-1]

options_clean %>% mutate(date=date(date))->options_clean
##########
axx <- list(
  nticks = 10,
  title = "Strike"
  # range = c(0, 2)
)

axy <- list(
  nticks = 10,
  title = "Time to Maturity (Years)"
  # range = c(0,1)
)

# axz <- list(
#   nticks = 10,
#   title = 'log sigma'
#   # range = c(-2,2)
# )
axz <- list(
  nticks = 8,
  title = "Implied Volatility"
  # range = c(-2,-1)
)

# Margin 
m <- list(
  l = 20,
  r = 20,
  b = 20,
  t = 20,
  pad = 4)
camera_view1 <-  list(eye = list(x = 1.6, y = 1.6, z = 1))  
camera_view2 <-  list(eye = list(x = 1.6, y = -1.6, z = 1))  
camera_view3 <-  list(eye = list(x = -1.6, y = 1.6, z = 1.1))  
camera_view4 <-  list(eye = list(x = -1.6, y = -1.6, z = 1.1))  

fig1<- plot_ly() %>% 
  add_trace(data = options_plot,  x=options_plot$strike, y=options_plot$time_maturity_rate, 
            z=options_plot$implied_volatility,mode= "markers", 
            opacity = .6, marker = list(size = 2),name = 'Observation',
            showlegend = F) %>% 
  add_trace(data = options_plot, x=options_plot$strike, y=options_plot$time_maturity_rate, 
            z=options_plot$implied_volatility, type = 'mesh3d', opacity=0.7, 
            name = 'Surface',intensity = ~implied_volatility,
            # colorscale = list(c(0,'red'),
            #                   c(0.1,'pink'),
            #                   c(0.2, 'green')),
            showlegend = F,reversescale = T) %>% 
  layout(title = "Implied Volatility Surface as of 2022-01-21",
         #font = list(size = 8),
         scene = list(xaxis=axx,yaxis=axy,
                      zaxis=axz, camera = camera_view1),margin = m,
         legend = list(x = 0.9, y = 0.9,Z=0.9))

#####plot########
options_clean %>% filter(date=="2022-01-21",call_put=="P") %>% 
  mutate(implied_vol_percent=implied_volatility*100) %>% 
  filter(implied_vol_percent>0,implied_vol_percent<30)->options_plot

options_plot %>% select(strike,time_maturity_rate,implied_volatility) %>% 
  write.csv(.,"options_plot.csv")



options_plot_reshaped <- options_plot %>%
  pivot_wider(
    names_from = strike,
    values_from = implied_volatility
  ) %>%
  mutate(time_maturity_rate = as.numeric(time_maturity_rate))


plot_ly(
  data = options_plot_reshaped,
  x = ~as.numeric(names(options_plot_reshaped)[-ncol(options_plot_reshaped)]),
  y = ~as.numeric(time_maturity_rate),
  z = ~.``,
  type = "surface",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Implied Volatility Surface as of 2022-01-21",
    font = list(size = 8),
    scene = list(
      xaxis = list(title = "Strike"),
      yaxis = list(title = "Time to Maturity (Years)"),
      zaxis = list(title = "Implied Volatility"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ),
    margin = m,
    autosize = FALSE,
    width = 500,
    height = 500
  ) %>%
  config(
    displayModeBar = FALSE
  )

# plot_ly()%>% 
#   add_trace(data = options_plot,  x=~strike, 
#             y=~time_maturity_rate, 
#             z=~implied_volatility, 
#             intensity = ~implied_volatility,
#             # colorscale = list(c(0,'red'),
#             #                   c(0.1,'green'),
#             #                   c(0.2, 'blue'),
#             #                   c(0.3, 'blue')),
#             type="mesh3d") %>%
#   layout(
#       title = "Implied Volatility Surface as of 2022-01-21",
#       font=list(size = 8),
#       scene = list(
#         xaxis = list(title = "Strike"),
#         yaxis = list(title = "Time to Maturity (Years)"),
#         zaxis = list(title = "Implied Volatility"),
#         camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))),
#         autosize = F, width = 500, height = 500, margin = m
#       )%>% hide_colorbar()
library(reshape2)

options_plot_matrix <- acast(options_plot, time_maturity_rate ~ strike, value.var = "implied_volatility")


# options_plot_matrix <- options_plot %>%
#   select(time_maturity_rate, strike, implied_volatility) %>%
#   spread(strike, implied_volatility) %>%
#   column_to_rownames("time_maturity_rate")

plot_ly(z = ~options_plot_matrix, type = "surface") %>%
  layout(
    title = "Implied Volatility Surface as of 2022-01-21",
    font = list(size = 8),
    scene = list(
      xaxis = list(title = "Strike"),
      yaxis = list(title = "Time to Maturity (Years)"),
      zaxis = list(title = "Implied Volatility"),
      camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
    ),
    autosize = FALSE, width = 500, height = 500, margin = m
  )




# plot_ly(
#   x = as.numeric(colnames(plot_matrix)), 
#   y = as.numeric(rownames(plot_matrix)), 
#   z = plot_matrix
# ) %>% 
#   add_surface() %>%
#   layout(
#     title = "",
#     scene = list(
#       xaxis = list(type = "log", title = "Total observations"),
#       yaxis = list(type = "log", title = "Firm size"),
#       zaxis = list(title = "Median"),
#       camera = list(eye = list(x = 1.95, y = -1.25, z = 1.25))
#     ))
  
##added data###

interest_rate<-read_excel("data_libor_sp500.xlsx",sheet="LIBOR")

underly<-read_excel("data_libor_sp500.xlsx",sheet="SP500idx")

# interest_rate %>% pivot_wider(.,names_from = days,values_from = rate) %>% 
#   mutate(date=date(date)) %>% 
#   janitor::remove_empty(., which = "cols")->wide_rates

underly %>% rename(s_price=close_price) %>% 
  mutate(date=date(date)) %>% 
  right_join(.,options_clean,by="date")->options_full

options_full %>% distinct(date)->distinct_dates

interest_rate %>% mutate(date=date(date))->interest_rate

setdiff(distinct_dates$date,unique(interest_rate$date))

get_rate<-function(i){
  
  date_sep=distinct_dates$date[i]
  
  options_full %>% filter(date==date_sep) %>% 
    select(-bid_low,-ask_high,-open_price,-expiration)->options_sep
  
  interest_rate %>% filter(date==date_sep)->rate_sep
  
  
  options_sep_sep_all<-NULL
  
  for (j in 1:dim(options_sep)[1]){
  
  options_sep_sep<-options_sep[j,]
  
  rate_sep %>% arrange(days) %>% 
    filter(days<=options_sep_sep$time_maturity) %>% 
    tail(1)->rate_lower
  
  rate_sep %>% arrange(days) %>% 
    filter(days>=options_sep_sep$time_maturity) %>% 
    head(1)->rate_upper
  
   if (dim(rate_upper)[1]==0){
     
     rate_sep_sep=rate_lower$rate
     
   }else if(dim(rate_lower)[1]==0){
     
     rate_sep_sep=rate_upper$rate
   }else if (rate_upper$days==rate_lower$days){
     rate_sep_sep=rate_upper$rate
  
  }else{
  
    rate_sep_sep=(rate_lower$rate*(rate_upper$days-options_sep_sep$time_maturity)+
                    rate_upper$rate*(options_sep_sep$time_maturity-rate_lower$days))/
      (rate_upper$days-rate_lower$days)
  }
  
  options_sep_sep %>% mutate(interest_rate=rate_sep_sep) %>% 
    rbind(options_sep_sep_all,.)->options_sep_sep_all
  
  }
  
  return(options_sep_sep_all)
}


# options_test<-bind_rows(lapply(1:10,get_rate))

testFunction <- function (i) {
  return(tryCatch(get_rate(i), error=function(e) NULL))
}

options_all_raw<-lapply(1:dim(distinct_dates)[1], testFunction)

options_all_final<-NULL
for (i in 1:1829){
  
  options_all_final<-rbind(options_all_final,options_all_raw[[i]])
}


options_all_final %>% mutate(abs_moneyness=strike/s_price-1) %>% 
  filter(abs(abs_moneyness)<=0.1,time_maturity<365) %>% 
  mutate(moneyness=strike/(exp(0.01*interest_rate*time_maturity_rate)*s_price),
         adj_moneyness=log(moneyness)/sqrt(time_maturity_rate)) %>% 
  filter(implied_volatility>0)->options_all_f


options_all_f %>% filter(abs(abs_moneyness)<=0.1) ->options_all_f


write.csv(options_all_f, "./cleaned_real_new/options_clean_final.csv")
options_all_f<-read.csv("./cleaned_real_new/options_clean_final.csv")


##check##
options_all_f$implied_volatility %>% summary()

options_all_f%>% filter(date>="2022-01-21",date<="2022-01-27") %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~implied_volatility,
          color = ~as.factor(date), 
          marker = list(size = 3))%>% 
  add_markers(color=~as.factor(date))%>%
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))



options_all_f$implied_volatility %>% summary()



##for options_final##

options_all_f %>% filter(call_put=="P",date=="2022-01-24")->option_sep_1

options_all_f %>% filter(call_put=="P",date=="2022-01-21")->option_sep_2

options_all_f %>% filter(call_put=="P",date=="2022-01-25")->option_sep_3

options_all_f %>% filter(call_put=="P",date=="2022-01-26")->option_sep_4

options_all_f %>% filter(call_put=="P",date=="2022-01-27")->option_sep_5


options_all_f %>% filter(call_put=="P",date=="2021-09-20")->option_sep_6

options_all_f %>% filter(call_put=="P",date=="2022-01-10")->option_sep_7



options_all_f %>% group_by(date,call_put) %>% 
  summarise(n=length(strike)) %>% 
  arrange(desc(n))



# option_sep_1 %>%  mutate(strike=strike/10000) %>% 
#   filter(abs(implied_volatility)<5)->option_sep_1_adj
# 
# option_sep_2%>%  mutate(strike=strike/10000) %>% 
#   filter(abs(implied_volatility)<5)->option_sep_2_adj


# option_sep_1 %>% select(strike,best_bid,best_offer,volume,open_interest,
#                         time_maturity_rate,implied_volatility) %>% 
#   mutate_at(vars(1:6), ~(scale(.) %>% as.vector))->option_sep_1_scale

option_sep_1 %>% mutate(implied_volatility_log=log(implied_volatility)) %>% 
  drop_na(implied_volatility)->option_sep_1_log

option_sep_3 %>% mutate(implied_volatility_log=log(implied_volatility)) %>% 
  drop_na(implied_volatility)->option_sep_3_log

option_sep_2 %>% mutate(implied_volatility_log=log(implied_volatility)) %>% 
  drop_na(implied_volatility)->option_sep_2_log

write.csv(option_sep_1,"./cleaned_real_new/option_sep_1.csv")
#write.csv(option_sep_1_log,"./cleaned_real_new/option_sep_1_log.csv")
#write.csv(option_sep_1_adj,"./cleaned_real_new/option_sep_1_adj.csv")

write.csv(option_sep_2,"./cleaned_real_new/option_sep_2.csv")
#write.csv(option_sep_2_log,"./cleaned_real_new/option_sep_2_log.csv")
#write.csv(option_sep_2_adj,"./cleaned_real_new/option_sep_2_adj.csv")


write.csv(option_sep_3,"./cleaned_real_new/option_sep_3.csv")
#write.csv(option_sep_3_log,"./cleaned_real_new/option_sep_3_log.csv")

write.csv(option_sep_4,"./cleaned_real_new/option_sep_4.csv")
write.csv(option_sep_5,"./cleaned_real_new/option_sep_5.csv")


write.csv(option_sep_6,"./cleaned_real_new/option_sep_6.csv")
write.csv(option_sep_7,"./cleaned_real_new/option_sep_7.csv")


options_all_f%>% filter(date>"2022-01-01",call_put=="P") ->options_p_recent

write.csv(options_p_recent,"./cleaned_real_new/options_p_recent.csv")






#####for sep 1###
option_sep_1<-read.csv("./cleaned_real_new/option_sep_1.csv")

option_sep_1%>% 
  mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_sep1

option_sep_1 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>% 
  nest %>% 
  inner_join(option_lm_sep1) %>% 
  mutate(preds = map2(fit, data, predict)) %>% 
  unnest(preds)->option_lm_sep1_f

option_sep_1 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(option_lm_sep1_f$preds),
         deep_pred=exp(-2*moneyness+sin(moneyness+time_maturity_rate)/
                         (3*time_maturity_rate+
                            (moneyness+2*time_maturity_rate)*sin(moneyness))),
         deep_pred2=cos(moneyness)/(moneyness^2+moneyness+
                                      sin(moneyness-
                                            log(moneyness*log(1/time_maturity_rate)+
                                                  time_maturity_rate*
                                                  abs(moneyness*time_maturity_rate+
                                                  moneyness*log(1/time_maturity_rate))))))->option_sep_1_res



option_sep_1_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","deep_pred","deep_pred2"), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_1_res%>% 
  pivot_longer(.,c("lm_pred","deep_pred","deep_pred2"), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))


###for sep 2 apply lm model##
option_sep_2<-read.csv("./cleaned_real_new/option_sep_2.csv")


option_sep_2%>% 
  mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_sep2

option_sep_2 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>% 
  nest %>% 
  inner_join(option_lm_sep2) %>% 
  mutate(preds = map2(fit, data, predict)) %>% 
  unnest(preds)->option_lm_sep2_f

option_sep_2 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(option_lm_sep2_f$preds),
         deep_pred_adj=time_maturity_rate/(2*time_maturity_rate*exp(exp(adj_moneyness))-
                                         time_maturity_rate),
         deep_pred_adj2=cos(exp(adj_moneyness))/(exp(1)-exp(-cos(adj_moneyness))*
                                                   cos(time_maturity_rate)),
         gp_pred_adj=(adj_moneyness-0.781)*(-0.2938),
         gp_pred=log(exp(-(log(moneyness)/((0.88+time_maturity_rate)*
                                             (0.88+2*time_maturity_rate))))*1.26),
         deep_pred=exp(-moneyness-log(time_maturity_rate)/
                         ((time_maturity_rate/moneyness+
                             exp(moneyness*(moneyness*(time_maturity_rate+log(moneyness))+
                                              2*moneyness+sin(time_maturity_rate)))/moneyness)*
                            (moneyness+time_maturity_rate+cos(moneyness))))*cos(moneyness),
         deep_pred2=cos(moneyness^2)/
           (moneyness*sin(moneyness+3*time_maturity_rate)+moneyness+2*time_maturity_rate),
         gp_pred_log=exp(-1.489-moneyness*exp(moneyness+time_maturity_rate+0.341)),
         deep_pred_log=exp(moneyness^2*cos(moneyness^2*(2*moneyness-
                                                          time_maturity_rate-
                                                          (log(moneyness))^2)*log(moneyness)*
                                             log(time_maturity_rate)-moneyness)-2*moneyness))->option_sep_2_res



option_sep_2_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","deep_pred_adj",
                   "deep_pred_adj2","gp_pred_adj","gp_pred",
                   "deep_pred","deep_pred2",'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~strike, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_2_res%>% 
  pivot_longer(.,c("lm_pred","deep_pred_adj",
                   "deep_pred_adj2","gp_pred_adj",
                   "gp_pred","deep_pred","deep_pred2",
                   'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))


option_sep_2_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","gp_pred",
                   "deep_pred2"), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~strike, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_2_res %>% select(implied_volatility,gp_pred, moneyness,
                            lm_pred, deep_pred2,time_maturity_rate) %>% 
  rename(deep_pred=deep_pred2)->option_sep_2_p




###data sep 3##

option_sep_3<-read.csv("./cleaned_real_new/option_sep_3.csv")

option_sep_3%>% 
  mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_sep3

option_sep_3 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>% 
  nest %>% 
  inner_join(option_lm_sep3) %>% 
  mutate(preds = map2(fit, data, predict)) %>% 
  unnest(preds)->option_lm_sep3_f

option_sep_3 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(option_lm_sep3_f$preds),
         deep_pred=cos(moneyness)/(2*moneyness+2*time_maturity_rate+
                                     (moneyness+time_maturity_rate*
                                        (time_maturity_rate-3*moneyness-
                                           cos(moneyness)))*
                                     (2*time_maturity_rate-cos(moneyness))),
         deep_pred2=exp(moneyness*(moneyness*time_maturity_rate-
                                     log(2*moneyness^2+2*moneyness*time_maturity_rate+
                                           6*time_maturity_rate+
                                           log(moneyness^2*(moneyness+
                                                            time_maturity_rate+cos(moneyness))*
                                                 log(moneyness^2)+3*moneyness+3*time_maturity_rate)))),
         gp_pred=1/moneyness-0.069/exp(0.758)*log(time_maturity_rate)-0.811,
         gp_adj=1/(exp(exp(adj_moneyness/0.829))+exp(adj_moneyness)+
                     time_maturity_rate),
         deep_adj=cos(adj_moneyness)/(adj_moneyness+time_maturity_rate+
                                        exp(time_maturity_rate)+
                                        exp(adj_moneyness+
                                              sin(time_maturity_rate+
                                                    exp(time_maturity_rate)))),
         gp_pred_log=exp(moneyness^2/(log(time_maturity_rate)-0.373-
                                        2*time_maturity_rate)-moneyness^3),
         deep_pred_log=exp(-moneyness+sin(log((exp(exp(moneyness)+1)+
                                                 log(time_maturity_rate^3))))))->option_sep_3_res



option_sep_3_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","deep_pred",
                   "deep_pred2","gp_pred","gp_adj","deep_adj",
                   'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_3_res%>% 
  pivot_longer(.,c("lm_pred","deep_pred","deep_pred2",
                   "gp_pred","gp_adj","deep_adj",
                   'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))


option_sep_3_res %>% select(implied_volatility,gp_pred, moneyness,
                            lm_pred, deep_pred2,time_maturity_rate) %>% 
  rename(deep_pred=deep_pred2)->option_sep_3_p

######
###data sep 4##

option_sep_4<-read.csv("./cleaned_real_new/option_sep_4.csv")

option_sep_4%>% 
  mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_sep4

option_sep_4 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>% 
  nest %>% 
  inner_join(option_lm_sep4) %>% 
  mutate(preds = map2(fit, data, predict)) %>% 
  unnest(preds)->option_lm_sep4_f




option_sep_4 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(option_lm_sep4_f$preds),
         deep_pred=cos(moneyness)/(exp(moneyness)-
                                     (log(moneyness)-log(time_maturity_rate)+
                                        sin(time_maturity_rate))/
                                     (moneyness-time_maturity_rate+exp(moneyness))),
         gp_pred=exp(-(0.174+moneyness+log(0.674*time_maturity_rate+moneyness)/
                         (time_maturity_rate+0.361))),
         gp_adj=1/(exp(0.855*adj_moneyness+exp(0.233))+
                     time_maturity_rate/0.673),
         deep_adj=exp(-exp(time_maturity_rate)+
                        exp(log(time_maturity_rate)/time_maturity_rate)-
                        exp(time_maturity_rate*exp(adj_moneyness)*
                              exp(-time_maturity_rate)-exp(time_maturity_rate)))/
           sin(exp(adj_moneyness)),
         gp_pred_log=exp(moneyness^2/(-0.972*(exp(-0.981/time_maturity_rate)+0.677))-
                           log(time_maturity_rate+0.709)),
         deep_pred_log=exp(-2*moneyness+cos(moneyness)+exp(-time_maturity_rate*exp(exp(1)))*
                             exp(moneyness*exp(-moneyness-time_maturity_rate*exp(1))-
                                   moneyness)))->option_sep_4_res



option_sep_4_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","deep_pred",
                   "gp_pred","gp_adj","deep_adj",
                   'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_4_res%>% 
  pivot_longer(.,c("lm_pred","deep_pred","gp_pred",'gp_adj',"deep_adj",
                   'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))


option_sep_4_res %>% select(implied_volatility,gp_pred, moneyness,
                            lm_pred, deep_pred,time_maturity_rate) ->option_sep_4_p

#####sep5###

option_sep_5<-read.csv("./cleaned_real_new/option_sep_5.csv")

option_sep_5%>% 
  mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_sep5

option_sep_5 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>% 
  nest %>% 
  inner_join(option_lm_sep5) %>% 
  mutate(preds = map2(fit, data, predict)) %>% 
  unnest(preds)->option_lm_sep5_f




option_sep_5 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(option_lm_sep5_f$preds),
         deep_pred=exp(-sin(time_maturity_rate)+((moneyness-time_maturity_rate)^2*
                                                   cos(2*moneyness*(time_maturity_rate+moneyness)))/
                         (moneyness*(moneyness+time_maturity_rate)))*sin(moneyness)*cos(moneyness),
         gp_pred=exp(moneyness/(-0.473)+0.861/(moneyness^2+time_maturity_rate)),
         gp_pred2=0.288*(exp(time_maturity_rate)-time_maturity_rate)/
           (moneyness^2*(moneyness+time_maturity_rate)),
         gp_adj=1/(exp(exp(adj_moneyness))+time_maturity_rate+exp(adj_moneyness)),
         deep_adj=exp(time_maturity_rate*sin(time_maturity_rate))/
           (time_maturity_rate^2+exp(time_maturity_rate+
                                       exp(adj_moneyness*
                                             exp(time_maturity_rate)))+
              sin(exp(adj_moneyness))),
         gp_pred_log=exp(-moneyness^3+moneyness/(log(time_maturity_rate)-
                                                   time_maturity_rate-0.187)),
         deep_pred_log=exp(moneyness*(exp(sin(moneyness+time_maturity_rate+
                                                exp(moneyness)))-sin(moneyness))-
                             moneyness))->option_sep_5_res



option_sep_5_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","deep_pred","gp_pred",
                   "gp_pred2","gp_adj","deep_adj",'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_5_res%>% drop_na() %>% 
  pivot_longer(.,c("lm_pred","deep_pred","gp_pred",
                   "gp_pred2","gp_adj","deep_adj",
                   'gp_pred_log','deep_pred_log'), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))


option_sep_5_res %>% select(implied_volatility,gp_pred2, moneyness,
                            lm_pred, deep_pred,time_maturity_rate) %>% 
  rename(gp_pred=gp_pred2)->option_sep_5_p

##############
###########surface plot##

saveRDS(list("sep2"=option_sep_2_p,
             "sep3"=option_sep_3_p,
             "sep4"=option_sep_4_p,
             "sep5"=option_sep_5_p),"plot_data.rds")


plot_data<-read_rds("plot_data.rds")
  
  
plot_3D_general_curves <- function(data,camera_view, file_path){
  axx <- list(
    nticks = 10,
    title = 'm'
    # range = c(0, 2)
  )
  
  axy <- list(
    nticks = 10,
    title = 'tau'
    # range = c(0,1)
  )
  
  # axz <- list(
  #   nticks = 10,
  #   title = 'log sigma'
  #   # range = c(-2,2)
  # )
  axz <- list(
    nticks = 8,
    title = 'implied volatility'
    # range = c(-2,-1)
  )
  
  # Margin 
  m <- list(
    l = 20,
    r = 20,
    b = 20,
    t = 20,
    pad = 4)
  
  fig1<- plot_ly() %>% 
    add_trace(data = data,  x=data$moneyness, y=data$time_maturity_rate, 
              z=data$implied_volatility,mode= "markers", 
              opacity = .6, marker = list(size = 2),name = 'Observation',
              showlegend = F) %>% 
    add_trace(data = data, x=data$moneyness, y=data$time_maturity_rate, 
              z=data$lm_pred, type = 'mesh3d', opacity=0.4, 
              name = 'Benchmark',showlegend = F) %>% 
    add_trace(data = data, x=data$moneyness, y=data$time_maturity_rate, 
              z=data$gp_pred, type = 'mesh3d',opacity=0.4,
              name = 'GP-SR',showlegend = F) %>% 
    add_trace(data = data, x=data$moneyness, y=data$time_maturity_rate, 
              z=data$deep_pred, type = 'mesh3d', opacity=0.4, 
              name = 'DSR',showlegend = F)%>%
    layout(scene = list(xaxis=axx,yaxis=axy,
                        zaxis=axz, camera = camera_view),margin = m,
            legend = list(x = 0.9, y = 0.9,Z=0.9))
  
   fig1
  
}


# combine_plots<-function{data, plot_title}{
#   
#   axx <- list(
#     gridcolor='rgb(255, 255, 255)',
#     zerolinecolor='rgb(255, 255, 255)',
#     showbackground=TRUE,
#     backgroundcolor='rgb(230, 230,230)'
#   )
#   
#   fig1<-plot_3D_general_curves(data,camera_view = camera_view1)
#   fig2<-plot_3D_general_curves(data,camera_view = camera_view2)
#   fig3<-plot_3D_general_curves(data,camera_view = camera_view3)
#   fig4<-plot_3D_general_curves(data,camera_view = camera_view4)
#   
#   fig <-subplot(fig1, fig2, fig3, fig4) 
#   
#   fig <- fig %>% layout(title = "3D Subplots",
#                         scene = list(domain=list(x=c(0,0.5),y=c(0.5,1)),
#                                      xaxis=axx, yaxis=axx, zaxis=axx,
#                                      aspectmode='cube'),
#                         scene2 = list(domain=list(x=c(0.5,1),y=c(0.5,1)),
#                                       xaxis=axx, yaxis=axx, zaxis=axx,
#                                       aspectmode='cube'),
#                         scene3 = list(domain=list(x=c(0,0.5),y=c(0,0.5)),
#                                       xaxis=axx, yaxis=axx, zaxis=axx,
#                                       aspectmode='cube'),
#                         scene4 = list(domain=list(x=c(0.5,1),y=c(0,0.5)),
#                                       xaxis=axx, yaxis=axx, zaxis=axx,
#                                       aspectmode='cube'))
#   fig
# 
#   }

camera_view1 <-  list(eye = list(x = 1.6, y = 1.6, z = 1))  
camera_view2 <-  list(eye = list(x = 1.6, y = -1.6, z = 1))  
camera_view3 <-  list(eye = list(x = -1.6, y = 1.6, z = 1.1))  
camera_view4 <-  list(eye = list(x = -1.6, y = -1.6, z = 1.1))  

plot_3D_general_curves(data = plot_data$sep5, 
                       camera_view=camera_view1)             



###for sep6###
#####for sep 1###
#option_sep_6<-read.csv("./cleaned_real_new/option_sep_6.csv")

option_sep_6%>% 
  mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_sep6

option_sep_6 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>% 
  nest %>% 
  inner_join(option_lm_sep6) %>% 
  mutate(preds = map2(fit, data, predict)) %>% 
  unnest(preds)->option_lm_sep6_f

option_sep_6 %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(option_lm_sep6_f$preds),
         gp_pred=0.461-0.269-log(moneyness)/moneyness)->option_sep_6_res


option_sep_6_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","gp_pred"), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_sep_1_res%>% 
  pivot_longer(.,c("lm_pred","deep_pred","deep_pred2"), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))



###options_p_recent##
options_p_recent<-read.csv("./cleaned_real_new/options_p_recent.csv")

options_p_recent%>% mutate(index=1) %>% 
  group_by(index) %>% 
  mutate(ln_iv=log(implied_volatility)) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .)) ->option_lm_recent

# options_p_recent %>% mutate(ln_iv=log(implied_volatility),index=1) %>% 
#   group_by(index) %>% 
#   nest %>% 
#   inner_join(option_lm_recent) %>% 
#   mutate(preds = map2(fit, data, predict)) %>% 
#   unnest(preds)->option_lm_recent_f



options_p_recent %>% mutate(ln_iv=log(implied_volatility)) %>% 
  mutate(lm_pred=exp(predict(option_lm_recent$fit[[1]])),
         deep_pred=exp(-2*moneyness+(moneyness*sin(exp(moneyness)))/
                         (time_maturity_rate+sin(time_maturity_rate+moneyness))),
         deep_pred2=exp(-moneyness^2+
                          (moneyness*(-moneyness-time_maturity_rate+cos(moneyness))*
                             exp(-time_maturity_rate)-time_maturity_rate)*
                          exp(-time_maturity_rate)),
         deep_pred_adj=exp((-exp(adj_moneyness)+
                             exp(adj_moneyness-time_maturity_rate-exp(adj_moneyness))*
                             sin(adj_moneyness-exp(time_maturity_rate)))*
                             exp(-exp(adj_moneyness))-exp(adj_moneyness)))->option_recent_res



option_recent_res%>% 
  pivot_longer(.,c("implied_volatility","lm_pred","deep_pred",
                   "deep_pred2","deep_pred_adj"), 
               names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~moneyness, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


option_recent_res%>% 
  pivot_longer(.,c("lm_pred","deep_pred","deep_pred2","deep_pred_adj"), 
               names_to = "type",values_to = "value") %>% 
  group_by(type) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))



###
library(Metrics)
option_sep_1_adj %>% 
  mutate(pred_gp=exp((time_maturity_rate-1.44*strike*exp(strike))/log(strike)),
         pred_deep=exp(strike*log(strike^2*time_maturity_rate)*
                         log(3*strike^2+strike+time_maturity_rate)/strike)-1) ->option_sep_1_adj_res


option_sep_2_adj%>% 
  mutate(pred_gp=(0.835-strike)/(0.126+strike^2)/(2*(exp(strike)+time_maturity_rate+0.851)),
         pred_deep=exp(strike*(-exp(exp(strike))+1/exp(1)+
                                    exp(-(strike^2+time_maturity_rate)/strike)/strike))) ->option_sep_2_adj_res

library(plotly)
option_sep_1_adj_res%>% 
  pivot_longer(.,c("implied_volatility","pred_gp","pred_deep"), names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~strike, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))

option_sep_2_adj_res%>% 
  pivot_longer(.,c("implied_volatility","pred_gp","pred_deep"), names_to = "type",values_to = "value") %>%
  filter(value<5) %>% 
  plot_ly(., x = ~time_maturity_rate, y = ~strike, z = ~value, 
          color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'tau'),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = 'IVS')))


mean(option_sep_1_log$implied_volatility)
rmse(option_sep_1_log_res$implied_volatility,option_sep_1_log_res$pred_1)
rmse(option_sep_1_log_res$implied_volatility,option_sep_1_log_res$pred_2)


mean(option_sep_2_adj$implied_volatility)


rmse(option_sep_2_adj_res$implied_volatility,option_sep_2_adj_res$pred_deep)
rmse(option_sep_2_adj_res$implied_volatility,option_sep_2_adj_res$pred_gp)


#######find functions####
library(nls.multstart) # devtools::install_github('padpadpadpad/nls.multstart')
library(broom)
library(purrr)
library(nlstools)
##function one##
func1<-function(moneyness, time_maturity_rate,c,a,b0,b1,b2){
  res<-c+a*log(moneyness)/(b0+b1*time_maturity_rate+b2*time_maturity_rate^2)
  return(res)
}

func3<-function(moneyness, time_maturity_rate,c0,c1,a0,a1,a2,b0,b1){
  inv_res<-b0+b1*moneyness+a0*log(a1*moneyness+a2*time_maturity_rate)/
    (c0+c1*time_maturity_rate)
  return(inv_res)
}

func4<-function(moneyness, time_maturity_rate,a0,b1,b2,c1){
  inv_res<-a0/(b1*moneyness^2+b2*time_maturity_rate)-c1*moneyness
  return(inv_res)
}


###this is the best###
func4_2<-function(moneyness, time_maturity_rate,a1,a2,b1,b2){
  res<-(a1*exp(time_maturity_rate)-a2*time_maturity_rate)/
    (b1*moneyness^3+b2*moneyness^2*time_maturity_rate+0.000001)
  return(res)
}

func4_2_2<-function(moneyness, time_maturity_rate,a1,a2,b1,b2,c0){
  res<-(a1*exp(time_maturity_rate)-a2*time_maturity_rate)/
    (b1*moneyness^3+b2*moneyness^2*time_maturity_rate+0.000001)+c0
  return(res)
}

# func1_adj<-function(adj_moneyness, time_maturity_rate,a1,a2,b1,b2){
#   res<-(a1*exp(time_maturity_rate)-a2*time_maturity_rate)/
#     (b1*moneyness^3+b2*moneyness^2*time_maturity_rate+0.000001)
#   return(res)
# }

func1_adj<-function(adj_moneyness, time_maturity_rate,a1,a2,b1,b2){
  res<-(a1*exp(time_maturity_rate)-a2*time_maturity_rate)/
    (b1*moneyness^3+b2*moneyness^2*time_maturity_rate+0.000001)
  return(res)
}


func2_adj_deep1<-function(adj_moneyness, time_maturity_rate,a1,a2,b1,b2,b3){
  res<-a1*cos(exp(adj_moneyness))/(b1+a2*exp(b2*cos(adj_moneyness))*
                                  cos(time_maturity_rate)+0.000001)+b3
  return(res)
}



##this is the best one####
func2_adj_deep2<-function(adj_moneyness, time_maturity_rate,a0,a1,a2,a3,b1,b2,b3,c0){
  res<-a1*cos(exp(b1*adj_moneyness))/(a0+a2*exp(a3*cos(b2*adj_moneyness))*
                                     cos(b3*time_maturity_rate))+c0
  return(res)
}

####no constant####
func2_adj_deep2_new<-function(adj_moneyness, time_maturity_rate,a0,a1,a2,a3,b1,b2,b3){
  res<-a1*cos(exp(b1*adj_moneyness))/(a0+a2*exp(a3*cos(b2*adj_moneyness))*
                                        cos(b3*time_maturity_rate))
  return(res)
}

#####
func4_adj<-function(adj_moneyness, time_maturity_rate,b0,b1,a0,a1,a2){
  res<-b1/(exp(a1*adj_moneyness+a0)+a2*time_maturity_rate)+b0
  return(res)
}

####
func5_adj<-function(adj_moneyness, time_maturity_rate,a1,a2,a3,b1,b2,c1,c2){
  res<-c1/(a1*exp(exp(b1*adj_moneyness))+a2*time_maturity_rate+
             a3*exp(b2*adj_moneyness))+c2
  return(res)
}

###log version###
func2_log<-function(moneyness, time_maturity_rate,a0,a1,b0,b1,b2){
  inv_res<-a0+a1*moneyness*exp(b1*moneyness+b2*time_maturity_rate+b0)
  return(inv_res)
}

func3_log<-function(moneyness, time_maturity_rate,a0,a1,a2,a3,b1,b2,c1,c2){
  inv_res<-a1*moneyness^2/(a2*exp(b1/time_maturity_rate)+c1)+
    a3*log(b2*time_maturity_rate+c2)+a0
  return(inv_res)
}

#####lm part#
options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(implied_volatility ~ inv_moneyness+log_t, data = .)) ->option_all_func2


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(ln_iv ~ adj_moneyness+ I(adj_moneyness^2)+time_maturity_rate+
                adj_moneyness*time_maturity_rate, data = .))->option_all_bench

options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = lm(implied_volatility ~ adj_moneyness, data = .))->option_all_func2_adj


#######non-linear part#######
options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~
                           func1(moneyness, time_maturity_rate,
                                 c,a,b0,b1,b2), data = .,
                         iter = 1000,
                         start_lower=c(a=-100, b0=-100, b1=-100,b2=-100,c=-100),
                         start_upper=c(a=100, b0=100, b1=100,b2=100,c=100),
                         supp_errors = "Y"))->option_all_func1


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(ln_iv~func3(moneyness, time_maturity_rate,
                                     c0,c1,a0,a1,a2,b0,b1), data = .,
                         iter = 1000,
                         start_lower=c(a0=-100,a1=-100,a2=-100, b0=-100, 
                                       b1=-100,c0=-100,c1=-100),
                         start_upper=c(a0=100,a1=100,a2=100, b0=100, 
                                       b1=100,c0=100,c1=100),
                         supp_errors = "Y"))->option_all_func3



options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(ln_iv~func4(moneyness, time_maturity_rate,a0,b1,b2,c1),
                         data = .,
                         iter = 1000,
                         start_lower=c(a0=-1, b1=-1,b2=-1,c1=-1),
                         start_upper=c(a0=1, b1=1,b2=1,c1=1),
                         supp_errors = "Y"))->option_all_func4


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func4_2(moneyness, time_maturity_rate,a1,a2,b1,b2),
                         data = .,
                         iter = 1000,
                         start_lower=c(a1=-1,a2=-1, b1=-1,b2=-1),
                         start_upper=c(a1=1,a2=1, b1=1,b2=1),
                         supp_errors = "Y"))->option_all_func4_2


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func4_2_2(moneyness, time_maturity_rate,
                                                      a1,a2,b1,b2,c0),
                         data = .,
                         iter = 1000,
                         start_lower=c(a1=-1,a2=-1,b1=-1,b2=-1,c0=-1),
                         start_upper=c(a1=1,a2=1, b1=1,b2=1,c0=1),
                         supp_errors = "Y"))->option_all_func4_2_2

options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func5_adj(adj_moneyness, time_maturity_rate,
                                                      a1,a2,a3,b1,b2,c1,c2),
                         data = .,
                         iter = 1000,
                         start_lower=c(a1=-1,a2=-1,a3=-1, b1=-1,b2=-1,c1=-1,c2=-1),
                         start_upper=c(a1=1,a2=1,a3=1, b1=1,b2=1,c1=1,c2=1),
                         supp_errors = "Y"))->option_all_func5_adj


# options_all_f %>% filter(call_put=="P") %>% 
#   mutate(inv_moneyness=1/moneyness,
#          log_t=log(time_maturity_rate),
#          ln_iv=log(implied_volatility)) %>% 
#   group_by(date) %>%
#   do(fit = nls_multstart(implied_volatility~func2_adj_deep1(adj_moneyness, time_maturity_rate,
#                                                       a1,a2,b1,b2),
#                          data = .,
#                          iter = 1000,
#                          start_lower=c(a1=-1,a2=-1, b1=-1,b2=-1),
#                          start_upper=c(a1=1,a2=1, b1=1,b2=1),
#                          supp_errors = "Y"))->option_all_func2_adj1
# 
# 
# options_all_f %>% filter(call_put=="P") %>% 
#   mutate(inv_moneyness=1/moneyness,
#          log_t=log(time_maturity_rate),
#          ln_iv=log(implied_volatility)) %>% 
#   group_by(date) %>%
#   do(fit = nls_multstart(implied_volatility~func2_adj_deep2(adj_moneyness, time_maturity_rate,
#                                                            a0,a1,a2,b1,b2,b3,c0),
#                          data = .,
#                          iter = 1000,
#                          start_lower=c(a1=-1,a2=-1,a0=-1, b1=-1,b2=-1,b3=-1,c0=-1),
#                          start_upper=c(a1=1,a2=1,a0=1, b1=1,b2=1,b3=1,c0=1),
#                          supp_errors = "Y"))->option_all_func2_adj2
# 

options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func2_adj_deep1(adj_moneyness, time_maturity_rate,
                                                            a1,a2,b1,b2,b3),
                         data = .,
                         iter = 1000,
                         start_lower=c(a1=-1,a2=-1, b1=-1,b2=-1,b3=-1),
                         start_upper=c(a1=1,a2=1, b1=1,b2=1,b3=1),
                         supp_errors = "Y"))->option_all_func2_adj1_new


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func2_adj_deep2(adj_moneyness, time_maturity_rate,
                                                            a0,a1,a2,a3,b1,b2,b3,c0),
                         data = .,
                         iter = 1000,
                         start_lower=c(a0=-1,a1=-1,a2=-1,a3=-1,b1=-1,b2=-1,b3=-1,c0=-1),
                         start_upper=c(a0=1,a1=1,a2=1,a3=1,b1=1,b2=1,b3=1,c0=1),
                         supp_errors = "Y"))->option_all_func2_adj2_new


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func2_adj_deep2_new(adj_moneyness, time_maturity_rate,
                                                            a0,a1,a2,a3,b1,b2,b3),
                         data = .,
                         iter = 1000,
                         start_lower=c(a0=-1,a1=-1,a2=-1,a3=-1,b1=-1,b2=-1,b3=-1),
                         start_upper=c(a0=1,a1=1,a2=1,a3=1,b1=1,b2=1,b3=1),
                         supp_errors = "Y"))->option_all_func2_adj2_new_2


options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(implied_volatility~func4_adj(adj_moneyness, time_maturity_rate,
                                                      a0,a1,a2,b0,b1),
                         data = .,
                         iter = 1000,
                         start_lower=c(a0=-1,a1=-1,a2=-1, b1=-1,b0=-1),
                         start_upper=c(a0=-1,a1=1,a2=1, b1=1,b0=1),
                         supp_errors = "Y"))->option_all_func4_adj

####log###
options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(ln_iv~func2_log(moneyness, time_maturity_rate,
                                         a0,a1,b0,b1,b2),
                         data = .,
                         iter = 1000,
                         start_lower=c(a0=-1,a1=-1,b0=-1,b1=-1,b2=-1),
                         start_upper=c(a0=1,a1=1,b0=1,b1=1,b2=1),
                         supp_errors = "Y"))->option_all_func2_log



options_all_f %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  group_by(date) %>%
  do(fit = nls_multstart(ln_iv~func3_log(moneyness, time_maturity_rate,
                                         a0,a1,a2,a3,b1,b2,c1,c2),
                         data = .,
                         iter = 1000,
                         start_lower=c(a0=-1,a1=-1,a2=-1,a3=-1,b1=-1,b2=-1,c1=-1,c2=-1),
                         start_upper=c(a0=1,a1=1,a2=1,a3=1,b1=1,b2=1,c1=1,c2=1),
                         supp_errors = "Y"))->option_all_func3_log


saveRDS(list('lm_func1'=option_all_func1,
             'lm_func3'=option_all_func3,
             'lm_func4'=option_all_func4),
        "./cleaned_real_new/real_functions_all.rds")

saveRDS(list('lm_func4_2'=option_all_func4_2,
             'lm_func5_adj'=option_all_func5_adj,
             'lm_func2_adj_deep1'=option_all_func2_adj1,
             'lm_func2_adj_deep2'=option_all_func2_adj2),
        "./cleaned_real_new/real_functions_add.rds")

saveRDS(option_all_func4_2_2,
        "./cleaned_real_new/real_func4_2_2.rds")


saveRDS(list('lm_func4_adj'=option_all_func4_adj,
             'lm_func2_adj_deep1_new'=option_all_func2_adj1_new,
             'lm_func2_adj_deep2_new'=option_all_func2_adj2_new),
        "./cleaned_real_new/real_functions_add2.rds")

saveRDS(option_all_func2_adj2_new_2,
        "./cleaned_real_new/lm_func2_adj_deep2_new_nocons.rds")

saveRDS(option_all_func3_log,
        "./cleaned_real_new/lm_func3_log.rds")

################
real_functions_all<-read_rds("./cleaned_real_new/real_functions_all.rds")
real_functions_add<-read_rds("./cleaned_real_new/real_functions_add.rds")

option_all_func1<-real_functions_all$lm_func1
option_all_func3<-real_functions_all$lm_func3
#option_all_func4<-real_functions_all$lm_func4

option_all_func4_2<-real_functions_add$lm_func4_2 
###this is the best for original 2022-01-27/model 4

option_all_func5_adj<-real_functions_add$lm_func5_adj
option_all_func4_2_2<-read_rds("./cleaned_real_new/real_func4_2_2.rds")


options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_bench) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_bench_f


options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func1) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func1_f



options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func2) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func2_f


options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func3) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func3_f

# options_p_recent %>% filter(call_put=="P") %>% 
#   mutate(inv_moneyness=1/moneyness,
#          log_t=log(time_maturity_rate),
#          ln_iv=log(implied_volatility)) %>%
#   group_by(date) %>%
#   nest %>%
#   inner_join(option_all_func4) %>%
#   mutate(preds = map2(fit, data, predict)) %>%
#   unnest(preds)->option_all_func4_f

options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func4_2) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func4_f_2   ###this is the best###

# options_p_recent %>% filter(call_put=="P") %>% 
#   mutate(inv_moneyness=1/moneyness,
#          log_t=log(time_maturity_rate),
#          ln_iv=log(implied_volatility)) %>%
#   group_by(date) %>%
#   nest %>%
#   inner_join(option_all_func4_2_2) %>%
#   mutate(preds = map2(fit, data, predict)) %>%
#   unnest(preds)->option_all_func4_f_2_2

options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func2_adj) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func2_f_adj  


options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func5_adj) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func5_f_adj

# options_p_recent %>% filter(call_put=="P") %>% 
#   mutate(inv_moneyness=1/moneyness,
#          log_t=log(time_maturity_rate),
#          ln_iv=log(implied_volatility)) %>%
#   group_by(date) %>%
#   nest %>%
#   inner_join(option_all_func2_adj1_new) %>%
#   mutate(preds = map2(fit, data, predict)) %>%
#   unnest(preds)->option_all_func2_f_adj1   ###have NULL##

options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func2_adj2_new) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func2_f_adj2 ###this is the best for func2_adj, full paras

options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func2_adj2_new_2) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func2_f_adj2_new

options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func4_adj) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func4_f_adj
####log###
options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func2_log) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func2_f_log


options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>%
  group_by(date) %>%
  nest %>%
  inner_join(option_all_func3_log) %>%
  mutate(preds = map2(fit, data, predict)) %>%
  unnest(preds)->option_all_func3_f_log



options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  mutate(pred_bench=exp(option_all_bench_f$preds),
         pred_func1=option_all_func1_f$preds,
         pred_func2=option_all_func2_f$preds,
         pred_func3=exp(option_all_func3_f$preds),
         pred_func4=option_all_func4_f_2$preds)->options_p_func_all

options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  mutate(pred_bench=exp(option_all_bench_f$preds),
         pred_func2_adj=option_all_func2_f_adj$preds,
         pred_func5_deep_adj=option_all_func5_f_adj$preds,
         pred_func2_deep_adj=option_all_func2_f_adj2$preds,
         pred_func4_adj=option_all_func4_f_adj$preds)->options_p_func_adj


options_p_recent %>% filter(call_put=="P") %>% 
  mutate(inv_moneyness=1/moneyness,
         log_t=log(time_maturity_rate),
         ln_iv=log(implied_volatility)) %>% 
  mutate(pred_bench=exp(option_all_bench_f$preds),
         pred_func3_log=exp(option_all_func3_f_log$preds))->options_p_func_log

###clean####
options_p_func_all %>%
  rename(Benchmark=pred_bench,Model1=pred_func1,Model2=pred_func2,
         Model3=pred_func3,Model4=pred_func4) %>% 
         #Method2_adj=pred_func2_adj,
         #Method5_adj=pred_func5_adj) %>% 
  pivot_longer(.,Benchmark:Model4, 
               names_to = "Model",values_to = "value") %>% 
  group_by(date,Model) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))->options_p_func_res_all

options_p_func_adj %>%
  rename(Benchmark=pred_bench,Method5=pred_func2_adj,
         Method8=pred_func4_adj,Method7=pred_func2_deep_adj,
         Method6=pred_func5_deep_adj) %>% 
  pivot_longer(.,Benchmark:Method8, 
               names_to = "Method",values_to = "value") %>% 
  group_by(date,Method) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))->options_p_func_res_adj

options_p_func_log %>%
  rename(Benchmark=pred_bench,Method3_log=pred_func3_log) %>% 
  pivot_longer(.,Benchmark:Method3_log, 
               names_to = "Method",values_to = "value") %>% 
  group_by(date,Method) %>% 
  summarise(RMSE=rmse(value,implied_volatility),
            corr=cor(value,implied_volatility))->options_p_func_res_log

####plot#####
options_p_func_res_all %>%  
  pivot_longer(.,RMSE:corr, names_to = "metric",values_to = "value") %>%
  ggplot(., aes(x=metric, y=value, fill=Model)) + 
  geom_boxplot(outlier.size = 0.1)+
  facet_wrap(~metric, scale="free")+
  theme(legend.position="bottom",
        legend.text = element_text(size = 10),
        axis.text=element_text(size=5),
        axis.title=element_text(size=8,face="bold"))

options_p_func_res_adj %>%  
  pivot_longer(.,RMSE:corr, names_to = "metric",values_to = "value") %>%
  ggplot(., aes(x=metric, y=value, fill=Method)) + 
  geom_boxplot(outlier.size = 0.1)+
  facet_wrap(~metric, scale="free")+
  theme(legend.position="bottom",
        legend.text = element_text(size = 6),
        axis.text=element_text(size=5),
        axis.title=element_text(size=8,face="bold"))

 
options_p_func_res_log %>%  
  pivot_longer(.,RMSE:corr, names_to = "metric",values_to = "value") %>%
  ggplot(., aes(x=metric, y=value, fill=Method)) + 
  geom_boxplot(outlier.size = 0.1)+
  facet_wrap(~metric, scale="free")+
  theme(legend.position="bottom",
        legend.text = element_text(size = 6),
        axis.text=element_text(size=5),
        axis.title=element_text(size=8,face="bold"))

###summarise####
options_p_func_res_all %>%  
  pivot_longer(.,RMSE:corr, names_to = "metric",values_to = "value") %>% 
  group_by(Method,metric) %>% 
  summarise(mean=mean(value),
            sd=sd(value),
            q25=quantile(value,0.25),
            median=median(value),
            q75=quantile(value,0.75))

options_p_func_res_adj %>%  
  pivot_longer(.,RMSE:corr, names_to = "metric",values_to = "value") %>% 
  group_by(Method,metric) %>% 
  summarise(mean=mean(value),
            sd=sd(value),
            q25=quantile(value,0.25),
            median=median(value),
            q75=quantile(value,0.75))


######open coeffiecients###

open_coef<-function(object){
  n=dim(object)[1]
  coef<-NULL
  for (i in 1:n){
    coef_sep<-coef(object$fit[[i]])
    
    if (!(is.null(coef_sep))){
    coef<-rbind(coef,coef_sep)
    }
  }
  
return(coef)

}


object_all<-c(option_all_func1,option_all_func3,option_all_func4_2,
              option_all_func5_adj,option_all_func2_adj2_new,
              option_all_func4_adj)


library(xtable)

#####model 1###
coef_func1_raw<-open_coef(option_all_func1)


coef_func1_raw %>% data.frame()%>%
  mutate_at(vars(1:5),funs(remove_outliers))->tt



coef_func1_raw %>% data.frame()%>% 
  pivot_longer(.,c:b2,names_to = "parameters",
               values_to = "estimates")->coef_func1_final



coef_func1_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func1_summary



print(xtable(coef_func1_summary,caption = 'Method 1'), type="latex", 
      file="func1_coef.tex",scalebox='0.75')

# coef_func1_final %>% 
#   ggplot(., aes(x=parameters, y=estimates, fill=parameters)) + 
#   geom_boxplot(outlier.shape = NA)+
#   theme(legend.position="bottom",
#         legend.text = element_text(size = 6),
#         axis.text=element_text(size=5),
#         axis.title=element_text(size=8,face="bold"))+
#   scale_y_continuous(limits = c(-100,100))


#####model 2###
coef_func2_raw<-open_coef(option_all_func2)

coef_func2_raw %>% data.frame()%>% 
  rename(a0=X.Intercept.,a1=inv_moneyness,a2=log_t) %>% 
  pivot_longer(.,a0:a2,names_to = "parameters",
               values_to = "estimates")->coef_func2_final



coef_func2_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func2_summary

print(xtable(coef_func2_summary,caption = 'Method 2'), type="latex", 
      file="func2_coef.tex",scalebox='0.75')

##########model 3###
coef_func3_raw<-open_coef(option_all_func3)

coef_func3_raw %>% data.frame() %>% 
  pivot_longer(.,c0:b1,names_to = "parameters",
               values_to = "estimates") %>% 
  group_by(parameters) %>% 
  arrange(desc(estimates)) %>% 
  slice(100:(n()-100))->coef_func3_final



coef_func3_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func3_summary

print(xtable(coef_func3_summary,caption = 'Method 3'), type="latex", 
      file="func3_coef.tex",scalebox='0.75')


##########model 4###
coef_func4_raw<-open_coef(option_all_func4_2_2)

coef_func4_raw %>% data.frame() %>% 
  pivot_longer(.,a1:c0,names_to = "parameters",
               values_to = "estimates")->coef_func4_final



coef_func4_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func4_summary

print(xtable(coef_func4_summary,caption = 'Method 4'), type="latex", 
      file="func4_coef.tex",scalebox='0.75')



##########model 5###
coef_func5_raw<-open_coef(option_all_func2_adj)

coef_func5_raw %>% data.frame() %>% 
  rename(b0=X.Intercept.,b1=adj_moneyness) %>% 
  pivot_longer(.,b0:b1,names_to = "parameters",
               values_to = "estimates")->coef_func5_final



coef_func5_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func5_summary

print(xtable(coef_func5_summary,caption = 'Method 5'), type="latex", 
      file="func5_coef.tex",scalebox='0.75')

##########model 6###
coef_func6_raw<-open_coef(option_all_func5_adj)

coef_func6_raw %>% data.frame() %>% 
  pivot_longer(.,a1:c2,names_to = "parameters",
               values_to = "estimates") %>% 
  group_by(parameters) %>% 
  arrange(desc(estimates)) %>% 
  slice(20:(n()-20))->coef_func6_final



coef_func6_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func6_summary

print(xtable(coef_func6_summary,caption = 'Method 6'), type="latex", 
      file="func6_coef.tex",scalebox='0.75')


##########model 7###
coef_func7_raw<-open_coef(option_all_func2_adj2_new)

coef_func7_raw %>% data.frame() %>% 
  pivot_longer(.,a0:c0,names_to = "parameters",
               values_to = "estimates")%>% 
  group_by(parameters) %>% 
  arrange(desc(estimates)) %>% 
  slice(10:(n()-10))->coef_func7_final



coef_func7_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func7_summary

print(xtable(coef_func7_summary,caption = 'Method 7'), type="latex", 
      file="func7_coef.tex",scalebox='0.75')


##########model 8##
coef_func8_raw<-open_coef(option_all_func4_adj)

coef_func8_raw %>% data.frame() %>% 
  pivot_longer(.,a0:b1,names_to = "parameters",
               values_to = "estimates")->coef_func8_final



coef_func8_final %>% group_by(parameters) %>% 
  summarise(quantile_10=quantile(estimates,0.10),
            quantile_25=quantile(estimates,0.25),
            median=median(estimates),
            mean=mean(estimates),
            quantile_75=quantile(estimates,0.75),
            quantile_90=quantile(estimates,0.90))->coef_func8_summary

print(xtable(coef_func8_summary,caption = 'Method 8'), type="latex", 
      file="func8_coef.tex",scalebox='0.75')
