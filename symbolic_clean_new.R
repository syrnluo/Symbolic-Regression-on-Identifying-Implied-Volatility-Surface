
library(tidyverse)

sym_1<-function(x1){
  
  res<-7*(x1-1.1)^2-1.6
  return(res)
}


sym_2<-function(x1,x2){
  
  res<-(7+x2)*(x1-1.1*x2)^2-1.6
  return(res)
}

sym_2_new<-function(x1,x2){
  
  res<-0.4*(1/x2*log(2-x1)+x2)^2-1
  return(res)
}

sym_2_new2<-function(x1,x2){
  
  res<-0.4*(1/x2*log(2-x1)+x2)^2-1
  return(res)
}

sym_2_new3<-function(x1,x2){
  
  res<-0.4*(1/x2*log(2-x1)+1/x2)^2-1
  return(res)
}

sym_3<-function(x1,x2,error){
  
  res<-(x1-1)/x2*log(x1)-x1+rnorm(1,0,error)
  return(res)
}

sym_4<-function(x1,x2,error){
  
  res<-sin(x1)+sin(x2^2+1)+rnorm(1,0,error)
  return(res)
}


sym_new_t<-function(error,n){
  set.seed(1000+n)
  x1=runif(1,0.5,1.5)
  x2=runif(1,0.1,0.9)
  res<-NULL
  for (t in 1:1500){
  res_sep<-(x1-1)/x2*log(x1)-x1+sqrt(t/365)+rnorm(1,0,error)
  res_sep %>% data.frame() %>% rename(y=".") %>% 
    mutate(x3=sqrt(t/365))->res_s
  res<-rbind(res,res_s)
  }
  res %>% mutate(x1=x1,x2=x2) %>% select(x1,x2,x3,y)->res_f
  return(res_f)
}

sym_new_0<-bind_rows(lapply(1:150,sym_new_t,error=0))
sym_new_error1<-bind_rows(lapply(1:150,sym_new_t,error=0.01))
sym_new_error2<-bind_rows(lapply(1:150,sym_new_t,error=0.5))

# write.csv(sym_new_0, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3par_0_new.csv",
#           row.names = F, col.names = T)

write.csv(sym_new_error1, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3par_error1.csv",
          row.names = F, col.names = T)

write.csv(sym_new_error2, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3par_error2.csv",
          row.names = F, col.names = T)


sym_new_error1<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3par_error1.csv")
sym_new_error0<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3par_0.csv")



#set.seed(1000)

sym_5<-function(x1,x2,error){
  res<-exp(x1)+x2^2+rnorm(1,0,error)
  return(res)
}

#
sym_6<-function(x1,x2,error){
  res<-x1+x1^2+x2+x1*x2+rnorm(1,0,error)
  return(res)
}
#
sym_7<-function(x1,x2,error){
  res<-log(x1)/sqrt(x2)+rnorm(1,0,error)
  return(res)
}


simu_function_all<-function(func,error,n,x1_upper,x1_lower,
                            x2_upper,x2_lower,rep){
  
  set.seed(sample(seq(1:10000000),1)+20*rep)
  
  x1_all<-runif(n,min=x1_lower,max=x1_upper)
  x2_all<-runif(n,min=x2_lower,max=x2_upper)

  x1_all %>% data.frame() %>% rename(x1=".") %>% 
    mutate(x2=x2_all,y_true=func(x1=x1,x2=x2,error = error),
           n=n,error=error,rep=rep) ->res
    
  return(res)  
  
}


error_all<-c(0,0.005,0.01,0.05,0.1)
n_all<-c(5000,4000,3000,2000,1000)

error_all %>% data.frame() %>% rename(error=".") %>% 
  mutate(n=3000) ->info_1

n_all %>% data.frame() %>% rename(n=".") %>% 
  mutate(error=0.01) %>% 
  slice(-3)->info_2

info_all<-rbind(info_1,info_2)

####simu1#####
simu1_all<-NULL

for (i in 1:100){

   simu1_sep_1<-bind_rows(lapply(info_1$error,simu_function_all, n=3000,
                                 func=sym_3,x1_lower=0.8,x1_upper=1.2,
                                 x2_lower=0.1,x2_upper=0.5,rep=i))

   simu1_sep_2<-bind_rows(lapply(info_2$n,simu_function_all, error=0.01,
                                 func=sym_3,x1_lower=0.8,x1_upper=1.2,
                                 x2_lower=0.1,x2_upper=0.5,rep=i))

   simu1_sep<-rbind(simu1_sep_1,simu1_sep_2)
   
   simu1_all<-rbind(simu1_all,simu1_sep)

}



p<-simu1_all %>% filter(n==4000,rep==20) %>%
    plot_ly(., x = ~x1, y = ~x2, z = ~y_true,
          marker = list(size = 3))%>%
    layout(scene = list(xaxis = list(title = TeX("\\tau")),
                      yaxis = list(title = 'k'),
                      zaxis = list(title = expression(ln(sigma^2)))))

p<- p%>% config(mathjax = 'cdn')


write.csv(simu1_all, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_1_all.csv",
          row.names = F, col.names = T)

simu1_all<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_1_all.csv")

simu1_all %>% group_by(n,error) %>% 
  summarise(n_rep=length(unique(rep))) %>% 
  arrange(n,error)

####simu2#####
simu2_all<-NULL

for (i in 1:100){
  
  simu2_sep_1<-bind_rows(lapply(info_1$error,simu_function_all, n=3000,
                                func=sym_4,x1_lower=0,x1_upper=1,
                                x2_lower=0,x2_upper=1,rep=i))
  
  simu2_sep_2<-bind_rows(lapply(info_2$n,simu_function_all, error=0.01,
                                func=sym_4,x1_lower=0,x1_upper=1,
                                x2_lower=0,x2_upper=1,rep=i))
  
  simu2_sep<-rbind(simu2_sep_1,simu2_sep_2)
  
  simu2_all<-rbind(simu2_all,simu2_sep)
  
}

write.csv(simu2_all, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_2_all.csv",
          row.names = F, col.names = T)

simu2_all<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_2_all.csv")

####simu3#####
simu3_all<-NULL

for (i in 1:100){
  
  simu3_sep_1<-bind_rows(lapply(info_1$error,simu_function_all, n=3000,
                                func=sym_5,x1_lower=-3,x1_upper=3,
                                x2_lower=-3,x2_upper=3,rep=i))
  
  simu3_sep_2<-bind_rows(lapply(info_2$n,simu_function_all, error=0.01,
                                func=sym_5,x1_lower=-3,x1_upper=3,
                                x2_lower=-3,x2_upper=3,rep=i))
  
  simu3_sep<-rbind(simu3_sep_1,simu3_sep_2)
  
  simu3_all<-rbind(simu3_all,simu3_sep)
  
}

write.csv(simu3_all, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_3_all.csv",
          row.names = F, col.names = T)

simu3_all<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_3_all.csv")

####simu4#####
simu4_all<-NULL

for (i in 1:100){
  
  simu4_sep_1<-bind_rows(lapply(info_1$error,simu_function_all, n=3000,
                                func=sym_6,x1_lower=-1,x1_upper=1,
                                x2_lower=0.1,x2_upper=1,rep=i))
  
  simu4_sep_2<-bind_rows(lapply(info_2$n,simu_function_all, error=0.01,
                                func=sym_6,x1_lower=-1,x1_upper=1,
                                x2_lower=0.1,x2_upper=1,rep=i))
  
  simu4_sep<-rbind(simu4_sep_1,simu4_sep_2)
  
  simu4_all<-rbind(simu4_all,simu4_sep)
  
}

write.csv(simu4_all, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_4_all.csv",
          row.names = F, col.names = T)

simu4_all<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_4_all.csv")

####simu5#####
simu5_all<-NULL

for (i in 1:100){
  
  simu5_sep_1<-bind_rows(lapply(info_1$error,simu_function_all, n=3000,
                                func=sym_7,x1_lower=0.8,x1_upper=1.2,
                                x2_lower=0.1,x2_upper=1,rep=i))
  
  simu5_sep_2<-bind_rows(lapply(info_2$n,simu_function_all, error=0.01,
                                func=sym_7,x1_lower=0.8,x1_upper=1.2,
                                x2_lower=0.1,x2_upper=1,rep=i))
  
  simu5_sep<-rbind(simu5_sep_1,simu5_sep_2)
  
  simu5_all<-rbind(simu5_all,simu5_sep)
  
}

write.csv(simu5_all, file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_5_all.csv",
          row.names = F, col.names = T)

simu5_all<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/simulation_5_all.csv")

######################3
rnorm(40000,1,0.5) %>% data.frame() %>% 
  rename(x1=".") %>% 
  filter(x1<1.2,x1>0.8) %>% 
  slice(1:5000) %>% 
  mutate(y=sym_1(x1)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_1.csv",
            row.names = F, col.names = T)


x1_raw=rnorm(40000,1,0.5)
x2_raw=rnorm(40000,0.4,0.2)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_5(x1=k,x2=tau,error=0)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_5.csv",
            row.names = F, col.names = F)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_5(x1=k,x2=tau,error=0.1)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_5_error1.csv",
            row.names = F, col.names = F)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_5(x1=k,x2=tau,error=0.01)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_5_error2.csv",
            row.names = F, col.names = F)


cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_5(x1=k,x2=tau,error=0.005)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_5_error3.csv",
            row.names = F, col.names = F)



cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_4(x1=k,x2=tau,error=0.1)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_new_error1.csv",
            row.names = F, col.names = F)


cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_4(x1=k,x2=tau,error=0.01)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_new_error2.csv",
            row.names = F, col.names = F)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_4(x1=k,x2=tau,error=0.005)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_new_error3.csv",
            row.names = F, col.names = F)



cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_4(x1=k,x2=tau,error=0)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_new.csv",
              row.names = F, col.names = F)


cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_4(x1=k,x2=tau,error=0.01)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_error2.csv",
            row.names = F, col.names = F)



cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_3(x1=k,x2=tau,error=0.1)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3_error1.csv",
            row.names = F, col.names = F)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_3(x1=k,x2=tau,error=0.01)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3_error2.csv",
            row.names = F, col.names = F)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_3(x1=k,x2=tau,error=0.005)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3_error3.csv",
            row.names = F, col.names = F)



cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_2_new(x1=k,x2=tau)) %>% 
  write.csv(., file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_2_new.csv",
            row.names = F, col.names = F)


test_data_1<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_1.csv")
test_data_2<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_2.csv")

test_data_3<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3.csv")
test_data_3_e1<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3_error1.csv")
test_data_3_e2<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_3_error2.csv")


test_data_4<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_new.csv")
test_data_4_e1<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_4_new_error1.csv")

test_data_5<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_5.csv")
test_data_5_e1<-read.csv("/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_5_error1.csv")


cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_2_new3(x1=k,x2=tau))->test_data_new

###check shape###
test_data_3 %>% filter(tau>0.35,tau<0.36) %>% 
  filter(k<1.1) %>% 
  ggplot(.,aes(x=k,y=y))+geom_point()

cos_tan<-function(x){
  return(cos(cos(tan(x))))
}

# inverse <- function(f, lower, upper){
#   function(y){
#     uniroot(function(x){f(x) - y}, lower = lower, upper = upper, tol=1e-3)[1]
#   }
# }

#inv_cos_tan<-inverse(cos_tan, lower=-20,upper=20)

inv_cos_tan <- function(x){
  res<-acos(cos(tan(x)))
  return(res)
}


# func_sin1<-function(x){
#   res<-sqrt(0.711/(x*cos_tan(x))+sqrt(inv_cos_tan(x)+tan(x)))-(cos_tan(x)+2*x)
#   return(res)
# }

func_sin1<-function(x){
  res<-sqrt(0.711/(x*cos_tan(x))+sqrt((cos_tan(x))^(-1)+tan(x)))-(cos_tan(x)+2*x)
  return(res)
}

# func_sin2<-function(x1, x2){
#   res<-0.94*x1^4+1.74*x1^3-3.14*x1^2-4.655*x1^2*x2+8.34*x1*x2-x2*x1^3+3*x2^2*x1-5.34*x2^2
#   return(res)
# }

func_sin2<-function(x0,x1){
  res=(3*x0-2.228*x1-1.34-x1/x0)*((x0+1.228*x1)/(x1+0.248))
  return(res)
}

func_sin2_s<-function(x0,x1){
  res=(3*x0^2*x1-14*(x1-0.335)+x0^3)*(x0^2-x1)
  return(res)
}

func_sin2_new<-function(x0,x1){
  res=abs(log(x0)*0.382/x1*x0*(0.15-x0)^2)-x0
  return(res)
}

func_sin2_new2<-function(x0,x1){
  res=abs(log(x0)*0.825/x1*(0.615-x0))-x0
  return(res)
}

func_sin2_new_deep<-function(x0,x1){
  res=abs((x1+0.261/x1*(x0-x1))*log(x0)*(x0-0.16))-x0
  return(res)
}

func_sin2_new_deep2<-function(x0,x1){
  res=abs(log(x0/x1)*(1-x0))-x0
  return(res)
}

func_sin3_error<-function(x0,x1){
  res=log(x0)*log(x0)/x1-x0
  return(res)
}

func_sin4_error<-function(x0,x1){
  res=sin(x0)+sin(1/(cos(tan(tan(tan(x1))))))
  return(res)
}

func_sin5_error<-function(x0,x1){
  res=exp(x0)+0.656*x1
  return(res)
}

func_sin3_deep<-function(x0,x1){
  res=log(x0)^2*(sin(x0)+sin(x1))/x1-x0
  return(res)
}

func_sin4_deep<-function(x0,x1){
  res=sin(x0)+sin(exp(x1^2*(x1*(1-x1)+x1)))
  return(res)
}

func_sin5_deep01<-function(x0,x1){
  res=exp(x0)+x1^2/(1+1/(exp(2*x0+exp(1))-x1))
  return(res)
}  ##0.01

func_sin5_deep005<-function(x0,x1){
  res=exp(x0)+x1^2*sin(x0+x1-log(x0))
  return(res)
}  ##0.005

func_sin5_deep<-function(x0,x1){
  res=exp(x0)+x1/2*cos(log(x1))
  return(res)
}  ##0.1

test_data_3_e1 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         gp_pred=func_sin3_error(x0,x1),
         deep_pred=func_sin3_deep(x0,x1))->simu1_all_p




test_data_4_e1 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         gp_pred=func_sin4_error(x0,x1),
         deep_pred=func_sin4_deep(x0,x1))->simu2_all_p

test_data_5_e1 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         gp_pred=func_sin5_error(x0,x1),
         deep_pred1=func_sin5_deep005(x0,x1),
         deep_pred2=func_sin5_deep01(x0,x1),
         deep_pred3=func_sin5_deep(x0,x1))->simu3_all_p

saveRDS(list("simu1"=simu1_all_p,
             "simu2"=simu2_all_p,
             "simu3"=simu3_all_p),"./symbolic_regression/plot_data_simu.rds")


plot_data_simu<-read_rds("./symbolic_regression/plot_data_simu.rds")


plot_data_simu$simu1 %>% pivot_longer(.,gp_pred:deep_pred, names_to = "type",
                             values_to ="pred") %>% 
  group_by(type) %>% 
  summarise(rmse=rmse(y,pred))

plot_data_simu$simu1$y %>% sd()/(plot_data_simu$simu1$y %>% mean())

plot_data_simu$simu2 %>% pivot_longer(.,gp_pred:deep_pred, names_to = "type",
                                      values_to ="pred") %>% 
  group_by(type) %>% 
  summarise(rmse=rmse(y,pred))

plot_data_simu$simu2$y %>% sd()/(plot_data_simu$simu2$y %>% mean())

plot_data_simu$simu3 %>% pivot_longer(.,gp_pred:deep_pred3, names_to = "type",
                                      values_to ="pred") %>% 
  group_by(type) %>% 
  summarise(rmse=rmse(y,pred))

#####surface plot##########
plot_3D_general_curves <- function(data,camera_view){
  axx <- list(
    nticks = 10,
    title = 'k'
    # range = c(0, 2)
  )
  
  axy <- list(
    nticks = 10,
    title = 't'
    # range = c(0,1)
  )
  
  # axz <- list(
  #   nticks = 10,
  #   title = 'log sigma'
  #   # range = c(-2,2)
  # )
  axz <- list(
    nticks = 8,
    title = 'IV'
    # range = c(-2,-1)
  )
  
  # Margin 
  m <- list(
    l = 10,
    r = 10,
    b = 10,
    t = 10,
    pad = 4)
  
  fig1<- plot_ly() %>% 
    add_trace(data = data,  x=data$x0, y=data$x1, 
              z=data$y,type= "mesh3d", 
              opacity = .6, name = 'True') %>% 
    add_trace(data = data, x=data$x0, y=data$x1, 
              z=data$gp_pred,type= "scatter3d", mode = 'markers', 
              opacity=0.4, marker = list(size = 2), name = 'GP-SR') %>% 
    add_trace(data = data, x=data$x0, y=data$x1, 
              z=data$deep_pred3,type= "scatter3d", mode = 'markers', 
              opacity=0.3, marker = list(size = 2), name = 'DSR-0.005')%>%
    add_trace(data = data, x=data$x0, y=data$x1, 
              z=data$deep_pred1,type= "scatter3d", mode = 'markers', 
              opacity=0.3, marker = list(size = 2), name = 'DSR-0.01') %>% 
    add_trace(data = data, x=data$x0, y=data$x1, 
              z=data$deep_pred2,type= "scatter3d", mode = 'markers', 
              opacity=0.3, marker = list(size = 2), name = 'DSR-0.1') %>% 
    layout(scene = list(xaxis=axx,yaxis=axy,
                        zaxis=axz, camera = camera_view),margin = m,
           legend = list(x = 0.9, y = 0.9,Z=0.9))
  
  hide_legend(fig1)
  
}

camera_view1 <-  list(eye = list(x = 1.6, y = 1.6, z = 1))  
camera_view2 <-  list(eye = list(x = 1.6, y = -1.6, z = 1))  
camera_view3 <-  list(eye = list(x = -1.6, y = 1.6, z = 1.1))  
camera_view4 <-  list(eye = list(x = -1.6, y = -1.6, z = 1.1))  

plot_3D_general_curves(data = plot_data_simu$simu1, 
                       camera_view=camera_view3)             



plot_data_simu$simu1 %>% select(x0,x1,y) ->simu1

simu1_plot<- plot_ly() %>%
  add_trace(data=simu1,x=simu1$x0, y=simu1$x1, z=simu1$y,
            "mesh3d", size=2,
            opacity = .6, name = 'True') %>% 
  layout(scene = list(xaxis = list(title = 'k'), 
                      yaxis = list(title = "t"),
                      zaxis = list(title = "y")))


simu1_plot



test_data_2 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         y_pred=func_sin2(x0,x1),
         y_pred_s=func_sin2_s(x0,x1)) %>%
  select(index,x0,x1,y,y_pred,y_pred_s) %>% 
  pivot_longer(.,y:y_pred_s,names_to="type",values_to="value") %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_line()

####new simu
test_data_2_new %>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin2_new(x0,x1),
         y_pred_deep=func_sin2_new_deep2(x0,x1)) %>% 
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  filter(x1>0.35,x1<0.36) %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point(alpha=0.6)

test_data_3 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         y_pred=func_sin3_error(x0,x1),
         y_pred_deep=func_sin3_deep(x0,x1),
         tau_type=ifelse((x1<0.25)&(x1>0.24),"1st",
                         ifelse((x1<0.34)&(x1>0.33),"median",
                                ifelse((x1<0.42)&(x1>0.41),"3rd",NA)))) %>%
  drop_na(tau_type) %>% 
  arrange(tau_type) %>% 
  select(index,x0,x1,y,y_pred,y_pred_deep,tau_type) %>% 
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  mutate(tau_type= fct_reorder(tau_type, x1)) %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point(alpha=0.6)+
  facet_grid(.~tau_type)+
  xlab("K") +
  ylab("Implied Volatility") 


test_data_4 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         y_pred=func_sin4_error(x0,x1),
         y_pred_deep=func_sin4_deep(x0,x1),
         tau_type=ifelse((x1<0.25)&(x1>0.24),"1st",
                         ifelse((x1<0.34)&(x1>0.33),"median",
                                ifelse((x1<0.42)&(x1>0.41),"3rd",NA)))) %>%
  drop_na(tau_type) %>% 
  arrange(tau_type) %>% 
  select(index,x0,x1,y,y_pred,y_pred_deep,tau_type) %>% 
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  mutate(tau_type= fct_reorder(tau_type, x1)) %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point(alpha=0.6)+
  facet_grid(.~tau_type)+
  xlab("x1") +
  ylab("y") 

test_data_5%>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         y_pred=func_sin5_error(x0,x1),
         y_pred_deep01=func_sin5_deep01(x0,x1),
         y_pred_deep005=func_sin5_deep005(x0,x1),
         y_pred_deep=func_sin5_deep(x0,x1),
         tau_type=ifelse((x1<0.25)&(x1>0.24),"1st",
                         ifelse((x1<0.34)&(x1>0.33),"median",
                                ifelse((x1<0.42)&(x1>0.41),"3rd",NA)))) %>%
  drop_na(tau_type) %>% 
  arrange(tau_type) %>% 
  select(index,x0,x1,y,y_pred,y_pred_deep01,y_pred_deep005,y_pred_deep,tau_type) %>% 
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  mutate(tau_type= fct_reorder(tau_type, x1)) %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point(alpha=0.6)+
  facet_grid(.~tau_type)+
  xlab("x1") +
  ylab("y") 




test_data_4_e1 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         y_pred=func_sin4_error1(x0,x1)) %>%
  select(index,x0,x1,y,y_pred) %>% 
  pivot_longer(.,y:y_pred,names_to="type",values_to="value") %>% 
  filter(x1>0.25,x1<0.26) %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point(alpha=0.6)


library(plotly)
test_data_3 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin3_error(x0,x1),
         y_pred_deep=func_sin3_deep(x0,x1)) %>%
  filter(x1>0.2,x1<0.5) %>% 
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  plot_ly(., x = ~x0, y = ~x1, z = ~value, color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'k'),
                   yaxis = list(title = 'tau'),
                   zaxis = list(title = 'IVS')))

test_data_4 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin4_error(x0,x1),
         y_pred_deep=func_sin4_deep(x0,x1)) %>%
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  plot_ly(., x = ~x0, y = ~x1, z = ~value, color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y')))

test_data_5 %>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin5_error(x0,x1),
         y_pred_deep005=func_sin5_deep005(x0,x1),
         y_pred_deep01=func_sin5_deep01(x0,x1),
         y_pred_deep=func_sin5_deep(x0,x1)) %>%
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  plot_ly(., x = ~x0, y = ~x1, z = ~value, color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y')))



test_data_3 %>%
  rename(x0=k,x1=tau) %>% 
  filter(x1>0.2,x1<0.4,x0<1.1) %>% 
  plot_ly(., x = ~x0, y = ~x1, z = ~y,  
          marker = list(size = 3))%>% 
  layout(scene = list(xaxis = list(title = 'k'),
                      yaxis = list(title = 'tau'),
                      zaxis = list(title = 'IVS')))


test_data_3_e1%>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin3_error1(x0,x1)) %>% 
  filter(x1>0.2,x1<0.4) %>% 
    pivot_longer(.,y:y_pred,names_to="type",values_to="value") %>% 
    plot_ly(., x = ~x0, y = ~x1, z = ~value, color = ~type, 
            marker = list(size = 3))%>% 
    add_markers(color=~type)%>% 
    layout(scene = list(xaxis = list(title = 'k'),
                        yaxis = list(title = 'tau'),
                        zaxis = list(title = 'IVS')))

test_data_4_e1%>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin4_error1(x0,x1)) %>% 
  filter(x1>0.2,x1<0.4) %>% 
  pivot_longer(.,y:y_pred,names_to="type",values_to="value") %>% 
  plot_ly(., x = ~x0, y = ~x1, z = ~value, color = ~type, 
          marker = list(size = 3))%>% 
  add_markers(color=~type)%>% 
  layout(scene = list(xaxis = list(title = 'k'),
                      yaxis = list(title = 'tau'),
                      zaxis = list(title = 'IVS')))





fig <- fig %>% add_markers(color=~type)
fig <- fig %>% layout(scene = list(xaxis = list(title = 'k'),
                                   yaxis = list(title = 'tau'),
                                   zaxis = list(title = 'deepsymbolic')))

fig




test_data_2_new %>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin2_new2(x0,x1)) %>% 
  pivot_longer(.,y:y_pred,names_to="type",values_to="value") %>% 
  filter(x1>0.22,x1<0.23) %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point(alpha=0.6)
######


test_data_2_new %>%
  rename(x0=k,x1=tau) %>% 
  mutate(index=row_number(),
         y_pred=func_sin2_new2(x0,x1),
         y_pred_deep=func_sin2_new_deep2(x0,x1))->test_data_2_all 

test_data_2_new %>%
  rename(x0=k,x1=tau) %>% 
  mutate(y_pred=func_sin2_new(x0,x1),
         y_pred_deep=func_sin2_new_deep2(x0,x1)) %>% 


test_data_2_all %>% filter(x1>0.22,x1<0.23) %>% 
write.csv(.,file = "/Users/jiayiluo/Dropbox/PHD/symbolic/simulations/symbolic_sim_2_all.csv",
          row.names = F, col.names = F)

test_data_2_all %>%
  filter(x1>0.22,x1<0.23) %>% 
  select(index,x0,x1,y,y_pred,y_pred_deep) %>% 
  pivot_longer(.,y:y_pred_deep,names_to="type",values_to="value") %>% 
  ggplot(.,aes(x=x0,y=value,color=type))+geom_point()





rmse(test_data_2_all$y,test_data_2_all$y_pred)
rmse(test_data_2_all$y,test_data_2_all$y_pred_s)


################################
test_y<-function(a,b){
  return(rmse(test_data$y,func_sin2(x=test_data$x1,a=a,b=b)))
}

para<-expand.grid(a=seq(-0.03,0.03,by=0.01),b=seq(-0.03,0.03,by=0.01))
tt<-mapply(test_y,a=para$a,b=para$b)

test_data %>% mutate(y_pred=-1.247+0.862/x1-x1) %>% 
  pivot_longer(.,y:y_pred,names_to="type",values_to="value") %>% 
  ggplot(.,aes(x=x1,y=value,color=type))+geom_line()

test_data %>% mutate(y_pred=-1.247+0.862/x1-x1)->tt2
test_data %>% mutate(y_pred=func_sin1(x1)) ->tt

rmse(tt$y,tt$y_pred)

tt2 %>% mutate(y_pred2=-1.3796525*x1)->tt2

plot(tt2$y_pred,tt2$y_pred2)


###two variables###
sym_sin_2<-function(k,tau){
  res<- 0.3*sin(5*pi/2*(k*tau))-exp(k+tau)
  return(res)
}


x1_raw=rnorm(40000,1,0.5)
x2_raw=rnorm(40000,0.4,0.2)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(k=x1_raw,tau=x2_raw) %>% 
  mutate(y=sym_sin_2(k=k,tau=tau)) %>% 
  write.table(., file = "/Users/LJY/example_data/symboic_test_2var_new.txt", sep = " ",
              row.names = F, col.names = F)

cbind(x1_raw,x2_raw) %>% data.frame() %>% 
  filter(x1_raw<1.2,x1_raw>0.8,x2_raw<0.5,x2_raw>0.1) %>% 
  slice(1:5000) %>% 
  rename(x0=x1_raw,x1=x2_raw) %>% 
  mutate(y=sym_sin_2(k=x0,tau=x1)) %>% 
  write.csv(., file = "/Users/LJY/example_data/symboic_test_2var_new.csv",
            row.names = F, col.names = T)
  


simu_var2<-read.csv("/Users/LJY/example_data/symboic_test_2var_new.csv")

gp_var2<-function(k,tau){
  
  res<-tan(-(k+tau))
  
  return(res)
}



gp_var2_1<-function(k,tau){
  
  tau_1<-tan(tan(tau))
  res<-cos(tau_1+k+tau_1)
  
  return(res)
}

gp_var2_2<-function(k,tau){
  
  res<-cos(k)-k*tau/0.468
  
  return(res)
}

gp_var2_2(1,1)
gp_var2(1,1)

gp_var2_3<-function(k,tau){
  
  res<-sqrt(0.243-log(tau))-(k+tau)
  
  return(res)
}

gp_var2_4<-function(k,tau){
  
  res<-1/k-tau-tan(tau+0.359)
  
  return(res)
}

simu_var2 %>% mutate(y_pred_1=gp_var2(k=x0,tau=x1),
                     y_pred_2=gp_var2_2(k=x0,tau=x1)) %>% 
  pivot_longer(.,y:y_pred_2,names_to="type",values_to="value") %>% 
  ggplot(.,aes(x=x1,y=value,color=type))+geom_line()

simu_var2 %>% mutate(y_pred_1=gp_var2(k=x0,tau=x1),
                     y_pred_2=gp_var2_2(k=x0,tau=x1),
                     #y_pred_3=gp_var2_3(k=x0,tau=x1),
                     y_pred_4=gp_var2_4(k=x0,tau=x1)) ->tt

tt%>% slice(1:1000) %>% 
  ggplot(.,aes(x=y,y=y_pred_4))+geom_point(alpha=0.4)

rmse(tt$y,tt$y_pred_1)
rmse(tt$y,tt$y_pred_2)
rmse(tt$y,tt$y_pred_3)
rmse(tt$y,tt$y_pred_4)


simu_var2 %>% filter(y>0,y<10)%>% mutate(y_pred=gp_var2(k=x0,tau=x1)) %>% 
  ggplot(.,aes(x=y,y=y_pred))+geom_point(alpha=0.5)


