### Measure power of 3 types of confidence intervals ------


library(tidyverse)
library(pwr)

### 3 functions to calculate CI for a proportion -------

list_of_functions <- list(binconf="Hmisc::binconf", 
                           prop.test="prop.test",
                           binom.test="binom.test")

### Execute 3 functions on a simulated data set -------

sim_one_data_set <- function(seed, nobs=100, p=0.05,
                             alpha=0.01){
  
  set.seed(seed)
  
  x <- rbinom(nobs, 1, prob=p)
  
  conf_int <- lapply(list_of_functions, function(f){
    f_parsed <- parse(text=f)
    f_func <- eval(f_parsed)
    
    if(f=="Hmisc::binconf"){
      out <- f_func(x=sum(x), n=length(x), alpha=alpha, return.df=TRUE)
      
      out$Type = f
      
      out <- out %>% 
        select(Lower, Upper, Type)
    } else {
      out <- f_func(x=sum(x), n=length(x), conf.level = 1 - alpha)
      
      out <- data.frame(Lower=out$conf.int[1], Upper=out$conf.int[2])
      
      out$Type = f
    }
    
    return(out)
  }
  )
  return(bind_rows(conf_int))
}

### Simulate 10,000 data sets ----

sim_many_datasets_CI <- lapply(1:10000, sim_one_data_set)

sim_many_df <- bind_rows(sim_many_datasets_CI)

### The true p is 0.05 (set inside sim_one_data_set)
### Suppose the null p is 0.15
### If the CI includes 0.15, then a Type 2 Error is committed

sim_many_df %>% 
  mutate(Type2Error = Upper >= 0.15) %>% 
  group_by(Type) %>% 
  summarize(TotErrors = sum(Type2Error),
            TotTests = n(),
            Power = 1 - TotErrors / TotTests)

### binom.test() has the highest power!!!!!!

### Benchmark using pwr package ----

pwr.p.test(h=ES.h(0.05, 0.15),
           n=100,
           sig.level=0.01)

### pwr estimates 80.7% power