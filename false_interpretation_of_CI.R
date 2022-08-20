### Confidence interval and the next estimate -----

### Suppose I calculate an estimate and a 95% confidence interval
###
### What is the probability that my next estimate (from a new sample)
### would be inside the original confidence interval?
###
### The answer is smaller than 95%


library(tidyverse)


### simulate the first sample --

sim_one_est_CI <- function(seed){
  
  set.seed(seed)
  
  one_sample <- rnorm(100, 20, 20)
  
  one_ci <- t.test(one_sample, alternative="two.sided", conf.level=0.95) 
  
  out <- data.frame(estimate=one_ci$estimate[1], lower=one_ci$conf.int[1],
                    upper=one_ci$conf.int[2])
  
  rownames(out) <- NULL
  
  return(out)
}

sim_one_est_CI(1)

# estimate   lower    upper
# 1 22.17775 18.6133 25.74219

orig_ci <- sim_one_est_CI(1)

### simulate 10,000 new samples ---

many_CI <- lapply(2:10001, sim_one_est_CI)

many_CI_df <- bind_rows(many_CI)

head(many_CI_df)

### Out of 10,000 new estimates,
### How many were in the original CI?

many_CI_df %>% 
  mutate(in_orig_CI = between(estimate, orig_ci$lower, orig_ci$upper)) %>% 
  summarize(tot_covered = sum(in_orig_CI),
            tot_estimates = n(),
            coverage = tot_covered / tot_estimates)

### About 75% of new estimates were inside the first 95% confidence interval

### You should rightfully question the results above
### What if I had an "unlucky" confidence interval in my first sample
### We can repeat the experiment by treating each CI as an "original CI"


estimates_10k <- many_CI_df$estimate

many_CI_df$orig_id <- 1:nrow(many_CI_df)

out <- many_CI_df %>% 
  rowwise() %>% 
  mutate(coverage_lst = lst(
    between(estimates_10k[-orig_id], lower, upper)
    ),
    tot_covered = sum(coverage_lst),
    tot_estimates = length(coverage_lst),
    coverage = tot_covered / tot_estimates,
  )

head(out)

quantile(out$coverage, probs=c(0.25, 0.5, 0.75))

# 25%       50%       75% 
# 0.7944794 0.8979898 0.9375938 

mean(out$coverage)

ggplot(out, aes(x=coverage)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 20) +
  scale_y_continuous(labels = scales::percent)
