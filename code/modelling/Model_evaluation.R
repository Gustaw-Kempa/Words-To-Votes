rm(list = ls())
library(rstan)
library(loo)
library(combinat)
library(tidyverse)
library(bayesplot)
library(xtable)
options(mc.cores = parallel::detectCores())
set.seed(42)
options(scipen=99999)
#### Fitting the best performing model ####

# Define your data
model_data <- read.csv('data/model_data.csv')
model_data <- model_data %>% na.omit()
model_data[,c(2:15,17:21)] <- scale(model_data[,c(2:15,17:21)])
model_data$country_id <- as.integer(as.factor(model_data$country))
data1 <- list(N =nrow(model_data), sent = model_data$mean_sentiment,
             info = model_data$mean_informativeness,
             pers = model_data$mean_persuasion, ang = model_data$mean_anger,
             surp = model_data$mean_surprise, hap = model_data$mean_happiness,
             fear = model_data$mean_fear,
             diff0 = model_data$diff0,
             country_id = model_data$country_id,
             N_country = max(model_data$country_id),
             cases = model_data$cases,
             # Here we need to set V to be out independent variable (diff1/diff2)
             V = model_data$diff1)

data2 <- list(N =nrow(model_data), sent = model_data$mean_sentiment,
              info = model_data$mean_informativeness,
              pers = model_data$mean_persuasion, ang = model_data$mean_anger,
              surp = model_data$mean_surprise, hap = model_data$mean_happiness,
              fear = model_data$mean_fear,
              diff0 = model_data$diff0,
              country_id = model_data$country_id,
              N_country = max(model_data$country_id),
              cases = model_data$cases,
              # Here we need to set V to be out independent variable (diff1/diff2)
              V = model_data$diff2)

# Write model code

      model_code1 <- "
        data {
          int<lower=0> N;
         vector[N] sent;
         vector[N] pers;
         vector[N] ang;
         vector[N] fear;
          vector[N] cases;
          vector[N] diff0;
          int country_id[N];
          int N_country;
          vector[N] V;
        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          real psi_sent;
          real psi_pers;
          real psi_ang;
          real psi_fear;
          real beta;
          real theta;
          vector[N_country] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_country] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + psi_sent *sent + psi_pers * pers + psi_ang * ang + psi_fear * fear +
           beta * diff0 + theta * cases + eta[country_id];
        }
        model {
          alpha ~ normal(0,1);
          psi_sent ~ normal(0,1);
          psi_pers ~ normal(0,1);
          psi_ang ~ normal(0,1);
          psi_fear ~ normal(0,1);
          beta ~ normal(0,1);
          sigma ~ normal(0,1);
          theta ~ normal(0,1);
          eta_raw ~ normal(0,1);
          sigma_eta ~ normal(0,1);
          nu ~ cauchy(0, 5);  // weakly informative prior for degrees of freedom
     
          V ~ student_t(nu, mu, sigma) ;}
    
           generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = student_t_lpdf(V[i] |nu, mu[i], sigma);
            y_rep[i] = student_t_rng(nu,mu[i], sigma);
          }
        }
      
        "
      model_code2 <- "
        data {
          int<lower=0> N;
         vector[N] hap;
          vector[N] cases;
          vector[N] diff0;
          int country_id[N];
          int N_country;
          vector[N] V;
        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          real psi_hap;
          real beta;
          real theta;
          vector[N_country] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_country] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + psi_hap* hap +
           beta * diff0 + theta * cases + eta[country_id];
        }
        model {
          alpha ~ normal(0,1);
          psi_hap ~ normal(0,1);
          beta ~ normal(0,1);
          sigma ~ normal(0,1);
          theta ~ normal(0,1);
          eta_raw ~ normal(0,1);
          sigma_eta ~ normal(0,1);
          nu ~ cauchy(0, 5);  // weakly informative prior for degrees of freedom
     
          V ~ student_t(nu, mu, sigma) ;}
    
           generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = student_t_lpdf(V[i] |nu, mu[i], sigma);
            y_rep[i] = student_t_rng(nu,mu[i], sigma);
          }
        }
      
        "
      # Fit the model
      fit1 <- stan(model_code = model_code1, 
                  data = data1, 
                  iter = 1500,
                  warmup = 750,
                  thin = 4, 
                  cores = 8,
                  chains = 8,
                  refresh = 100) 
      fit2 <- stan(model_code = model_code2, 
                                 data = data2, 
                                 iter = 1500,
                                 warmup = 750,
                                 thin = 4, 
                                 cores = 8,
                                 chains = 8,
                                 refresh = 100) 
      
      array_of_draws1 <- as.array(fit1)   
      array_of_draws2 <- as.array(fit2)     
      param.sims1 = 
        rstan::extract(
          fit1,
          #pars = pars, # when we want to specify the parameters to be simulated
          permuted = TRUE, 
          inc_warmup = FALSE,
          include = TRUE)
      
      param.sims2 = 
        rstan::extract(
          fit2,
          #pars = pars, # when we want to specify the parameters to be simulated
          permuted = TRUE, 
          inc_warmup = FALSE,
          include = TRUE)
#### Convergence diagnostics #####

      
      ### Rhat ###
      Rhats1 <- c()
      par_names1 <- c()
      for (i in 1:length(dimnames(array_of_draws1)[[3]])) {
        Rhats1[i] <- Rhat(array_of_draws1[,,i])
        par_names1[i] <- dimnames(array_of_draws1)[[3]][[i]]
      }
      
      # Should be within 0.95-1.1
      max(Rhats1)
      min(Rhats1)
      
      Rhats1 <- tibble(Rhats1, par_names1)
      head(Rhats1, n= 7)
      
      Rhats2 <- c()
      par_names2 <- c()
      for (i in 1:length(dimnames(array_of_draws2)[[3]])) {
        Rhats2[i] <- Rhat(array_of_draws2[,,i])
        par_names2[i] <- dimnames(array_of_draws2)[[3]][[i]]
      }
      
      # Should be within 0.95-1.1
      max(Rhats2)
      min(Rhats2)
      
      Rhats2 <- tibble(Rhats2, par_names2)
      head(Rhats2, n= 5)      
      ### ESS (tail and bulk) ###
      
      ESSs_b1 <- c()
      ESSs_t1 <- c()
      
      for (i in 1:length(dimnames(array_of_draws1)[[3]])) {
        ESSs_b1[i] <- ess_bulk(array_of_draws1[,,i])
        ESSs_t1[i] <- ess_tail(array_of_draws1[,,i])
      }
      
      # Should be above 100*chains = 800
      max(ESSs_b1)
      min(ESSs_b1)
      
      max(ESSs_t1)
      min(ESSs_t1)
      
      ESSs1 <- tibble(ESSs_b1, ESSs_t1, par_names1)
      head(ESSs1, n = 7)
      
      
      ESSs_b2 <- c()
      ESSs_t2 <- c()
      
      for (i in 1:length(dimnames(array_of_draws2)[[3]])) {
        ESSs_b2[i] <- ess_bulk(array_of_draws2[,,i])
        ESSs_t2[i] <- ess_tail(array_of_draws2[,,i])
      }
      
      # Should be above 100*chains = 800
      max(ESSs_b2)
      min(ESSs_b2)
      
      max(ESSs_t2)
      min(ESSs_t2)
      
      ESSs2 <- tibble(ESSs_b2, ESSs_t2, par_names2)
      head(ESSs2, n = 5)      
      
      
  
      
# Creation of table to be extracted - model1
tab_diff1 <-data.frame(Rhats1[1:8,2], sapply(param.sims1[1:8], mean),  sapply(param.sims1[1:8], sd), Rhats1[1:8,1],ESSs1[1:8,1], ESSs1[1:8,2]
)
rownames(tab_diff1) <- NULL  
tab_diff1[,2:6] <- round(tab_diff1[,2:6],3)
colnames(tab_diff1) <- c('Parameter names', 'Mean', 'Standard deviation', 'Rhat', 'ESS bulk', 'ESS tail')

#xtable(tab_diff1)

#model2
tab_diff2 <-data.frame(Rhats2[1:5,2], sapply(param.sims2[1:5], mean),  sapply(param.sims2[1:5], sd), Rhats2[1:5,1],ESSs2[1:5,1], ESSs2[1:5,2]
)
rownames(tab_diff2) <- NULL  
tab_diff2[,2:6] <- round(tab_diff2[,2:6],4)
colnames(tab_diff2) <- c('Parameter names', 'Mean', 'Standard deviation', 'Rhat', 'ESS bulk', 'ESS tail')
#xtable(tab_diff2)


    # Traceplots
traceplot(fit1, pars = c("alpha", "sigma", "nu", "psi_sent", "psi_pers", "psi_ang", "psi_fear","beta", "theta")) 

traceplot(fit2, pars = c("alpha", "sigma", "nu", "psi_hap", "beta", "theta")) 


#### Posterior predictive check #####

y_rep1 <- rstan::extract(fit1, "y_rep")$y_rep
y_rep2 <- rstan::extract(fit2, "y_rep")$y_rep
# Fitting the draws against V
pp_check(data1$V, y_rep1, fun = "dens_overlay") + xlim(-7,7)
pp_check(data2$V, y_rep2, fun = "dens_overlay") + xlim(-7,7)


### R-suared statistic ###
# model1 #
var_res.emp = apply(data1$V-param.sims1$mu,1,var)
var_mu.emp = apply(param.sims1$mu,1,var)

rsq1 <- var_mu.emp/ (var_mu.emp + var_res.emp)
# par(bg=NA)
hist(rsq1, sub = paste('Rsquared model 1 mean: ',
                       round(mean(rsq1),3)))
abline(v=mean(rsq1), lty =2, col = 'blue')

# dev.copy(png,'myplot.png')
# dev.off()

# model2 #
var_res.emp = apply(data2$V-param.sims2$mu,1,var)
var_mu.emp = apply(param.sims2$mu,1,var)

rsq2 <- var_mu.emp/ (var_mu.emp + var_res.emp)

hist(rsq2, sub = paste('Rsquared model 2 mean: ',
                       round(mean(rsq2),3)))
abline(v=mean(rsq2), lty =2, col = 'blue')

#### De-standardization ####
### Model1 ###
real_data <- read.csv('data/model_data.csv')

psi_sent_real1 <- param.sims1$psi_sent * (sd(real_data$diff1)/sd(real_data$mean_sentiment))
psi_pers_real1 <- param.sims1$psi_pers * (sd(real_data$diff1)/sd(real_data$mean_persuasion))
psi_ang_real1 <- param.sims1$psi_ang * (sd(real_data$diff1)/sd(real_data$mean_anger))
psi_fear_real1 <- param.sims1$psi_fear * (sd(real_data$diff1)/sd(real_data$mean_fear))

beta_real1 <- param.sims1$beta * (sd(real_data$diff1)/sd(real_data$diff0))
theta_real1 <- param.sims1$theta * (sd(real_data$diff1)/sd(real_data$cases))


alpha_real1 <- param.sims1$alpha * sd(real_data$diff1) + mean(real_data$diff1) -
  (param.sims1$psi_sent * sd(real_data$diff1) * (mean(real_data$mean_sentiment)/sd(real_data$mean_sentiment)) +
  param.sims1$psi_pers * sd(real_data$diff1) * (mean(real_data$mean_persuasion)/sd(real_data$mean_persuasion)) +
    param.sims1$psi_ang * sd(real_data$diff1) * (mean(real_data$mean_anger)/sd(real_data$mean_anger)) +
    param.sims1$psi_fear * sd(real_data$diff1) * (mean(real_data$mean_fear)/sd(real_data$mean_fear)) +
    param.sims1$beta * sd(real_data$diff1) * (mean(real_data$diff0)/sd(real_data$diff0)) +
    param.sims1$theta * sd(real_data$diff1) * (mean(real_data$cases)/sd(real_data$cases)))

eta_real1 <- matrix(ncol = data1$N_country, nrow = length(alpha_real1))
for (i in 1:data1$N_country){
  eta_real1[,i] <- param.sims1$eta[,i] * sd(real_data$diff1) + mean(real_data$diff1) -
    (param.sims1$psi_sent * sd(real_data$diff1) * (mean(real_data$mean_sentiment)/sd(real_data$mean_sentiment)) +
       param.sims1$psi_pers * sd(real_data$diff1) * (mean(real_data$mean_persuasion)/sd(real_data$mean_persuasion)) +
       param.sims1$psi_ang * sd(real_data$diff1) * (mean(real_data$mean_anger)/sd(real_data$mean_anger)) +
       param.sims1$psi_fear * sd(real_data$diff1) * (mean(real_data$mean_fear)/sd(real_data$mean_fear)) +
       param.sims1$beta * sd(real_data$diff1) * (mean(real_data$diff0)/sd(real_data$diff0)) +
       param.sims1$theta * sd(real_data$diff1) * (mean(real_data$cases)/sd(real_data$cases)))
}

### Model2 ###

psi_hap_real2 <- param.sims2$psi_hap * (sd(real_data$diff2)/sd(real_data$mean_happiness))

beta_real2 <- param.sims2$beta * (sd(real_data$diff2)/sd(real_data$diff0))
theta_real2 <- param.sims2$theta * (sd(real_data$diff2)/sd(real_data$cases))


alpha_real2 <- param.sims2$alpha * sd(real_data$diff2) + mean(real_data$diff2) -
  (param.sims2$psi_hap * sd(real_data$diff2) * (mean(real_data$mean_happiness)/sd(real_data$mean_happiness)) +
     param.sims2$beta * sd(real_data$diff2) * (mean(real_data$diff0)/sd(real_data$diff0)) +
     param.sims2$theta * sd(real_data$diff2) * (mean(real_data$cases)/sd(real_data$cases)))

eta_real2 <- matrix(ncol = data2$N_country, nrow = length(alpha_real2))
for (i in 1:data1$N_country){
  eta_real2[,i] <- param.sims2$eta[,i] * sd(real_data$diff2) + mean(real_data$diff2) -
    (param.sims2$psi_hap * sd(real_data$diff2) * (mean(real_data$mean_happiness)/sd(real_data$mean_happiness)) +
       param.sims2$beta * sd(real_data$diff2) * (mean(real_data$diff0)/sd(real_data$diff0)) +
       param.sims2$theta * sd(real_data$diff2) * (mean(real_data$cases)/sd(real_data$cases)))
}


#### Interpretations and visualizations ####
### Model1 ###
# Mean Sentiment #
ggplot(data = NULL, aes(x = psi_sent_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.8,0.9) +
  xlab("psi_sent") +  ggtitle("Posterior Distribution of Effect of Sentiment") +
  geom_vline(xintercept = mean(psi_sent_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(psi_sent_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(psi_sent_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(psi_sent > 0):", round(mean(psi_sent_real1 > 0), 3))) +
  theme_minimal()
mean(psi_sent_real1)
sd(psi_sent_real1)

# Mean Persuasion #
ggplot(data = NULL, aes(x = psi_pers_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-1.5,0.9) +
  xlab("psi_pers") +  ggtitle("Posterior Distribution of Effect of Persuasion") +
  geom_vline(xintercept = mean(psi_pers_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(psi_pers_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(psi_pers_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(psi_pers > 0):", round(mean(psi_pers_real1 > 0), 3))) +
  theme_minimal()
mean(psi_pers_real1)
sd(psi_pers_real1)
# Mean Anger #
ggplot(data = NULL, aes(x = psi_ang_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-1.2,1.5) +
  xlab("psi_ang") +  ggtitle("Posterior Distribution of Effect of Anger") +
  geom_vline(xintercept = mean(psi_ang_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(psi_ang_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(psi_ang_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(psi_ang > 0):", round(mean(psi_ang_real1 > 0), 3))) +
  theme_minimal()
mean(psi_ang_real1)
sd(psi_ang_real1)
# Mean Fear #
ggplot(data = NULL, aes(x = psi_fear_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-1.4,1.5) +
  xlab("psi_fear") +  ggtitle("Posterior Distribution of Effect of Fear") +
  geom_vline(xintercept = mean(psi_fear_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(psi_fear_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(psi_fear_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(psi_fear > 0):", round(mean(psi_fear_real1 > 0), 3))) +
  theme_minimal()
mean(psi_fear_real1)
sd(psi_fear_real1)
# AR component #
ggplot(data = NULL, aes(x = beta_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.5,0) +
  xlab("beta") +  ggtitle("Posterior Distribution of the AR component") +
  geom_vline(xintercept = mean(beta_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(beta_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(beta_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(beta > 0):", round(mean(beta_real1 > 0), 3))) +
  theme_minimal()
mean(beta_real1)
sd(beta_real1)
# Cases # 
ggplot(data = NULL, aes(x = theta_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.00001,0.00001) +
  xlab("theta") +  ggtitle("Posterior Distribution of the cases") +
  geom_vline(xintercept = mean(theta_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(theta_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(theta_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(theta > 0):", round(mean(theta_real1 > 0), 3))) +
  theme_minimal()
mean(theta_real1)
sd(theta_real1)

# Alpha # 
ggplot(data = NULL, aes(x = alpha_real1)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-3,5.2) +
  xlab("alpha") +  ggtitle("Posterior Distribution of the AR component") +
  geom_vline(xintercept = mean(alpha_real1), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(alpha_real1, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(alpha_real1, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(alpha > 0):", round(mean(alpha_real1 > 0), 3))) +
  theme_minimal()
mean(alpha_real1)
sd(alpha_real1)

# Etas #
# Convert the matrix into a data frame and reshape it into a long format
eta_real1_df <- as.data.frame(eta_real1) %>%
  mutate(row_id = 1:n()) %>%
  gather(key = "column", value = "value", -row_id)
# Update the column names
colnames(eta_real1_df)[which(names(eta_real1_df) == "column")] <- "country"
eta_real1_df$country <- factor(eta_real1_df$country, labels = c("USA", "UK", "ES", "FR", "DE", "IT", "CA"))

# printing % above 0
for (i in 1:7){
  print(paste0('pr(eta_',unique(eta_real1_df$country)[i],") = ",  round(mean(eta_real1[,i] > 0), 3)))
}

# Create the overlaid density plots with distinct colors and updated variable names
ggplot(data = eta_real1_df, aes(x = value, fill = country)) +
  geom_density(color = "black", alpha = 0.4) +
  xlim(-5, 7) +
  xlab("eta") + ggtitle("Posterior Distribution of control variable - cases") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  scale_alpha_continuous(range = c(0.5, 0.5)) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcoral", "lightskyblue", "lightgray")) +
  theme_minimal()

### Model2 ###

# Mean happiness #
ggplot(data = NULL, aes(x = psi_hap_real2)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-1.3, 0.7) +
  xlab("psi_hap") +  ggtitle("Posterior Distribution of Effect of Happiness") +
  geom_vline(xintercept = mean(psi_hap_real2), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  #geom_vline(xintercept = quantile(psi_hap_real2, 0.05), linetype = "dotted", color = "red") +
  #geom_vline(xintercept = quantile(psi_hap_real2, 0.95), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(psi_hap > 0):", round(mean(psi_hap_real2 > 0), 3))) +
  theme_minimal()
mean(psi_hap_real2)
sd(psi_hap_real2)

# the AR component #
ggplot(data = NULL, aes(x = beta_real2)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.5, 0.3) +
  xlab("beta") +  ggtitle("Posterior Distribution of the AR component") +
  geom_vline(xintercept = mean(beta_real2), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  # geom_vline(xintercept = quantile(beta_real2, 0.025), linetype = "dotted", color = "red") +
  # geom_vline(xintercept = quantile(beta_real2, 0.975), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(beta > 0):", round(mean(beta_real2 > 0), 3))) +
  theme_minimal()
mean(beta_real2)
sd(beta_real2)

# Control variable - cases #
ggplot(data = NULL, aes(x = theta_real2)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.00001, 0.00001) +
  xlab("theta") +  ggtitle("Posterior Distribution of control variable - cases * 1000") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  geom_vline(xintercept = mean(theta_real2), linetype = "dashed") +
  # geom_vline(xintercept = quantile(theta_real2, 0.025), linetype = "dotted", color = "red") +
  # geom_vline(xintercept = quantile(theta_real2, 0.975), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(theta > 0):", round(mean(theta_real2 > 0), 3))) +
  theme_minimal()
mean(theta_real2)
sd(theta_real2)
# Control variable - cases #
ggplot(data = NULL, aes(x = theta_real2)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.00001, 0.00001) +
  xlab("theta") +  ggtitle("Posterior Distribution of control variable - cases") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  geom_vline(xintercept = mean(theta_real2), linetype = "dashed") +
  # geom_vline(xintercept = quantile(theta_real2, 0.025), linetype = "dotted", color = "red") +
  # geom_vline(xintercept = quantile(theta_real2, 0.975), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(theta > 0):", round(mean(theta_real2 > 0), 3))) +
  theme_minimal()
mean(theta_real2)
sd(theta_real2)
# the AR component #
ggplot(data = NULL, aes(x = beta_real2)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-0.4, 0.2) +
  xlab("beta") +  ggtitle("Posterior Distribution of the AR component") +
  geom_vline(xintercept = mean(beta_real2), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  # geom_vline(xintercept = quantile(beta_real2, 0.025), linetype = "dotted", color = "red") +
  # geom_vline(xintercept = quantile(beta_real2, 0.975), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(beta > 0):", round(mean(beta_real2 > 0), 3))) +
  theme_minimal()
mean(beta_real2)
sd(beta_real2)

# Alpha #
ggplot(data = NULL, aes(x = alpha_real2)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  xlim(-3.5, 4.2) +
  xlab("alpha") +  ggtitle("Posterior Distribution of control variable - cases") +
  geom_vline(xintercept = mean(alpha_real2), linetype = "dashed") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  # geom_vline(xintercept = quantile(alpha_real2, 0.025), linetype = "dotted", color = "red") +
  # geom_vline(xintercept = quantile(alpha_real2, 0.975), linetype = "dotted", color = "red") +
  labs(caption = paste("Pr(alpha > 0):", round(mean(alpha_real2 > 0), 3))) +
  theme_minimal()
mean(alpha_real2)
sd(alpha_real2)
# Convert the matrix into a data frame and reshape it into a long format
eta_real2_df <- as.data.frame(eta_real2) %>%
  mutate(row_id = 1:n()) %>%
  gather(key = "column", value = "value", -row_id)
# Update the column names
colnames(eta_real2_df)[which(names(eta_real2_df) == "column")] <- "country"
eta_real2_df$country <- factor(eta_real2_df$country, labels = c("USA", "UK", "ES", "FR", "DE", "IT", "CA"))
# Create the overlaid density plots with distinct colors and updated variable names
ggplot(data = eta_real2_df, aes(x = value, fill = country)) +
  geom_density(color = "black", alpha = 0.4) +
  xlim(-5, 7) +
  xlab("eta") + ggtitle("Posterior Distribution of control variable - cases") +
  geom_vline(xintercept = 0, color = 'red', linetype = 1) +
  scale_alpha_continuous(range = c(0.5, 0.5)) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcoral", "lightskyblue", "lightgray")) +
  theme_minimal()

# printing % above 0
for (i in 1:7){
  print(paste0('pr(eta_',unique(eta_real2_df$country)[i],") = ",  round(mean(eta_real2[,i] > 0), 3)))
}



ggplot(real_data, aes(x = seq_along(diff1), y = diff1)) +
  geom_line(color = "blue") +
  geom_line(aes(y = diff2), color = "red") +
  labs(x = "Index", y = "Value") +
  ggtitle("Line Plot of diff1 and diff2") +
  scale_x_continuous(breaks = seq(1, length(real_data$diff1), by = 10))


cor(real_data$diff1, real_data$diff2)
