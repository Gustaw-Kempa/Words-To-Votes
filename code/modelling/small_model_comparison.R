#### For models with diff1 ####
rm(list = ls())
library(rstan)
library(loo)
library(combinat)
library(tidyverse)
options(mc.cores = parallel::detectCores())
set.seed(42)

# Define your data
model_data <- read.csv('data/model_data.csv')
model_data <- model_data %>% na.omit()
model_data[,c(2:15,17:21)] <- scale(model_data[,c(2:15,17:21)])
model_data$country_id <- as.integer(as.factor(model_data$country))

# first analysis is for the dependent variable diff1
data <- list(N =nrow(model_data), sent = model_data$mean_sentiment,
             info = model_data$mean_informativeness,
             pers = model_data$mean_persuasion, ang = model_data$mean_anger,
             surp = model_data$mean_surprise, hap = model_data$mean_happiness,
             fear = model_data$mean_fear,
             diff0 = model_data$diff0,
             country_id = model_data$country_id,
             N_country = max(model_data$country_id),
             cases = model_data$cases,
             V = model_data$diff1)

emotions <- c( "info", "sent","pers","ang", "surp", "hap", "fear")

distributions <- c("normal", "student_t", "laplace", "logistic")

loo_results <- list()
estimates <- list()


for (i in 1:length(emotions)) {
emotion <- emotions[i] 
  for (k in 1:4) {

  distribution <- distributions[k]
      # Build the model code
      model_code <- paste0("
        data {
          int<lower=0> N;
          ", paste(paste0("vector[N] ", emotion)), ";
          vector[N] cases;
          vector[N] diff0;
          int country_id[N];
          int N_country;
          vector[N] V;
        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          ", paste(paste0("real psi_", emotion)), ";
          real beta;
          real theta;
          vector[N_country] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_country] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + ", paste(paste0("psi_", emotion, " * ", emotion)), " + beta * diff0 + theta * cases + eta[country_id];
        }
        model {
          alpha ~ normal(0,1);
          ", paste(paste0("psi_", emotion, " ~ normal(0,1)")), ";
          beta ~ normal(0,1);
          sigma ~ normal(0,1);
          theta ~ normal(0,1);
          eta_raw ~ normal(0,1);
          sigma_eta ~ normal(0,1);
          nu ~ cauchy(0, 5);  // weakly informative prior for degrees of freedom
      ")
      
      if (distribution == "normal") {
        model_code <- paste0(model_code, "
          V ~ normal(mu, sigma);}
        ")
      } else if (distribution == "student_t") {
        model_code <- paste0(model_code, "
          V ~ student_t(nu, mu, sigma) ;}
        ")
      } else if (distribution == "laplace") {
        model_code <- paste0(model_code, "
          V ~ double_exponential(mu, sigma) ;}
        ")
      } else if (distribution == "logistic") {
        model_code <- paste0(model_code, "
          V ~ logistic(mu, sigma) ;}
        ")
      }
      if (distribution == "student_t") {
        model_code <- paste0(model_code, "
           generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = ", distribution, "_lpdf(V[i] |nu, mu[i], sigma);
            y_rep[i] = ", distribution, "_rng(nu,mu[i], sigma);
          }
        }
      
        ") } else if (distribution == "laplace") {
          model_code <- paste0(model_code, "
        generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = double_exponential_lpdf(V[i] | mu[i], sigma);
            y_rep[i] = double_exponential_rng(mu[i], sigma);
          }
        }
      ") } else{
        model_code <- paste0(model_code, "
        generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = ", distribution, "_lpdf(V[i] | mu[i], sigma);
            y_rep[i] = ", distribution, "_rng(mu[i], sigma);
          }
        }
      ") }
      
      # Fit the model
      fit <- stan(model_code = model_code, 
                  data = data, 
                  iter = 1500,
                  warmup = 750,
                  thin = 4, 
                  cores = 8,
                  chains = 8,
                  refresh = 0) # This line suppresses the output from Stan
      
      # Compute the loo
      log_lik <- extract_log_lik(fit, merge_chains = FALSE)
      r_eff <- relative_eff(exp(log_lik), cores = 8) 
      loo_result <- loo::loo(log_lik, r_eff = r_eff, cores = 8)
      loo_results <- c(loo_results, tibble(loo_result$estimates))
      names(loo_results)[length(loo_results)] <- paste0(distribution, "_", emotion)
      
      res <- data.frame(matrix(ncol = 3, nrow = 0))
      param.sims =  rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE, include = TRUE)
      for (i in 1:5) {
       
        # Extract the relevant values from param.sims
        p_gt_0 <- mean(param.sims[[i]] > 0)
        mean_val <- mean(param.sims[[i]])
        sd_val <- sd(param.sims[[i]])
        name <- names(param.sims)[[i]]
        # Append the values to the data frame
        res <- rbind(res, c(p_gt_0, mean_val, sd_val))
      }
      colnames(res) <- c("p>0", "mean", "sd")
      rownames(res) <- names(param.sims[1:5])
      estimates[[length(estimates)+1]] <- res
      names(estimates)[length(estimates)] <- paste0(distribution, "_", emotion)
      
      # Print the current loop stage
      print(paste(emotion, distribution))
      
      
      # Clean the environment, but keep the necessary variables
      rm(list=setdiff(ls(), c("i", "j", "k", "data", "emotions", "distributions", "distribution", "estimates","loo_results", 'emotion')))
    }
  }




# combining the two lists
full_diff1 <- Map(function(x, y) list(x, y), loo_results, estimates)
# sorting the results
full_diff1 <- full_diff1[order(sapply(full_diff1, function(x) x[[1]][1,1]), decreasing = T)]


#### For diff2 ####
rm(list = ls())
library(rstan)
library(loo)
library(combinat)
library(tidyverse)
options(mc.cores = parallel::detectCores())
set.seed(42)

# Define your data
model_data <- read.csv('data/model_data.csv')
model_data <- model_data %>% na.omit()
model_data[,c(2:15,17:21)] <- scale(model_data[,c(2:15,17:21)])
model_data$country_id <- as.integer(as.factor(model_data$country))

# second analysis is for the dependent variable diff2
data <- list(N =nrow(model_data), sent = model_data$mean_sentiment,
             info = model_data$mean_informativeness,
             pers = model_data$mean_persuasion, ang = model_data$mean_anger,
             surp = model_data$mean_surprise, hap = model_data$mean_happiness,
             fear = model_data$mean_fear,
             diff0 = model_data$diff0,
             country_id = model_data$country_id,
             N_country = max(model_data$country_id),
             cases = model_data$cases,
             V = model_data$diff2)

emotions <- c( "info", "sent","pers","ang", "surp", "hap", "fear")

distributions <- c("normal", "student_t", "laplace", "logistic")

loo_results <- list()
estimates <- list()


for (i in 1:length(emotions)) {
  emotion <- emotions[i] 
  for (k in 1:4) {
    
    distribution <- distributions[k]
    # Build the model code
    model_code <- paste0("
        data {
          int<lower=0> N;
          ", paste(paste0("vector[N] ", emotion)), ";
          vector[N] cases;
          vector[N] diff0;
          int country_id[N];
          int N_country;
          vector[N] V;
        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          ", paste(paste0("real psi_", emotion)), ";
          real beta;
          real theta;
          vector[N_country] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_country] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + ", paste(paste0("psi_", emotion, " * ", emotion)), " + beta * diff0 + theta * cases + eta[country_id];
        }
        model {
          alpha ~ normal(0,1);
          ", paste(paste0("psi_", emotion, " ~ normal(0,1)")), ";
          beta ~ normal(0,1);
          sigma ~ normal(0,1);
          theta ~ normal(0,1);
          eta_raw ~ normal(0,1);
          sigma_eta ~ normal(0,1);
          nu ~ cauchy(0, 5);  // weakly informative prior for degrees of freedom
      ")
    
    if (distribution == "normal") {
      model_code <- paste0(model_code, "
          V ~ normal(mu, sigma);}
        ")
    } else if (distribution == "student_t") {
      model_code <- paste0(model_code, "
          V ~ student_t(nu, mu, sigma) ;}
        ")
    } else if (distribution == "laplace") {
      model_code <- paste0(model_code, "
          V ~ double_exponential(mu, sigma) ;}
        ")
    } else if (distribution == "logistic") {
      model_code <- paste0(model_code, "
          V ~ logistic(mu, sigma) ;}
        ")
    }
    if (distribution == "student_t") {
      model_code <- paste0(model_code, "
           generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = ", distribution, "_lpdf(V[i] |nu, mu[i], sigma);
            y_rep[i] = ", distribution, "_rng(nu,mu[i], sigma);
          }
        }
      
        ") } else if (distribution == "laplace") {
          model_code <- paste0(model_code, "
        generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = double_exponential_lpdf(V[i] | mu[i], sigma);
            y_rep[i] = double_exponential_rng(mu[i], sigma);
          }
        }
      ") } else{
        model_code <- paste0(model_code, "
        generated quantities {
          vector[N] log_lik;
          vector[N] y_rep;
          for (i in 1:N) {
            log_lik[i] = ", distribution, "_lpdf(V[i] | mu[i], sigma);
            y_rep[i] = ", distribution, "_rng(mu[i], sigma);
          }
        }
      ") }
    
    # Fit the model
    fit <- stan(model_code = model_code, 
                data = data, 
                iter = 1500,
                warmup = 750,
                thin = 4, 
                cores = 8,
                chains = 8,
                refresh = 0) # This line suppresses the output from Stan
    
    # Compute the loo
    log_lik <- extract_log_lik(fit, merge_chains = FALSE)
    r_eff <- relative_eff(exp(log_lik), cores = 8) 
    loo_result <- loo::loo(log_lik, r_eff = r_eff, cores = 8)
    loo_results <- c(loo_results, tibble(loo_result$estimates))
    names(loo_results)[length(loo_results)] <- paste0(distribution, "_", emotion)
    
    res <- data.frame(matrix(ncol = 3, nrow = 0))
    param.sims =  rstan::extract(fit, permuted = TRUE, inc_warmup = FALSE, include = TRUE)
    for (i in 1:5) {
      
      # Extract the relevant values from param.sims
      p_gt_0 <- mean(param.sims[[i]] > 0)
      mean_val <- mean(param.sims[[i]])
      sd_val <- sd(param.sims[[i]])
      name <- names(param.sims)[[i]]
      # Append the values to the data frame
      res <- rbind(res, c(p_gt_0, mean_val, sd_val))
    }
    colnames(res) <- c("p>0", "mean", "sd")
    rownames(res) <- names(param.sims[1:5])
    estimates[[length(estimates)+1]] <- res
    names(estimates)[length(estimates)] <- paste0(distribution, "_", emotion)
    
    # Print the current loop stage
    print(paste(emotion, distribution))
    
    
    # Clean the environment, but keep the necessary variables
    rm(list=setdiff(ls(), c("i", "j", "k", "data", "emotions", "distributions", "distribution", "estimates","loo_results", 'emotion')))
  }
}




# combining the two lists
full_diff1 <- Map(function(x, y) list(x, y), loo_results, estimates)
# sorting the results
full_diff1 <- full_diff1[order(sapply(full_diff1, function(x) x[[1]][1,1]), decreasing = T)]

