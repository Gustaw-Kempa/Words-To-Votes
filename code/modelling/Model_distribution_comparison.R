#### for models with diff1 ####
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

# Define your emotions
emotions <- c( "info", "sent","pers","ang", "surp", "hap", "fear")

distributions <- c("normal", "student_t", "laplace", "logistic")


for (i in 1:length(emotions)) {

  for (k in 1:4) {
    combinations <- combinat::combn(emotions, i) 
    distribution <- distributions[k]
    
    if (!is.matrix(combinations)) {
      emotion_combination <- combinations 
    }
    
    for (j in 1:ncol(combinations)) {
      emotion_combination <- combinations[,j]
      
      # Build the model code
      model_code <- paste0("
        data {
          int<lower=0> N;
          ", paste(paste0("vector[N] ", emotion_combination), collapse = "; "), ";
          vector[N] cases;
          vector[N] diff0;
          int country_id[N];
          int N_country;
          vector[N] V;
        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          ", paste(paste0("real psi_", emotion_combination), collapse = "; "), ";
          real beta;
          real theta;
          vector[N_country] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_country] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + ", paste(paste0("psi_", emotion_combination, " * ", emotion_combination), collapse = " + "), " + beta * diff0 + theta * cases + eta[country_id];
        }
        model {
          alpha ~ normal(0,1);
          ", paste(paste0("psi_", emotion_combination, " ~ normal(0,1)"), collapse = "; "), ";
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
      
      # Save the loo object to a file
      saveRDS(loo_result, file = paste0("data/temp/loo_results_diff1", paste(emotion_combination, collapse = "_"), "_", distribution, ".rds"))
      
      # Print the current loop stage
print(emotion_combination)
print(distribution)

      # Clean the environment, but keep the necessary variables
      rm(list=setdiff(ls(), c("combinations", "i", "j", "k", "data", "emotions", "distributions", "distribution")))
}
}
}


#### MODEL COMPARISON ####

names_list <- list.files(path = "data/temp/loo_results_diff1",
                         pattern = ".rds", full.names = T)
model_names <- gsub("data/temp/loo_results_diff1/|\\.rds", "", names_list)
loos <- lapply(names_list, FUN =readRDS)

comparison <- unlist(loo_compare(loos))
model_nums <- as.numeric(gsub("model", "",rownames(comparison)))

model_names <- model_names[model_nums]
comparison <- cbind(model_names,comparison)
comparison[,-1] <- round(as.numeric(comparison[,-1]), 5)
#### Extratcion to LateX ####
library(xtable)
extraction <- head(comparison[,-c(2,3,9,8)], n = 10)
rownames(extraction) <- NULL
extraction[,-1] <- round(as.numeric(extraction[,-1]), 3)


# xtable(extraction)


#### For models with diff2 ####
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

# first analysis is for the dependent variable diff2
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

# Define your emotions
emotions <- c( "info", "sent","pers","ang", "surp", "hap", "fear")

distributions <- c("normal", "student_t", "laplace", "logistic")


for (i in 1:length(emotions)) {
  
  for (k in 1:4) {
    combinations <- combinat::combn(emotions, i) 
    distribution <- distributions[k]
    
    if (!is.matrix(combinations)) {
      emotion_combination <- combinations 
    }
    
    for (j in 1:ncol(combinations)) {
      emotion_combination <- combinations[,j]
      
      # Build the model code
      model_code <- paste0("
        data {
          int<lower=0> N;
          ", paste(paste0("vector[N] ", emotion_combination), collapse = "; "), ";
          vector[N] cases;
          vector[N] diff0;
          int country_id[N];
          int N_country;
          vector[N] V;
        }
        parameters {
          real alpha;
          real<lower=0> sigma;
          ", paste(paste0("real psi_", emotion_combination), collapse = "; "), ";
          real beta;
          real theta;
          vector[N_country] eta_raw;
          real<lower = 0> sigma_eta;
          real<lower = 0> nu;  // degrees of freedom for Student's t-distribution
        }
        transformed parameters {
          vector[N_country] eta = sigma_eta * eta_raw;
          vector[N] mu = alpha + ", paste(paste0("psi_", emotion_combination, " * ", emotion_combination), collapse = " + "), " + beta * diff0 + theta * cases + eta[country_id];
        }
        model {
          alpha ~ normal(0,1);
          ", paste(paste0("psi_", emotion_combination, " ~ normal(0,1)"), collapse = "; "), ";
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
      
      # Save the loo object to a file
      saveRDS(loo_result, file = paste0("data/temp/loo_results_diff2", paste(emotion_combination, collapse = "_"), "_", distribution, ".rds"))
      
      # Print the current loop stage
      print(emotion_combination)
      print(distribution)
      
      # Clean the environment, but keep the necessary variables
      rm(list=setdiff(ls(), c("combinations", "i", "j", "k", "data", "emotions", "distributions", "distribution")))
    }
  }
}


#### MODEL COMPARISON ####

names_list <- list.files(path = "data/temp/loo_results_diff2",
                         pattern = ".rds", full.names = T)
model_names <- gsub("data/temp/loo_results_diff2/|\\.rds", "", names_list)
loos <- lapply(names_list, FUN =readRDS)

comparison <- unlist(loo_compare(loos))
model_nums <- as.numeric(gsub("model", "",rownames(comparison)))

model_names <- model_names[model_nums]
comparison <- cbind(model_names,comparison)
comparison[,-1] <- round(as.numeric(comparison[,-1]), 5)
#### Extratcion to LateX ####
library(xtable)
extraction <- head(comparison[,-c(2,3,9,8)], n = 10)
rownames(extraction) <- NULL
extraction[,-1] <- round(as.numeric(extraction[,-1]), 3)


# xtable(extraction)


