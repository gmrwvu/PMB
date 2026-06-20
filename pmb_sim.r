# ************************************************************
# PROGRAMMATIC MEDIA BUYING SIMULATION
# ************************************************************
# Purpose:
# Use simulated programmatic campaign data to examine the 
# relationship between conversion performance and uncertainty
#
# Outputs:
# 1. Correlations between conversion and uncertainty for 3 scenarios
# ************************************************************

#******************************
#1. FUNCTIONS
#******************************

#-----------------------------------------------------
#Use uniform dist to randomly assign dimension effects
#-----------------------------------------------------
assign_effects<-function (wts, min, max,names) {
  eff <-numeric()
  for (i in 1:length(wts)) {
      eff[i] <- runif(1, min = min, max = max)*(wts[i]/max(wts))
  }
  eff_vec <- setNames(eff, names)
  return(eff_vec)
}

#----------------------------------------------------------------
#Add effects for each random variable value for a particular trial
#-----------------------------------------------------------------
dim_effects_each_trial<-function (start_logit, pmb_df, d_eff, a_eff, u_eff, i_eff) {
  logit <- start_logit +
    d_eff[pmb_df$device] +
    a_eff[pmb_df$audience] +
    u_eff[pmb_df$usp] +
    i_eff[pmb_df$inventory] 
  names(logit) <- paste0("trial", 1:length(logit))
  return(logit)
}

#---------------------------------------------------
#Add interact effects to trials that have appropriate 
#combinations of dimension values
#---------------------------------------------------
interact_effects_each_trial <- function(logit, pmb_df, interact_df) {
  for (i in 1:nrow(interact_df)) {
    mask <- pmb_df[[interact_df$dim1[i]]] == interact_df$val1[i] &
            pmb_df[[interact_df$dim2[i]]] == interact_df$val2[i]
    logit[mask] <- logit[mask] + interact_df$effect[i]
  }
  return(logit)
}

#---------------------------------------------------
#Format correlation test results for console output
#---------------------------------------------------
format_cor_result <- function(label, test_result, alpha = 0.05) {
  estimate <- unname(test_result$estimate)
  ci <- test_result$conf.int
  sig_label <- ifelse(test_result$p.value < alpha,
                      "significant", "not significant")

  cat(sprintf(
    "  %s\n    r = %.4f | 95%% CI [%.4f, %.4f] | p = %.4g | %s at alpha = %.2f\n",
    label,
    estimate,
    ci[1],
    ci[2],
    test_result$p.value,
    sig_label,
    alpha
  ))
}

#******************************
#2. ASSIGNMENTS
#******************************

#----------------
#Number of trials 
#----------------
n <- 50000

#------------------------------------
#Assign labels to the four dimensions
#------------------------------------
audiences <- c("aud1", "aud2", "aud3", "aud4")
usps <- c("usp1", "usp2", "usp3", "usp4")
inventory <- c("inv1", "inv2", "inv3", "inv4", "inv5")
device <- c("dev1", "dev2", "dev3", "dev4", "dev5", "dev6")

#-----------------------------------
#Assign availability/utility weights
#-----------------------------------
aud_wt <- c(0.15, 0.25, 0.35, 0.25)
usp_wt <- c(0.15, 0.20, 0.30, 0.35)
inv_wt <- c(0.30, 0.28, 0.18, 0.15, 0.09)
dev_wt <- c(0.20, 0.55, 0.08, 0.10, 0.04, 0.03)

#--------------------------
#Assign interaction effects
#--------------------------
interactions <- data.frame(
  dim1   = c("audience","audience","audience","inventory","inventory","device"),
  val1   = c("aud2",    "aud4",    "aud3",    "inv2",     "inv4",     "dev2"),
  dim2   = c("usp",     "usp",     "usp",     "usp",      "audience", "audience"),
  val2   = c("usp2",    "usp4",    "usp4",    "usp2",     "aud2",     "aud2"),
  effect = c(0.35,       0.25,      0.30,      0.20,       0.25,       0.15)
)

#*********************************
#3. Run Simulation
#*********************************
iSeed <- 42
entropy_summary <- data.frame(
  start_logit = numeric(),
  start_conv_prob = numeric(),
  mean_conv_prob = numeric(),
  mean_entropy = numeric()
)
entropy_detail <- data.frame(
  start_logit = numeric(),
  entropy = numeric()
)

for (start_logit in -2:-6) {
  set.seed(iSeed)

  #-------------------------------------------------------
  #Randomly select dimension effects from published ranges
  #-------------------------------------------------------
  audience_effect <-assign_effects(aud_wt, .097, .428, audiences)
  usp_effect <-assign_effects(usp_wt, .19, .48, usps)
  inventory_effect <-assign_effects(inv_wt, .1, .52, inventory) 
  device_effect <-assign_effects(dev_wt, .1, .52, device) 

  #-----------------------------------------------------------------
  #For each scenario, create simulation core with 4 random variables
  #-----------------------------------------------------------------
  pmb_df <- data.frame(
    audience = sample(audiences, n, replace = TRUE,
                    prob = aud_wt),
    usp = sample(usps, n, replace = TRUE, 
                    prob = usp_wt),
    inventory = sample(inventory, n, replace = TRUE,
                     prob = inv_wt),
    device = sample(device, n, replace = TRUE,
                     prob = dev_wt)
  )  

  #---------------------------
  #Calculate marketing effects
  #---------------------------
  logit <- dim_effects_each_trial(start_logit, pmb_df, device_effect,
                                  audience_effect, usp_effect, inventory_effect)
  logit <- interact_effects_each_trial(logit, pmb_df, interactions)

  #----------------------------------------------------
  #Transform resulting logit to probability and entropy
  #----------------------------------------------------
  conv_prob <- 1 / (1 + exp(-logit))
  entropy <- -(conv_prob * log2(conv_prob) +
               (1 - conv_prob) * log2(1 - conv_prob))
  logit_effect <- logit - start_logit
  entropy_detail <- rbind(
    entropy_detail,
    data.frame(
      start_logit = start_logit,
      entropy = as.numeric(entropy)
    )
  )

  #---------------------------------------
  #Calculate correlation and print results
  #---------------------------------------
  cor_effect_test <- cor.test(entropy, logit_effect)
  cor_prob_test <- cor.test(entropy, conv_prob)

  rs<-list(
    gmrCor = cor_effect_test$estimate,
    cor_prob = cor_entropy_prob <- cor_prob_test$estimate,
    effect_p_value = cor_effect_test$p.value,
    prob_p_value = cor_prob_test$p.value,
    effect_conf_int = cor_effect_test$conf.int,
    prob_conf_int = cor_prob_test$conf.int,
    mean_conv = mean(1 / (1 + exp(-logit))),
    mean_entropy = mean(entropy)
  )
  start_conv_prob <- 1 / (1 + exp(-start_logit))
  entropy_summary <- rbind(
    entropy_summary,
    data.frame(
      start_logit = start_logit,
      start_conv_prob = start_conv_prob,
      mean_conv_prob = rs$mean_conv,
      mean_entropy = rs$mean_entropy
    )
  )

  cat(sprintf(
    "\nStarting Logit: %.4f | Starting Conv Prob: %.4f | Mean Conv Prob: %.4f | Mean Entropy: %.4f\n",
    start_logit, start_conv_prob, rs$mean_conv, rs$mean_entropy
   ))
  format_cor_result("Entropy vs Conversion Probability", cor_prob_test)
  format_cor_result("Entropy vs Logit Effect", cor_effect_test)

   iSeed <-iSeed + 200
}

#-------------------------------------------------------------
#Plot starting conversion probability against mean entropy
#-------------------------------------------------------------
entropy_summary <- entropy_summary[order(entropy_summary$start_conv_prob), ]

png("pmb_entropy_by_start_prob.png", width = 900, height = 650)
plot(
  entropy_summary$start_conv_prob * 100,
  entropy_summary$mean_entropy,
  type = "b",
  pch = 19,
  col = "#0072B2",
  lwd = 2,
  xlab = "Starting Conversion Probability (%)",
  ylab = "Mean Entropy",
  main = "Mean Entropy by Starting Conversion Probability"
)
grid()
text(
  entropy_summary$start_conv_prob * 100,
  entropy_summary$mean_entropy,
  labels = paste0("logit ", entropy_summary$start_logit),
  pos = 4,
  cex = 0.8
)
dev.off()

cat("\nEntropy summary:\n")
print(entropy_summary)
cat("\nGraph saved to: pmb_entropy_by_start_prob.png\n")

png("pmb_entropy_by_mean_conv_prob.png", width = 900, height = 650)
plot(
  entropy_summary$mean_conv_prob * 100,
  entropy_summary$mean_entropy,
  type = "b",
  pch = 19,
  col = "#D55E00",
  xlab = "Mean Conversion Probability (%)",
  ylab = "Mean Entropy",
  main = "Mean Entropy by Mean Conversion Probability"
)
grid()
text(
  entropy_summary$mean_conv_prob * 100,
  entropy_summary$mean_entropy,
  labels = paste0("logit ", entropy_summary$start_logit),
  pos = 4,
  cex = 0.8
)
dev.off()

cat("Graph saved to: pmb_entropy_by_mean_conv_prob.png\n")

png("pmb_entropy_boxplot_by_start_logit.png", width = 900, height = 650)
boxplot(
  entropy ~ factor(start_logit, levels = sort(unique(start_logit))),
  data = entropy_detail,
  col = "#56B4E9",
  border = "#1A2540",
  xlab = "Base Logit",
  ylab = "Entropy",
  main = "Entropy Distribution by Base Logit"
)
grid()
dev.off()

cat("Graph saved to: pmb_entropy_boxplot_by_start_logit.png\n")




