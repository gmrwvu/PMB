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
for (start_logit in -3:-5) {
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

  #---------------------------------------
  #Calculate correlation and print results
  #---------------------------------------
  rs<-list(
    cor = cor.test(entropy, logit_effect)$estimate,
    cor_prob = cor_entropy_prob <- cor.test(entropy, conv_prob)$estimate,
    mean_conv = mean(1 / (1 + exp(-logit))),
    mean_entropy = mean(entropy)
  )
  cat(sprintf(
    "Starting Logit: %.4f | Correlation w Prob: %.4f | Correlation w Effect: %.4f\n",
    start_logit, rs$cor_prob, rs$cor
   ))
   iSeed <-iSeed + 200
}
