# Accompanying code to the 2019 version of paper.
# A container with all packages in order to run the examples2019.R and the supplementary_code2019.R 
# is available at docker hub and can be accessed via: docker run -p 8787:8787 -e PASSWORD=pwd grho/cswr.

### example 1: load data
library(scorecard)
data(germancredit)
# transform character variable purpose into factor
germancredit$purpose <- as.factor(germancredit$purpose)

tv <- split_df(germancredit, y = "creditability", ratio = c(0.7,0.3) , seed = 42, 
               no_dfs = 2, name_dfs = c("train", "valid"))
train <- tv$train
valid <- tv$valid

# several packages require the target variables to take values 0/1 
train2 <- train; valid2 <- valid
train2$creditability <- as.integer(train2$creditability == "good")
valid2$creditability <- as.integer(valid2$creditability == "good")



### example 2: automatic binning
library(scorecard)
bins         <- woebin(train, y = "creditability", method = "tree")

# binning results for variable purpose
options(digits = 3)
bins$purpose[,c(2,3,4,5,6,7,8,10)]

# vizualize bins for variable purpose
woebin_plot(bins, x = "purpose", line_value = "woe")



### example 3: apply binning to data
train_bins <- woebin_ply(train, bins, to = "bin")
valid_bins <- woebin_ply(valid, bins, to = "bin")



### example 4: computing and applying WoEs (based on example 3)
library(klaR)
# woe() requires variable type factor 
train_bins <- dplyr::mutate_if(train_bins, sapply(train_bins, is.character), as.factor)
valid_bins <- dplyr::mutate_if(valid_bins, sapply(valid_bins, is.character), as.factor)

# Compute WoEs on training data
woe_model <- woe(creditability ~ ., data = train_bins)
# ...woes for variable purpose
woe_model$woe$purpose_bin

# apply WoEs
train_woes <- data.frame(creditability = train_bins$creditability, 
                         woe_model$xnew)
valid_woes <- predict(woe_model, valid_bins)



### example 5: computing IVs  (based on example 4)
library(klaR)
woe_model <- woe(creditability ~ ., data = train_bins, zeroadj = 0.5)
# ...the IVs are automatically computed and can be assessed via: 
woe_model$IV



### example 6: population stability analysis for all variables
library(creditR)
SSI.calc.data(train_bins, valid_bins, "creditability")



### example 7: population stability analysis for a single variable
library(riskr)
# PSI for binned variable purpose (based on example 3)
psi(train_bins$purpose_bin, valid_bins$purpose_bin)

psi_purp <- psi(train_bins$purpose_bin, valid_bins$purpose_bin)
psi_purp$table[,-c(2,4)]



### example 8: visualizing correlations (based on example 4)
# reordered correlation matrix
library(corrplot)
# crop redundant prefixes from variable names for plot
X <- train_woes
names(X) <- substr(names(X), 5, 12)
cmat <- cor(X[,-(1:2)])
corrplot(cmat, order = "hclust", method = "ellipse", hclust.method = "complete")

# phylogenetic tree
library(ClustOfVar)
library(ape)
vctree  <- hclustvar(X.quanti = X[,-(1:2)]) 
plot(as.phylo(vctree), type = "fan")



### example 9: variable clustering using ClustVarLV
library(ClustVarLV)
clverg <- CLV(train_woes[,-(1:2)], method = 1)
plot(clverg)
summary(clverg, K = 3)  



### example 10: Cramer's V based variable selection  (based on example 3)
library(scorecardModelUtils)
# package requires 0/1 target:
train_bins2 <- train_bins
train_bins2$creditability <- as.integer(train_bins2$creditability == "good")

# first data frames of IVs and Cramer's V have to be computed
ivtable   <- iv_table(train_bins2, "creditability", cat_var_name = names(train_bins2)[-1])
cvtable   <- cv_table(train_bins2, names(train_bins2)[-1])
selection <- cv_filter(cvtable$cv_val_tab, ivtable$iv_table, threshold = 0.3)
selection



### example 11: BIC variable selection (based on example 4)
# column 2 (variable foreign.worker_bin) excluded as binned variable has only one level  
null <- glm(creditability ~ 1, data = train_woes[,-2], family = binomial)
full <- glm(creditability ~ ., data = train_woes[,-2], family = binomial)
bicglm <- step(null, scope=formula(full), direction="both", k=log(nrow(train_woes)))



### example 12: VIF (based on example 11)
car::vif(bicglm)
scorecard::vif(bicglm)
creditR::vif.calc(bicglm)



### example 13: calculation of scores (based on example 2)
sc2    <- scorecard2(bins = bins, dt = train, y = 'creditability', x = names(train)[1:19])
# note: variable 20 (foreign.worker) not used (cf. also example 11)
sc2
train_scored <- scorecard_ply(train, sc2, only_total_score = FALSE)
valid_scored <- scorecard_ply(valid, sc2, only_total_score = FALSE)



### example 14: output from example 13 for variables 'duration.in.month' and 'purpose'
sc2$duration.in.month[,c(1,2,4,5,6,7,13)]
sc2$purpose[,c(1,2,4,5,6,7,13)]



### example 15: scorecard points for model based on bins, not WoEs (based on example 4)
library(scorecardModelUtils)
# create glm using factor variables -- foreign worker excluded (cf. above) 
full_bins <- glm(creditability ~ ., data = train_bins[,-21], family = binomial)
# calculate scorecard points from effects
sc3 <- scalling(train_bins, "creditability", model = full_bins, point = 15, factor = 2)	
sc3

# apply scorecard to new data
scoring(valid_bins, target = "creditability", sc3)



### example 16: Gini coefficient using {pROC} (based on example 13)
library(pROC)
curve <- roc(valid$creditability, valid_scored$score, levels = c("good","bad"), direction = ">")
# levels = c("controls", "cases"), 
# direction = controls > cases 
auc(curve)
# gini coefficient:
2 * (auc(curve) - 0.5)
# confidence limit:
ci(auc(curve), method = "bootstrap")



### example 17: score bin frequencies (based on example 13) 
library(scorecard)
gt <- gains_table(valid_scored$score, valid$creditability, bin_num = 8)
gt[,c(2,4,5,6,7,8,10,11,12)]



### example 18: scorecard performance summary (based on example 13)
library(smbinning)
perf_dat <- data.frame("creditability" = as.integer(valid$creditability == "good"), 
                       "score" = valid_scored$score)
smbinning.metrics(perf_dat, "score", "creditability", cutoff = 450) 

# roc curve, ecdf, score distribution and gain chart
library(riskr)
gg_perf(as.integer(valid$creditability == "good"), valid_scored$score)



### example 19: Rating calibration (based on example 13)
library(creditR)
# calculate PDs from scores
odds <- 1/19 * 2^(-(valid_scored$score - 600)/50)
pd   <- odds / (1 + odds)
pd.dat <- data.frame(pd = pd, creditability = as.integer(valid$creditability == "bad"))

# aggregate scores to rating grades 
mscale <- creditR::master.scale(pd.dat, "creditability", "pd") 

# transform $Bad.Rate into numeric
mscale$Bad.Rate <- as.numeric(gsub("%","",mscale$Bad.Rate))/100

# test calibration of the rating grades
chisquare.test(mscale, "PD", "Bad.Count", "Total.Observations")
bintest <- Binomial.test(mscale, "Total.Observations", "PD", "Bad.Rate")
bintest[,c(1,3,4,5,8,9,14)]



### example 20: reject inference using parcelling (based on example 4) 
library(scoringTools)
# use validation data as 'rejects' for this example
# ...remove target variable and constant variable foreign.worker_bin
reject_woes <-  valid_woes[,-(1:2)]
# apply parcelling
set.seed(42) # reproducibility
ri_parc <- parcelling(xf = train_woes[,-(1:2)], xnf = reject_woes, 
                      yf = ifelse(train_woes[,1] == "bad", 1, 0), 
                      probs = c(0, 0.25, 0.5, 0.7, 0.8, 0.9, 1), alpha = rep(1, 6))
# final model after reject inference
class(ri_parc@infered_model)   
# observations weights
ri_parc@infered_model$weights
# combined sample after parcelling (note automatically renamed variables)
str(ri_parc@infered_model$data)

# recompute WoEs on combined sample using weight (cf. also example 4)
combined_bins                <- rbind(train_bins, valid_bins)
combined_bins$creditability  <- as.factor(ifelse(ri_parc@infered_model$data$labels == 1, 
                                                 "bad", "good")) 
library(klaR)
woe_model_after_ri <- woe(creditability ~ ., data = combined_bins, 
                          weights = ri_parc@infered_model$weights)
combined_woes      <- data.frame(creditability = combined_bins$creditability, 
                                 woe_model_after_ri$xnew)
