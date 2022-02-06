# > sessionInfo()
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] scoringTools_0.1.2          smbinning_0.9               Formula_1.2-4               partykit_1.2-15             mvtnorm_1.1-3              
# [6] libcoin_1.0-9               sqldf_0.4-11                RSQLite_2.2.9               gsubfn_0.7                  proto_1.0.0                
# [11] pROC_1.18.0                 scorecardModelUtils_0.0.1.0 creditmodel_1.3.0           ClustVarLV_2.0.1            ape_5.6                    
# [16] ClustOfVar_1.1              corrplot_0.92               riskr_1.0                   creditR_0.1.0               MLmetrics_1.1.1            
# [21] cluster_2.1.2               woeBinning_0.1.6            magrittr_2.0.1              klaR_0.6-15                 MASS_7.3-54                
# [26] scorecard_0.3.6            
# 
# loaded via a namespace (and not attached):
# [1] colorspace_2.0-2    ellipsis_0.3.2      class_7.3-19        PCAmixdata_3.1      rstudioapi_0.13     proxy_0.4-26        farver_2.1.0       
# [8] bit64_4.0.5         fansi_0.5.0         lubridate_1.8.0     codetools_0.2-18    splines_4.1.2       doParallel_1.0.16   cachem_1.0.6       
# [15] knitr_1.37          jsonlite_1.7.2      shiny_1.7.1         compiler_4.1.2      assertthat_0.2.1    Matrix_1.4-0        fastmap_1.1.0      
# [22] cli_3.1.0           later_1.3.0         htmltools_0.5.2     tools_4.1.2         gtable_0.3.0        glue_1.6.0          reshape2_1.4.4     
# [29] dplyr_1.0.7         Rcpp_1.0.7          carData_3.0-4       vctrs_0.3.8         nlme_3.1-153        iterators_1.0.13    inum_1.0-4         
# [36] xfun_0.29           stringr_1.4.0       openxlsx_4.2.5      mime_0.12           miniUI_0.1.1.1      lifecycle_1.0.1     scales_1.1.1       
# [43] hms_1.1.1           promises_1.2.0.1    parallel_4.1.2      yaml_2.2.1          memoise_2.0.1       gridExtra_2.3       ggplot2_3.3.5      
# [50] labelled_2.9.0      rpart_4.1-15        stringi_1.7.6       highr_0.9           foreach_1.5.1       randomForest_4.6-14 e1071_1.7-9        
# [57] zip_2.2.0           shape_1.4.6         chron_2.3-56        rlang_0.4.12        pkgconfig_2.0.3     evaluate_0.14       lattice_0.20-45    
# [64] ROCR_1.0-11         purrr_0.3.4         labeling_0.4.2      bit_4.0.4           tidyselect_1.1.1    gbm_2.1.8           plyr_1.8.6         
# [71] R6_2.5.1            generics_0.1.1      combinat_0.0-8      DBI_1.1.2           pillar_1.6.4        haven_2.4.3         survival_3.2-13    
# [78] abind_1.4-5         tibble_3.1.6        crayon_1.4.2        car_3.0-12          questionr_0.7.5     xgboost_1.5.0.2     utf8_1.2.2         
# [85] rmarkdown_2.11      data.table_1.14.2   blob_1.2.2          forcats_0.5.1       digest_0.6.29       xtable_1.8-4        tidyr_1.1.4        
# [92] httpuv_1.6.5        munsell_0.5.0       glmnet_4.1-3        tcltk_4.1.2    


### example 1: load data
library(scorecard)
data(germancredit)
# transform character variable purpose into factor
germancredit$purpose <- as.factor(germancredit$purpose)

tv <- split_df(germancredit, y = "creditability", ratio = c(0.7, 0.3), seed = 42, 
               no_dfs = 2, name_dfs = c("train", "valid"))
train <- tv$train
valid <- tv$valid

# several packages require the target variables to take values 0/1 
train2 <- train; valid2 <- valid
train2$creditability <- as.integer(train2$creditability == "good")
valid2$creditability <- as.integer(valid2$creditability == "good")

# the package creditmodel does not support variables of type Factor
train3 <- as.data.frame(train2)
valid3 <- as.data.frame(valid2)
for (j in which(sapply(train3[,-21], is.factor))) {
  train3[,j] <- as.character(train3[,j])
  valid3[,j] <- as.character(valid3[,j])
}


### example 2: automatic binning
library(scorecard)
bins         <- woebin(train, y = "creditability", method = "tree")

# binning results table for variable purpose
options(digits = 3)
bins$purpose[,c(2,4,5,6,7,8)]

# visualize bins for variable purpose
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


### example 10: Automatic correlation-based variable selection (based on example 4)
library(creditmodel)
# create list of variables sorted according to IV 
iv_list = feature_selector(dat_train = train_woes, dat_test = NULL, 
                           target = "creditability",
                           filter = "IV", iv_cp = 0.02, vars_name = FALSE)
iv_list

# select variables
fast_high_cor_filter(dat = train_woes, com_list = iv_list, p = 0.15, 
                     cor_class = TRUE ,vars_name = TRUE)


### example 11: Cramer's V based variable selection  (based on example 3)
library(scorecardModelUtils)
# package requires 0/1 target:
train_bins2 <- train_bins
train_bins2$creditability <- as.integer(train_bins2$creditability == "good")

# first data frames of IVs and Cramer's V have to be computed
ivtable   <- iv_table(train_bins2, "creditability", cat_var_name = names(train_bins2)[-1])
cvtable   <- cv_table(train_bins2, names(train_bins2)[-1])
selection <- cv_filter(cvtable$cv_val_tab, ivtable$iv_table, threshold = 0.3)
selection


### example 12: BIC variable selection (based on example 4)
# column 2 (variable foreign.worker_bin) excluded as binned variable has only one level  
null <- glm(creditability ~ 1, data = train_woes[,-2], family = binomial)
full <- glm(creditability ~ ., data = train_woes[,-2], family = binomial)
bicglm <- step(null, scope=formula(full), direction="both", k=log(nrow(train_woes)))


### example 13: VIF (based on example 11)
car::vif(bicglm)
scorecard::vif(bicglm)
creditR::vif.calc(bicglm)
creditmodel::lr_vif(bicglm)


### example 14: calculation of scores (based on example 2)
sc    <- scorecard2(bins = bins, dt = train, y = 'creditability', x = names(train)[1:19])
# note: variable 20 (foreign.worker) not used (cf. also example 11)
sc
sc$duration.in.month[,c(1,2,4,5,6,7,13)]
sc$purpose[,c(1,2,4,5,6,7,13)]

train_scored <- scorecard_ply(train, sc, only_total_score = FALSE)
valid_scored <- scorecard_ply(valid, sc, only_total_score = FALSE)


### example 15: scorecard points for model based on bins, not WoEs (based on example 4)
library(scorecardModelUtils)
# create glm using factor variables -- foreign worker excluded (cf. above) 
full_bins <- glm(creditability ~ ., data = train_bins[,-21], family = binomial)
# calculate scorecard points from effects
sc2 <- scalling(train_bins, "creditability", model = full_bins, point = 15, factor = 2)	
sc2

# apply scorecard to new data
scoring(valid_bins, target = "creditability", sc2)


### example 16: directly predict score points from glm object (based on example 12)
library(creditmodel)
train_scored_3 <- score_transfer(bicglm, train_woes, a = 500, b = 20)
valid_scored_3 <- score_transfer(bicglm, valid_woes, a = 500, b = 20)


### example 17: Gini coefficient using {pROC} (based on example 14)
library(pROC)
curve <- roc(valid$creditability, valid_scored$score, levels = c("good","bad"), direction = ">")
# levels = c("controls", "cases"), 
# direction = controls > cases 
auc(curve)
# gini coefficient:
2 * (auc(curve) - 0.5)
# confidence limit:
ci(auc(curve), method = "bootstrap")


### example 18: score bin frequencies (based on example 14) 
library(scorecard)
gt <- gains_table(valid_scored$score, valid$creditability, bin_num = 8)
gt[,c(2,4,5,6,7,8,10,11,12)]


### example 19: scorecard performance summary (based on example 13)
library(smbinning)
perf_dat <- data.frame("creditability" = as.integer(valid$creditability == "good"), 
                       "score" = valid_scored$score)
smbinning.metrics(perf_dat, "score", "creditability", cutoff = 450) 

# roc curve, ecdf, score distribution and gain chart
library(riskr)
gg_perf(as.integer(valid$creditability == "good"), valid_scored$score)


### example 20: Rating calibration (based on example 14)
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


### example 21: reject inference using parcelling (based on example 4) 
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
