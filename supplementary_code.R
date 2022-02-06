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
# [1] klaR_0.6-15                 MASS_7.3-54                 woeBinning_0.1.6            scorecardModelUtils_0.0.1.0 smbinning_0.9              
# [6] Formula_1.2-4               partykit_1.2-15             mvtnorm_1.1-3               libcoin_1.0-9               sqldf_0.4-11               
# [11] RSQLite_2.2.9               gsubfn_0.7                  proto_1.0.0                 riskr_1.0                   scorecard_0.3.6            
# 
# loaded via a namespace (and not attached):
# [1] lubridate_1.8.0     bit64_4.0.5         doParallel_1.0.16   tools_4.1.2         utf8_1.2.2          R6_2.5.1            rpart_4.1-15       
# [8] DBI_1.1.2           questionr_0.7.5     colorspace_2.0-2    gbm_2.1.8           tidyselect_1.1.1    gridExtra_2.3       chron_2.3-56       
# [15] bit_4.0.4           compiler_4.1.2      glmnet_4.1-3        cli_3.1.0           scales_1.1.1        randomForest_4.6-14 proxy_0.4-26       
# [22] stringr_1.4.0       creditR_0.1.0       digest_0.6.29       rmarkdown_2.11      pkgconfig_2.0.3     htmltools_0.5.2     labelled_2.9.0     
# [29] fastmap_1.1.0       highr_0.9           rlang_0.4.12        rstudioapi_0.13     MLmetrics_1.1.1     shiny_1.7.1         shape_1.4.6        
# [36] generics_0.1.1      combinat_0.0-8      jsonlite_1.7.2      dplyr_1.0.7         zip_2.2.0           car_3.0-12          magrittr_2.0.1     
# [43] Matrix_1.4-0        Rcpp_1.0.7          munsell_0.5.0       fansi_0.5.0         abind_1.4-5         lifecycle_1.0.1     stringi_1.7.6      
# [50] yaml_2.2.1          inum_1.0-4          carData_3.0-4       plyr_1.8.6          blob_1.2.2          parallel_4.1.2      promises_1.2.0.1   
# [57] forcats_0.5.1       crayon_1.4.2        miniUI_0.1.1.1      lattice_0.20-45     haven_2.4.3         splines_4.1.2       hms_1.1.1          
# [64] knitr_1.37          pillar_1.6.4        tcltk_4.1.2         xgboost_1.5.0.2     reshape2_1.4.4      codetools_0.2-18    glue_1.6.0         
# [71] evaluate_0.14       data.table_1.14.2   vctrs_0.3.8         httpuv_1.6.5        foreach_1.5.1       creditmodel_1.3.0   gtable_0.3.0       
# [78] purrr_0.3.4         tidyr_1.1.4         assertthat_0.2.1    cachem_1.0.6        ggplot2_3.3.5       xfun_0.29           openxlsx_4.2.5     
# [85] mime_0.12           xtable_1.8-4        e1071_1.7-9         later_1.3.0         class_7.3-19        survival_3.2-13     tibble_3.1.6       
# [92] iterators_1.0.13    memoise_2.0.1       cluster_2.1.2       ellipsis_0.3.2      ROCR_1.0-11       


####################
### Section Data ###

### snippet 1: load data
library(scorecard)
data(germancredit)
# transform character variable purpose into factor
germancredit$purpose <- as.factor(germancredit$purpose)

tv <- split_df(germancredit, y = "creditability", ratio = c(0.7,0.3), seed = 42, 
               no_dfs = 2, name_dfs = c("train", "valid"))
train <- tv$train
valid <- tv$valid

# several packages require the target variables to take values 0/1 
train2 <- train; valid2 <- valid
train2$creditability <- as.integer(train2$creditability == "good")
valid2$creditability <- as.integer(valid2$creditability == "good")
names(train2) <- names(valid2) <- gsub("\\.", "_", names(train2))

# the package creditmodel does not allow variables of type Factor but character instead.
train3 <- as.data.frame(train2)
valid3 <- as.data.frame(valid2)
for (j in which(sapply(train3[,-21], is.factor))) {
  train3[,j] <- as.character(train3[,j])
  valid3[,j] <- as.character(valid3[,j])
}


################################
### Section Binning and woes ###

### snippet 2: workaround for applying bins to new data for ctree based binning using the package {riskr}
library(riskr)

# automatic binning of variable purpose
bins_purpose_riskr <- superv_bin(tibble::as_tibble(train2)$purpose, tibble::as_tibble(train2)$creditability, 
                                 min.p = 0.05, min.cri = 0.95, max.depth = 5)

# element where the tree model is stored
bins_purpose_riskr$tree 

# binned training data
head(bins_purpose_riskr$data)

# summary for binning of all variables 
ez_summ_biv(tibble::as_tibble(train2), "creditability")

# assign bins to new data. 
# ...first, helper variable with name 'variable has to be created'
valid2_bins              <- valid2
valid2_bins$variable     <- valid2_bins$purpose
valid2_bins$purpose_bin  <- as.factor(predict(bins_purpose_riskr$tree, valid2_bins, type = "node"))
valid2_bins$variable     <- NULL # remove helper variable
# no assignment of WoEs possible, for WoEs add e.g. subsequent package {klaR} 
table(valid2_bins$purpose, valid2_bins$purpose_bin)


### snippet 3: store binning of factor variable in lookup table and apply it to data set
# sort factor levels according to default rate
sort(prop.table(table(train$purpose, train$creditability),2)[,1])

# create lookup-table after manual binning
mapping <- list(
  'retraining'            ='retraining/domestic/others/repairs',
  'domestic appliances'   ='retraining/domestic/others/repairs',
  'others'                ='retraining/domestic/others/repairs',
  'repairs'               ='retraining/domestic/others/repairs',
  
  'car (used)'            ='used_car/edu/business',
  'education'             ='used_car/edu/business',
  'business'              ='used_car/edu/business',
  
  'furniture/equipment'   ='furniture/radiotele/new_car',
  'radio/television'      ='furniture/radiotele/new_car',
  'car (new)'             ='furniture/radiotele/new_car'
)

# apply lookup table to data
train_bins           <- train
train_bins$purpose_binned <- as.factor(as.character(mapping[as.character(train_bins$purpose)]))

table(train_bins$purpose, train_bins$purpose_binned)


### snippet 4: Example of looping through all (numeric) variables for package {smbinning} 
library(smbinning)

# additional preprocessing as no points in variable names allowed... 
train4 <- train2; valid4 <- valid2
names(train4) <- names(valid4) <- gsub("\\.", "_", names(train4))

# identify names of all numeric variables
numerics <- names(train4)[sapply(train4, is.numeric)]

# loop binning through all numeric variables 
bins2_list  <- lapply(numerics, function(z) smbinning(train4, y = "creditability", x = z))

# apply binning to training and validation data for all numeric variables 
for(i in seq(along.with = bins2_list)){
  varname <- paste(numerics[i],"bin",sep="_")
  # check for each variabe whether a binning has been generated (i.e. at least 5 distinct values) 
  if(is.list(bins2_list[[i]])){
    train4 <- smbinning.gen(train4, bins2_list[[i]], varname)
    valid4 <- smbinning.gen(valid4, bins2_list[[i]], varname)
  }
  else{
    train4[[varname]] <- as.factor(train4[[numerics[i]]])
    valid4[[varname]] <- as.factor(valid4[[numerics[i]]])
  }
} 

str(train4) # note: original variables still available


### snippet 5: group levels with low frequencies together
library(scorecardModelUtils)

# names of all factor variables 
factors  <- names(train2)[which(sapply(train2[,-21], is.factor))]
# 
mapping <- cat_new_class(base = train2, target = "creditability", cat_var_name = factors, threshold = 0.05)

# mapping is directly applied to 'base' data
head(mapping$base_new) 

# mapping table
head(mapping$cat_class_new)
# mapping for variable purpose
mapping$cat_class_new[mapping$cat_class_new$Variable_name == "purpose" ,]

# script to apply mapping to new data
train5 <- mapping$base_new  # training data is already mapped
valid5 <- valid2
for(i in 1:nrow(mapping$cat_class_new)){
  vname <- mapping$cat_class_new$Variable_name[i]
  x <- as.character(valid5[[vname]])
  x[x == mapping$cat_class_new$old_class[i]] <- mapping$cat_class_new$new_class[i]
  valid5[[vname]] <- x
}

mapping$cat_class_new[mapping$cat_class_new$Variable_name == "purpose" ,]
table(valid2$purpose, valid5$purpose)


### snippet 6:
library(scorecard)
bins_purpose <- woebin(train, y = "creditability", x = "purpose", method = "chimerge") 

# sort levels according to default rate
defrates <- prop.table(table(train$purpose, train$creditability), 2)
defrates[order(defrates[,1]),]
# ...comparison with bins 
bins_purpose 
#...bins are merged alphabetically


### snippet 7: function to transform results from woeBinning::woe.binning() ... 
###            ...into a breaks_list that can be imported by scorecard::woebin() 

woeBins2breakslist <- function(bins, data){
  # bins: resulting object of calling woeBinning::woe.binning() on a data frame  
  # data: training data (necessary for numerich variables in order to correct cutpoints from ']' to ')')
  breaks_list = list()
  variables <- unlist(bins[sapply(bins, is.character)])
  dfsIDs    <- which(sapply(bins, is.data.frame))
  for(i in seq(along.with = variables)){
    #i <- 1; i <- 3; i <- 4
    varname <- variables[i] 
    df <- bins[[dfsIDs[i]]]
    if(names(df)[1] == "Group.2"){
      df$Group.1 <- as.character(df$Group.1) # relabel 'missing' category
      df$Group.1[df$Group.1 == "Missing"] <- "missing"
      breaks <- sapply(unique(df$Group.2), function(z) do.call(paste, c(as.list(df$Group.1[df$Group.2 == z]), sep = "%,%")))
      breaks_list[[varname]] <- breaks
    }
    if(names(df)[2] == "cutpoints.final"){
      breaks <- unlist(df[3])
      if(any(breaks == "Missing")) breaks[breaks == "Missing"] <- "missing" # relabel 'missing' category
      # note: {woeBinning} uses ']' whereas {scorecard} uses ')' ~> shift of the cut half distance to next observed value
      suniq <- sort(unique(data[[varname]]))
      breaks[breaks < Inf] <- sapply(breaks[breaks < Inf],function(z) mean(suniq[which(suniq == z) + 0:1])) 
      breaks_list[[varname]] <- breaks
    }
  }
  return(breaks_list)  
}

# automatic binning using package woeBinning
bins_woeb  <- woeBinning::woe.binning(df = as.data.frame(train), target.var = "creditability", 
                                      pred.var = train, min.perc.total = 0.05, stop.limit = 0.1)
# create breaks_list from woe.binning() result... 
wb     <- woeBins2breakslist(bins = bins_woeb, data = train)
# ...and import bins for further use of scorecard package 
bins_imported <- scorecard::woebin(train, y = "creditability", breaks_list = wb)


### snippet 8: assign WoEs
library(scorecard)
bins         <- woebin(train, y = "creditability")
train_woes   <- woebin_ply(train, bins, to = "woe")
str(train_woes) # returns woes

library(woeBinning)
bins       <- woe.binning(as.data.frame(train), "creditability", as.data.frame(train), event.class = "bad")
train_woes <- woe.binning.deploy(train, bins, add.woe.or.dum.var = "woe")
str(train_woes) # returns original, binned and woes


#######################
### Scorecard Model ###

### snippet 9: create scorecard object using the {scorecard} package from woe data using {klaR}

# data compute woe data as in example 1 to 4
library(scorecard)
data(germancredit)
germancredit$purpose <- as.factor(germancredit$purpose)
tv <- split_df(germancredit, y = "creditability", ratio = c(0.7, 0.3), seed = 42, no_dfs = 2, name_dfs = c("train", "valid"))
train <- tv$train
valid <- tv$valid

bins         <- woebin(train, y = "creditability", method = "tree")
train_bins <- woebin_ply(train, bins, to = "bin")
valid_bins <- woebin_ply(valid, bins, to = "bin")

library(klaR)
train_bins <- dplyr::mutate_if(train_bins, sapply(train_bins, is.character), as.factor)
valid_bins <- dplyr::mutate_if(valid_bins, sapply(valid_bins, is.character), as.factor)
woe_model <- woe(creditability ~ ., data = train_bins)
train_woes <- data.frame(creditability = train_bins$creditability, woe_model$xnew)
valid_woes <- predict(woe_model, valid_bins)

null <- glm(creditability ~ 1, data = train_woes[,-2], family = binomial)
full <- glm(creditability ~ ., data = train_woes[,-2], family = binomial)
bicglm <- step(null, scope=formula(full), direction="both", k=log(nrow(train_woes)))

# rename bins object such that it fits names of woe data as returned from klaR
renamed_bins <- bins
for(i in 1:length(renamed_bins))
  renamed_bins[[i]]$variable <- paste("woe",renamed_bins[[i]]$variable,"bin", sep = "_")

sc <- scorecard(renamed_bins, bicglm, points0 = 600, odds0 = 1/19, pdo = 50, basepoints_eq0 = FALSE)
sc


### snippet 10: (re-)compute scorecard using scorecard2() from an existing glm object 
###             ...based on example 12 (bicglm) 

# create vector of input variable names in original data set from existing glm object (on woe variables) 
orig.names <- names(coef(bicglm))[-1] # exclude intercept
orig.names <- sub("_bin", "", orig.names)
orig.names <- sub("woe_", "", orig.names)

sc2    <- scorecard2(bins = bins, dt = train, y = 'creditability', x = orig.names)
sc2
