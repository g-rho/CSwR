# Accompanying code to the 2019 version of paper.
# A container with all packages in order to run the examples2019.R and the supplementary_code2019.R 
# is available at docker hub and can be accessed via: docker run -p 8787:8787 -e PASSWORD=pwd grho/cswr.

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




################################
### Section Binning and woes ###

### snippet 2: workaround for applying bins to new data for ctree based binning using the package {riskr}
library(riskr)

# automatic binning of variable purpose
bins_purpose_riskr <- superv_bin(train2$purpose, train2$creditability, 
                                 min.p = 0.05, min.cri = 0.95, max.depth = 5)

# element where the tree model is stored
bins_purpose_riskr$tree 

# binned training data
head(bins_purpose_riskr$data)

# summary for binning of all variables 
ez_summ_biv(train2, "creditability")

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
train3 <- train2; valid3 <- valid2
names(train3) <- names(valid3) <- gsub("\\.", "_", names(train3))

# identify names of all numeric variables
numerics <- names(train3)[sapply(train3, is.numeric)]

# loop binning through all numeric variables 
bins2_list  <- lapply(numerics, function(z) smbinning(train3, y = "creditability", x = z))

# apply binning to training and validation data for all numeric variables 
for(i in seq(along.with = bins2_list)){
  varname <- paste(numerics[i],"bin",sep="_")
  # check for each variabe whether a binning has been generated (i.e. at least 5 distinct values) 
  if(is.list(bins2_list[[i]])){
    train3 <- smbinning.gen(train3, bins2_list[[i]], varname)
    valid3 <- smbinning.gen(valid3, bins2_list[[i]], varname)
  }
  else{
    train3[[varname]] <- as.factor(train3[[numerics[i]]])
    valid3[[varname]] <- as.factor(valid3[[numerics[i]]])
  }
} 

str(train3) # note: original variables still available




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
train4 <- mapping$base_new  # training data is already mapped
valid4 <- valid2
for(i in 1:nrow(mapping$cat_class_new)){
  vname <- mapping$cat_class_new$Variable_name[i]
  x <- as.character(valid4[[vname]])
  x[x == mapping$cat_class_new$old_class[i]] <- mapping$cat_class_new$new_class[i]
  valid4[[vname]] <- x
}

mapping$cat_class_new[mapping$cat_class_new$Variable_name == "purpose" ,]
table(valid2$purpose, valid4$purpose)




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

library(woeR)
# use train3 (cf. snippet 4) -- variable names must not contain '.'s
bins_dur <- woe_binning(train3, "duration_in_month", dv = "creditability", 
                        min_perc = 0.02, initial_bins = 50, woe_cutoff = 0.1)

train_woe_dur <- apply_woe(train3, bins_dur)
str(train_woe_dur) # original data frame plus additional woe column for duration 




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
###             ...based on example 11 (bicglm) 

# create vector of input variable names in original data set from existing glm object (on woe variables) 
orig.names <- names(coef(bicglm))[-1] # exclude intercept
orig.names <- sub("_bin", "", orig.names)
orig.names <- sub("woe_", "", orig.names)

sc    <- scorecard2(bins = bins, dt = train, y = 'creditability', x = orig.names)
sc
