#Task: Creating Stone Price Index

options(RStata.StataPath='ssh -q stata.sesync.org /usr/local/stata15/stata')
options(RStata.StataVersion=15)

library(RStata)
library(foreign)
library(Hmisc)
library(psych)
library(data.table)
library(xtable)
library(stargazer)
library(mvoutlier)
library(dplyr)
library(broom)
library(texreg)
library(naniar)
library(expss)

dataset <- read.dta("data.dta")
#### Assign Labels to Dataset ####

dataset = 
  apply_labels(dataset, 
               xfath = "Food at home exp",
               xrest = "Food from restaurant exp", 
               xhhop = "Household operations exp" ,
               xwclth = "Women's clothing exp", 
               xmclth = "Men's clothing exp" ,
               xkclth = "Children's clothing exp" , 
               xcaruse =  "Car operations exp"  ,
               xtran = "Public transportation exp", 
               xcare = "Personal care exp" , 
               xrecr = "Recreation exp",
               xtob =  "Tobbacco exp",
               xalc = "Alcohol exp", 
               pfath = "Food at home price",
               prest = "Food from restaurant price", 
               phhop = "Household operations price",
               pwclth = "Women's clothing price", 
               pmclth =  "Men's clothing price", 
               pkclth = "Children's clothing price",
               pcaruse = "Car operations price", 
               ptran =  "Public transportation price",
               pcare = "Personal care price",
               precr ="Recreation price",
               ptob = "Tobbacco price" ,
               palc =  "Alcohol price" 
  )


# create new variables #
dataset$xtran1 = dataset$xtran + dataset$xcaruse
dataset$xserv = dataset$xhhop + dataset$xcare
dataset$xvices = dataset$xalc + dataset$xtob
dataset$xcloth = dataset$xwclth + dataset$xmclth + dataset$xkclth
##############################################################
#       Create new prices for the commodities #
##############################################################

dataset$total_exp <- as.numeric(apply(dataset[,8:19], 1, sum))
##############################################################
#       Create log prices for the commodities #
##############################################################
dataset[paste0(names(dataset)[20:31], "_log_price")] <- lapply(dataset[20:31], log)
##############################################################
#       Create generate quantities for the commodities #
##############################################################
dataset[paste0(names(dataset)[8:19], "_quantity")] <- dataset[, 8:19] / dataset[ , 20:31]
dataset[paste0(names(dataset)[52:63], "_log")] <- lapply(dataset[52:63], log)
##############################################################
#       Create  budget shares for the commodities #
##############################################################
dataset[paste0(names(dataset)[8:19], "_budgetshare")] <- dataset[, 8:19] / dataset$total_exp

dataset = dataset %>%
  group_by(time,region)%>%
  mutate(
    xfath_budgetshare_mean = mean(xfath_budgetshare),
    xrest_budgetshare_mean = mean(xrest_budgetshare),
    xhhop_budgetshare_mean = mean(xhhop_budgetshare),
    xwclth_budgetshare_mean = mean(xwclth_budgetshare),
    xmclth_budgetshare_mean = mean(xmclth_budgetshare),
    xkclth_budgetshare_mean = mean(xkclth_budgetshare),
    xcaruse_budgetshare_mean = mean(xcaruse_budgetshare),
    xtran_budgetshare_mean = mean(xtran_budgetshare),
    xcare_budgetshare_mean = mean(xcare_budgetshare),
    xrecr_budgetshare_mean = mean(xrecr_budgetshare),
    xtob_budgetshare_mean = mean(xtob_budgetshare),
    xalc_budgetshare_mean = mean(xalc_budgetshare)
  )

dataset[paste0(names(dataset)[76:87], "_mean")] <- lapply(dataset[76:87], mean)
##############################################################
#       Create  price index for the commodities #
##############################################################
dataset <- dataset %>% replace_with_na_all(condition = ~.x == -Inf)

dataset1 = dataset %>%
  group_by(time,region)%>%
  mutate(
    ptrans = (xtran_budgetshare_mean/(xtran_budgetshare_mean + xcaruse_budgetshare_mean))*ptran_log_price + 
      (xcaruse_budgetshare_mean/(xtran_budgetshare_mean + xcaruse_budgetshare_mean))*pcaruse_log_price,
    ptrans = exp(ptrans),
    pserv = (xhhop_budgetshare_mean/(xhhop_budgetshare_mean + xcare_budgetshare_mean))*phhop_log_price + 
      (xcare_budgetshare_mean/(xhhop_budgetshare_mean + xcare_budgetshare_mean))*pcare_log_price,
    pserv = exp(pserv),
    pvices = (xalc_budgetshare_mean/(xalc_budgetshare_mean + xtob_budgetshare_mean))*palc_log_price + 
      (xtob_budgetshare_mean/(xalc_budgetshare_mean + xtob_budgetshare_mean))*ptob_log_price,
    pvices = exp(pvices),
    pcloth = (xwclth_budgetshare_mean/(xwclth_budgetshare_mean +xmclth_budgetshare_mean + xkclth_budgetshare_mean))*pwclth_log_price + 
      (xmclth_budgetshare_mean/(xwclth_budgetshare_mean +xmclth_budgetshare_mean + xkclth_budgetshare_mean))*pmclth_log_price + 
      (xkclth_budgetshare_mean/(xwclth_budgetshare_mean +xmclth_budgetshare_mean + xkclth_budgetshare_mean))*pkclth_log_price ,
    pcloth = exp(pcloth)
    
  )


dataset1 = 
  apply_labels(dataset1, 
               ptrans = "Transportation price",
               pserv = "Services price",
               pvices = "Vices pricee",
               pcloth = "Clothing price"
               
  )

(dataset1$ptrans[which(dataset1$time == 5 & dataset1$region == 3)])

dataset1 = dataset1 %>%
  mutate(
    lptrans = log(ptrans),
    lpserv = log(pserv),
    lpvices = log(pvices),
    lpcloth = log(pcloth)
  )

dataset1 = dataset1 %>%
  mutate(
    wtrans = xtran_budgetshare + xcaruse_budgetshare,
    wserv =xhhop_budgetshare + xcare_budgetshare,
    wvices = xalc_budgetshare + xtob_budgetshare,
    wcloth = xwclth_budgetshare +xmclth_budgetshare + xkclth_budgetshare
  )

##############################################################
#     Stone Price Index #
##############################################################

dataset1$stp_index = exp(dataset1$xfath_budgetshare*dataset1$pfath_log_price + 
                           dataset1$xrest_budgetshare*dataset1$prest_log_price + 
                           dataset1$wcloth*dataset1$lpcloth + 
                           dataset1$wtrans*dataset1$lptrans + 
                           dataset1$wserv*dataset1$lpserv + 
                           dataset1$xrecr_budgetshare*dataset1$precr_log_price + 
                           dataset1$wvices*dataset1$lpvices)

#### real total expenditure

dataset1$lrxtotal = log(dataset1$total_exp) - log(dataset1$stp_index)

write.dta(dataset1, "dataset.dta")

