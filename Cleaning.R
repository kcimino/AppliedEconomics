library(tidyverse)
library(desc)
library(survey)
library(ggplot2)

# load data
init <- read.csv("ESS1-9e01_1.csv")

# create binary satdem, recode NAs, select only columns of interest, create satdem which is a binary
mydata <- init %>% 
  mutate(
    satecon = ifelse(stfeco > 5 & stfeco <11, 1, ifelse(stfeco == 5 | stfeco > 10, NA, 0)),
    stfdem = factor(ifelse(stfdem>10, NA, stfdem)),
    stfeco = factor(ifelse(stfeco>10, NA, stfeco)),
    hinctnta = factor(ifelse(hinctnta >10, NA, hinctnta)),
    hincfel = factor(ifelse(hincfel > 4, NA, hincfel))
  ) %>% 
 filter(ctzcntr==1 & !is.na(stfdem) & !is.na(satecon)) %>% 
  select(anweight, idno, stfeco, stfdem, hinctnta, hincfel, satecon)


# create design and weight appropriately (as guided by ESS documentation)
library(foreign)
weights <- read.dta("ESS8SDDFe01_1.dta")
weights <- weights %>% filter(cntry == "RU")
weights.all <- left_join(mydata, weights, by = "idno")
rm("init")
design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, data = weights.all)
rm("weights.all")


# create designs for summary data
mydata.su <- mydata %>% 
  mutate(
    stfdem = factor(ifelse(stfdem == 0 | stfdem == 1 | stfdem == 2 | stfdem == 3, 1, 
                    ifelse(stfdem == 4 | stfdem == 5 | stfdem == 6, 2,
                           ifelse(stfdem == 7 | stfdem==8 | stfdem == 9 | stfdem == 10, 3, NA)))),
    stfdem = factor(stfdem),
    hinctnta = factor(ifelse(hinctnta == 0 | hinctnta == 1 | hinctnta == 2 | hinctnta == 3, 1, 
                    ifelse(hinctnta == 4 | hinctnta == 5 | hinctnta == 6, 2,
                           ifelse(hinctnta == 7 | hinctnta==8 | hinctnta == 9 | hinctnta == 10, 3, NA)))),
    hinctnta = factor(hinctnta),
  )
mydata.sat <- mydata.su %>% filter(satecon == 1)
mydata.notsat <- mydata.su %>% filter(satecon == 0)
weights.su <- left_join(mydata.su, weights, by = "idno") 
weights.sat <- left_join(mydata.sat, weights, by = "idno") 
weights.notsat <- left_join(mydata.notsat, weights, by = "idno") 
design.su <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, data = weights.su)
design.sat <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, data = weights.sat)
design.notsat <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight, data = weights.notsat)
rm("mydata.sat","mydata.notsat","weights.sat","weights.notsat","weights", "mydata.su", "weights.su")

# find appropriate statistical summary data using svy (so weights are applied)
svymean(~satecon+stfdem+hinctnta+hincfel, design.su, na.rm =TRUE)
svymean(~satecon+stfdem+hinctnta+hincfel, design.sat, na.rm =TRUE)
svymean(~satecon+stfdem+hinctnta+hincfel, design.notsat, na.rm =TRUE)

rm("design.sat","design.notsat", "design.su")

# graphs, use markdown and copy here later

# create model and get information from it
model.weighted <- svyglm(satecon~stfdem+hinctnta+hincfel, design=design)
summary(model.weighted)


# get r^2 because it's not available in survey package

svyvar(~satecon, design)
r.squared <- 1 - 0.1288195 / 0.18963
adj.r.squared <- 1 - (1 - r.squared) * ((2430 - 1)/(2430-3-1))


# plotting important ones

ggplot(mydata, 
       aes(x = stfecon, 
           fill = stfdem)) + 
  geom_bar(position = "stack")

init %>% 
  ggplot(aes(x=stfecon, fill=cntry))+
  geom_bar(position="fill")+
  labs(
    title= "Is the Primary Casting Stat the Highest Stat?",
    x = "Casting Stat",
    y = "Percentage",
    fill = ""
  )



