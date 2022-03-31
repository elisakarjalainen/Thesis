setwd("C:/Users/35840/Documents/#MASTER THESIS/full-oldb-20220110 (1)")

# Loading libraries
require(dplyr)
require(plyr)
require(stringr)
require(reshape2)
require(pdftools)
require(tidyverse)
require(ggplot2)
require(remotes)
require(TimeWarp)
require(synthdid)
require(lfe)
require(gridExtra)


entities <- read.csv("nodes-entities.csv", header = T)
intermediaries <- read.csv("nodes-intermediaries.csv", header = T)
officers <- read.csv("nodes-officers.csv", header = T)
others <- read.csv("nodes-others.csv", header = T)

full.df <- rbind.fill(entities, intermediaries, officers, others)
full.df$sourceID <- str_extract(full.df$sourceID, ".+?(?= )")
full.df$name <- toupper(full.df$name)
full.df$jurisdiction_description <- toupper(full.df$jurisdiction_description)

#-------------------------------------------------------------------------------

# USA incorporation per tax haven per year:

#-------------------------------------------------------------------------------

# full.df filtered: country = USA and incorporation date NAs removed
usa.only <- full.df[full.df$country_codes=="USA",]
full.df %>%
  .[grep(";", .$country_codes),] %>%
  .[grep("USA", .$country_codes),] -> usa.doubles

usa.df <- rbind(usa.only, usa.doubles)

# Extract months and years from incorporation dates
usa.df$incorporation_date <- as.Date(usa.df$incorporation_date, format= "%d-%b-%Y")
usa.df$year <- format(usa.df$incorporation_date,"%Y")
usa.df$month <- format(usa.df$incorporation_date,"%m")

# Remove NAs, check for maximum and minimum year
usa.df <- usa.df[complete.cases(usa.df$incorporation_date),]
max(usa.df$incorporation_date)
min(usa.df$incorporation_date)

# Remove undetermined from usa.df
usa.df <- subset(usa.df, jurisdiction_description != "UNDETERMINED")

# Data frame: Incorporation dates per year per tax haven
count.data <- function(dataframe){
  data <- data.frame("Tax havens" = dataframe$jurisdiction_description,
                     "Years" = dataframe$year)
  entities <- dcast(data, data$Years ~ data$Tax.havens)
  rownames(entities) <- entities[,1]
  entities <- entities[,-1]
  
  return(entities)
}

count.entities <- count.data(usa.df)

# Checking the jurisdiction frequencies and select locations with 100+ entries 
jur <- as.data.frame(table(usa.df$jurisdiction_description))
over.100 <- jur[jur$Freq > 100,]

#===============================================================================
# TABLE ON THE SOURCES OF U.S. DATA
#===============================================================================
source.df <- data.frame(matrix(nrow=0,ncol=6))
for (i in over.100$Var1){
  leaks <- c()
  a <- i
  for (j in c("Bahamas", "Offshore", "Panama", "Paradise", "Pandora")){
    b <- usa.df[usa.df$jurisdiction_description==i,]
    b <- length(which(b$sourceID==j))
    leaks <- append(leaks,b)
  }
  row <- append(a,leaks)
  source.df <- rbind(source.df,row)
  source.df
}
colnames(source.df)<-c("Jurisdiction", "Bahamas Leaks", "Offshore Leaks","Panama Papers", "Paradise Papers", "Pandora Papers")
source.df[,2:6] <- lapply(source.df[,2:6], as.numeric)
source.df$Total <- rowSums(source.df[,-1])

#-------------------------------------------------------------------------------

# TIEA dates between USA and other countries:

#-------------------------------------------------------------------------------

url <- c("https://openaccess.nhh.no/nhh-xmlui/bitstream/handle/11250/2578161/1918.pdf?sequence=1&isAllowed=y")
raw_text <- map(url, pdf_text)
raw_text <- raw_text[[1]][28] # page 28 of the pdf

clean_table1 <- function(raw) {
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  table_start <- stringr::str_which(raw, "Date")
  table_end <- stringr::str_which(raw, "1 Jan 1996")
  table_end <- table_end[min(which(table_end > table_start))]
  
  #Build the table  and remove special characters
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  #Create a list of column names 
  colnames(data_table) <- c("Agreement", "Date","Country","SH","Agreement1", "Date1","Country1","SH1")
  data_table
}

results <- map_df(raw_text, clean_table1) 
part2 <- results[,5:8] 
colnames(part2) <- c("Agreement", "Date","Country","SH")

tiea_dates <- rbind(results[,1:4],part2) # enforcement dates
tiea_dates$Country <- toupper(tiea_dates$Country)
tiea_dates$Date <- as.Date(tiea_dates$Date, format="%d %b %Y")

# Fixing BVI
tiea_dates$Country[tiea_dates$Country == "VIRGIN ISLANDS"] <- "BRITISH VIRGIN ISLANDS"


#-------------------------------------------------------------------------------

# Regression - YEAR

#-------------------------------------------------------------------------------
# Plots to demonstrate trend in incorporation over the years
# individual countries (use this function):
bar_plots <- function(name){
  barplot(t(as.matrix(count.entities[name])),
          xlab = "Year", ylab = "Number of new entities created",
          main = paste("New offshore entities created over time:",name))
}
# overall plot:
barplot(t(as.matrix(count.entities)),
        xlab = "Year", ylab = "Number of new entities created",
        main = "New offshore entities created over time: Overall")

# All plots in one figure
par(mfrow=c(3,2))
lapply(over.100$Var1,bar_plots)

# Creating data frames for all jurisdictions
# List of jurisdictions with over 100 observations
namelist <- as.vector(over.100$Var1)

jur_df <- function(name, timedata){
  df <- data.frame("Jurisdiction" = name,
                   "Year" = rownames(timedata),
                   "Entities" = timedata[,name == colnames(timedata)])
  date <- format(tiea_dates$Date[tiea_dates$Country == name], "%Y")
  df$TIEA <- if_else(df$Year < date, 0, 1)
  return(df)}

all <- list()
for (m in 1:length(namelist)){
  all[[m]] <- jur_df(namelist[m], count.entities)
}
names(all) <- namelist

# Using the same list to get linear regression results

lin_reg <- data.frame("Country" = namelist)

for (c in 1:length(lin_reg$Country)){
  df <- all[[c]]
  reg <- lm(Entities ~ TIEA, data = df)
  sum_reg <- summary(reg)
  lin_reg$Intercept[c] <- format(round(reg$coefficients[1], 6))
  lin_reg$TIEA_estimate[c] <- sum_reg$coefficients[2]
  lin_reg$TIEA_Error[c] <- sum_reg$coefficients[4]
  lin_reg$P_Value[c] <- format(round(sum_reg$coefficients[8], 12))
  lin_reg$Residual_Std_Error[c] <- sum_reg$sigma
}


# ===================== INCOMPLETE BELOW ===================== #
### Fixed effect - pooled regression
combined.df <- bind_rows(all)

#felm <- felm(`Entities`~`TIEA` | `Jurisdiction`, data = combined.df)
#summary(felm)

within <- plm(`Entities`~`TIEA` + factor(`Jurisdiction`), data = combined.df, model = 'within')
random <- plm(`Entities`~`TIEA`+ factor(`Jurisdiction`), data = combined.df, model = 'random')
summary(random)

twoways <- plm(`Entities`~`TIEA`+ factor(`Jurisdiction`), data = combined.df,
               effect = 'twoways')

phtest(random, felm)

phtest(random, within)
# How to interpret results?

## The estimate values are same for both of the above models, indicating
## that 
## A fixed-effects model supports prediction about only 
## the levels/categories of features used for training. Meaning that 
## The Hausman test is a test that the fixed effects and random effects 
## estimators are the same. If you can conclude that they are the same 
## one can conclude that the omitted effects are uncorrelated with 
## the x variables and you can use random effects estimates.


#=====================================================================#
# SynthDiD - TEST for 3 countries
# Control = all no-TIEA countries
#==============================================================================# 

# Looking for the countries USA does not have a TIEA with
all_jur <- unique(usa.df$jurisdiction_description)
yes_tiea <- intersect(all_jur,tiea_dates$Country)
no_tiea <- all_jur[! all_jur %in% yes_tiea] %>%
  .[! . %in% c("UNDETERMINED")] 
# British Virgin Islands = Virgin Islands in usa.df, and for 'undetermined' we
  # cannot know if it includes some countries USA has TIEA with. Hence, excluded.

# Creating panel data with no_tiea
control <- function(list, timedata){
  panel_control = data.frame(matrix(nrow = 0, ncol = 4))
  colnames(panel_control) <- c("Jurisdiction","Year","Entities","TIEA")
  for (a in list){
    b <- data.frame("Jurisdiction" = a,
                    "Year" =  rownames(timedata),
                    "Entities" = timedata[[a]],
                    "TIEA" = 0)
    panel_control <- rbind(panel_control, b)}
  return(panel_control)
}

# Treatment = FUNCTION 
treatment <- function(g, data){
  df <- all[[g]][all[[g]]$Year %in% rownames(data),]
  panel_data <- rbind(df, panel_control)
  panel_matrix <- panel.matrices(panel_data, unit = 1, time = 2, outcome = 3, treatment = 4)
  tau.hat = synthdid_estimate(panel_matrix$Y, 
                              panel_matrix$N0, 
                              panel_matrix$T0)
  se = sqrt(vcov(tau.hat, method='placebo')) 
  
  top18unitwgt = synthdid_controls(tau.hat)
  synthdid_units_plot(tau.hat, units = rownames(top18unitwgt))
  
  return(synthdid_plot(tau.hat,treated.name = names(all[g])))
}

#------------------------------------------------------------------------------#

bahamas.synth <- treatment(1,count.entities)
barbados.synth <- treatment(2,count.entities)
bermuda.synth <- treatment(3,count.entities)
bvi.synth <- treatment(4,count.entities)
cayman.synth <- treatment(5,count.entities)
panama.synth <- treatment(6,count.entities)

plots <- list(bahamas.synth,barbados.synth,bermuda.synth,bvi.synth,cayman.synth,panama.synth)
grid.arrange(grobs=plots,top="Synthetic difference-in-difference, years 1898-2016")

#==============================================================================#
# SynthDiD: TREATMENT LOOP, all TIEA countries
#==============================================================================#

yes_tiea <- yes_tiea[-3] # for some reason, including New Zealand returns an error -> excluded

did_results_frame <- data.frame(matrix(nrow = 0, ncol = 3))
for (z in yes_tiea){
  country.df <- jur_df(z,count.entities)
  
  # synth DiD for the country
  panel_data <- rbind(country.df, panel_control)
  panel_matrix <- panel.matrices(panel_data, unit = 1, time = 2, outcome = 3, treatment = 4)
  tau.hat = synthdid_estimate(panel_matrix$Y, 
                              panel_matrix$N0, 
                              panel_matrix$T0)
  se = sqrt(vcov(tau.hat, method='placebo'))
  
  # append to df
  did_results_frame <- rbind(did_results_frame, c(z, as.character(tau.hat), se))
  did_results_frame
  }
colnames(did_results_frame) <- c("Jurisdiction", "DiD", "Error")

#------------------------------------------------------------------------------#

# SynthDiD, years 2000-2010

#------------------------------------------------------------------------------#

# take years 2000-2010
which(rownames(count.entities) == "2000") 
which(rownames(count.entities) == "2010") 
newdata <- count.entities[38:48,]

# Creating panel data with no_tiea, adding countries with TIEA later than 2010
panel_control = data.frame(matrix(nrow = 0, ncol = 4))
colnames(panel_control) <- c("Jurisdiction","Year","Entities","TIEA")

d <- as.Date("2010-01-01")
add <- tiea_dates[which(tiea_dates$Date >= d),"Country"] %>%
  intersect(all_jur,.)
all_control <- append(no_tiea, add)

for (a in all_control){
  b <- data.frame("Jurisdiction" = a,
                  "Year" =  rownames(newdata),
                  "Entities" = newdata[[a]],
                  "TIEA" = 0)
  panel_control <- rbind(panel_control,b)
}

#------------------------------------------------------------------------------#
# Treatment = Bahamas
didplot_bahamas <- treatment(1, newdata)

# Treatment = BVI
didplot_bvi <- treatment(4, newdata)

# Treatment = CAYMAN ISLANDS 
didplot_cayman <- treatment(5, newdata)

# Panama added to control group - so excluded

# Check TIEA dates
# tiea_dates [tiea_dates$Country == "BARBADOS",]
# BERMUDA(2) = 1988-12-02 and 
# BARBADOS(3) = 1984-11-03 both have TIEA before 2000, hence, the 2000-2016
# range is not applicable to them.

#------------------------------------------------------------------------------#

# WORK WITH MONTHLY DATA - 12 months before & after TIEA

#------------------------------------------------------------------------------#

# Data frame: Incorporation dates per month per tax haven
usa.df$year_month <- format(usa.df$incorporation_date,"%Y-%m")
count.data2 <- data.frame("Tax havens" = usa.df$jurisdiction_description,
                          "Months" = usa.df$year_month)
count.entities2 <- dcast(count.data2, count.data2$Months ~ count.data2$Tax.havens)
rownames(count.entities2) <- count.entities2[,1]
count.entities2 <- count.entities2[,-1]


jur_df_monthly <- function(name, time_data){
  df <- data.frame("Jurisdiction" = name,
                   "Month" = rownames(time_data),
                   "Entities" = time_data[,name == colnames(time_data)])
  date <- format(tiea_dates$Date[tiea_dates$Country == name], "%Y-%m")
  df$TIEA <- if_else(df$Month < date, 0, 1)
  ind <- as.numeric(rownames(df[df$Month == date,]))
  df <- df[(ind-12):(ind+12),]
  
  return(df)}

# BARBADOS has 2 agreements dates - TIEA and DTC, hence, excluding BARBADOS
# from the list and making the df separately
barbados <- data.frame("Jurisdiction" = "BARBADOS",
                       "Month" = rownames(count.entities2),
                       "Entities" = count.entities2[,"BARBADOS" == colnames(count.entities2)])
date <- format(tiea_dates$Date[tiea_dates$Country == "BARBADOS"], "%Y-%m")
barbados$TIEA <- if_else(barbados$Month < date[2], 0, 1)
ind <- as.numeric(rownames(barbados[barbados$Month == date[2],]))
barbados <- barbados[(ind-12):(ind+12),]

# LM on monthly data for over.100 countries - 5
newlist <- namelist[-2] # Remove BARBADOS

all.monthly <- list()
for (n in 1:length(newlist)){
  all.monthly[[n]] <- jur_df_monthly(newlist[n], count.entities2)
}

# Add BARBADOS
all.monthly <- append(all.monthly, list(barbados), after = 1)
names(all.monthly) <- namelist

# Using monthly data to get linear regression results
lin_reg_monthly <- data.frame("Country" = namelist)

for (c in 1:length(lin_reg_monthly$Country)){
  df <- all.monthly[[c]]
  reg <- lm(Entities ~ TIEA, data = df)
  sum_reg <- summary(reg)
  lin_reg_monthly$Intercept[c] <- format(round(reg$coefficients[1], 6))
  lin_reg_monthly$TIEA_estimate[c] <- sum_reg$coefficients[2]
  lin_reg_monthly$TIEA_Error[c] <- sum_reg$coefficients[4]
  lin_reg_monthly$P_Value[c] <- format(round(sum_reg$coefficients[8], 12))
  lin_reg_monthly$Residual_Std_Error[c] <- sum_reg$sigma
  lin_reg_monthly$Rsquared[c] <- sum_reg$r.squared
}


### Synth DiD on monthly data
panel_control <- control(no_tiea, count.entities2)
colnames(panel_control)[2] <- "Month"

treatment_monthly <- function(g, data){
  df <- all.monthly[[g]][all.monthly[[g]]$Month %in% rownames(data),]
  panel_control <- filter(panel_control, panel_control$Month %in% all.monthly[[g]]$Month)
  panel_data <- rbind(df, panel_control)
  panel_matrix <- panel.matrices(panel_data, unit = 1, time = 2, outcome = 3, treatment = 4)
  tau.hat = synthdid_estimate(panel_matrix$Y, 
                              panel_matrix$N0, 
                              panel_matrix$T0)
  se = sqrt(vcov(tau.hat, method='placebo')) 
  
  top18unitwgt = synthdid_controls(tau.hat)
  synthdid_units_plot(tau.hat, units = rownames(top18unitwgt))
  
  return(plot(tau.hat, treated.name = names(all.monthly[g])))
}

bahamas_monthly <- treatment_monthly(1, count.entities2)



#========================================================================#
#========================================================================#
#========================================================================#
#                       USING INACTIVATION DATES                         #
#========================================================================#

# Extract months and years from inactivation dates
usa.df2 <- usa.df[,1:24]

usa.df2$inactivation_date <- as.Date(usa.df2$inactivation_date, format= "%d-%b-%Y")
usa.df2$year <- format(usa.df2$inactivation_date,"%Y")
usa.df2$month <- format(usa.df2$inactivation_date,"%m")
usa.df2 <- usa.df2[complete.cases(usa.df2$inactivation_date),]

# Data frame: inactivation dates per year per tax haven
inactivation <- count.data(usa.df2)
inactive_jur <- as.data.frame(table(usa.df2$jurisdiction_description))
closed <- inactive_jur[inactive_jur$Freq > 100,]

bahamas_closed <- jur_df("BAHAMAS", inactivation)
bvi_closed <- jur_df("BRITISH VIRGIN ISLANDS", inactivation)
panama_closed <- jur_df("PANAMA", inactivation)

all_closed <- list(bahamas_closed, bvi_closed, panama_closed)

closed_list <- as.vector(closed$Var1)
lin_reg_closed <- data.frame("Country" = closed_list)

for (c in 1:length(lin_reg_closed$Country)){
  df <- all[[c]]
  reg <- lm(Entities ~ TIEA, data = df)
  sum_reg <- summary(reg)
  lin_reg_closed$Intercept[c] <- format(round(reg$coefficients[1], 6))
  lin_reg_closed$TIEA_estimate[c] <- sum_reg$coefficients[2]
  lin_reg_closed$TIEA_Error[c] <- sum_reg$coefficients[4]
  lin_reg_closed$P_Value[c] <- format(round(sum_reg$coefficients[8], 12))
  lin_reg_closed$Residual_Std_Error[c] <- sum_reg$sigma
}

#----------------------------------------------------------------------------#
# DID INACTIVATION
#----------------------------------------------------------------------------#

no_tiea_inact <- intersect(no_tiea, inactive_jur$Var1)
panel_control <- control(no_tiea_inact, inactivation)


# Treatment = Bahamas
did.inact_bahamas <- treatment(1, inactivation)

# Treatment = BVI
did.inact_bvi <- treatment(4, inactivation)

# Treatment = Panama
did.inact_panama <- treatment(6, inactivation)

did.inact_bahamas
did.inact_bvi
did.inact_panama

plots <- list(did.inact_bahamas, did.inact_bvi, did.inact_panama)
grid.arrange(grobs=plots,top="Inactivation dates synth. DiD, years 1989-2016")

# 2000-2010
which(rownames(inactivation) == "2000")
which(rownames(inactivation) == "2010")
newdata_inact <- inactivation[19:29,]