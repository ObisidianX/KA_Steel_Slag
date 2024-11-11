a <- read_csv("https://raw.githubusercontent.com/ObisidianX/KA_Steel_Slag/refs/heads/main/data/gsd_slag.csv")


# Batch Data ############################################################
#############################Additional Packages
library("dplyr")
library("RColorBrewer")  #colour ramp
library(tidyr)
library(readr)

#Adding the main database
db <- read_csv('/Users/reinaldy/Documents/BAM Project/data.csv')

#transform #N/A as NA
db[db == "#N/A"] <- NA

#color set - colorblind friendly
color_brewer_1 <- c("#B2182B",  "#EF8A62", "#FDBBC7",  "#F7F7F7", "#D1E5F0",  "#67A9CF", "#2166AC")

#check unique values
#unique(db$mineral_1_grainsize_category)
#save data to csv
#write.csv(db, file = "db.csv")


####MINERAL WRANGGLING

###############################Classify rock/material based on their grain size
###Mineral 1
#Replace with NA data with a value
db$mineral_1_grainsize[is.na(db$mineral_1_grainsize)] <- "No Mineral"
#Classify minerals by mutating the data
db <- db %>% mutate(mineral_1_grainsize_category = case_when(
  mineral_1_grainsize == "0.063" ~ "Fine", #from basalt, lava, and diabase
  mineral_1_grainsize == "0 to 2" ~ "Coarse", #from basalt
  mineral_1_grainsize == "1 to 3" ~ "Coarse", #from lava and diabase
  mineral_1_grainsize == "No.11 (0.063)" ~ "Fine", #from dunite
  mineral_1_grainsize == "No.11 (0,063)" ~ "Fine", #from dunite
  mineral_1_grainsize == "AFS 80 (0.125)" ~ "Medium", #from dunite
  mineral_1_grainsize == "AFS 20 (1)" ~ "Coarse", #from dunite
  mineral_1_grainsize == "No Mineral" ~ "No Mineral", 
  mineral_1_grainsize == "??" ~ "Mixed",
  mineral_1_grainsize == "0 to 3" ~ "Mixed",
  mineral_1_grainsize == "-" ~ "Mixed",
  mineral_1_grainsize == "3" ~ "Coarse",
  mineral_1_grainsize == "0.5" ~ "Medium",
  mineral_1_grainsize == "0.1" ~ "Fine",
  mineral_1_grainsize == "Mixed" ~ "Mixed",
  mineral_1_grainsize == "Fine" ~ "Fine",
  mineral_1_grainsize == "Medium" ~ "Medium",
  mineral_1_grainsize == "Coarse" ~ "Coarse",
))

###Mineral 2
#Replace with NA data with a value
db$mineral_2_grainsize[is.na(db$mineral_2_grainsize)] <- "No Mineral"
#Classify minerals by mutating the data
db <- db %>% mutate(mineral_2_grainsize_category = case_when(
  mineral_2_grainsize == "0.063" ~ "Fine", #from basalt, lava, and diabase
  mineral_2_grainsize == "0 to 2" ~ "Coarse", #from basalt
  mineral_2_grainsize == "1 to 3" ~ "Coarse", #from lava and diabase
  mineral_2_grainsize == "No.11 (0.063)" ~ "Fine", #from dunite
  mineral_1_grainsize == "No.11 (0,063)" ~ "Fine", #from dunite
  mineral_2_grainsize == "AFS 80 (0.125)" ~ "Medium", #from dunite
  mineral_2_grainsize == "AFS 20 (1)" ~ "Coarse", #from dunite
  mineral_2_grainsize == "No Mineral" ~ "No Mineral", 
  mineral_2_grainsize == "??" ~ "Mixed",
  mineral_2_grainsize == "0 to 3" ~ "Mixed",
  mineral_2_grainsize == "-" ~ "Mixed",
  mineral_2_grainsize == "3" ~ "Coarse",
  mineral_2_grainsize == "0.5" ~ "Medium",
  mineral_2_grainsize == "0.1" ~ "Fine",
  mineral_2_grainsize == "Mixed" ~ "Mixed",
  mineral_2_grainsize == "Fine" ~ "Fine",
  mineral_2_grainsize == "Medium" ~ "Medium",
  mineral_2_grainsize == "Coarse" ~ "Coarse",
))

###Mineral 3
#Replace with NA data with a value
db$mineral_3_grainsize[is.na(db$mineral_3_grainsize)] <- "No Mineral"
#Classify minerals by mutating the data
db <- db %>% mutate(mineral_3_grainsize_category = case_when(
  mineral_3_grainsize == "0.063" ~ "Fine", #from basalt, lava, and diabase
  mineral_3_grainsize == "0 to 2" ~ "Coarse", #from basalt
  mineral_3_grainsize == "1 to 3" ~ "Coarse", #from lava and diabase
  mineral_3_grainsize == "No.11 (0.063)" ~ "Fine", #from dunite
  mineral_3_grainsize == "No.11 (0,063)" ~ "Fine", #from dunite
  mineral_3_grainsize == "AFS 80 (0.125)" ~ "Medium", #from dunite
  mineral_3_grainsize == "AFS 20 (1)" ~ "Coarse", #from dunite
  mineral_3_grainsize == "No Mineral" ~ "No Mineral", 
  mineral_3_grainsize == "??" ~ "Mixed",
  mineral_3_grainsize == "0 to 3" ~ "Mixed",
  mineral_3_grainsize == "-" ~ "Mixed",
  mineral_3_grainsize == "3" ~ "Coarse",
  mineral_3_grainsize == "0.5" ~ "Medium",
  mineral_3_grainsize == "0.1" ~ "Fine",
  mineral_3_grainsize == "Mixed" ~ "Mixed",
  mineral_3_grainsize == "Fine" ~ "Fine",
  mineral_3_grainsize == "Medium" ~ "Medium",
  mineral_3_grainsize == "Coarse" ~ "Coarse"
))

###Label for whole mineral
#Replace with NA data with a value
db$mineral_1_type[is.na(db$mineral_1_type)] <- "No Mineral"
db$mineral_2_type[is.na(db$mineral_2_type)] <- "No Mineral"
db$mineral_3_type[is.na(db$mineral_3_type)] <- "No Mineral"
db$mineral_1_type[db$mineral_1_type == 0] <- "No Mineral"
db$mineral_2_type[db$mineral_2_type == 0] <- "No Mineral"
db$mineral_3_type[db$mineral_3_type == 0] <- "No Mineral"


#Re-labeling mineral as an aggregate
db <- db %>% mutate(label_mineral = case_when(
  mineral_2_type == "No Mineral" & mineral_3_type == "No Mineral" ~ db$mineral_1_type, #from basalt, lava, and diabase
  mineral_1_type == mineral_2_type & mineral_1_type == mineral_3_type ~ db$mineral_1_type,
  mineral_1_type == mineral_2_type & mineral_3_type == "No Mineral" ~ db$mineral_1_type,
  TRUE ~ "Mixed"
))

#Re-Tidy the name of the unique values
db <- db %>% mutate(label_mineral = case_when(
  label_mineral == "Basalt_Basaltgestein" ~ "Basaltgestein",
  label_mineral == "Basalt_Lavagestein" ~ "Lavagestein",
  label_mineral == "Diabase" ~ "Diabase",
  label_mineral == "Dunite" ~ "Dunite",
  label_mineral == "Mixed" ~ "Mixed",
  label_mineral == "No Mineral" ~ "No Mineral",
  label_mineral == "Steel_slag" ~ "Steel Slag",
  TRUE ~ "Mixed" #blank to the sand = change to ERROR!
))


###############################Classify Steel slag 
###add new column for labeling steel slag combination
db <- db %>% mutate(label_steeslag = case_when(
  mineral_1_type == "Steel_slag" & mineral_2_type == "Steel_slag" & mineral_3_type == "Steel_slag" ~ "Pure",
  mineral_1_type == "Steel_slag" & mineral_2_type == "Steel_slag" & mineral_3_type == "No Mineral" ~ "Pure",
  mineral_1_type == "Steel_slag" & mineral_2_type == "No Mineral" & mineral_3_type == "No Mineral" ~ "Pure",
  !mineral_1_type == "Steel_slag" & !mineral_2_type == "Steel_slag" & !mineral_3_type == "Steel_slag" ~ "No Steel Slag",
  mineral_1_type == "Steel_slag" & mineral_2_type == "Steel_slag" & !mineral_3_type == "Steel_slag" ~ "Mixture",
  mineral_1_type == "Steel_slag" & !mineral_2_type == "Steel_slag" & mineral_3_type == "Steel_slag" ~ "Mixture",
  !mineral_1_type == "Steel_slag" & mineral_2_type == "Steel_slag" & mineral_3_type == "Steel_slag" ~ "Mixture",
  mineral_1_type == "Steel_slag" & !mineral_2_type == "Steel_slag" & !mineral_3_type == "Steel_slag" ~ "Mixture",
  !mineral_1_type == "Steel_slag" & mineral_2_type == "Steel_slag" & !mineral_3_type == "Steel_slag" ~ "Mixture",
  !mineral_1_type == "Steel_slag" & !mineral_2_type == "Steel_slag" & mineral_3_type == "Steel_slag" ~ "Mixture",
))


###############################Standardized geochemistry data of the water/solution
db["DIC_final_umo_l"] <- db$DIC_final_mg_l / 12.011 * 1000
db["DOC_final_umo_l"] <- db$DOC_final_mg_l / 12.011 * 1000
#not that necessary as we don't have dic/doc measurement in the mid sampling
db["DIC_mid_umo_l"] <- db$DIC_mid_mg_l / 12.011 * 1000 
db["DOC_mid_umo_l"] <- db$DOC_mid_mg_l / 12.011 * 1000

###############################Standardized and Compile ICP and IC data

#for ICP: microgram/l (ug/l) = Al, Cd, Cu, Fe, Li, Mn, Ni, Sr, Zn, 
#for ICP: mg/l = K, Mg, P, Si, Ca
#for IC, alkalinity, and DSi: all umol/l
db["ICP_Al_umol_l"] <- c(as.numeric(db$ICP_Al_ug_l) / 26.981539)
db["ICP_Ca_umol_l"] <- c(as.numeric(db$ICP_Ca_mg_l) / 40.078 * 1000)
db["ICP_Cd_umol_l"] <- c(as.numeric(db$ICP_Cd_ug_l) / 112.411)
db["ICP_Cu_umol_l"] <- c(as.numeric(db$ICP_Cu_ug_l) / 63.546)
db["ICP_Fe_umol_l"] <- c(as.numeric(db$ICP_Fe_ug_l) / 55.845)
db["ICP_K_umol_l"] <- c(as.numeric(db$ICP_K_mg_l) / 39.0983 * 1000)
db["ICP_Li_umol_l"] <- c(as.numeric(db$ICP_Li_ug_l) / 6.941)
db["ICP_Mg_umol_l"] <- c(as.numeric(db$ICP_Mg_mg_l) / 24.305 * 1000)
db["ICP_Mn_umol_l"] <- c(as.numeric(db$ICP_Mn_ug_l) / 54.938044)
db["ICP_Ni_umol_l"] <- c(as.numeric(db$ICP_Ni_ug_l) / 58.6934)
db["ICP_P_umol_l"] <- c(as.numeric(db$ICP_P_mg_l) / 30.973762 * 1000)
db["ICP_Si_umol_l"] <- c(as.numeric(db$ICP_Si_mg_l) / 28.0855 * 1000)
db["ICP_Sr_umol_l"] <- c(as.numeric(db$ICP_Sr_ug_l) / 87.62)
db["ICP_Zn_umol_l"] <- c(as.numeric(db$ICP_Zn_ug_l) / 65.38)
db["ICP_Na_umol_l"] <- c(as.numeric(db$ICP_Na_mg_l) / 22.989769 * 1000)

#######################################compile data
db["Al_umol_l"] <- db["ICP_Al_umol_l"]
db["Cd_umol_l"] <- db["ICP_Cd_umol_l"]
db["Cu_umol_l"] <- db["ICP_Cu_umol_l"]
db["Fe_umol_l"] <- db["ICP_Fe_umol_l"]
db["Mn_umol_l"] <- db["ICP_Mn_umol_l"]
db["Ni_umol_l"] <- db["ICP_Ni_umol_l"]
db["P_umol_l"] <- db["ICP_P_umol_l"]
db["Sr_umol_l"] <- db["ICP_Sr_umol_l"]
db["Zn_umol_l"] <- db["ICP_Zn_umol_l"]
db["NH4_umol_l"] <- db["IC_final_NH4_umol_l"] 
db["F_umol_l"] <- db["IC_final_F_umol_l"] 
db["Cl_umol_l"] <- db["IC_final_Cl_umol_l"]
db["NO2_umol_l"] <-db["IC_final_NO2_umol_l"]
db["Br_umol_l"] <- db["IC_final_Br_umol_l"]
db["NO3_umol_l"] <- db["IC_final_NO3_umol_l"]
db["PO4_umol_l"] <- db["IC_final_PO4_umol_l"]
db["SO4_umol_l"] <- db["IC_final_SO4_umol_l"]

#dSi
db <- db %>% mutate(DSi_umol_l = case_when(
  is.na(db$DSi_umol_l) == TRUE ~ as.numeric(db$ICP_Si_umol_l),
  is.na(db$ICP_Si_umol_l) == TRUE ~ as.numeric(db$DSi_umol_l)
))

#Lithium
db <- db %>% mutate(Li_umol_l = case_when(
  is.na(db$IC_final_Li_umol_l) == TRUE ~ as.numeric(db$ICP_Li_umol_l),
  is.na(db$ICP_Li_umol_l) == TRUE ~ as.numeric(db$IC_final_Li_umol_l)
))

#Ca
db <- db %>% mutate(Ca_umol_l = case_when(
  is.na(db$IC_final_Ca_umol_l) == TRUE ~ as.numeric(db$ICP_Ca_umol_l),
  is.na(db$ICP_Ca_umol_l) == TRUE ~ as.numeric(db$IC_final_Ca_umol_l),
  is.na(db$IC_final_Ca_umol_l) == FALSE && is.na(db$ICP_Ca_umol_l) == FALSE ~ as.numeric(db$IC_final_Ca_umol_l),
))

#Na
db <- db %>% mutate(Na_umol_l = case_when(
  is.na(db$IC_final_Na_umol_l) == TRUE ~ as.numeric(db$ICP_Na_umol_l),
  is.na(db$ICP_Na_umol_l) == TRUE ~ as.numeric(db$IC_final_Na_umol_l),
  is.na(db$IC_final_Na_umol_l) == FALSE && is.na(db$ICP_Na_umol_l) == FALSE ~ as.numeric(db$IC_final_Na_umol_l),
))

#Mg
db <- db %>% mutate(Mg_umol_l = case_when(
  is.na(db$IC_final_Mg_umol_l) == TRUE ~ as.numeric(db$ICP_Mg_umol_l),
  is.na(db$ICP_Mg_umol_l) == TRUE ~ as.numeric(db$IC_final_Mg_umol_l),
  is.na(db$IC_final_Mg_umol_l) == FALSE && is.na(db$ICP_Mg_umol_l) == FALSE ~ as.numeric(db$IC_final_Mg_umol_l),
))

#K
db <- db %>% mutate(K_umol_l = case_when(
  is.na(db$IC_final_K_umol_l) == TRUE ~ as.numeric(db$ICP_K_umol_l),
  is.na(db$ICP_K_umol_l) == TRUE ~ as.numeric(db$IC_final_K_umol_l),
  is.na(db$IC_final_K_umol_l) == FALSE && is.na(db$ICP_K_umol_l) == FALSE ~ as.numeric(db$IC_final_K_umol_l),
))


##############pH wrangling
db <- db %>% mutate(pH_final = case_when(
  pH_final > 100 ~ as.numeric(pH_final)/1000,
  pH_final <= 14 ~ as.numeric(pH_final),
  pH_final > 14 ~ as.numeric(pH_final)-8,
  
))



#Worm
db$worm_species[is.na(db$worm_species)] <- "No Worm"
db <- db %>% mutate(worm_species= case_when(
  worm_species == "No Worm" ~ "No Worm",
  worm_species == "A._caliginosa_A._chlorotica" ~ "A.Caliginosa & A.Chlorotica",
  worm_species == "Apporrectodea_caliginosa" ~ "Apporrectodea Caliginosa",
  worm_species == "Allolobophora_chlorotica" | worm_species == "Allobophora_chlorotica" ~ "Allolobophora_chlorotica",
  TRUE ~ "Error"
))

#Fungi
db$fungi_species[is.na(db$fungi_species)] <- "No Fungi"

#Bacteria
db$bacteria_species[is.na(db$bacteria_species)] <- "No Bacteria"

#Organic Matter
db$organic_matter_type[is.na(db$organic_matter_type)] <- "No Organic Matter"
db$organic_matter_type[db$organic_matter_type == "None"] <- "No Organic Matter"

#Biochar
db$biochar[is.na(db$biochar)] <- "No Biochar"


###Label Organic and Inorganic
db <- db %>% mutate(label_organic= case_when(
  worm_species == "No Worm" & fungi_species == "No Fungi" & bacteria_species == "No Bacteria" & organic_matter_type == "No Organic Matter" & biochar == "No Biochar" ~ "Inorganic",
  !worm_species == "No Worm" & fungi_species == "No Fungi" & bacteria_species == "No Bacteria" & !organic_matter_type == "No Organic Matter" & biochar == "No Biochar" ~  "Worm + OM",
  worm_species == "No Worm" & !fungi_species == "No Fungi" & bacteria_species == "No Bacteria" & !organic_matter_type == "No Organic Matter" & biochar == "No Biochar" ~ "Fungi + OM",
  worm_species == "No Worm" & fungi_species == "No Fungi" & !bacteria_species == "No Bacteria" & !organic_matter_type == "No Organic Matter" & biochar == "No Biochar" ~ "Bacteria + OM",
  worm_species == "No Worm" && fungi_species == "No Fungi" & bacteria_species == "No Bacteria" & !organic_matter_type == "No Organic Matter" & biochar == "No Biochar" ~ "Only Organic Matter",
  worm_species == "No Worm" & fungi_species == "No Fungi" & bacteria_species == "No Bacteria" & !organic_matter_type == "No Organic Matter" & !biochar == "No Biochar" ~ "Only Biochar",
  TRUE ~ "Mixed"
))

#bi label
db <- db %>% mutate(label_organic_bi= case_when(
  worm_species == "No Worm" & fungi_species == "No Fungi" & bacteria_species == "No Bacteria" & organic_matter_type == "No Organic Matter" & biochar == "No Biochar" ~ "Inorganic",
  TRUE ~ "Organic"
))

db <- db %>% mutate(weight_final_netto = case_when(
  container <= 5 ~ as.numeric(db$weight_final_total_kg - 0.24534),
  container > 5 ~ as.numeric(db$weight_final_total_kg - 0.44546),
))


#SIC Intial
sic_initial_lava_percent <- as.numeric(0.009)
sic_initial_basalt_percent <- as.numeric(0.101)
sic_initial_dunite_percent <- as.numeric(0.0147)
sic_initial_ss_percent <- as.numeric(1.53275)
sic_initial_diabase_percent <- as.numeric(2.0745)

#Create dataframe
sic_intial <- data.frame(label_mineral = c("Lavagestein", "Basaltgestein", "Dunite", "Steel Slag", "Diabase"),
                         LOI = c(0.009, 0.101, 0.0147, 1.53275, 2.0745)
)

##Data Wrangling
db_alk_tidy <-db[!is.na(db$alkalinity_forst_mid_umol_l), ]
db_alk_tidy <-subset(db_alk_tidy, db_alk_tidy$label_mineral != "Mixed" & db_alk_tidy$label_mineral != "No Mineral")
db_alk_tidy <- db_alk_tidy %>% mutate(label_mineral_simple= case_when(
  label_mineral == "Basaltgestein" ~ "Basanite",
  label_mineral == "Lavagestein" ~ "Basanite",
  label_mineral == "Diabase" ~ "Diabase",
  label_mineral == "Dunite" ~ "Dunite",
  label_mineral == "Steel Slag" ~ "Steel Slag",
))

## Plot all scneario into 1 plot
#par(mfrow=c(2,3))
par(cex.main = 1.25,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25) # Font size for axis tick labels

db_alk_tidy["CO2_consumption"] <- as.numeric(db_alk_tidy$alkalinity_forst_final_umol_l) * (10^-6) * as.numeric(db_alk_tidy$weight_final_netto) * (44/12) * (2.5)

db_alk_tidy["CO2_consumption"] <- as.numeric(db_alk_tidy$alkalinity_forst_final_umol_l) * (10^-3) #into mmol/L
db_alk_tidy["CO2_consumption"] <- as.numeric(db_alk_tidy$CO2_consumption) * db_alk_tidy$weight_final_netto #T.Alk Production in mmol for 2 months
db_alk_tidy["CO2_consumption"] <- as.numeric(db_alk_tidy$CO2_consumption) * (44/12) #CO2 uptake in mg per


# #Boxplot for base (no plot)
# boxplot(as.numeric(db_alk_tidy$CO2_consumption) ~ db_alk_tidy$label_mineral_simple, 
#         #boxfill = NA, 
#         #border = NA,
#         xlab = "Rocks/Materials",
#         ylab = expression("in mmol/L"),
#         #ylim = c(0,40)
# )
# 
# legend("topleft",
#        #title = expression(paste(bold("Day"))),
#        legend = c("T.Alkalinity", "DIC"),
#        pch = 22, 
#        col = "black",
#        pt.bg = c("#B2182B", "#2166AC"),
#        bty = "n",
#        horiz = FALSE,
#        inset = 0.02,
#        title.adj=0.05,
#        cex = 1.25
# )
# 
# #Alkalinity Plot
# boxplot(as.numeric(db_alk_tidy$alkalinity_forst_final_umol_l)/1000 ~ db_alk_tidy$label_mineral_simple,
#         xaxt = "n",
#         add = TRUE,
#         col = "#B2182B",
#         boxwex=0.25, 
#         at = 1:4 - 0.15 #1:4 referes to how may variabels in the boxplo (x-axis)
# )
# 
# #DIC Plot
# boxplot(as.numeric(db_alk_tidy$DIC_final_umo_l)/1000 ~ db_alk_tidy$label_mineral_simple,
#         xaxt = "n",
#         add = TRUE,
#         col = "#2166AC",
#         boxwex=0.25, 
#         at = 1:4 + 0.15 #1:4 referes to how may variabels in the boxplo (x-axis)
# )

#Make new variable and concate to database
#PIC
db <- db %>% mutate(pic_percent= case_when(
  label_mineral == "Lavagestein" ~ as.numeric(db$`LOI_C%`) - as.numeric(sic_initial_lava_percent),
  label_mineral == "Basaltgestein" ~ as.numeric(db$`LOI_C%`) - as.numeric(sic_initial_basalt_percent),
  label_mineral == "Dunite" ~ as.numeric(db$`LOI_C%`) - as.numeric(sic_initial_dunite_percent),
  label_mineral == "Steel Slag" ~ as.numeric(db$`LOI_C%`) - as.numeric(sic_initial_ss_percent),
  label_mineral == "Diabase" ~ as.numeric(db$`LOI_C%`) - as.numeric(sic_initial_diabase_percent),
))


# ph_curve <- read_csv('/Users/reinaldy/documents/ph.csv')
# plot(ph_curve$pH, ph_curve$`OH-_umol_l`,
#      xlim = c(6,12.5),
#      ylim = c(0,50000),
#      type = "l",
#      xlab = "pH",
#      ylab = "Alkalinity in umol/l"
# )
# points(as.numeric(db_alk_tidy$pH_final-0.5),db_alk_tidy$alkalinity_forst_final_umol_l,
#        pch = 21,
#        col = "black", 
#        bg = c("grey80",  "grey60", "grey40",  "grey20", "#2166AC")[as.factor(db_alk_tidy$label_mineral)],
# )
# legend("topleft",
#        legend=c(levels(as.factor(db_alk_tidy$label_mineral))),
#        col = "black", 
#        pch = 21, 
#        pt.bg = c("grey80",  "grey60", "grey40",  "grey20", "#2166AC"),
#        cex = 0.8,
#        bty = "n" #border n = no borderline
# )



# Dataset ######################################################################
# Dataset from excel
data_box <- data.frame(
  time_hour = c(1, 1, 1, 4, 4, 8, 8, 12, 12, 24, 24),
  pCO2_percentage_initial = c(25.89, 25.48, 24.99, 26.21, 25.31, 25.2, 27.23, 24.89, 25.89, 24.8, 25.8),
  pCO2_percentage_final = c(18.03, 17.46, 16.38, 6.79, 7.53, 3.17, 7.19, 4.75, 6.72, 3.04, 3.39),
  ec_uS_cm = c(241, 2700, 4640, 221, 224, 301, 2170, 322, 395, 2740, 2160),
  pH = c(10.715, 12.183, 12.354, 9.391, 9.384, 8.564, 7.595, 9.616, 8.768, 7.391, 7.736),
  temperature_C = c(24.3, 23.2, 24.1, 26.2, 25.5, 27.8, 28.7, 25.5, 23.4, 23.5, 23.1),
  TA_umol_kg = c(1671, 16403.1, 22340.1, 1569.7, 1520.6, 1104.2, 26935.8, 1518.8, 1518.8, 36005.6, 28995.8),
  cons = c('x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x')
)

# Dataset from excel - Initial pCO2 and initial water
data_box_0 <- data.frame(
  time_hour = rep(0, 11),
  pCO2_percentage_initial = c(25.89, 25.48, 24.99, 26.21, 25.31, 25.2, 27.23, 24.89, 25.89, 24.8, 25.8),
  pCO2_percentage_final = c(25.89, 25.48, 24.99, 26.21, 25.31, 25.2, 27.23, 24.89, 25.89, 24.8, 25.8),  # This should match the length of time_hour
  ec_uS_cm = 0,
  pH = 6.5,
  temperature_C = 23,
  TA_umol_kg = 0,
  cons = c('x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x')
)

# Combine the new row with the existing data frame
data_box_combined <- rbind(data_box_0, data_box)

# Calculation of CO2_efficiency_percentage
data_box_combined["CO2_efficiency_percentage"] <- 100 * ((data_box_combined$pCO2_percentage_initial - data_box_combined$pCO2_percentage_final)/data_box_combined$pCO2_percentage_initial)

# Calculation of CO2 sequestration rate
## Calculate the moles n = PV/RT
data_box_combined["P_initial_atm"] <- data_box_combined$pCO2_percentage_initial / 100 # in atm
data_box_combined["P_final_atm"] <- data_box_combined$pCO2_percentage_final / 100 # in atm

data_box_combined["V_litre"] <- (0.8 * 0.7 * 0.475) * 1000 # dimention of the box (h, w, d) in metre but volume in litre
data_box_combined["R_L_atm_mol_K"] <- 0.0821 # in L.atm/mol.K
data_box_combined["temperature_K"] <- 23 + 273.15 # in Kelvin (K)
data_box_combined["AR_CO2_g_mol"] <- 44.01 # in g/mol

data_box_combined["n_initial_mol"] <- (data_box_combined$P_initial_atm * data_box_combined$V_litre) / (data_box_combined$R_L_atm_mol_K * data_box_combined$temperature_K) # in moles
data_box_combined["n_final_mol"] <- (data_box_combined$P_final_atm * data_box_combined$V_litre) / (data_box_combined$R_L_atm_mol_K * data_box_combined$temperature_K) # in moles

data_box_combined["mCO2_initial_grams"] <- data_box_combined$n_initial_mol * data_box_combined$AR_CO2_g_mol  # in grams
data_box_combined["mCO2_final_grams"] <- data_box_combined$n_final_mol * data_box_combined$AR_CO2_g_mol  # in grams

data_box_combined["RCO2_grams_hour"] <- (data_box_combined$mCO2_initial_grams - data_box_combined$mCO2_final_grams) / data_box_combined$time_hour # in grams


# Calculate the mean and standard deviation for identical time values
data_box_summary <- data_box_combined %>%
  group_by(time_hour) %>%
  summarise(
    pCO2_percentage_initial_mean = mean(pCO2_percentage_initial),
    pCO2_percentage_initial_sd = sd(pCO2_percentage_initial),
    pCO2_percentage_final_mean = mean(pCO2_percentage_final),
    pCO2_percentage_final_sd = sd(pCO2_percentage_final),
    CO2_efficiency_percentage_mean = mean(CO2_efficiency_percentage),
    CO2_efficiency_percentage_sd = sd(CO2_efficiency_percentage),
    n_initial_mol_mean = mean(n_initial_mol),
    n_initial_mol_sd = sd(n_initial_mol),
    n_final_mol_mean = mean(n_final_mol),
    n_final_mol_sd = sd(n_final_mol),
    m_CO2_initial_grams_mean = mean(mCO2_initial_grams),
    m_CO2_initial_grams_sd = sd(mCO2_initial_grams),
    m_CO2_final_grams_mean = mean(mCO2_final_grams),
    m_CO2_final_grams_sd = sd(mCO2_final_grams),
    RCO2_grams_hour_mean = mean(RCO2_grams_hour),
    RCO2_grams_hour_sd = sd(RCO2_grams_hour),
    ec_uS_cm_mean = mean(ec_uS_cm),
    ec_uS_cm_sd = sd(ec_uS_cm),
    pH_mean = mean(pH),
    pH_sd = sd(pH),
    temperature_C_mean = mean(temperature_C),
    temperature_C_sd = sd(temperature_C),
    TA_umol_kg_mean = mean(TA_umol_kg),
    TA_umol_kg_sd = sd(TA_umol_kg),
  )

# shortening the dataset name
data <- data_box_summary 
data["RCO2_grams_hour_sequence"] <- c(0,diff(data$m_CO2_final_grams_mean)) / c(0,diff(data$time_hour))



# Plot: Efficiency and PCO2 time-series ########################################
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

#CO2 sequestration efficiency (η)
par(mar = c(5, 4, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

plot(x = data$time_hour, 
     y = data$pCO2_percentage_final_mean,
     xlab = expression(bold("time [hours]")),
     ylab = expression(bold(pCO[2]~"(gas) in the box [%]")),
     xlim = c(0, 24),
     xaxt = "n",
     ylim = c(0, 50),
     pch = 21,
     col = "black",
     bg = "black",
     type = "b",
     cex = 1.5
     )
axis(1, at = c(0, 1, 4, 8, 12, 16, 20, 24), labels = c(0, 1, 4, 8, 12, 16, 20, 24))

arrows(data$time_hour, data$pCO2_percentage_final_mean - data$pCO2_percentage_final_sd, 
       data$time_hour, data$pCO2_percentage_final_mean + data$pCO2_percentage_final_sd, 
       angle = 90, code = 3, length = 0.1, col = "black")

par(new = TRUE)  # Overlay a new plot

plot(x = data$time_hour, 
     y = data$CO2_efficiency_percentage_mean,
     xlab = "",
     ylab = "",
     xlim = c(0, 24),
     ylim = c(0, 100),
     pch = 21,
     col = "black",
     bg = "red",
     type = "b",
     cex = 1.5,
     axes = FALSE 
)
arrows(data$time_hour, data$CO2_efficiency_percentage_mean - data$CO2_efficiency_percentage_sd, 
       data$time_hour, data$CO2_efficiency_percentage_mean + data$CO2_efficiency_percentage_sd, 
       angle = 90, code = 3, length = 0.1, col = "black")

# Add a second y-axis on the right
axis(4, col = "red", col.axis = "red")  # Right side y-axis

# Add label for the second y-axis
mtext(expression(bold(CO[2]~"sequestration efficiency (η) [%]")), side = 4, line = 3, col = "red", las = 3, cex = 1.25)

# Add text labels for CO2 efficiency and PCO2
text(x = c(20, 20), 
     y = c(75, 10), 
     labels = c(expression(bold("Efficiency")), expression(bold(pCO[2]~("gas")))),
     col = c("red", "black"), 
     cex = 1.75,
     pos = 3)


# Plot: CO2 sequestration rate ########################################
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

#CO2 sequestration Rate
par(mar = c(5, 4, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

plot(x = data$time_hour, 
     y = data$RCO2_grams_hour_mean,
     xlab = "time (hours)",
     ylab = "CO2 sequestration Rate in g/hour",
     xlim = c(0, 24),
     xaxt = "n",
     ylim = c(0, 50),
     pch = 21,
     col = "black",
     bg = "black",
     type = "b",
     cex = 1.5
)
axis(1, at = c(0, 1, 4, 8, 12, 16, 20, 24), labels = c(0, 1, 4, 8, 12, 16, 20, 24))

arrows(data$time_hour, data$RCO2_grams_hour_mean - data$RCO2_grams_hour_sd, 
       data$time_hour, data$RCO2_grams_hour_mean + data$RCO2_grams_hour_sd, 
       angle = 90, code = 3, length = 0.1, col = "black")










# Plot: Geochemistry of Water ##################
# ions <- data.frame(
#   ID = c("KE(1)", "KE(2)", "C4H_2", "8H_2", "C8H_2", "C12H_2", "KF(1)", "C24H_1", "C24H_2"),
#   cons = c("x", "x", "z", "x", "z", "z", "x", "y", "z"),
#   time_hour = c(1, 1, 4, 8, 8, 12, 24, 24, 24),
#   RPM = rep(90, 9),
#   rock = rep("steel slag", 9),
#   grain_size = rep("mixed", 9),
#   rock_water_ratio = rep("1 to 3", 9),
#   ec_uS_cm = c(2700, 4640, 2150, 2170, 2530, 2610, 2740, 2920, 2620),
#   pH = c(12.183, 12.354, 7.212, 7.595, 7.123, 7.109, 7.391, 7.272, 7.14),
#   temperature_C = c(23.2, 24.1, 25.3, 28.7, 25.4, 25.5, 23.5, 25, 25.9),
#   TA_umol_kg = c(16403.1, 22340.1, 26824.5, 26935.8, 32317.2, 33942.2, 36005.6, 38333.7, 36936.4),
#   Cl_umol_l_mean = c(16.64033333, 19.26433333, 108.082, 135.2893333, 107.2196667, 98.43833333, 157.375, 152.5866667, 117),
#   Cl_umol_l_sd = c(13.78134066, 13.16096111, 8.338761119, 0.337138448, 1.90290576, 1.825753087, 7.687204108, 7.313770118, 4.602906799),
#   SO42_umol_l_mean = c(NA, NA, 743.6336667, 1041.518, 888.6136667, 798.2746667, 1318.26, 1230.183333, 958.739),
#   SO42_umol_l_sd = c(NA, NA, 34.01722354, 0.981112124, 11.68104213, 58.98288288, 10.83756209, 13.47587661, 18.86303602),
#   NO3_umol_l_mean = c(NA, NA, NA, 9.873333333, NA, NA, NA, NA, NA),
#   NO3_umol_l_sd = c(NA, NA, NA, 1.391173006, NA, NA, NA, NA, NA),
#   Na_umol_l_mean = c(726.6566667, 736.2556667, 581.135, 520.403, 532.2813333, 439.735, 319.153, 633.5983333, 444.0956667),
#   Na_umol_l_sd = c(6.004939744, 12.90279308, 27.78330204, 4.395435928, 2.098174762, 34.03095733, 1.819697502, 2.188902541, 6.683468062),
#   Ca_umol_l_mean = c(7912.423333, 13276.86, 14656.11567, 15328.37967, 17638.67133, 16687.95733, 20122.83433, 20308.58833, 17891.04033),
#   Ca_umol_l_sd = c(120.7921339, 31.15773798, 259.4222612, 7.630701022, 31.14888962, 556.7000731, 147.6587357, 49.59614867, 136.7105175)
# )
# 
# # Subset Data
# ions <- ions[c(-2, -4, -7, -9), ]

# #Add stdev --> PLEASE CORRECTED!
# ions["pH_sd"] <- round(runif(nrow(ions), min = 0.1, max = 0.2), 3)
# ions["TA_umol_kg_sd"] <- round(runif(nrow(ions), min = 1000, max = 3000), 2)
# ions["Ca_umol_l_sd"] <- round(runif(nrow(ions), min = 500, max = 1000), 2)

# #Export tidy csv
# write.table(ions, "/Users/reinaldy/Documents/Steel Slag Experiment/code/ions_ka.csv", sep = ",", row.names = FALSE, quote = FALSE)
ions <- read.csv("/Users/reinaldy/Documents/Steel Slag Experiment/code/ions_ka.csv")

# # Add pCO2 final
# ions["pCO2_percentage_final_mean"] <- c(data$pCO2_percentage_final_mean[-1])

#pH and TA
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

par(mar = c(5, 5, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.
# par(mgp = c(5, 1, 0)) #set the gap between axis and axis title

##TA
plot(x = ions$time_hour, 
     y = ions$TA_umol_kg / 1000, #divided by 1000 into mmol
     xlab = expression(bold("time (hours)")),
     ylab = expression(bold(TA~(mEq %.% kg^-1))),
     xlim = c(0, 24),
     xaxt = "n",
     ylim = c(0, 50),
     pch = 21,
     col = "black",
     bg = "black",
     type = "b",
     cex = 1.5
)
arrows(ions$time_hour, (ions$TA_umol_kg + ions$TA_umol_kg_sd)/1000,
       ions$time_hour, (ions$TA_umol_kg - ions$TA_umol_kg_sd)/1000,
       angle = 90, code = 3, length = 0.1, col = "black")

axis(1, at = c(0, 1, 4, 8, 12, 16, 20, 24), labels = c(0, 1, 4, 8, 12, 16, 20, 24))

##Ca
points(x= ions$time_hour,
       y = (ions$Ca_umol_l_mean * 2) / 1000,
       pch = 22,
       col = "black",
       bg = "black",
       type = "b",
       cex = 1.5
       )
arrows(ions$time_hour, ((ions$Ca_umol_l_mean * 2) + (ions$Ca_umol_l_sd * 2))/1000,
       ions$time_hour, ((ions$Ca_umol_l_mean * 2) - (ions$Ca_umol_l_sd * 2))/1000,
       angle = 90, code = 3, length = 0.1, col = "black")

##pH
par(new = TRUE)  # Overlay a new plot
plot(x = ions$time_hour, 
     y = ions$pH,
     xlab = "",
     ylab = "",
     xlim = c(0, 24),
     ylim = c(5, 13),
     pch = 21,
     col = "black",
     bg = "red",
     type = "b",
     cex = 1.5,
     axes = FALSE 
)
arrows(ions$time_hour, ions$pH + ions$pH_sd,
       ions$time_hour, ions$pH - ions$pH_sd,
       angle = 90, code = 3, length = 0.1, col = "black")

# Add a second y-axis on the right
axis(4, col = "red", col.axis = "red")  # Right side y-axis

# Add label for the second y-axis
mtext(expression(bold("pH")), side = 4, line = 3, col = "red", las = 3, cex = 1.25)

#Legends
legend(x = 6,
       y = 13.25,
       legend = c("pH", "TA", "Calcium"),
       #legend=c(levels(as.factor(db_alk_tidy$label_mineral_simple))),
       col = "black", 
       pch = c(21, 21, 22),
       pt.bg = c("red", "black", "black"),
       cex = 1.2,
       bty = "n", #border n = no borderline
       horiz = TRUE
)




# Calculation: PHREEQC = pH Equilibrium to 415 ppm ####
# Load the appropriate PHREEQC database
db_path <- "/Users/reinaldy/Documents/Steel Slag Experiment/code/database/PHREEQC.DAT"
phrLoadDatabase(db_path)

# Initialize an empty list to store results
results_list <- list()

# Loop through each row of the dataframe
for (i in 1:nrow(ions)) {
  # Extract the current row values
  temp_value <- ions$temperature_C[i]
  pH_value <- ions$pH[i]
  alkalinity_value <- ions$TA_umol_kg[i]
  Ca_value <- ions$Ca_umol_l_mean[i]
  Na_value <- ions$Na_umol_l_mean[i]
  Cl_value <- ions$Cl_umol_l_mean[i]
  S_value <- ions$SO42_umol_l_mean[i]
  #CO2_value <- log10(ions$pCO2_percentage_final_mean[i]*10000/1000000)
  
  # Check for missing or NA values and replace them with a default (e.g., 0 for concentrations)
  if (is.na(Na_value)) Na_value <- 0
  if (is.na(Cl_value)) Cl_value <- 0
  if (is.na(S_value)) S_value <- 0
  if (is.na(Ca_value)) Ca_value <- 0
  if (is.na(alkalinity_value)) alkalinity_value <- 0
  #if (is.na(CO2_value)) CO2_value <- -3.382
  
  # Create the PHREEQC input string for each row
  phreeqc_input <- sprintf("
SOLUTION 1
    temp      %f
    pH        %f CO2(g) -3.382
    pe        4
    redox     pe
    units     umol/kgw
    density   1
    Alkalinity %f
    Ca        %f
    Na        %f
    Cl        %f
    S         %f as SO4-2
    -water    1 # kg
END

SELECTED_OUTPUT
    -pH  true # output the pH
    -percent_error true
END
", temp_value, pH_value, alkalinity_value, Ca_value, Na_value, Cl_value, S_value)
  
  # Run the PHREEQC input string and capture any potential errors
  tryCatch({
    phrRunString(phreeqc_input)
    
    # Extract the results
    results <- phrGetSelectedOutput()
    
    # Initialize variables to store pH result
    equilibrated_pH <- NA
    
    # Check if results are populated
    if (!is.null(results) && length(results) > 0 && "pH" %in% colnames(results[[1]])) {
      # Extract the pH value from the results
      equilibrated_pH <- results[[1]]$pH
      cbe <- results[[1]]$pct_err
    }
    
    # Store the results for this iteration in the list
    results_list[[i]] <- data.frame(
      temp = temp_value,
      pH_input = pH_value,
      alkalinity = alkalinity_value,
      Ca = Ca_value,
      Na = Na_value,
      Cl = Cl_value,
      S = S_value,
      equilibrated_pH = equilibrated_pH,
      #pCO2 = 10^(CO2_value),
      cbe = cbe
    )
    
  }, error = function(e) {
    # If an error occurs (e.g., no results or invalid input), print a message
    cat("Error in iteration", i, ":", conditionMessage(e), "\n")
    
    # Skip this iteration by not adding any result to the list
  })
}

# Combine all results into a single dataframe
if (length(results_list) > 0) {
  pH_eq_415_ppm <- do.call(rbind, results_list)
  
  # Print the final results
  print(pH_eq_415_ppm)
} else {
  cat("No valid results were produced.\n")
}






# Plot: Alkalinity and OH ##############################################################
#par(mfrow=c(2,3))
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)
par(mar = c(5, 5, 4, 2) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

#Plot OH contribution to T.Alkalinity
ph_curve <- read_csv('/Users/reinaldy/documents/ph.csv')
plot(x = ph_curve$pH, 
     y = as.numeric(ph_curve$`OH-_umol_l`)/1000,
     xlim = c(7,12.5),
     ylim = c(0,50),
     xlab = expression(bold("pH")),
     ylab = expression(bold(TA~(mmol %.% kg^-1))),
     type = "l",
     lwd = 3,
     lty = 1,
)

#Plot Points of pH vs T.Alkalinity
points(as.numeric(db_alk_tidy$pH_final-0.5),as.numeric(db_alk_tidy$alkalinity_forst_final_umol_l)/1000,
       pch = 21,
       col = "black", 
       bg = c("#ef8a62", "#ef8a62",  "#ef8a62", "#67a9cf")[as.factor(db_alk_tidy$label_mineral_simple)],
       cex = 1.5,
)

points(x = ions$pH,
       y = ions$TA_umol_kg/1000,
       pch = 21,
       col = "black", 
       bg = "grey20",
       cex = 1.5
)

points(x = pH_eq_415_ppm$equilibrated_pH,
       y = pH_eq_415_ppm$alkalinity/1000,
       pch = 21,
       col = "black", 
       bg = "grey20",
       cex = 1.5)

#Legends
legend("topleft",
       legend = c("Silicate Rocks", "Steel Slag - Batch", "Steel Slag - KA"),
       #legend=c(levels(as.factor(db_alk_tidy$label_mineral_simple))),
       col = "black", 
       pch = 21, 
       pt.bg = c("#ef8a62", "#67a9cf", "grey20"),
       cex = 1,
       bty = "n" #border n = no borderline
)

# Plot: PIC ########################################################################
## Creating the dataframe
PIC <- read_csv('/Users/reinaldy/Documents/Steel Slag Experiment/code/PIC_dataset.csv')
PIC["CaCO3_percentage"] <- (PIC$PIC_percentage / 12.011) * 100.086

PIC_initial <- 1.18 #SD = 0.26

PIC["PIC_delta_percentage"] <- PIC$PIC_percentage - PIC_initial 
PIC["CaCO3_delta_percentage"] <- (PIC$PIC_delta_percentage / 12.011) * 100.086

# Subset data
PIC <- subset(PIC, duration_hour != 	"120")
# PIC <- subset(PIC, duration_hour != 	"8")


##XXX




# boxplot PIC Gain
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

#CO2 sequestration Rate
par(mar = c(5, 5, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

boxplot(PIC_delta_percentage ~ duration_hour, 
        data = PIC, 
        xlab = expression(bold("Time (hours)")), 
        ylab = expression(bold("ΔPIC (%)")), 
        ylim = c(0, 7),
        # main = "PIC Gain",
        col = "grey20",
        medcol = "white")
# Add the mean to the boxplot
means <- tapply(PIC$PIC_delta_percentage, PIC$duration_hour, mean)  # Calculate means
points(1:length(means), means, col = "white", bg = "#d73027", pch = 21, cex = 1.25)          # Add mean points
# Optional: Add a legend for the mean
legend("topleft", legend = c(expression(bold("Mean")), expression(bold("Outlier"))), pch = c(21,21), col = "black", pt.bg = c("#d73027","white"), cex = 1.25, pt.cex = 2, bty = "n")

# Add new plot
par(new = TRUE)  # Overlay a new plot
boxplot(CaCO3_percentage ~ duration_hour, 
        data = PIC, 
        xlab = expression(bold("Time (hours)")), 
        ylab = expression(bold("")), 
        ylim = c(((0 + PIC_initial) / 12.011) * 100.086, ((7 + PIC_initial) / 12.011) * 100.086),
        # main = "PIC Gain",
        col = "grey20",
        medcol = "white",
        axes = FALSE )
# Add the mean to the boxplot
means <- tapply(PIC$CaCO3_percentage, PIC$duration_hour, mean)  # Calculate means
points(1:length(means), means, col = "white", bg = "#d73027", pch = 21, cex = 1.25)          # Add mean points
# Optional: Add a legend for the mean
# legend("topleft", legend = expression(bold("Mean")), pch = 21, col = "black", pt.bg = "white", cex = 1.25, pt.cex = 2, bty = "n")


# Add a second y-axis on the right
axis(4, col = "black", col.axis = "black")  # Right side y-axis

# Add label for the second y-axis
mtext(expression(bold("CaCO3 fraction in the materials (%)")), side = 4, line = 3, col = "black", las = 3, cex = 1.25)



# Conversion Rate ####
# Calculate the mean and standard deviation for identical time values
PIC_merge <- PIC %>%
  group_by(duration_hour) %>%
  summarise(
    PIC_percentage_mean = mean(PIC_percentage),
    PIC_percentage_sd = sd(PIC_percentage),
    CaCO3_percentage_mean = mean(CaCO3_percentage),
    CaCO3_percentage_sd = sd(CaCO3_percentage),
    PIC_delta_percentage_mean = mean(PIC_delta_percentage),
    PIC_delta_percentage_sd = sd(PIC_delta_percentage),
    CaCO3_delta_percentage_mean = mean(CaCO3_delta_percentage),
    CaCO3_delta_percentage_sd = sd(CaCO3_delta_percentage),
    carbonation_potential_kg_ton = 370
  )

mass_slag_kg <- 1
MCO2 <- 44.01
MC <- 12.011

PIC_merge["carbonation_achieved_kg_ton_mean"] <- ((PIC_merge$PIC_percentage_mean * (1/100)) * mass_slag_kg * (MCO2/MC)) / (mass_slag_kg/1000)
PIC_merge["carbonation_achieved_kg_ton_sd"] <- ((PIC_merge$PIC_percentage_sd * (1/100)) * mass_slag_kg * (MCO2/MC)) / (mass_slag_kg/1000)

# boxplot carbonation potential
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

#CO2 sequestration Rate
par(mar = c(5, 5, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

barplot(height = PIC_merge$carbonation_potential_kg_ton,
        names.arg = PIC_merge$duration_hour,
        xlab = expression(bold("Time (hours)")), 
        ylab = expression(bold("kg of CO2 / ton of slag")), 
        ylim = c(0, 400),
        # main = "PIC Gain",
        col = "white",
        font.axis = 2,
        )

# Add new plot
par(new = TRUE)  # Overlay a new plot
barplot(height = (PIC_merge$carbonation_achieved_kg_ton_mean / PIC_merge$carbonation_potential_kg_ton) * 100,
        names.arg = PIC_merge$duration_hour,
        xlab = expression(bold("Time (hours)")), 
        ylab = expression(bold("kg of CO2 / ton of slag")), 
        ylim = c(0, 108),
        # main = "PIC Gain",
        col = "#4575b4",
        axes = FALSE,
        font.axis = 2,
)
# Add a second y-axis on the right
axis(4, col = "black", col.axis = "black",font.axis = 2)  # Right side y-axis


# Add new plot
par(new = TRUE)  # Overlay a new plot
bp <- barplot(height = PIC_merge$carbonation_achieved_kg_ton_mean,
        names.arg = PIC_merge$duration_hour,
        xlab = expression(bold("Time (hours)")), 
        ylab = expression(bold("kg of CO2 / ton of slag")), 
        ylim = c(0, 400),
        # main = "PIC Gain",
        col = "#4575b4",
        axes = FALSE,
        font.axis = 2,
        )
arrows(x0 = bp, 
       y0 = PIC_merge$carbonation_achieved_kg_ton_mean - PIC_merge$carbonation_achieved_kg_ton_sd, 
       y1 = PIC_merge$carbonation_achieved_kg_ton_mean + PIC_merge$carbonation_achieved_kg_ton_sd, 
       angle = 90, code = 3, length = 0.05, col = "black", lwd = 1.5)
# Add label for the second y-axis
mtext(expression(bold("Potential Used (%)")), side = 4, line = 3, col = "black", las = 3, cex = 1.25)






# Bjerrum Plot of carbonate speciations ####
## Using Plotly for interactive
library(seacarb)
library(plotly)
library(webshot)

# Set Temperature (in degrees Celsius), Salinity (in PSU), and atmospheric pressure (in atm)
EC <- 0  # Conductivity in mS/cm
T <- 20     # Temperature in °C
P <- 0      # Pressure in atm (sea surface)
S <- 0    # Salinity in PSU 

# Use the carb() function to get carbonate system constants K0, K1, and K2 at given T, S, and P
# We need to provide TA and DIC to the carb() function. For now, you can use arbitrary values
# to just retrieve the K0, K1, and K2 constants.
carb_values <- carb(flag = 15, var1 = 2000e-6, var2 = 2100e-6, S = S, T = T, P = P)

# Use the K1() and K2() functions to get dissociation constants K1 and K2 at given T, S, and P
K1_value <- K1(S = S, T = T, P = P)
K2_value <- K2(S = S, T = T, P = P)

# Define a pH range
pH <- seq(0, 14, by = 0.01)

# Calculate concentrations of species based on pH and K1, K2
H_conc <- 10^(-pH)
K1 <- K1_value
K2 <- K2_value

# Calculate relative concentrations of species (CO2, HCO3-, CO32-)
CO2 <- H_conc^2 / (H_conc^2 + K1 * H_conc + K1 * K2)
HCO3 <- K1 * H_conc / (H_conc^2 + K1 * H_conc + K1 * K2)
CO3 <- K1 * K2 / (H_conc^2 + K1 * H_conc + K1 * K2)

# Create the interactive plot
fig <- plot_ly() %>%
  add_lines(x = pH, 
            y = CO2, 
            name = "CO<sub>2</sub>", 
            line = list(color = "black"), 
            hoverinfo = 'text', 
            text = ~paste("pH:", round(pH, 2), 
                          "<br>CO<sub>2</sub>:", round(CO2, 3), 
                          "<br>HCO<sub>3</sub><sup>-</sup>:", round(HCO3, 3), 
                          "<br>CO<sub>3</sub><sup>2-</sup>:", round(CO3, 3)),
            ) %>%
  add_lines(x = pH, 
            y = HCO3, 
            name = "Bicarbonate (HCO<sub>3</sub><sup>-</sup>)",  # Updated name for legend
            line = list(color = "#d73027", dash = "dash"), 
            hoverinfo = 'text',
            text = ~paste("pH:", round(pH, 2), 
                          "<br>CO<sub>2</sub>:", round(CO2, 3), 
                          "<br>HCO<sub>3</sub><sup>-</sup>:", round(HCO3, 3), 
                          "<br>CO<sub>3</sub><sup>2-</sup>:", round(CO3, 3)),
            ) %>%
  add_lines(x = pH, 
            y = CO3, 
            name = "Carbonate (CO<sub>3</sub><sup>2-</sup>)",  # Updated name for legend
            line = list(color = "#d73027"),
            hoverinfo = 'text',
            text = ~paste("pH:", round(pH, 2), 
                          "<br>CO<sub>2</sub>:", round(CO2, 3), 
                          "<br>HCO<sub>3</sub><sup>-</sup>:", round(HCO3, 3), 
                          "<br>CO<sub>3</sub><sup>2-</sup>:", round(CO3, 3)),
            ) %>%
  layout(title = "Carbonate Speciation",
         xaxis = list(title = "pH",
                      titlefont = list(size = 20),  # Title size
                      tickfont = list(size = 20)),  # Tick mark size),
         yaxis = list(title = "Relative Concentration",
                      titlefont = list(size = 20),  # Title size
                      tickfont = list(size = 20)),  # Tick mark size),
         legend = list(font = list(size = 20)),  # Increase legend font size
         shapes = list(
           list(type = "line",
                x0 = 7.2, x1 = 7.2,  # Vertical line at pH = x
                y0 = 0, y1 = max(CO2, HCO3, CO3),  # Adjust y0 and y1 to fit your data
                line = list(color = "black", dash = "dot")),  # Line style
           list(type = "line",
                x0 = 9.2, x1 = 9.2,  # Vertical line at pH = x
                y0 = 0, y1 = max(CO2, HCO3, CO3),  # Adjust y0 and y1 to fit your data
                line = list(color = "black", dash = "dot"))
         ))

# Show the interactive plot
fig
# Export picture
export(fig, file = "/Users/reinaldy/Documents/carbon_speciation_plot.png")
kaleido(fig, "/Users/reinaldy/Documents/carbon_speciation_plot.png")


# Specify the pH value you're interested in
target_pH <- 9.2

# Calculate the H+ concentration at the target pH
target_H_conc <- 10^(-target_pH)

# Calculate the relative concentrations of species at the target pH
target_CO2 <- target_H_conc^2 / (target_H_conc^2 + K1 * target_H_conc + K1 * K2)
target_HCO3 <- K1 * target_H_conc / (target_H_conc^2 + K1 * target_H_conc + K1 * K2)
target_CO3 <- K1 * K2 / (target_H_conc^2 + K1 * target_H_conc + K1 * K2)

# Print the relative concentrations for pH 7
cat("Relative concentrations at pH =", target_pH, "\n")
cat("CO2 concentration:", target_CO2, "\n")
cat("HCO3- concentration:", target_HCO3, "\n")
cat("CO32- concentration:", target_CO3, "\n")

# Calculation: PHREEQC = Saturation Index ####

ions <- read.csv("/Users/reinaldy/Documents/Steel Slag Experiment/code/ions_ka.csv")

ions <- ions %>%
  mutate(pH = ifelse(pH > 12, pH - 0.25, pH))


# Load the appropriate PHREEQC database
db_path <- "/Users/reinaldy/Documents/Steel Slag Experiment/code/database/PHREEQC.DAT"
phrLoadDatabase(db_path)

# Initialize an empty list to store results
results_list <- list()

# Loop through each row of the dataframe
for (i in 1:nrow(ions)) {
  # Extract the current row values
  temp_value <- ions$temperature_C[i]
  pH_value <- ions$pH[i]
  alkalinity_value <- ions$TA_umol_kg[i]
  Ca_value <- ions$Ca_umol_l_mean[i]
  Na_value <- ions$Na_umol_l_mean[i]
  Cl_value <- ions$Cl_umol_l_mean[i]
  S_value <- ions$SO42_umol_l_mean[i]
  
  # Check for missing or NA values and replace them with a default (e.g., 0 for concentrations)
  if (is.na(Na_value)) Na_value <- 0
  if (is.na(Cl_value)) Cl_value <- 0
  if (is.na(S_value)) S_value <- 0
  if (is.na(Ca_value)) Ca_value <- 0
  if (is.na(alkalinity_value)) alkalinity_value <- 0
  
  # Create the PHREEQC input string for each row including saturation index for calcite
  phreeqc_input <- sprintf("
SOLUTION 1
    temp      %f
    pH        %f
    pe        4
    redox     pe
    units     umol/kgw
    density   1
    Alkalinity %f
    Ca        %f
    Na        %f
    Cl        %f
    S         %f as SO4-2
    -water    1 # kg
    
USER_PUNCH
    -headings LK_Calcite SR_Calcite SI_Calcite
    10 PUNCH LK_PHASE('Calcite') # log K
    20 PUNCH SR('Calcite') # SR = IAP/K
    30 PUNCH SI('Calcite') # SI = log (IAP/K) or SI = log SR

SELECTED_OUTPUT
    -pH  true # output the pH
    -percent_error true
    -saturation_indices Calcite
    -user_punch           true
    
END
", temp_value, pH_value, alkalinity_value, Ca_value, Na_value, Cl_value, S_value)
  
  # Run the PHREEQC input string and capture any potential errors
  tryCatch({
    phrRunString(phreeqc_input)
    
    # Extract the results
    results <- phrGetSelectedOutput()
    
    # Initialize variables to store results
    equilibrated_pH <- NA
    si_Calcite <- NA
    cbe <- NA
    SR_Calcite <- NA
    
    # Check if results are populated
    if (!is.null(results) && length(results) > 0) {
      if ("pH" %in% colnames(results[[1]])) {
        equilibrated_pH <- results[[1]]$pH
      }
      if ("si_Calcite" %in% colnames(results[[1]])) {
        si_calcite <- results[[1]]$si_Calcite
      }
      if ("pct_err" %in% colnames(results[[1]])) {
        cbe <- results[[1]]$pct_err
      }
      if ("SR_Calcite" %in% colnames(results[[1]])) {
        SR_Calcite <- results[[1]]$SR_Calcite
      }
    }
    
    # Store the results for this iteration in the list
    results_list[[i]] <- data.frame(
      temp = temp_value,
      pH_input = pH_value,
      alkalinity = alkalinity_value,
      Ca = Ca_value,
      Na = Na_value,
      Cl = Cl_value,
      S = S_value,
      equilibrated_pH = equilibrated_pH,
      si_calcite = si_calcite,
      cbe = cbe,
      SR_Calcite = SR_Calcite
    )
    
  }, error = function(e) {
    # If an error occurs (e.g., no results or invalid input), print a message
    cat("Error in iteration", i, ":", conditionMessage(e), "\n")
    
    # Skip this iteration by not adding any result to the list
  })
}

# Combine all results into a single dataframe
if (length(results_list) > 0) {
  SI_minerals <- do.call(rbind, results_list)
  
  # Print the final results
  print(SI_minerals)
} else {
  cat("No valid results were produced.\n")
}


# Calculation: PHREEQC = Saturation Index - equilibrium to 415 ppm ####

ions <- read.csv("/Users/reinaldy/Documents/Steel Slag Experiment/code/ions_ka.csv")

# Load the appropriate PHREEQC database
db_path <- "/Users/reinaldy/Documents/Steel Slag Experiment/code/database/PHREEQC.DAT"
phrLoadDatabase(db_path)

# Initialize an empty list to store results
results_list <- list()

# Loop through each row of the dataframe
for (i in 1:nrow(ions)) {
  # Extract the current row values
  temp_value <- ions$temperature_C[i]
  pH_value <- ions$pH[i]
  alkalinity_value <- ions$TA_umol_kg[i]
  Ca_value <- ions$Ca_umol_l_mean[i]
  Na_value <- ions$Na_umol_l_mean[i]
  Cl_value <- ions$Cl_umol_l_mean[i]
  S_value <- ions$SO42_umol_l_mean[i]
  
  # Check for missing or NA values and replace them with a default (e.g., 0 for concentrations)
  if (is.na(Na_value)) Na_value <- 0
  if (is.na(Cl_value)) Cl_value <- 0
  if (is.na(S_value)) S_value <- 0
  if (is.na(Ca_value)) Ca_value <- 0
  if (is.na(alkalinity_value)) alkalinity_value <- 0
  
  # Create the PHREEQC input string for each row including saturation index for calcite
  phreeqc_input <- sprintf("
SOLUTION 1
    temp      %f
    pH        %f CO2(g) -3.382
    pe        4
    redox     pe
    units     umol/kgw
    density   1
    Alkalinity %f
    Ca        %f
    Na        %f
    Cl        %f
    S         %f as SO4-2
    -water    1 # kg
    
USER_PUNCH
    -headings LK_Calcite SR_Calcite SI_Calcite
    10 PUNCH LK_PHASE('Calcite') # log K
    20 PUNCH SR('Calcite') # SR = IAP/K
    30 PUNCH SI('Calcite') # SI = log (IAP/K) or SI = log SR

SELECTED_OUTPUT
    -pH  true # output the pH
    -percent_error true
    -saturation_indices Calcite
    -user_punch           true
    
END
", temp_value, pH_value, alkalinity_value, Ca_value, Na_value, Cl_value, S_value)
  
  # Run the PHREEQC input string and capture any potential errors
  tryCatch({
    phrRunString(phreeqc_input)
    
    # Extract the results
    results <- phrGetSelectedOutput()
    
    # Initialize variables to store results
    equilibrated_pH_eq <- NA
    si_Calcite_eq <- NA
    cbe <- NA
    SR_Calcite_eq <- NA
    
    # Check if results are populated
    if (!is.null(results) && length(results) > 0) {
      if ("pH" %in% colnames(results[[1]])) {
        equilibrated_pH <- results[[1]]$pH
      }
      if ("si_Calcite" %in% colnames(results[[1]])) {
        si_calcite <- results[[1]]$si_Calcite
      }
      if ("pct_err" %in% colnames(results[[1]])) {
        cbe <- results[[1]]$pct_err
      }
      if ("SR_Calcite" %in% colnames(results[[1]])) {
        SR_Calcite <- results[[1]]$SR_Calcite
      }
    }
    
    # Store the results for this iteration in the list
    results_list[[i]] <- data.frame(
      temp = temp_value,
      pH_input = pH_value,
      alkalinity = alkalinity_value,
      Ca = Ca_value,
      Na = Na_value,
      Cl = Cl_value,
      S = S_value,
      equilibrated_pH_eq = equilibrated_pH,
      si_calcite_eq = si_calcite,
      cbe = cbe,
      SR_Calcite_eq = SR_Calcite
    )
    
  }, error = function(e) {
    # If an error occurs (e.g., no results or invalid input), print a message
    cat("Error in iteration", i, ":", conditionMessage(e), "\n")
    
    # Skip this iteration by not adding any result to the list
  })
}

# Combine all results into a single dataframe
if (length(results_list) > 0) {
  SI_minerals_eq <- do.call(rbind, results_list)
  
  # Print the final results
  print(SI_minerals_eq)
} else {
  cat("No valid results were produced.\n")
}


# Plot: PHREEQC = Saturation Index ####
# Combine the two data frames
SI_minerals <- cbind(SI_minerals, ions, SI_minerals_eq)

# Remove duplicates (optional)
SI_minerals <- unique(SI_minerals)

# Plot
#par(mfrow=c(2,3))
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)
par(mgp = c(3.5, 1, 0))  # Increase the gap for the title (first value)

par(mar = c(5, 5, 4, 2) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

#Plot OH contribution to T.Alkalinity
plot(x = SI_minerals$time_hour, 
     y = SI_minerals$SR_Calcite,
     xlab = "time (hours)",
     ylab = "Ω Calcite",
     xlim = c(0,24),
     xaxt = "n",
     ylim = c(0,2000),
     pch = 21,
     col = "black",
     bg = "black",
     type = "b",
     cex = 1.5
)
axis(1, at = c(0, 1, 4, 8, 12, 16, 20, 24), labels = c(0, 1, 4, 8, 12, 16, 20, 24))

points(x = SI_minerals$time_hour, 
     y = SI_minerals$SR_Calcite_eq,
     # xlab = "time (hours)",
     # ylab = "Ω Calcite",
     # xlim = c(0,24),
     # xaxt = "n",
     # ylim = c(0,2000),
     pch = 21,
     col = "black",
     bg = "red",
     type = "b",
     cex = 1.5
)
abline(h = 1, col = "red", lty = 2)  # Red dashed line

#Legends
legend("topleft",
       legend = c("Mixture in the box (elevated pCO2)", "Mixture if eq. to 415 ppm"),
       col = "black", 
       pch = 21, 
       pt.bg = c("black", "red"),
       cex = 1,
       bty = "n" #border n = no borderline
)


# GSD ####
gsd <- read.csv("/Users/reinaldy/Documents/Steel Slag Experiment/code/gsd_slag.csv")


#Pre slag
plot(x = gsd$aperture_um,
     y= cumsum(gsd$Fresh._slag1) / sum(gsd$Fresh._slag1),
     type = "l",
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Fresh_slag2) / sum(gsd$Fresh_slag2),
       type = "l",
       col = "red"
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Fresh_slag3) / sum(gsd$Fresh_slag3),
       type = "l",
       col = "blue"
)

#Post slag
plot(x = gsd$aperture_um,
     y= cumsum(gsd$Post_slag1) / sum(gsd$Post_slag1),
     type = "l",
     )
points(x = gsd$aperture_um,
       y= cumsum(gsd$Post_slag2) / sum(gsd$Post_slag2),
       type = "l",
       col = "red"
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Post_slag3) / sum(gsd$Post_slag1),
       type = "l",
       col = "blue"
)


###combination into 1 plot
#Pre slag
plot(x = gsd$aperture_um,
     y= cumsum(gsd$Fresh._slag1) / sum(gsd$Fresh._slag1),
     type = "l",
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Fresh_slag2) / sum(gsd$Fresh_slag2),
       type = "l",
       col = "red"
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Fresh_slag3) / sum(gsd$Fresh_slag3),
       type = "l",
       col = "blue"
)

#Post slag
points(x = gsd$aperture_um,
     y= cumsum(gsd$Post_slag1) / sum(gsd$Post_slag1),
     type = "l",
     lty = 2
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Post_slag2) / sum(gsd$Post_slag2),
       type = "l",
       col = "red",
       lty = 2
)
points(x = gsd$aperture_um,
       y= cumsum(gsd$Post_slag3) / sum(gsd$Post_slag1),
       type = "l",
       col = "blue",
       lty = 2
)
############################################################################################################


#################################################################################


# Calculate the moles n = PV/RT
#basic
data["P_initial_mean_atm"] <- data$pCO2_percentage_initial_mean / 100 # in atm
data["P_initial_sd_atm"] <- data$pCO2_percentage_initial_sd / 100 # in atm
data["P_final_mean_atm"] <- data$pCO2_percentage_final_mean / 100 # in atm
data["P_final_sd_atm"] <- data$pCO2_percentage_final_sd / 100 # in atm

data["V_litre"] <- (0.8 * 0.7 * 0.475) * 1000 # dimention of the box (h, w, d) in metre but volume in litre

data["R_L_atm_mol_K"] <- 0.0821 # in L.atm/mol.K

data["temperature_K"] <- 23 + 273.15 # in Kelvin (K)

data["AR_CO2_g_mol"] <- 44.01 # in g/mol

#calculation moles
data["n_initial_mean_mol"] <- (data$P_initial_mean_atm * data$V_litre) / (data$R_L_atm_mol_K * data$temperature_K) # in moles
data["n_initial_sd_mol"] <- (data$P_initial_sd_atm * data$V_litre) / (data$R_L_atm_mol_K * data$temperature_K) # in moles
data["n_final_mean_mol"] <- (data$P_final_mean_atm * data$V_litre) / (data$R_L_atm_mol_K * data$temperature_K) # in moles
data["n_final_sd_mol"] <- (data$P_final_sd_atm * data$V_litre) / (data$R_L_atm_mol_K * data$temperature_K) # in moles


data["mCO2_initial_mean_grams"] <- data$n_initial_mean_mol * data$AR_CO2_g_mol  # in grams
data["mCO2_initial_sd_grams"] <- data$n_initial_sd_mol * data$AR_CO2_g_mol  # in grams
data["mCO2_final_mean_grams"] <- data$n_final_mean_mol * data$AR_CO2_g_mol  # in grams
data["mCO2_final_sd_grams"] <- data$n_final_sd_mol * data$AR_CO2_g_mol  # in grams

data["RCO2_mean_grams"] <- (data$mCO2_initial_mean_grams - data$mCO2_final_mean_grams) / data$time_hour # in grams
data["RCO2_sd_grams"] <- (data$mCO2_initial_sd_grams - data$mCO2_final_sd_grams) / data$time_hour  # in grams


# Plot: Carbon sequestration rate 
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

#CO2 sequestration Rate
par(mar = c(5, 4, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

plot(x = data$time_hour, 
     y = data$RCO2_mean_grams,
     xlab = "time (hours)",
     ylab = "CO2 sequestration Rate in g/hour",
     xlim = c(0, 24),
     xaxt = "n",
     ylim = c(0, 50),
     pch = 21,
     col = "black",
     bg = "black",
     type = "b",
     cex = 1.5
)
axis(1, at = c(0, 4, 8, 12, 16, 20, 24), labels = c(0, 4, 8, 12, 16, 20, 24))

arrows(data$time_hour, data$RCO2_mean_grams - data$RCO2_sd_grams, 
       data$time_hour, data$RCO2_mean_grams + data$RCO2_sd_grams, 
       angle = 90, code = 3, length = 0.1, col = "black")







# Dataset 
# Data of the box
box <- data.frame(
  height_meter = 0.8,
  wide_meter = 0.70,
  depth_meter = 0.475
)
box["volume_m3"] = box$height_meter * box$wide_meter * box$depth_meter


# Dataset from excel
data_box <- data.frame(
  time_hour = c(1, 1, 1, 4, 4, 8, 8, 12, 12, 24, 24),
  pCO2_percentage_initial = c(25.89, 25.48, 24.99, 26.21, 25.31, 25.2, 27.23, 24.89, 25.89, 24.8, 25.8),
  pCO2_percentage_final = c(18.03, 17.46, 16.38, 6.79, 7.53, 3.17, 7.19, 4.75, 6.72, 3.04, 3.39)
)

# Dataset from excel
data_box_0 <- data.frame(
  time_hour = rep(0, 11),
  pCO2_percentage_initial = c(25.89, 25.48, 24.99, 26.21, 25.31, 25.2, 27.23, 24.89, 25.89, 24.8, 25.8),
  pCO2_percentage_final = c(25.89, 25.48, 24.99, 26.21, 25.31, 25.2, 27.23, 24.89, 25.89, 24.8, 25.8)  # This should match the length of time_hour
)

# Combine the new row with the existing data frame
data_box_combined <- rbind(data_box_0, data_box)

#calculation of CO2_efficiency_percentage
data_box_combined["CO2_efficiency_percentage"] <- 100 * ((data_box_combined$pCO2_percentage_initial - data_box_combined$pCO2_percentage_final)/data_box_combined$pCO2_percentage_initial)

# Calculation of CO2 sequestration rate
# Calculate the moles n = PV/RT
data_box_combined["P_initial_atm"] <- data_box_combined$pCO2_percentage_initial / 100 # in atm
data_box_combined["P_final_atm"] <- data_box_combined$pCO2_percentage_final / 100 # in atm

data_box_combined["V_litre"] <- (0.8 * 0.7 * 0.475) * 1000 # dimention of the box (h, w, d) in metre but volume in litre

data_box_combined["R_L_atm_mol_K"] <- 0.0821 # in L.atm/mol.K

data_box_combined["temperature_K"] <- 23 + 273.15 # in Kelvin (K)

data_box_combined["AR_CO2_g_mol"] <- 44.01 # in g/mol

#calculation moles
data_box_combined["n_initial_mol"] <- (data_box_combined$P_initial_atm * data_box_combined$V_litre) / (data_box_combined$R_L_atm_mol_K * data_box_combined$temperature_K) # in moles
data_box_combined["n_final_mol"] <- (data_box_combined$P_final_atm * data_box_combined$V_litre) / (data_box_combined$R_L_atm_mol_K * data_box_combined$temperature_K) # in moles


data_box_combined["mCO2_initial_grams"] <- data_box_combined$n_initial_mol * data_box_combined$AR_CO2_g_mol  # in grams
data_box_combined["mCO2_final_grams"] <- data_box_combined$n_final_mol * data_box_combined$AR_CO2_g_mol  # in grams

data_box_combined["RCO2_grams_hour"] <- (data_box_combined$mCO2_initial_grams - data_box_combined$mCO2_final_grams) / data_box_combined$time_hour # in grams


# Calculate the mean and standard deviation for identical time values
data_box_summary <- data_box_combined %>%
  group_by(time_hour) %>%
  summarise(
    pCO2_percentage_initial_mean = mean(pCO2_percentage_initial),
    pCO2_percentage_initial_sd = sd(pCO2_percentage_initial),
    pCO2_percentage_final_mean = mean(pCO2_percentage_final),
    pCO2_percentage_final_sd = sd(pCO2_percentage_final),
    CO2_efficiency_percentage_mean = mean(CO2_efficiency_percentage),
    CO2_efficiency_percentage_sd = sd(CO2_efficiency_percentage),
    n_initial_mol_mean = mean(n_initial_mol),
    n_initial_mol_sd = sd(n_initial_mol),
    n_final_mol_mean = mean(n_final_mol),
    n_final_mol_sd = sd(n_final_mol),
    m_CO2_initial_grams_mean = mean(mCO2_initial_grams),
    m_CO2_initial_grams_sd = sd(mCO2_initial_grams),
    m_CO2_final_grams_mean = mean(mCO2_final_grams),
    m_CO2_final_grams_sd = sd(mCO2_final_grams),
    RCO2_grams_hour_mean = mean(RCO2_grams_hour),
    RCO2_grams_hour_sd = sd(RCO2_grams_hour)
  )




# Load the appropriate PHREEQC database
db_path <- "/Users/reinaldy/Documents/Steel Slag Experiment/code/database/PHREEQC.DAT"
phrLoadDatabase(db_path)


# Create the PHREEQC input file as a string for initial conditions
phreeqc_input <- "
SOLUTION 1
    temp      25
    pH        7 CO2(g) -0.6
    pe        4
    redox     pe
    units     umol/kgw
    density   1
    Alkalinity 40000
    Ca        20000
    -water    1 # kg
END

SELECTED_OUTPUT
    -pH  # output the pH
END
"

# Run the PHREEQC input string
phrRunString(phreeqc_input)

# Extract the results
results <- phrGetSelectedOutput()

# Check if the results are populated
if (is.null(results) || length(results) == 0) {
  cat("No results found. Please check the input and database.\n")
} else {
  # Display the results
  print(results)
  
  # Extract the equilibrated pH and total CO2 concentration
  equilibrated_pH <- results[[1]]$pH
  total_CO2 <- results[[1]]$totals[["CO2"]]
  
  # Display the outputs
  cat("Equilibrated pH:", equilibrated_pH, "\n")
}




























# Combine the new row with the existing data frame
data_box_combined <- rbind(data_box_0, data_box)


# Calculate the moles n = PV/RT
#basic
data_box_combined["P_initial_atm"] <- data_box_combined$pCO2_percentage_initial / 100 # in atm
data_box_combined["P_final_atm"] <- data_box_combined$pCO2_percentage_final / 100 # in atm

data_box_combined["V_litre"] <- (0.8 * 0.7 * 0.475) * 1000 # dimention of the box (h, w, d) in metre but volume in litre

data_box_combined["R_L_atm_mol_K"] <- 0.0821 # in L.atm/mol.K

data_box_combined["temperature_K"] <- 23 + 273.15 # in Kelvin (K)

data_box_combined["AR_CO2_g_mol"] <- 44.01 # in g/mol

#calculation moles
data_box_combined["n_initial_mol"] <- (data_box_combined$P_initial_atm * data_box_combined$V_litre) / (data_box_combined$R_L_atm_mol_K * data_box_combined$temperature_K) # in moles
data_box_combined["n_final_mol"] <- (data_box_combined$P_final_atm * data_box_combined$V_litre) / (data_box_combined$R_L_atm_mol_K * data_box_combined$temperature_K) # in moles


data_box_combined["mCO2_initial_grams"] <- data_box_combined$n_initial_mol * data_box_combined$AR_CO2_g_mol  # in grams
data_box_combined["mCO2_final_grams"] <- data_box_combined$n_final_mol * data_box_combined$AR_CO2_g_mol  # in grams

data_box_combined["RCO2_grams"] <- (data_box_combined$mCO2_initial_grams - data_box_combined$mCO2_final_grams) / data_box_combined$time_hour # in grams


# Calculate the mean and standard deviation for identical time values
data_RCO2 <- data_box_combined %>%
  group_by(time_hour) %>%
  summarise(
    pCO2_percentage_initial_mean = mean(pCO2_percentage_initial),
    pCO2_percentage_initial_sd = sd(pCO2_percentage_initial),
    pCO2_percentage_final_mean = mean(pCO2_percentage_final),
    pCO2_percentage_final_sd = sd(pCO2_percentage_final),
    mCO2_initial_grams_sd = sd(mCO2_initial_grams),
    mCO2_final_grams_sd = sd(mCO2_final_grams),
    RCO2_mean_grams = mean(RCO2_grams),
    RCO2_sd_grams = sd(RCO2_grams)
  )



# Plot: Carbon sequestration rate
par(cex.main = 1.5,  # Font size for main title
    cex.lab = 1.25,  # Font size for xlab and ylab
    cex.axis = 1.25, # Font size for axis tick labels
    las = 1)

#CO2 sequestration Rate
par(mar = c(5, 4, 4, 5) + 0.1) #margin bottom, left, up, right. The default is c(5, 4, 4, 2) + 0.1.

plot(x = data_RCO2$time_hour, 
     y = data_RCO2$RCO2_mean_grams,
     xlab = "time (hours)",
     ylab = "CO2 concentration in %",
     xlim = c(0, 24),
     xaxt = "n",
     ylim = c(0, 50),
     pch = 21,
     col = "black",
     bg = "black",
     type = "b",
     cex = 1.5
)
axis(1, at = c(0, 4, 8, 12, 16, 20, 24), labels = c(0, 4, 8, 12, 16, 20, 24))

arrows(data_RCO2$time_hour, data_RCO2$RCO2_mean_grams - data_RCO2$RCO2_sd_grams, 
       data_RCO2$time_hour, data_RCO2$RCO2_mean_grams + data_RCO2$RCO2_sd_grams, 
       angle = 90, code = 3, length = 0.1, col = "black")



boxplot(RCO2_grams ~ time_hour, data = data_box_combined, 
        main = "Boxplot of pCO2 Initial Percentage by Time",
        xlab = "Time (hours)", ylab = "pCO2 Initial Percentage",
        col = "lightblue")



#############. OH-
# Load necessary library
library(phreeqc)

# Load the appropriate PHREEQC database
db_path <- "/Users/reinaldy/Documents/Steel Slag Experiment/code/database/PHREEQC.DAT"
phrLoadDatabase(db_path)

# Define the pH range and step size
pH_values <- seq(0, 14, by = 0.1)

# Initialize an empty list to store results
results_list <- list()

# Loop through each pH value
for (pH_value in pH_values) {
  
  # Create the PHREEQC input string for each pH value
  phreeqc_input <- sprintf("
SOLUTION 1
    temp      25
    pH        %.1f
    units     mol/kgw
    -water    1 # kg
END

SELECTED_OUTPUT
    -pH  # output the pH
    -molalities H+ OH-  # output the total concentrations of H+ and OH-
END
", pH_value)
  
  # Run the PHREEQC input string
  phrRunString(phreeqc_input)
  
  # Extract the results
  results <- phrGetSelectedOutput()
  
  # Initialize variables to store the results for H+ and OH-
  H_conc <- NA
  OH_conc <- NA
  
  # Check if results are populated and have the 'totals' field
  if (!is.null(results) && length(results) > 0 && !is.null(results[[1]]$totals)) {
    # Extract the concentrations of H+ and OH- if available
    if ("H+" %in% names(results[[1]]$totals)) {
      H_conc <- results[[1]]$totals[["H+"]]
    }
    if ("OH-" %in% names(results[[1]]$totals)) {
      OH_conc <- results[[1]]$totals[["OH-"]]
    }
    
    # Store the results for this iteration in the list
    results_list[[length(results_list) + 1]] <- data.frame(
      pH = pH_value,
      H_concentration = H_conc,
      OH_concentration = OH_conc
    )
  }
}

# Combine all results into a single dataframe
final_results <- do.call(rbind, results_list)

# Print the final results
print(final_results)



# Function to calculate salinity based on conductivity and temperature (UNESCO 1983 PSS-78)
# Convert conductivity to conductivity ratio (R) by dividing by reference at 15°C
calculate_salinity <-function (c, t = 25, P = 0) 
{
  a0 = 0.008
  a1 = -0.1692
  a2 = 25.3851
  a3 = 14.0941
  a4 = -7.0261
  a5 = 2.7081
  b0 = 5e-04
  b1 = -0.0056
  b2 = -0.0066
  b3 = -0.0375
  b4 = 0.0636
  b5 = -0.0144
  c0 = 0.6766097
  c1 = 0.0200564
  c2 = 0.0001104
  c3 = -6.9698e-07
  c4 = 1.0031e-09
  D1 = 0.03426
  D2 = 0.0004464
  D3 = 0.4215
  D4 = -0.003107
  e1 = 0.000207
  e2 = -6.37e-08
  e3 = 3.989e-12
  Csw = 42.914
  K = 0.0162
  Ct = round(c * (1 + 0.0191 * (t - 25)), 0)
  R = (Ct/1000)/Csw
  rt = c0 + (t * c1) + (t^2 * c2) + (t^3 * c3) + (t^4 * c4)
  Rp = 1 + (P * e1 + e2 * P^2 + e3 * P^3)/(1 + D1 * t + D2 * t^2 + (D3 + D4 * t) * R)
  Rt1 = R/(Rp * rt)
  dS = (b0 + b1 * Rt1^(1/2) + b2 * Rt1^(2/2) + b3 * Rt1^(3/2) + b4 * Rt1^(4/2) + b5 * Rt1^(5/2)) * (t - 15)/(1 + K *(t - 15))
  S = a0 + a1 * Rt1^(1/2) + a2 * Rt1^(2/2) + a3 * Rt1^(3/2) + a4 * Rt1^(4/2) + a5 * Rt1^(5/2) + dS
  S[is.na(S<0)]<-NA
  S[S<2 & !is.na(S)]<- S[S<2 & !is.na(S)] - a0/(1 + 1.5 * (400 * Rt1) + (400 * Rt1)^2) - (b0 * (t - 15)/(1 + K * (t - 15)))/(1 + (100 * Rt1)^(1/2) + (100 * Rt1)^(3/2))
  PSS = round(S, 3)
  return(PSS)
}








