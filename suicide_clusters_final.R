options(scipen = 999)

setwd("D:/projects/chi_suicide_greenspace/data/")
source("D:/projects/dep-walkability/dep-walkability/R code/libs.R")
library(dplyr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(sf)
library(tmap)
library(purrr)
library(tmap)
library(ggplot2)
library(sociome)
library(INLA)
# install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE, type = "binary")
library(spdep)
library(patchwork) # For arranging multiple plots
library(car)
library(flextable)
library(stringr)
library(gtsummary)
library(flextable)
######################################################################################


# Load your data
suicides2324 <- read.csv("suicides2324.csv") # Replace with actual file path

suicides2324 <- suicides2324 %>%
  mutate(Race_Latino = case_when(
    Latino == TRUE ~ "Latino",          # If Latino is TRUE, assign "Latino"
    Latino == FALSE ~ as.character(Race) # Otherwise, retain the value in Race
  ))

suicides2324 <- suicides2324 %>%
  mutate(
    Incident = tolower(Incident),        # Convert Incident to lowercase
    Residence = tolower(Residence),     # Convert Residence to lowercase
    Same_Location = ifelse(Incident == Residence, 1, 0)  # Compare the fields
  )

suicides2324 <- suicides2324 %>%
  mutate(Cause_Category = case_when(
    str_detect(Primary.Ca, regex("hanging|asphyxia|plastic|exhaust|strangulation|carbon", ignore_case = TRUE)) ~ "Hanging/Asphyxia",
    str_detect(Primary.Ca, regex("drugs|toxicity|overdose|caustic|ethanol|intoxication", ignore_case = TRUE)) ~ "Drugs/Toxicity",
    str_detect(Primary.Ca, regex("immolation|fire|thermal", ignore_case = TRUE)) ~ "Immolation",
    str_detect(Primary.Ca, regex("drowning", ignore_case = TRUE)) ~ "Drowning",
    str_detect(Primary.Ca, regex("gun|gunshot|firearm|blast|rifle", ignore_case = TRUE)) ~ "Gunshot/Firearm",
    str_detect(Primary.Ca, regex("jump|falls|fall", ignore_case = TRUE)) ~ "Jump/Falls",
    # Check for blunt force in conjunction with falls or jumps
    str_detect(Primary.Ca, regex("blunt force", ignore_case = TRUE)) &
      str_detect(Primary.Ca, regex("jump|falls|fall", ignore_case = TRUE)) ~ "Blunt Force from Jump/Falls",
    # If blunt force but no mention of jump/falls
    str_detect(Primary.Ca, regex("blunt force", ignore_case = TRUE)) ~ "Blunt Force from Other Causes",
    str_detect(Primary.Ca, regex("crossbow|knife|incise|incision|stab|sharp", ignore_case = TRUE)) ~ "Sharp Object",
    str_detect(Primary.Ca, regex("struck|striking|train|motor|pedestrian", ignore_case = TRUE)) ~ "Blunt Force from Other Causes",
    str_detect(Primary.Ca, regex("electrocution|electrocuted", ignore_case = TRUE)) ~ "Electrocution",
    TRUE ~ "Other"
  ))


summary_table <- suicides2324 %>%
  mutate(year = as.factor(year)) %>%
  dplyr::select(year, Gender, Race_Latino, Age, Cause_Category, Same_Location) %>%
  tbl_summary(by = year, # Stratify by gender
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              missing = "no") %>%
  add_p(test = list(Cause_Category ~ "fisher.test"), 
        test.args = list(Cause_Category = list(simulate.p.value = TRUE))) %>%
  as_flex_table()

summary_table <- summary_table %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all")

save_as_docx(summary_table, path = "DescriptiveSuicides.docx")

#############################################################################################
gap_df = st_read("analysis_data_PS_TE_TRANSIT_ME_TC_PROXIMITY_11.28.2024.shp")

suicides_sf <- suicides2324 %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%  # Remove rows with NA in longitude or latitude
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # Assuming WGS84 CRS (EPSG:4326)

suicides_sf_transformed <- st_transform(suicides_sf, st_crs(gap_df))

gap_df_nogeom <- gap_df %>% st_drop_geometry()
# Function to replace NA with the mean of the column
replace_na_with_mean <- function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
}

numeric_columns <- c("LUS_Open.S", "LUS_Develo", "LUS_Forest", "LUS_Water_", "LUS_Wetlan", "LUS_Grassl", "LUS_Herbac")

gap_df_nogeom[numeric_columns] <- lapply(gap_df_nogeom[numeric_columns], function(x) as.numeric(as.character(x)))

gap_df_nogeom[numeric_columns] <- lapply(gap_df_nogeom[numeric_columns], replace_na_with_mean)

gap_df_nogeom$not_developed <- 1 - rowSums(gap_df_nogeom[numeric_columns], na.rm = TRUE)

gap_df_nogeom$not_developed[gap_df_nogeom$not_developed < 0] <- 0

simpsons_diversity_index <- function(proportions) {
  sum_of_squares <- sum(proportions^2, na.rm = TRUE)
  diversity_index <- 1 - sum_of_squares
  return(diversity_index)
}

proportions <- gap_df_nogeom[c(numeric_columns, "not_developed")]

diversity_indices <- apply(proportions, 1, simpsons_diversity_index)

gap_df$LCdiversity <- diversity_indices

library(tmap)

map1 <- tm_shape(gap_df) +
  tm_borders() +
  tm_fill(col = "LUS_Open.S", palette = "YlGnBu", title = "Developed Open Space") +
  tm_shape(suicides_sf) +
  tm_dots(col = "indianred", size = 0.1, alpha = 0.2, title = "Suicides") +  # Translucent dots
  tm_layout(frame = FALSE)

map2 <- tm_shape(gap_df) +
  tm_borders() +
  tm_fill(col = "LCdiversity", palette = "YlGnBu", title = "Land Cover Diversity") +
  tm_shape(suicides_sf) +
  tm_dots(col = "indianred", size = 0.1, alpha = 0.2, title = "Suicides") +  # Translucent dots
  tm_layout(frame = FALSE)

tmap_arrange(map1, map2, ncol = 2)

plot(gap_df$geometry)
head(gap_df)
names(gap_df)

cor(gap_df$LCdiversity, gap_df$LUS_Open.S, use = "complete.obs")
cor(gap_df$LCdiversity, gap_df$Treecanopy, use = "complete.obs")
cor(gap_df$LUS_Open.S, gap_df$Treecanopy, use = "complete.obs")
# Ensure columns and new_column_names match in length
columns <- c(
  "LUS_Open.S",
  "LUS_WaterY",
  "LCdiversity",
  "acresc30",
  "tes",
  "Treecanopy",
  "parkpriori",
  "unemprate",
  "heatanamol",
  "treepriori",
  "popperacre",
  "temperatur",
  "pctpoc",
  "pctpov",
  "lingisolat",
  "healthburd",
  "tt3rdgroc",
  "tt3rdhosp",
  "tt3rdpharm",
  "tt3rdurgen"
)

# Subset and rename columns
gap_df_sub <- gap_df[, columns] %>% st_drop_geometry() %>% dplyr::select(-LUS_WaterY)

new_column_names <- c(
  "Developed Open Space",
  "Land Use Diversity",
  "Park Acreage 15 Min",
  "Tree Equity Score",
  "Tree Canopy Cover",
  "Park Priority Index",
  "Unemployment Rate",
  "Temperature Anomaly",
  "Tree Priority Index",
  "Population Density",
  "Temperature",
  "Non White",
  "Poverty",
  "Linguistic Isolation",
  "Health Burden",
  "Travel Time to Grocery",
  "Travel Time to Hospital",
  "Travel Time to Pharmacy",
  "Travel Time to Urgent Care"
)


colnames(gap_df_sub) <- new_column_names

histograms <- purrr::map(new_column_names, ~ gghistogram(gap_df_sub, x = .x))

ggarrange(plotlist = histograms)

adi_cook <- get_adi(
  "block group",
  state = "Illinois",
  county = "Cook",
  year = 2022,
  geometry = FALSE,
  keep_indicators = TRUE
  )

gap_df <- gap_df %>% left_join(adi_cook)

gap_df_subset <- gap_df %>%
  dplyr::select(
    GEOID,
    LUS_Open.S,
  LUS_WaterY,
  LUS_OSYN,
  hwyTF,
  outside1.m,
  LCdiversity,
  acresc30,
  tes,
  Treecanopy,
  parkpriori,
  unemprate,
  heatanamol,
  treepriori,
  popperacre,
  temperatur,
  pctpoc,
  pctpov,
  lingisolat,
  healthburd,
  ttjobs60,
  tt3rdgroc,
  tt3rdhosp,
  tt3rdpharm,
  tt3rdurgen,
  var_sui_N,
  acspop,
  pctOwnerOccupiedHousing,
  medianHouseValue
  )  %>% drop_na()


cleaned_data <- gap_df_subset

# Scale the variables
cleaned_data <- cleaned_data %>%
  mutate(
    GEOID = GEOID,
    LUS_Open.S = scale(LUS_Open.S),
    LCdiversity = scale(LCdiversity),
    acresc30 = scale(acresc30),
    tes = scale(tes),
    Treecanopy = scale(Treecanopy),
    parkpriori = scale(parkpriori),
    unemprate = scale(unemprate),
    heatanamol = scale(heatanamol),
    treepriori = scale(treepriori),
    popperacre  = scale(popperacre),
    temperatur  = scale(temperatur),
    pctpoc  = scale(pctpoc),
    pctpov  = scale(pctpov),
    lingisolat  = scale(lingisolat),
    ttjobs60 = scale(ttjobs60),
    healthburd= scale(healthburd),
    tt3rdgroc  = scale(tt3rdgroc),
    tt3rdhosp  = scale(tt3rdhosp),
    tt3rdpharm  = scale(tt3rdpharm),
    tt3rdurgen  = scale(tt3rdurgen),
    pctOwnerOccupiedHousing  = scale(pctOwnerOccupiedHousing),
    medianHouseValue  = scale(medianHouseValue),
  )

formula1 <- var_sui_N ~ LUS_Open.S + LCdiversity + Treecanopy + LUS_WaterY
formula2 <- var_sui_N ~ outside1.m + hwyTF + acresc30
formula3 <- var_sui_N ~ tes + parkpriori + treepriori + popperacre + ttjobs60 + tt3rdgroc + tt3rdhosp + tt3rdpharm + tt3rdurgen
formula4 <- var_sui_N ~ pctOwnerOccupiedHousing + medianHouseValue + unemprate + pctpov + pctpoc + lingisolat
formula5 <- var_sui_N ~ heatanamol + temperatur + healthburd

formula12 <- var_sui_N ~ LUS_Open.S + LCdiversity + Treecanopy + LUS_WaterY +  outside1.m + hwyTF + acresc30
formula123  <- var_sui_N ~ LUS_Open.S + LCdiversity + Treecanopy + LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori + popperacre + ttjobs60 + tt3rdgroc + tt3rdhosp + tt3rdpharm + tt3rdurgen
formula1234 <- var_sui_N ~ LUS_Open.S + LCdiversity + Treecanopy + LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori +
   popperacre + ttjobs60 + tt3rdgroc + tt3rdhosp + tt3rdpharm + tt3rdurgen +  unemprate + lingisolat +pctOwnerOccupiedHousing + medianHouseValue
formula12345 <- var_sui_N ~ LUS_Open.S + LCdiversity +  LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori +
  popperacre + ttjobs60 + tt3rdpharm + tt3rdurgen +  unemprate + lingisolat +pctOwnerOccupiedHousing + medianHouseValue +
  temperatur + healthburd

model1 <- glm(formula1, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model2 <- glm(formula2, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model3 <- glm(formula3, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model4 <- glm(formula4, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model5 <- glm(formula5, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model12 <- glm(formula12, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model123 <- glm(formula123, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model1234 <- glm(formula1234, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))
model12345 <- glm(formula12345, data = gap_df_subset, family = poisson(link = "log"), offset = log(acspop))

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model12)
summary(model123)
summary(model1234)
summary(model12345)

vif(model1)
vif(model2)
vif(model3)
vif(model4)
vif(model5)
vif(model12)
vif(model123)
vif(model1234)
vif(model12345)

std_errors <- summary(model12345)$coefficients[, "Std. Error"]
deviance <- model12345$deviance
df_residual <- model12345$df.residual

overdispersion <- deviance / df_residual
overdispersion

num_params <- length(coef(model12345))
DIC <- deviance + 2 * num_params
DIC

library(corrplot)

cordf <- cleaned_data %>% st_drop_geometry()

numeric_columns <- c("LUS_Open.S", "LCdiversity", "acresc30", "tes",   "parkpriori",
                     "treepriori", "ttjobs60", "tt3rdpharm", "tt3rdurgen",  "lingisolat",
                     "pctOwnerOccupiedHousing", "medianHouseValue", "temperatur", "healthburd")

short_names <- c("OS", "LCDIV", "PACRE", "TES",  "PPRIOR",
                 "TPRIOR",  "JOBS",  "PHARM", "URG",  "LNGISO",
                  "HOMEOWN", "HOUSEV", "TEMP",  "HEALTH")

cor_matrix <- cor(cordf[numeric_columns], use = "complete.obs")

print(cor_matrix)

colnames(cor_matrix) <- short_names
rownames(cor_matrix) <- short_names

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.8)

library(igraph)
library(ggraph)
library(tidyverse)

threshold <- 0.3
cor_matrix[abs(cor_matrix) < threshold] <- 0

cor_graph <- graph_from_adjacency_matrix(cor_matrix,
     weighted = TRUE,
     diag = FALSE,
     mode = "undirected"
     )

E(cor_graph)$weight <- E(cor_graph)$weight  # Keep signed weights
E(cor_graph)$sign <- ifelse(E(cor_graph)$weight > 0, "positive", "negative")  # Sign of correlation

ggraph(cor_graph, layout = "circle") +
  geom_edge_link(aes(width = abs(weight) / 14, color = sign), alpha = 0.8) +  # Scale edge width
  geom_node_point(size = 6, color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_edge_color_manual(values = c("positive" = "red", "negative" = "blue")) +
  labs(title = "Correlation Network", edge_color = "Correlation Type") +
  theme_void()

######################################################################################### spatial autocorrelation
nb <- poly2nb(gap_df, queen = TRUE)

lw <- nb2listw(nb, style = "W",zero.policy=TRUE)

moran_test <- moran.test(gap_df$var_sui_N, lw)
print(moran_test)

local_moran <- localmoran(gap_df$var_sui_N, lw)

gap_df$local_moran_I <- local_moran[, 1]  # Store the Moran's I values
head(gap_df$local_moran_I)

gap_df$local_moran_p <- local_moran[, 5]

gap_df$cluster_type <- ifelse(
  gap_df$local_moran_p < 0.05 & gap_df$local_moran_I > 0, "High-High",  # High-high: positive spatial autocorrelation
  ifelse(
    gap_df$local_moran_p < 0.05 & gap_df$local_moran_I < 0, "Low-Low",  # Low-low: negative spatial autocorrelation
    "Other"  # If Moran's I is not significant or has other patterns
  )
)

table(gap_df$cluster_type)

head(gap_df[, c("local_moran_I", "local_moran_p")])

gap_df$significance <- cut(
  gap_df$local_moran_p,
     breaks = c(-Inf, 0.01, 0.05, 0.10, 1),
     labels = c("p < 0.01", "p < 0.05", "p < 0.10", "ns"),
     include.lowest = TRUE
  )


ggplot(data = gap_df) +
  geom_sf(aes(fill = significance), color = "grey40", linewidth = .00002) +  # Set borders to white
  scale_fill_manual(
    values = c("p < 0.01" = "blue",
       "p < 0.05" = "purple",
       "p < 0.10" = "orange",
       "ns" = "grey85"),
        na.value = "red") +  # Fill NA with grey
  theme_void() +
  labs(fill = "Significance Level") +
  theme(legend.position = "bottom")

gap_df$cluster_type <- ifelse(
  gap_df$local_moran_p < 0.05 & gap_df$local_moran_I > 0, "High-High",   # High-high: positive spatial autocorrelation
  ifelse(
    gap_df$local_moran_p < 0.05 & gap_df$local_moran_I < 0, "Low-Low",    # Low-low: negative spatial autocorrelation
    "Other"  # If Moran's I is not significant or has other patterns
  )
)

gap_df_cluster <- gap_df %>%
  dplyr::select(GEOID, cluster_type)

unscaled_data <- gap_df_subset %>%  # <- this must still be spatial
  left_join(st_drop_geometry(gap_df_cluster), by = "GEOID")

print(class(unscaled_data))  # Should show "sf"

suicides_sf <- st_transform(suicides_sf, st_crs(unscaled_data))

joined_data <- st_join(suicides_sf, unscaled_data, join = st_within)
unmatched_points <- suicides_sf[is.na(joined_data$cluster_type), ]
nearest_ids <- st_nearest_feature(unmatched_points, unscaled_data)
nearest_matches <- unscaled_data[nearest_ids, ] %>%
  st_drop_geometry()

joined_data_fallback <- bind_cols(unmatched_points, nearest_matches)
joined_data_complete <- bind_rows(
  joined_data %>% filter(!is.na(cluster_type)),
  joined_data_fallback
)
joined_data <- joined_data_complete

ggplot(data = gap_df) +
  geom_sf(aes(fill = cluster_type), color = "grey40", linewidth = .00002) +  # Set borders to grey
  scale_fill_manual(values = c("High-High" = "red",
                               "Low-Low" = "blue",
                               "Other" = "grey85"),
                    na.value = "black") +  # Fill NA with grey
  theme_void() +
  labs(fill = "Cluster Type") +
  theme(legend.position = "bottom")

ggplot(data = gap_df) +
  geom_sf(aes(fill = cluster_type), color = "grey40", linewidth = .00002) +  # Set borders to grey
  scale_fill_manual(values = c("High-High" = "red",
                               "Low-Low" = "blue",
                               "Other" = "grey85"),
                    na.value = "black") +  # Fill NA with grey
  theme_void() +
  labs(fill = "Cluster Type") +
  theme(legend.position = "top",
        legend.text = element_text(size = 7), # Adjust font size for legend text
        legend.title = element_text(size = 8)) # Adjust font size for legend title


##################################################################################

formula1 <- var_sui_N ~ LUS_Open.S + LCdiversity + Treecanopy + LUS_WaterY
formula2 <- var_sui_N ~ outside1.m + hwyTF + acresc30
formula3 <- var_sui_N ~ tes + parkpriori + treepriori + popperacre + ttjobs60 + tt3rdgroc + tt3rdhosp + tt3rdpharm + tt3rdurgen
formula4 <- var_sui_N ~ pctOwnerOccupiedHousing + medianHouseValue + unemprate + pctpov + pctpoc + lingisolat
formula5 <- var_sui_N ~ heatanamol + temperatur + healthburd

cleaned_data$ID.area<-seq(1,nrow(cleaned_data))

formulaUH <- var_sui_N~ f(ID.area, model = "iid") # specify model
resultUH <- inla(formulaUH,family="poisson",
  data=cleaned_data, control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),E=(2*acspop))
summary(resultUH)
exp(resultUH$summary.fixed)
(exp(resultUH$summary.fixed)*100000) #7.75 per 100000 population

re1<-resultUH$summary.random$ID.area[,2]
plot(density(re1)) # plot density random effects
cleaned_data$UH<-re1

#####################################################################################

nbs<-knearneigh(st_centroid(cleaned_data), k = 5, longlat = T)

nbs<-knn2nb(nbs, row.names = cleaned_data$ID.area, sym = T) #force symmetry!!
mat <- nb2mat(nbs, style="B",zero.policy=TRUE)
colnames(mat) <- rownames(mat)
mat <- as.matrix(mat[1:dim(mat)[1], 1:dim(mat)[1]])
nb2INLA("cl_graph",nbs)
am_adj <-paste(getwd(),"/cl_graph",sep="")
H<-inla.read.graph(filename="cl_graph")

##############
nbs <- poly2nb(cleaned_data, queen = TRUE, row.names = cleaned_data$ID.area)

mat <- nb2mat(nbs, style = "B", zero.policy = TRUE)
colnames(mat) <- rownames(mat) <- cleaned_data$struct
mat <- as.matrix(mat)

nb2INLA("cl_graph", nbs)
am_adj <- paste0(getwd(), "/cl_graph")
H <- inla.read.graph(filename = "cl_graph")

print(H)
#############
cleaned_data$offset_log <- log(2 * cleaned_data$acspop)
cleaned_data$struct<-1:dim(cleaned_data)[1]
formulaCAR <- var_sui_N ~ f(struct, model="bym", scale.model = T, constr = T, graph=H)
resultCAR <- inla(formulaCAR,family="poisson", # run inla
  data=cleaned_data, control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),E=(2*acspop))
summary(resultCAR)

summary(resultCAR)
exp(resultCAR$summary.fixed)
(exp(resultCAR$summary.fixed)*100000) #7.75 per 10,000 population

# add social factors
formulaCOV1 <- var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
  pctOwnerOccupiedHousing + medianHouseValue + unemprate + pctpov + pctpoc + lingisolat
resultCOV1 <- inla(formulaCOV1,family="poisson", # run inla
                  data=cleaned_data, control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),E=(2*acspop))
summary(resultCOV1)
exp(resultCOV1$summary.fixed)

formulaCOV2 <- var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
  heatanamol + temperatur + healthburd +  pctOwnerOccupiedHousing + medianHouseValue + unemprate + pctpov + pctpoc + lingisolat
resultCOV2 <- inla(formulaCOV2,family="poisson", # run inla
                   data=cleaned_data, control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),E=(2*acspop))
summary(resultCOV2)
exp(resultCOV2$summary.fixed)

formulaCOV3 <- var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
  tes + parkpriori + treepriori + popperacre + ttjobs60 + tt3rdgroc + tt3rdhosp + tt3rdpharm + tt3rdurgen + heatanamol + temperatur + healthburd +  pctOwnerOccupiedHousing + medianHouseValue + unemprate + pctpov + pctpoc + lingisolat
resultCOV3 <- inla(formulaCOV3,family="poisson", # run inla
                   data=cleaned_data, control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),E=(2*acspop))
summary(resultCOV3)
exp(resultCOV3$summary.fixed)

formulaCOV4 <- var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
  offset(scale(2*acspop)) +
  LUS_Open.S + LCdiversity +  LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori +
  popperacre + ttjobs60 + tt3rdpharm + tt3rdurgen +  unemprate + lingisolat +pctOwnerOccupiedHousing + medianHouseValue +
  temperatur + healthburd

formulaCOV4 <- var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
  offset(scale(2*acspop)) +
  LUS_Open.S + LCdiversity +  LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori +
  popperacre + ttjobs60 + tt3rdpharm + tt3rdurgen +  unemprate + lingisolat +pctOwnerOccupiedHousing + medianHouseValue +
  temperatur + healthburd

resultCOV4 <- inla(formula = formulaCOV4,
             data = cleaned_data,
             family = "poisson",
             control.compute = list(waic = TRUE, dic = TRUE,cpo=TRUE, return.marginals.predictor = TRUE),
             control.predictor = list(link = 1, compute = TRUE),
             num.threads = 3,
             verbose = FALSE)


summary(resultCOV4)
exp(resultCOV4$summary.fixed)

resultCOV4$summary.fitted.values
resultCOV4$marginals.fitted.values


formulaCOV5 <- var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
  outside1.m + hwyTF + acresc30 + LUS_Open.S + LCdiversity + Treecanopy + LUS_WaterY + tes +
  parkpriori + treepriori + popperacre + ttjobs60 + tt3rdgroc + tt3rdhosp + tt3rdpharm +
  tt3rdurgen + heatanamol + temperatur + healthburd +  pctOwnerOccupiedHousing + medianHouseValue + unemprate + pctpov + pctpoc + lingisolat
resultCOV5 <- inla(formulaCOV5,family="poisson", # run inla
                   data=cleaned_data, control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),E=(2*acspop))
summary(resultCOV5)
exp(resultCOV5$summary.fixed)

##################################################################################################
mod3_summary <- summary(resultCOV4)

str(mod3_summary)

fixed_effects_summary <- data.frame(mod3_summary$fixed)

str(fixed_effects_summary)

model_summary_df <- data.frame(
  Parameter = rownames(fixed_effects_summary),
  Mean = fixed_effects_summary$mean,
  `Lower 95% CI` = fixed_effects_summary$X0.025quant,
  `Upper 95% CI` = fixed_effects_summary$X0.975quant,
  `Std. Dev.` = fixed_effects_summary$sd
)

head(model_summary_df)

model_summary_flextable <- flextable(model_summary_df)

model_summary_flextable

model_summary_flextable <- flextable(model_summary_df) %>%
  bold(j = "Parameter") %>%   # Bold only the 'Parameter' column
  set_table_properties(width = 1, layout = "autofit") %>%
  align(align = "center", part = "all")  # Center-align all text

model_summary_flextable
library(officer)
doc <- read_docx() %>%
  body_add_flextable(model_summary_flextable)

file_path <- "../output/model_summary_table_SH.docx"
print(doc, target = file_path)

file_path

resultCOV4$summary.hyperpar
hyperpar_summary <- resultCOV4$summary.hyperpar
sigma_S <- hyperpar_summary["sigma_S", "mean"]  # Adjust the name if it differs
sigma_e <- hyperpar_summary["sigma_e", "mean"]   # Adjust the name if it differs
gamma <- hyperpar_summary["gamma", "mean"]

1/resultCOV4$summary.hyperpar$mean
cleaned_data$bym_est<-resultCOV4$summary.fitted.values$mean
cleaned_data_sp <- as(cleaned_data, "Spatial")

m3a <- inla.tmarginal(
  function(x) (1/x),  # Transformation function: inverse of precision to get variance
  resultCOV4$marginals.hyperpar$`Precision for ID.area (iid component)`  # Precision values for iid component
)

m3b <- inla.tmarginal(
  function(x) (1/x),  # Same transformation to variance
  resultCOV4$marginals.hyperpar$`Precision for ID.area (spatial component)`# Precision values for spatial component
)
mean(m3a)
mean(m3b)

(proportion_spatial <- mean(m3a) / mean(m3b))
# Define thresholds
thresholds <- c(1, 1.25, 1.5)

for (a in thresholds) {
  cleaned_data_sp[[paste0("inlaprob_", a)]] <- unlist(lapply(
    resultCOV4$marginals.fitted.values,
    function(X) 1 - inla.pmarginal(a, X)
  ))
}

cleaned_data_sf <- cleaned_data_sp %>% st_as_sf()

plot_exceedance <- function(data, threshold, colors, label) {
  data <- data %>%
    mutate(
      exceed_cut = cut(
        !!sym(paste0("inlaprob_", threshold)),
        breaks = c(0, 0.2, 0.5, 0.8, 0.95, 1),
        include.lowest = TRUE
      )
    )

  ggplot(data) +
    geom_sf(aes(fill = exceed_cut), color = "grey80", size = 0.1) + # Add borders
    scale_fill_manual(
      values = colors,
      name = "Exceedance Probability"
    ) +
    labs(
      title = paste("Exceedance Probability: Pr(θ >", threshold, ")")
    ) +
    theme_void() + # Removes axes
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    ) +
    annotate("text", x = -Inf, y = Inf, label = label, hjust = -0.5, vjust = 1.5, size = 5, fontface = "bold")
}

colors <- brewer.pal(n = 6, name = "Blues")

plot_a <- plot_exceedance(cleaned_data_sf, 1, colors, "A")
plot_b <- plot_exceedance(cleaned_data_sf, 1.25, colors, "B")
plot_c <- plot_exceedance(cleaned_data_sf, 1.5, colors, "C")

combined_plot <- (plot_a + plot_b + plot_c) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot

high_risk_blocks <- cleaned_data_sf %>%
  filter(inlaprob_1 > 0.8)  # Adjust condition based on the desired threshold

# Print GEOIDs of high-risk blocks
high_risk_geoids <- high_risk_blocks$GEOID

cleaned_data_sf$risk <- ifelse(cleaned_data_sf$inlaprob_1 > 0.8, "high risk", "not high risk")
cleaned_data_sf$risk <- as.factor(cleaned_data_sf$risk)
###############################################################

variables_to_unscale <- c(
  "LCdiversity", "Treecanopy", "acresc30", "tes", "parkpriori", "treepriori",
  "popperacre", "ttjobs60", "tt3rdgroc", "tt3rdhosp", "tt3rdpharm", "pctpoc", "pctpov" ,
  "tt3rdurgen", "unemprate", "lingisolat", "pctOwnerOccupiedHousing",
  "medianHouseValue", "heatanamol", "temperatur", "healthburd","LUS_Open.S"
)

unscaled_data <- cleaned_data_sf

for (var in variables_to_unscale) {
  # Check if the variable exists in the data frame
  if (!var %in% names(cleaned_data_sf)) {
    warning(paste("Variable", var, "not found in the data frame. Skipping."))
    next
  }

  var_mean <- attr(cleaned_data_sf[[var]], "scaled:center")
  var_sd <- attr(cleaned_data_sf[[var]], "scaled:scale")

  if (!is.null(var_mean) && !is.null(var_sd)) {
    # Reverse scaling
    unscaled_data[[var]] <- (cleaned_data_sf[[var]] * var_sd) + var_mean
  } else {
    warning(paste("No scaling information found for variable:", var))
  }
}

head(unscaled_data)

################################
library(gtsummary)
library(flextable)

table2 <- unscaled_data %>%
  st_drop_geometry() %>%
  tbl_summary(
    by = risk,
    include = c(
      LUS_OSYN,
      hwyTF,
      LUS_Open.S,
      LCdiversity,
      acresc30,
      tes,
      Treecanopy,
      parkpriori,
      unemprate,
      heatanamol,
      outside1.m,
      treepriori,
      popperacre,
      lingisolat,
      temperatur,
      pctpoc,
      pctpov,
      healthburd,
      medianHouseValue,
      var_sui_N,
      pctOwnerOccupiedHousing,
      ttjobs60, tt3rdgroc, tt3rdhosp, tt3rdpharm,tt3rdurgen

    ),
    missing = "no",
    label = list(
      LUS_OSYN ~ "Open Space",
      hwyTF ~ "Transportation Access",
      LUS_Open.S ~ "NCLD Land Cover - Open Space",
      LCdiversity ~ "NCLD Land Cover Diversity",
      acresc30 ~ "Park Acreage Access w/in 15 min",
      Treecanopy ~ "NCLD Tree Canopy Cover (%)",
      tes ~ "Tree Equity Score",
      parkpriori ~ "Park Priority Index",
      unemprate ~ "Unemployment Rate",
      heatanamol ~ "Heat Anomaly",
      outside1.m ~ "Outside 15 min Walk from Park",
      treepriori ~ "Tree Priority Index",
      popperacre ~ "Population per Acre",
      lingisolat ~ "Linguistic Isolation",
      temperatur ~ "Temperature",
      pctpoc ~ "Percentage of Population of Color",
      pctpov ~ "Percentage in Poverty",
      healthburd ~ "Health Burden",
      medianHouseValue ~ "Median Home Value",
      var_sui_N ~ "Suicide Count",
      pctOwnerOccupiedHousing ~ "Owner Occupied Housing",
      ttjobs60 ~ "Job Accessibility 60 mins",
      tt3rdgroc~ "Travel Time (Grocery)",
      tt3rdhosp ~ "Travel Time (Hosp)",
      tt3rdpharm~ "Travel Time (Pharmacy)",
      tt3rdurgen~ "Travel Time (Urgent Care)"
    ),
    type = list(
      LUS_OSYN ~ "categorical",
      hwyTF ~ "categorical",
      lingisolat ~"continuous",
      LUS_Open.S ~ "continuous",
      LCdiversity ~ "continuous",
      acresc30 ~ "continuous",
      tes ~ "continuous",
      Treecanopy ~ "continuous",
      parkpriori ~ "continuous",
      unemprate ~ "continuous",
      heatanamol ~ "continuous",
      outside1.m ~ "categorical",
      treepriori ~ "continuous",
      popperacre ~ "continuous",
      temperatur ~ "continuous",
      pctpoc ~ "continuous",
      pctpov ~ "continuous",
      healthburd ~ "continuous",
      medianHouseValue ~  "continuous",
      var_sui_N ~  "continuous",
      pctOwnerOccupiedHousing ~  "continuous",
      ttjobs60 ~  "continuous",
      tt3rdgroc~ "continuous",
      tt3rdhosp ~ "continuous",
      tt3rdpharm~ "continuous",
      tt3rdurgen~ "continuous"

    )
  ) %>%
  add_n() %>% # Add column with total number of non-missing observations
  modify_header(list(label = "**Variable**")) %>% # Update the column header
  bold_labels()  %>%
  add_p()

# Convert to flextable and save as DOCX
table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "../output/tabletwo.docx")

library(brinla)
bri.fixed.plot(resultCOV4)
bri.hyperpar.summary(resultCOV4)
spatial_effects <- resultCOV4$summary.random$ID.area

# Extracting model parameters

sigma_S <- 1 / sqrt(resultCOV4$summary.hyper[1, "mean"] )
par(mfrow = c(2, 4))  # display a unique layout for all graphs
## Posterior density plot for the intercepts
plot(resultCOV4$marginals.fixed[[2]], type = "l", main = "% Open Space in CBG", ylab = "", xlab = expression(beta[1]))
plot(resultCOV4$marginals.fixed[[3]], type = "l", main = "Land Cover Diversity", ylab = "", xlab = expression(beta[2]))
plot(resultCOV4$marginals.fixed[[4]], type = "l", main = "Water in CBG", ylab = "", xlab = expression(beta[3]))
plot(resultCOV4$marginals.fixed[[8]], type = "l", main = "Tree Equity Index", ylab = "", xlab = expression(beta[7]))
plot(resultCOV4$marginals.fixed[[9]], type = "l", main = "Park Priority Index", ylab = "", xlab = expression(beta[8]))
plot(resultCOV4$marginals.fixed[[10]], type = "l", main = "Tree Priority Index", ylab = "", xlab = expression(beta[9]))
plot(resultCOV4$marginals.fixed[[18]], type = "l", main = "Median Home Value", ylab = "", xlab = expression(beta[17]))
plot(resultCOV4$marginals.fixed[[20]], type = "l", main = "Health Burden", ylab = "", xlab = expression(beta[19]))
par(mfrow = c(1, 1))
###############################################

# Add cluster_type from gap_df into unscaled_data
unscaled_data <- unscaled_data %>%
  left_join(
    gap_df %>%
      st_drop_geometry() %>%
      dplyr::select(GEOID, cluster_type),
    by = "GEOID"
  )

point_data <- st_transform(suicides_sf, st_crs(unscaled_data))
joined_data <- st_join(point_data, unscaled_data, join = st_within)
# View the first few rows of the joined data
head(joined_data)

# Select the continuous variables
continuous_vars <- c(
  "LUS_Open.S", "LUS_WaterY", "LUS_OSYN", "hwyTF", "outside1.m", "LCdiversity",
  "acresc30", "tes", "Treecanopy", "parkpriori", "unemprate", "heatanamol",
  "treepriori", "popperacre", "temperatur", "pctpoc", "pctpov", "lingisolat",
  "healthburd", "ttjobs60", "tt3rdgroc", "tt3rdhosp", "tt3rdpharm", "tt3rdurgen",
  "var_sui_N", "acspop", "pctOwnerOccupiedHousing", "medianHouseValue",
  "bym_est", "inlaprob_1", "inlaprob_1.25", "inlaprob_1.5"
)
joined_data <- st_join(joined_data_complete, unscaled_data, join = st_within)
# Summarize continuous variables
summary(joined_data[, continuous_vars])

library(dplyr)

# Select CBGs in unscaled_data that are NOT in joined_data
unscaled_data_filtered <- unscaled_data %>%
  filter(!GEOID %in% joined_data$GEOID)

table2 <- unscaled_data_filtered %>%
  st_drop_geometry() %>%
  tbl_summary(
     include = c(
      LUS_OSYN,
      hwyTF,
      LUS_Open.S,
      LCdiversity,
      acresc30,
      tes,
      Treecanopy,
      parkpriori,
      unemprate,
      heatanamol,
      outside1.m,
      treepriori,
      popperacre,
      lingisolat,
      temperatur,
      pctpoc,
      pctpov,
      healthburd,
      medianHouseValue,
      var_sui_N,
      pctOwnerOccupiedHousing,
      ttjobs60, tt3rdgroc, tt3rdhosp, tt3rdpharm,tt3rdurgen

    ),
    missing = "no",
    label = list(
      LUS_OSYN ~ "Open Space",
      hwyTF ~ "Transportation Access",
      LUS_Open.S ~ "NCLD Land Cover - Open Space",
      LCdiversity ~ "NCLD Land Cover Diversity",
      acresc30 ~ "Park Acreage Access w/in 15 min",
      Treecanopy ~ "NCLD Tree Canopy Cover (%)",
      tes ~ "Tree Equity Score",
      parkpriori ~ "Park Priority Index",
      unemprate ~ "Unemployment Rate",
      heatanamol ~ "Heat Anomaly",
      outside1.m ~ "Outside 15 min Walk from Park",
      treepriori ~ "Tree Priority Index",
      popperacre ~ "Population per Acre",
      lingisolat ~ "Linguistic Isolation",
      temperatur ~ "Temperature",
      pctpoc ~ "Percentage of Population of Color",
      pctpov ~ "Percentage in Poverty",
      healthburd ~ "Health Burden",
      medianHouseValue ~ "Median Home Value",
      var_sui_N ~ "Suicide Count",
      pctOwnerOccupiedHousing ~ "Owner Occupied Housing",
      ttjobs60 ~ "Job Accessibility 60 mins",
      tt3rdgroc~ "Travel Time (Grocery)",
      tt3rdhosp ~ "Travel Time (Hosp)",
      tt3rdpharm~ "Travel Time (Pharmacy)",
      tt3rdurgen~ "Travel Time (Urgent Care)"
    ),
    type = list(
      LUS_OSYN ~ "categorical",
      hwyTF ~ "categorical",
      lingisolat ~"continuous",
      LUS_Open.S ~ "continuous",
      LCdiversity ~ "continuous",
      acresc30 ~ "continuous",
      tes ~ "continuous",
      Treecanopy ~ "continuous",
      parkpriori ~ "continuous",
      unemprate ~ "continuous",
      heatanamol ~ "continuous",
      outside1.m ~ "categorical",
      treepriori ~ "continuous",
      popperacre ~ "continuous",
      temperatur ~ "continuous",
      pctpoc ~ "continuous",
      pctpov ~ "continuous",
      healthburd ~ "continuous",
      medianHouseValue ~  "continuous",
      var_sui_N ~  "continuous",
      pctOwnerOccupiedHousing ~  "continuous",
      ttjobs60 ~  "continuous",
      tt3rdgroc~ "continuous",
      tt3rdhosp ~ "continuous",
      tt3rdpharm~ "continuous",
      tt3rdurgen~ "continuous"

    )
  ) %>%
  add_n() %>% # Add column with total number of non-missing observations
  modify_header(list(label = "**Variable**")) %>% # Update the column header
  bold_labels()

# Convert to flextable and save as DOCX
table2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "../output/tablethree1.docx")
####################################
library(CARBayes)

nb <- poly2nb(cleaned_data_sp)  # Generate neighbors
W <- nb2mat(nb, style = "B", zero.policy = TRUE)  # Create adjacency matrix

coords <- coordinates(cleaned_data_sp)

distances <- spDistsN1(coords, coords[3064, ], longlat = FALSE)
nearest <- which.min(distances[-3064])  # Exclude itself

W[3064, nearest] <- 1
W[nearest, 3064] <- 1  # Ensure symmetry

formulaCOV4 <- var_sui_N ~ offset(scale(2 * acspop)) +
  LUS_Open.S + LCdiversity +  LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori +
  popperacre + ttjobs60 + tt3rdpharm + tt3rdurgen +  unemprate + lingisolat +pctOwnerOccupiedHousing + medianHouseValue +
  temperatur + healthburd

resultCOV4 <- S.CARbym(
  formula = formulaCOV4,
  data = cleaned_data_sp,
  family = "poisson",
  W = W,  # Spatial adjacency matrix
  burnin = 1000,  # Number of burn-in iterations
  n.sample = 10000,  # Total number of MCMC samples
  thin = 10  # Thinning rate
)
print(resultCOV4)


(table1 <-
    gap_df %>%
    st_drop_geometry() %>%
    tbl_summary(
      include = c(
        LUS_Open.S,
        LUS_WaterY,
        LUS_OSYN,
        hwyTF,
        outside1.m,
        LCdiversity,
        acresc30,
        tes,
        Treecanopy,
        parkpriori,
        unemprate,
        heatanamol,
        treepriori,
        popperacre,
        temperatur,
        pctpoc,
        pctpov,
        lingisolat,
        healthburd,
        tt3rdgroc,
        tt3rdhosp,
        tt3rdpharm,
        tt3rdurgen,
        medianHouseValue,outside1.m, ttjobs60
      ),
      missing = "no",
      label = list(LUS_Open.S ~ "NCLD Land Cover - Open Space",
                   acresc30 ~ "Park Acerage Access w/in 30 min",
                   Treecanopy ~ "NCLD Tree Canopy Cover (%)")) %>%
    add_n() %>% # add column with total number of non-missing observations
    modify_header(label = "**Variable**") %>% # update the column header
    bold_labels() ) %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "../output/tableone.docx")

############################################################# Revisions
# POC-based interactions
cleaned_data$LUS_WaterY <- as.numeric(cleaned_data$LUS_WaterY == "T")

cleaned_data$int_opendiv       <- cleaned_data$LUS_Open.S    * cleaned_data$LCdiversity
cleaned_data$int_tes_div        <- cleaned_data$tes    * cleaned_data$LCdiversity
cleaned_data$int_treediv   <- cleaned_data$Treecanopy   * cleaned_data$LCdiversity
cleaned_data$int_park_div        <- cleaned_data$parkpriori    * cleaned_data$LCdiversity
cleaned_data$int_treepriori_div  <- cleaned_data$treepriori    * cleaned_data$LCdiversity
cleaned_data$int_blue_div        <- cleaned_data$LUS_WaterY    * cleaned_data$LCdiversity
cleaned_data$int_medhv_div  <- cleaned_data$medianHouseValue    * cleaned_data$LCdiversity
cleaned_data$int_health_div  <- cleaned_data$healthburd    * cleaned_data$LCdiversity
cleaned_data$int_poc_div  <- cleaned_data$pctpoc    * cleaned_data$LCdiversity
cleaned_data$int_pov_div  <- cleaned_data$pctpov    * cleaned_data$LCdiversity

interaction_terms <- c(
  "int_opendiv", "int_tes_div", "int_treediv", "int_park_div", "int_treepriori_div", 
  "int_blue_div", "int_medhv_div", "int_health_div", "int_poc_div", "int_pov_div"
)
interaction_terms <- c(
  "int_opendiv"
)

base_formula <- "var_sui_N ~ f(ID.area, model='bym', scale.model=TRUE, constr=TRUE, graph=H) + 
  LUS_Open.S + LCdiversity + LUS_WaterY + tes +
  parkpriori + treepriori + healthburd + 
  medianHouseValue + pctpov + pctpoc"

model_list <- list()

for (term in interaction_terms) {
  full_formula <- as.formula(paste(base_formula, "+", term))
  model_list[[term]] <- inla(
    formula = full_formula,
    family = "poisson",
    data = cleaned_data,
    E = 2 * cleaned_data$acspop,
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    control.fixed = list(prec = 0.01)
  )
}

extract_interaction <- function(model, term) {
  coef <- model$summary.fixed[term, ]
  data.frame(
    Interaction = term,
    Mean = round(coef["mean"], 3),
    `95% CrI Lower` = round(coef["0.025quant"], 3),
    `95% CrI Upper` = round(coef["0.975quant"], 3),
    DIC = round(model$dic$dic, 2),
    WAIC = round(model$waic$waic, 2),
    Significant = ifelse(coef["0.025quant"] > 0 | coef["0.975quant"] < 0, "Yes", "No")
  )
}

results_list <- lapply(names(model_list), function(term) {
  extract_interaction(model_list[[term]], term)
  
})

interaction_results_df <- bind_rows(results_list)

# Export as a Word table
interaction_table <- flextable(interaction_results_df) %>%
  bold(part = "header") %>%
  autofit() %>%
  set_table_properties(width = 1, layout = "autofit") %>%
  align(align = "center", part = "all")

doc <- read_docx() %>%
  body_add_par("Table: Interaction Effects from INLA Models", style = "heading 1") %>%
  body_add_flextable(interaction_table)

print(doc, target = "D:/projects/chi_suicide_greenspace/output/inla_interaction_results.docx")



# Define the formula with the interaction term
base_formula <- "var_sui_N ~ f(ID.area, model='bym', scale.model=TRUE, constr=TRUE, graph=H) + 
                 LUS_Open.S + LCdiversity + LUS_WaterY + tes +
                 parkpriori + treepriori + healthburd + 
                 medianHouseValue + pctpov + pctpoc + int_opendiv"

# Fit the model with the interaction term
model <- inla(
  formula = var_sui_N~ f(ID.area, model="bym", scale.model = T, constr = T, graph=H) +
    offset(scale(2*acspop)) +
    LUS_Open.S + LCdiversity +  LUS_WaterY +  outside1.m + hwyTF + acresc30 +tes + parkpriori + treepriori +
    popperacre + ttjobs60 + tt3rdpharm + tt3rdurgen +  unemprate + lingisolat +pctOwnerOccupiedHousing + medianHouseValue +
    temperatur + healthburd ,
  family = "poisson",
  data = cleaned_data,
  E = 2 * cleaned_data$acspop,  # Expected counts (population)
  control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
  control.fixed = list(prec = 0.01)
)

summary(model)

interaction_coef <- model$summary.fixed["int_opendiv", ]
interaction_coef

predicted_sui_rate <- model$summary.fitted.values[, "mean"]

prediction_data <- cleaned_data
prediction_data$predicted_sui_rate <- predicted_sui_rate

ggplot(prediction_data, aes(x = LCdiversity, y = predicted_sui_rate, color = int_opendiv)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction between Land Cover Diversity and Open Space",
    x = "Land Cover Diversity",
    y = "Predicted Suicide Rate",
    color = "Interaction Term (int_opendiv)"
  ) +
  theme_minimal()

######################################
library(gtsummary)
library(dplyr)
library(gtsummary)

joined_data <- joined_data %>%
  left_join(
    gap_df %>% st_drop_geometry() %>% 
      dplyr::select(GEOID, local_moran_I, local_moran_p, cluster_type),
    by = "GEOID"
  )


joined_data <- joined_data %>%
  left_join(
    cleaned_data_sf %>%
      st_drop_geometry(),
    by = "GEOID"
  )

library(dplyr)
library(gtsummary)
library(flextable)

joined_data %>%
  st_drop_geometry() %>%
  #mutate(
   # inlaprob_1 = coalesce(inlaprob_1.25.x, inlaprob_1.25.y),
    #risk_group = case_when(
      #inlaprob_1 > 0.8 & local_moran_I > 0 & local_moran_p < 0.05 ~ "Both",
     # inlaprob_1 > 0.8 ~ "Exceedance only",
     # local_moran_I > 0 & local_moran_p < 0.05 ~ "Moran only",
     # TRUE ~ "Neither"
   # )
 # ) %>%
  dplyr::select(
    cluster_type, Gender, Race_Latino, Age, Cause_Category, Same_Location, hwyTF.x,
    LUS_Open.S.x, LCdiversity.x, acresc30.x, tes.x, Treecanopy.x, parkpriori.x, LUS_WaterY.x,LUS_OSYN.x,
    unemprate.x, heatanamol.x, treepriori.x, popperacre.x, temperatur.x,
    pctpoc.x, pctpov.x, lingisolat.x, healthburd.x, ttjobs60.x, tt3rdgroc.x,
    tt3rdhosp.x, tt3rdpharm.x, tt3rdurgen.x, pctOwnerOccupiedHousing.x,
    medianHouseValue.x
  ) %>%
  tbl_summary(
    by = cluster_type,
    type = list(
      Gender ~ "categorical",
      Race_Latino ~ "categorical",
      Age ~ "continuous",
      Cause_Category ~ "categorical",
      Same_Location ~ "categorical",
      where(is.numeric) ~ "continuous"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      Race_Latino ~ "fisher.test",
      Cause_Category ~ "fisher.test"
    ),
    test.args = list(
      Race_Latino = list(simulate.p.value = TRUE),
      Cause_Category = list(simulate.p.value = TRUE)
    )
  ) %>%
  as_flex_table()

joined_data %>%
  st_drop_geometry() %>%
  mutate(
      inlaprob_1 = coalesce(inlaprob_1.25.x, inlaprob_1.25.y),
      risk_group = case_when(
      local_moran_I > 0 & local_moran_p < 0.05 ~ "Moran High-Risk Clusters", 
      inlaprob_1 > 0.8 ~ "At Least One Suicide", 
      TRUE ~ "No Suicides"
    )
  ) %>%
  dplyr::select(
    risk_group, Gender, Race_Latino, Age, Cause_Category, Same_Location,
    LUS_Open.S.x, LCdiversity.x, acresc30.x, tes.x, Treecanopy.x, parkpriori.x,
    unemprate.x, heatanamol.x, treepriori.x, popperacre.x, temperatur.x,
    pctpoc.x, pctpov.x, lingisolat.x, healthburd.x, ttjobs60.x, tt3rdgroc.x,
    tt3rdhosp.x, tt3rdpharm.x, tt3rdurgen.x, pctOwnerOccupiedHousing.x,
    medianHouseValue.x
  ) %>%
  tbl_summary(
    by = risk_group,
    type = list(
      Gender ~ "categorical",
      Race_Latino ~ "categorical",
      Age ~ "continuous",
      Cause_Category ~ "categorical",
      Same_Location ~ "categorical",
      where(is.numeric) ~ "continuous"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      Race_Latino ~ "fisher.test",
      Cause_Category ~ "fisher.test"
    ),
    test.args = list(
      Race_Latino = list(simulate.p.value = TRUE),
      Cause_Category = list(simulate.p.value = TRUE)
    )
  ) %>%
  as_flex_table()

tract_comparison_tbl <- joined_data %>%
  st_drop_geometry() %>%
  mutate(
    # Combine risk probabilities
    inlaprob_1 = coalesce(inlaprob_1.5.x, inlaprob_1.5.y),
    
    # Define risk groups excluding Moran's I areas
    risk_group = case_when(
      # Only include risk areas where inlaprob_1 > 0.8 but exclude Moran's I clusters
      inlaprob_1 < 0.2 & !(local_moran_I > 0 & local_moran_p < 0.05) ~ "Low RR", 
      inlaprob_1 < 0.8 & !(local_moran_I > 0 & local_moran_p < 0.05) ~ "High RR", 
      local_moran_I > 0 & local_moran_p < 0.05 ~ "Moran High-Risk Clusters", 
      TRUE ~ NA
    )
  ) %>%
  dplyr::select(
    risk_group, Gender, Race_Latino, Age, Cause_Category, Same_Location,
    LUS_Open.S.x, LCdiversity.x, acresc30.x, tes.x, Treecanopy.x, parkpriori.x,
    unemprate.x, heatanamol.x, treepriori.x, popperacre.x, temperatur.x,
    pctpoc.x, pctpov.x, lingisolat.x, healthburd.x, ttjobs60.x, tt3rdgroc.x,
    tt3rdhosp.x, tt3rdpharm.x, tt3rdurgen.x, pctOwnerOccupiedHousing.x, LUS_OSYN.x,
    medianHouseValue.x, LUS_WaterY.x
  ) %>%
  tbl_summary(
    by = risk_group,
    type = list(
      Gender ~ "categorical",
      Race_Latino ~ "categorical",
      Age ~ "continuous", # Now just reporting mean for Age
      Cause_Category ~ "categorical",
      Same_Location ~ "categorical",
      where(is.numeric) ~ "continuous" # Apply this to all numeric variables
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      Race_Latino ~ "fisher.test",
      Cause_Category ~ "fisher.test"
    ),
    test.args = list(
      Race_Latino = list(simulate.p.value = TRUE),
      Cause_Category = list(simulate.p.value = TRUE)
    )
  ) %>%
  as_flex_table()

cleaned_data_sf <- cleaned_data_sf %>%
  left_join(
    gap_df %>%
      st_drop_geometry() %>%
      dplyr::select(GEOID, local_moran_I, local_moran_p),
    by = "GEOID"
  )

cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    tract_risk_group = case_when(
      inlaprob_1.25 > 0.8 ~ "High Risk (Exceedance)",
      var_sui_N > 0 ~ "Observed Suicide(s)",
      var_sui_N == 0 ~ "No Suicides"
    )
  )

cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    tract_risk_group = case_when(
      inlaprob_1.25 > 0.8 ~ "High Risk (Exceedance)",
      inlaprob_1.25 <= 0.8 & var_sui_N > 0 ~ "Observed Suicide(s)",
      var_sui_N == 0 ~ "No Suicides"
    )
  )



cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    tract_risk_group = case_when(
      local_moran_I > 0 & local_moran_p < 0.05 ~ "Clustered",
      inlaprob_1.25 > 0.8 ~ "High Risk Only",
      var_sui_N > 0 ~ "Observed Suicide(s) Only",
      var_sui_N == 0 ~ "No Suicide"
    )
  )

# Create the grouping variable
cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    tract_risk_group = case_when(
      (local_moran_I > 0 & local_moran_p < 0.05) | inlaprob_1.25 > 0.8 ~ "High Risk/Cluster",
      var_sui_N == 0 ~ "No Suicide",
      TRUE ~ "Other"
    )
  ) %>%
  filter(tract_risk_group != "Other")

cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    highrisk_cluster = case_when(
      (local_moran_I > 0 & local_moran_p < 0.05) | inlaprob_1.25 > 0.8 ~ "High Risk/Cluster",
      TRUE ~ "Other"
    )
  )

cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    tract_risk_group = case_when(
      (local_moran_I > 0 & local_moran_p < 0.05) | inlaprob_1.25 > 0.8 ~ "High Risk/Cluster",
      var_sui_N > 0 ~ "Observed Suicide(s) Only",
      var_sui_N == 0 ~ "No Suicide"
    )
  )

cleaned_data_sf <- cleaned_data_sf %>%
  mutate(
    high_risk_cluster = (local_moran_I > 0 & local_moran_p < 0.10) | (inlaprob_1.5 > 0.8),
    tract_risk_group = case_when(
      high_risk_cluster ~ "High Risk/Cluster",
      var_sui_N > 0 & !high_risk_cluster ~ "Observed Suicide(s) Only",
      var_sui_N == 0 ~ "No Suicide"
    )
  )

# Drop geometry for analysis
tract_comparison_tbl <- cleaned_data_sf %>%
  st_drop_geometry() %>%
  dplyr::select(
    tract_risk_group,
    LUS_Open.S, LUS_OSYN, LUS_WaterY, LCdiversity, acresc30, tes, Treecanopy, parkpriori, 
    unemprate, heatanamol, treepriori, popperacre, temperatur,
    pctpoc, pctpov, lingisolat, healthburd, ttjobs60, tt3rdgroc,
    tt3rdhosp, tt3rdpharm, tt3rdurgen, pctOwnerOccupiedHousing,
    medianHouseValue
  ) %>%
  tbl_summary(
    by = tract_risk_group,
    type = list(
      where(is.numeric) ~ "continuous"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing = "no"
  ) %>%
  add_p(test = all_continuous() ~ "aov") 


# Save the table
flextable::save_as_docx(tract_comparison_tbl, path = "../output/ind_comparison_with_highrisk.docx")
######################################################################
library(dplyr)
library(ggplot2)
library(gtsummary)
library(forcats)

# Make sure year and gender are properly formatted
suicides_sf <- suicides_sf %>%
  filter(!is.na(Gender), !is.na(year)) %>%
  mutate(
    Gender = str_to_title(Gender),
    year = as.factor(year)
  )

# 1. Plot suicide deaths by gender over time
ggplot(suicides_sf, aes(x = year, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Suicide Deaths by Gender and Year",
    x = "Year",
    y = "Number of Deaths",
    fill = "Gender"
  ) +
  theme_minimal()

# 2. Table: Gender breakdown by year
gender_table <- suicides_sf %>% st_drop_geometry() %>%
  dplyr::select(Gender, year) %>%
  tbl_summary(
    by = year,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(test = everything() ~ "chisq.test") %>%
  bold_labels()

gender_table

# 3. Chi-square test across all years
gender_year_table <- table(suicides_sf$Gender, suicides_sf$year)
chisq.test(gender_year_table)

library(dplyr)
library(gtsummary)

# Drop geometry
suicides_env_nogeo <- joined_data %>%
  st_drop_geometry()

# Coerce all relevant environmental variables to numeric (if needed)
vars_to_check <- c(
  "LUS_Open.S", "LCdiversity", "acresc30", "tes", "Treecanopy", "parkpriori",
  "unemprate", "heatanamol", "treepriori", "popperacre", "temperatur",
  "pctpoc", "pctpov", "lingisolat", "healthburd", "ttjobs60", "tt3rdgroc",
  "tt3rdhosp", "tt3rdpharm", "tt3rdurgen", "pctOwnerOccupiedHousing",
  "medianHouseValue"
)
vars_to_check <- c(
  "LUS_Open.S.x", "LUS_WaterY.x", "LUS_OSYN.x", "hwyTF.x", "outside1.m.x", 
  "LCdiversity.x", "acresc30.x", "tes.x", "Treecanopy.x", "parkpriori.x", 
  "unemprate.x", "heatanamol.x", "treepriori.x", "popperacre.x", "temperatur.x", 
  "pctpoc.x", "pctpov.x", "lingisolat.x", "healthburd.x", "ttjobs60.x", 
  "tt3rdgroc.x", "tt3rdhosp.x", "tt3rdpharm.x", "tt3rdurgen.x", 
  "var_sui_N.x", "acspop.x", "pctOwnerOccupiedHousing.x", "medianHouseValue.x", 
  "ID.area.x", "UH.x", "offset_log.x", "struct.x", "bym_est.x", 
  "inlaprob_1.x", "inlaprob_1.25.x", "inlaprob_1.5.x", "risk.x"
)

# Ensure all are numeric
suicides_env_nogeo <- suicides_env_nogeo %>%
  mutate(across(all_of(vars_to_check), as.numeric))

# Create the summary table by Gender
#env_gender_summary <- suicides_env_nogeo %>%
#  filter(Gender %in% c("Male", "Female")) %>%
#  tbl_summary(
#    by = Gender,
#    include = all_of(vars_to_check),
#    type = list(all_continuous() ~ "continuous"),
#    statistic = list(all_continuous() ~ "{mean} ({sd})"),
#    missing = "no"
#  ) %>%
#  add_p() %>%
#  as_flex_table()

# Save the table
#flextable::save_as_docx(env_gender_summary, path = "../output/gender_comparison_with_envs.docx")

############################
library(car)        # For VIF
library(corrplot)   # For correlation matrix


# Convert factor/character vars to numeric if needed
cleaned_data_vif <- cleaned_data_sf %>%
  mutate(
    LUS_WaterY = as.numeric(LUS_WaterY == "T"),
    LUS_OSYN = as.numeric(LUS_OSYN == "T")
  )

# List of variables
vars <- c(
  "LUS_Open.S", "LUS_WaterY", "LUS_OSYN", "hwyTF", "outside1.m", "var_sui_N",
  "LCdiversity", "acresc30", "tes", "Treecanopy", "parkpriori",
  "unemprate", "heatanamol", "treepriori", "popperacre", "temperatur",
  "lingisolat",  "ttjobs60",
  "tt3rdgroc", "tt3rdhosp", "tt3rdpharm", "tt3rdurgen",
  "pctOwnerOccupiedHousing", "medianHouseValue"
)

# Remove geometry and subset
multicol_data <- cleaned_data_vif %>%
  st_drop_geometry() %>%
  dplyr::select(all_of(vars)) %>%
  na.omit()
# Spearman correlation
spearman_cor <- cor(multicol_data, method = "spearman")

# Visualize
corrplot(spearman_cor, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7)
# Fit a dummy model (you can use var_sui_N if you like)
vif_model <- lm(var_sui_N ~ ., data = multicol_data)  # Replace with your actual DV if desired

# Calculate VIF
vif_values <- vif(vif_model)

# View sorted VIFs
vif_values[order(vif_values, decreasing = TRUE)]
# Flag variables with high VIF (e.g., > 5 or 10)
high_vif <- vif_values[vif_values > 5]
print(high_vif)
################################
# Recover unscaled treepriori from attributes
treepriori_unscaled <- with(attributes(cleaned_data$treepriori), 
   cleaned_data$treepriori * `scaled:scale` + `scaled:center`)

# Create quantile-based bins on the unscaled version (avoids the stack error)


cleaned_data$pdp_bin <- Hmisc::cut2(treepriori_unscaled, g = 20)  # 20 bins with even count

# Create a summary for PDP
pdp_summary <- cleaned_data %>%
  mutate(treepriori_unscaled = treepriori_unscaled) %>%
  group_by(pdp_bin) %>%
  summarise(
    treepriori_mean = mean(treepriori_unscaled, na.rm = TRUE),
    pred_mean = mean(bym_est, na.rm = TRUE),
    .groups = "drop"
  )

# Plot partial dependence style curve
ggplot(pdp_summary, aes(x = treepriori_mean, y = pred_mean)) +
  geom_line(color = "darkblue") +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Partial Dependence-like Plot for Tree Priority Index",
    x = "Tree Priority Index (Unscaled)",
    y = "Mean Predicted Suicide Rate"
  ) +
  theme_minimal()

# Fit loess smoother to the PDP summary
loess_fit <- loess(pred_mean ~ treepriori_mean, data = pdp_summary, span = 0.75)

# Add smoothed values to the dataframe
pdp_summary$pred_mean_smooth <- predict(loess_fit)

# Plot both original and smoothed values
ggplot(pdp_summary, aes(x = treepriori_mean)) +
 # geom_point(aes(y = pred_mean), color = "grey60") +
  geom_line(aes(y = pred_mean_smooth), color = "darkblue", linewidth = 1.2) +
  labs(
    title = "Smoothed PDP for Tree Priority Index",
    x = "Tree Priority Index (Unscaled)",
    y = "Predicted Suicide Rate (Smoothed)"
  ) +
  theme_minimal()

# If treepriori was scaled with attributes:
scaled_mean <- attr(cleaned_data$treepriori, "scaled:center")
scaled_sd <- attr(cleaned_data$treepriori, "scaled:scale")

# Recreate unscaled version
cleaned_data$treepriori_unscaled <- cleaned_data$treepriori * scaled_sd + scaled_mean
summary(cleaned_data$treepriori_unscaled)

library(dplyr)
library(ggplot2)

# Drop rows with NA values
safe_data <- cleaned_data %>%
  dplyr::select(treepriori_unscaled, tes, bym_est) %>%
  filter(!is.na(treepriori_unscaled), !is.na(tes), !is.na(bym_est))

# Create quantiles for tes
safe_data <- safe_data %>%
  mutate(
    tes_q = ntile(tes, 4),
    pdp_bin = cut(treepriori_unscaled, breaks = 20)
  )

# Summarize for PDP
pdp_2d_facet <- safe_data %>%
  group_by(tes_q, pdp_bin) %>%
  summarise(
    treepriori_mean = mean(treepriori_unscaled, na.rm = TRUE),
    pred_mean = mean(bym_est, na.rm = TRUE),
    .groups = "drop"
  )

# Plot PDP by tes quantile
ggplot(pdp_2d_facet, aes(x = treepriori_mean, y = pred_mean)) +
  geom_line(color = "darkblue", linewidth = 1) +
  facet_wrap(~ tes_q, labeller = label_both) +
  labs(
    title = "PDP of Tree Priority by Tree Equity Quartiles",
    x = "Tree Priority Index (Unscaled)",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()

###################################
cleaned_data$treepriori_unscaled <- as.numeric(cleaned_data$treepriori_unscaled)
cleaned_data$tes <- as.numeric(cleaned_data$tes)
cleaned_data$bym_est <- as.numeric(cleaned_data$bym_est)

safe_data <- cleaned_data %>%
  filter(
    !is.na(treepriori_unscaled),
    !is.na(tes),
    !is.na(bym_est)
  ) %>%
  mutate(
    tes_q = ntile(tes, 4),  # Create quartiles for tes
    pdp_bin = cut(treepriori_unscaled, breaks = pretty(range(treepriori_unscaled), n = 20), include.lowest = TRUE)
  )

pdp_2d_facet <- safe_data %>%
  group_by(tes_q, pdp_bin) %>%
  summarise(
    treepriori_mean = mean(treepriori_unscaled, na.rm = TRUE),
    pred_mean = mean(bym_est, na.rm = TRUE),
    .groups = "drop"
  )
library(ggplot2)

ggplot(pdp_2d_facet, aes(x = treepriori_mean, y = pred_mean)) +
  geom_line(color = "darkblue", linewidth = 1) +
  facet_wrap(~ tes_q, labeller = label_both) +
  labs(
    title = "PDP of Tree Priority by Tree Equity Quartiles",
    x = "Tree Priority Index (Unscaled)",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()

cleaned_data$LCdiversity <- as.numeric(cleaned_data$LCdiversity)
cleaned_data$LUS_Open.S <- as.numeric(cleaned_data$LUS_Open.S)
cleaned_data$bym_est <- as.numeric(cleaned_data$bym_est)

safe_data1 <- cleaned_data %>%
  filter(
    !is.na(LCdiversity),
    !is.na(LUS_Open.S),
    !is.na(bym_est)
  ) %>%
  mutate(
    open_q = ntile(LUS_Open.S, 4),  # Quartiles for Open Space
    pdp_bin = cut(LCdiversity, breaks = pretty(range(LCdiversity), n = 5), include.lowest = TRUE)
  )

pdp_open <- safe_data1 %>%
  group_by(open_q, pdp_bin) %>%
  summarise(
    LCdiversity_mean = mean(LCdiversity, na.rm = TRUE),
    pred_mean = mean(bym_est, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(pdp_open, aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  facet_wrap(~ open_q, labeller = label_both) +
  labs(
    title = "PDP of Land Cover Diversity by Open Space Quartiles",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()
###############################################
cleaned_data$LUS_TreeCan <- as.numeric(cleaned_data$Treecanopy)

safe_data2 <- cleaned_data %>%
  filter(
    !is.na(LCdiversity),
    !is.na(Treecanopy),
    !is.na(bym_est)
  ) %>%
  mutate(
    treecan_q = ntile(LUS_TreeCan, 4),  # Quartiles for Tree Canopy
    pdp_bin = cut(LCdiversity, breaks = pretty(range(LCdiversity), n = 4), include.lowest = TRUE)
  )

pdp_treecanopy <- safe_data2 %>%
  group_by(treecan_q, pdp_bin) %>%
  summarise(
    LCdiversity_mean = mean(LCdiversity, na.rm = TRUE),
    pred_mean = mean(bym_est, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(pdp_treecanopy, aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ treecan_q, labeller = label_both) +
  labs(
    title = "PDP of Land Cover Diversity by Tree Canopy Quartiles",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()

ggplot(pdp_open, aes(x = LCdiversity_mean, y = pred_mean)) +
  #geom_line(color = "darkgreen", linewidth = 1) +
  geom_smooth(method = "loess", color = "darkgreen", size = 1, se = FALSE) +  # Add smoothing line
  facet_wrap(~ open_q, labeller = label_both) +
  labs(
    title = "PDP of Land Cover Diversity by Open Space Quartiles",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()
ggplot(pdp_treecanopy, aes(x = LCdiversity_mean, y = pred_mean)) +
 # geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(method = "loess", color = "steelblue", size = 1, se = FALSE) +  # Add smoothing line
  facet_wrap(~ treecan_q, labeller = label_both) +
  labs(
    title = "PDP of Land Cover Diversity by Tree Canopy Quartiles",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()

###############################
# Filter data to only keep Q1 and Q4 for both open_q and treecan_q
pdp_open_q1_q4 <- pdp_open %>%
  filter(open_q %in% c(1, 4))

pdp_treecanopy_q1_q4 <- pdp_treecanopy %>%
  filter(treecan_q %in% c(1, 4))

# Create the plots for PDP of LCdiversity by Open Space Quartiles (Q1 and Q4)
plot_open <- ggplot(pdp_open_q1_q4, aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_smooth(method = "loess", color = "darkgreen", size = 1, se = FALSE) +
  facet_wrap(~ open_q, labeller = label_both) +
  labs(
    title = "PDP of Land Cover Diversity by Open Space Quartiles (Q1 & Q4)",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()

# Create the plots for PDP of LCdiversity by Tree Canopy Quartiles (Q1 and Q4)
plot_treecanopy <- ggplot(pdp_treecanopy_q1_q4, aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_smooth(method = "loess", color = "steelblue", size = 1, se = FALSE) +
  facet_wrap(~ treecan_q, labeller = label_both) +
  labs(
    title = "PDP of Land Cover Diversity by Tree Canopy Quartiles (Q1 & Q4)",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal()

library(patchwork)
plot_open + plot_treecanopy

library(ggplot2)
library(dplyr)
library(patchwork)

y_min <- min(c(pdp_open_q1_q4$pred_mean, pdp_treecanopy_q1_q4$pred_mean), na.rm = TRUE)
y_max <- max(c(pdp_open_q1_q4$pred_mean, pdp_treecanopy_q1_q4$pred_mean), na.rm = TRUE)

plot_open_q1 <- ggplot(pdp_open_q1_q4 %>% filter(open_q == 1), aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_smooth(method = "loess", color = "grey30", size = .7, se = FALSE) +
  labs(
    title = "PDP of Land Cover Diversity by Open Space Quartile Q1",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max))  # Set y-axis limits

plot_open_q4 <- ggplot(pdp_open_q1_q4 %>% filter(open_q == 4), aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_smooth(method = "loess", color = "grey30", size = .7, se = FALSE) +
  labs(
    title = "PDP of Land Cover Diversity by Open Space Quartile Q4",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max))  # Set y-axis limits

plot_treecan_q1 <- ggplot(pdp_treecanopy_q1_q4 %>% filter(treecan_q == 1), aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_smooth(method = "loess", color = "grey30", size = .7, se = FALSE) +
  labs(
    title = "PDP of Land Cover Diversity by Tree Canopy Quartile Q1",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max))  # Set y-axis limits

plot_treecan_q4 <- ggplot(pdp_treecanopy_q1_q4 %>% filter(treecan_q == 4), aes(x = LCdiversity_mean, y = pred_mean)) +
  geom_smooth(method = "loess", color = "grey30", size = .7, se = FALSE) +
  labs(
    title = "PDP of Land Cover Diversity by Tree Canopy Quartile Q4",
    x = "Land Cover Diversity Index",
    y = "Predicted Suicide Rate"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(y_min, y_max))  # Set y-axis limits

combined_plot <- plot_open_q1 + plot_treecan_q1 + plot_open_q4 + plot_treecan_q4 +
  plot_layout(ncol = 2, nrow = 2)

combined_plot

