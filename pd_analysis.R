
#------ Generalised Linear Mixed Models for Parkinson's Disease Neuropsychiatry meta-analyses ------ #

# code produced by Dr Cameron Watson 

# last updated on 27/09/23 

## PREPARATION -------------------------------------------------------------------------------------------------------------------

# Load packages ------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(metafor)
library(readr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(meta)
library(openxlsx)
library(patchwork)

# for SMD and Vd calculations (conducted seperately), see bottom of this sheet for code
data <- read.csv("pd_master_03_09.csv")

# filter the extraction database down to those in full meta  
complete_data <- data %>% filter(which_meta == "main")

# some studies have adjusted SMDs, some unadjusted, others both. In perference for this analysis we take those with adjusted SMDs, but unadjusted if only that available 
complete_data <- complete_data %>% 
  mutate(new_SMD = ifelse(!is.na(SMD_adjusted), SMD_adjusted, SMD_unadjusted),
         new_Vd = ifelse(!is.na(Vd_adjusted), Vd_adjusted, Vd_unadjusted))

complete_data$adjusted <- ifelse(!is.na(complete_data$Vd_adjusted), "adjusted", "unadjusted")

# calculate the standard deviation and standard errors for new SMD + Vds
complete_data$standard_error <-sqrt(complete_data$new_Vd) # create SEM for overall SMD
complete_data$standard_error_unadj <-sqrt(complete_data$Vd_unadjusted) # create SMD for unadjusted scores
complete_data$standard_error_adj <-sqrt(complete_data$Vd_adjusted) # create SMD for adjusted scores


## DESCRIPTIVE STATS -------------------------------------------------------------------------------------------------------------------

pd_desc <- complete_data
view(pd_desc)

# Remove duplicates based on study_id and calculate the sum of n_total
unique_studs <- pd_desc %>%
  distinct(study_id, .keep_all = TRUE) 

# number of studies used = 49 studies, n studied = 164,171
unique_studs$n_total 
sum(unique_studs$n_total)

# gender stats = 44 studies, n studied = 162,857
pd_desc_gender <- unique_studs[complete.cases(unique_studs[, c("n_male", "n_female")]), ] # removes those studies where gender data is incomplete
total_n_gender <- sum(pd_desc_gender$n_total)
n_female <- sum(pd_desc_gender$n_female) # n = 64,856
n_male <- sum(pd_desc_gender$n_male) # n = 98,129
percent_male <- sum((n_male/total_n_gender)*100) # 60.3% male 
percent_female <- sum((n_female/total_n_gender)*100) # 60.3% male 

# ethnicity stats = 2 studies, n studied = 986
pd_desc_ethnicity <- unique_studs[complete.cases(unique_studs[, c("n_white")]), ] # removes those studies where ethnicity data is incomplete
total_n_eth <- sum(pd_desc_ethnicity$n_total)
# can look at percentages if wanted but at the moment the data is incomplete

# age stats = 40 studies, n studied = 162,747, mean age = 71.9, SD = 11.5
pd_desc_age <- unique_studs[complete.cases(unique_studs[, c("age_mean", "age_sd")]), ] # removes those studies where age data is incomplete
total_n_age <- sum(pd_desc_age$n_total)

total_mean <- 0
total_n <- 0

for (i in 1:nrow(pd_desc_age)) {
  # calculate the weight for the current sample
  weight <- pd_desc_age$n_total[i]
  # calculate the weighted mean for the current sample
  weighted_mean <- pd_desc_age$age_mean[i] * weight
  # add the weighted mean to the running total
  total_mean <- total_mean + weighted_mean
  # add the sample size to the running total
  total_n <- total_n + weight
}
# calculate the overall pooled mean
pooled_mean <- total_mean / total_n

# initialize variables for calculating the overall pooled SD
total_ss <- 0
total_n <- 0

# loop over each sample in the data frame
for (i in 1:nrow(pd_desc_age)) {
  # calculate the weight for the current sample
  weight <- pd_desc_age$n_total[i]
  # calculate the sum of squares for the current sample
  ss <- (pd_desc_age$n_total[i] - 1) * pd_desc_age$age_sd[i]^2
  # add the sum of squares to the running total
  total_ss <- total_ss + ss
  # add the sample size to the running total
  total_n <- total_n + weight
}

# calculate the degrees of freedom
df_total <- total_n - length(pd_desc_age$n_total)
# calculate the pooled variance
pooled_var <- total_ss / df_total
# calculate the pooled standard deviation
pooled_sd <- sqrt(pooled_var)

pooled_mean
pooled_sd

# other things we might want to do descriptive states on
# country of study 
# Replace all occurrences of "B" with "F" in the y column
unique_studs$country <- replace(unique_studs$country, unique_studs$country == "Italy ", "Italy")
unique_studs$country <- replace(unique_studs$country, unique_studs$country == "Norway ", "Norway")
unique_studs$country <- replace(unique_studs$country, unique_studs$country == "Republic of Korea ", "Republic of Korea")
unique_studs$country <- replace(unique_studs$country, unique_studs$country == "South Korea ", "Republic of Korea")
table(unique_studs$country)

# USA - 13 studies
# Norway - 6 studies 
# Italy - 5 studies
# Spain & UK - 4 studies 
# Korea - 3 studies 
# France, Germany, Japan & Singapore - 2 studies (+ 2 studies which were multicountry)
# Brazil, Israel, Serbia and Switzerland - 1 study  

# study design 
table(unique_studs$study_design) # case-control - 2 studies, cohort - 47 studies 
table(unique_studs$centres) # multi - 19, single 30
table(unique_studs$retrospective_prospective) # prosp - 41, retro 8
table(unique_studs$funding)

## META-ANALYSES -------------------------------------------------------------------------------------------------------------------

# ------------ exposure / outcome pairs ------------ #

table(complete_data$exposure_class, complete_data$outcome_class) # EXPOSURE OUTCOME PAIRS FOR WHOLE COHORT N = 83
adj_df <- complete_data %>% filter(!is.na(SMD_adjusted))
table(adj_df$outcome_class, adj_df$exposure_class) # EXPOSURE OUTCOME PAIRS FOR ADJUSTED COHORT N = 21

# ------------------ apathy ------------------------ #

apathy_df <- complete_data %>% filter(exposure_class == "Apathy")
apathy_df <- apathy_df %>% filter(new_SMD != "NA")
table(apathy_df$outcome_class) # 7 studies 

# unadjusted subgroup analyses 

# Subgroup analysis for "Cognitive impairment " - 4 studies 
cog <- rma(yi = new_SMD, vi = new_Vd, data = apathy_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-apathy on Cognition", cog), header="Author(s) and Year")
print(cog)
funnel(cog, main = "Funnel Plot of Studies Examining the Impact of Apathy on Cognition")
regtest(cog)

# Subgroup analysis for "Disease progression" - 3 studies 
prog <- rma(yi = new_SMD, vi = new_Vd, data = apathy_df,
            subset = (outcome_class == "Disease progression"),
            slab = paste(meta_label))
forest(prog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-apathy on disease progression", prog), header="Author(s) and Year")
print(prog)
funnel(prog, main = "Funnel Plot of Studies Examining the Impact of Apathy on Disease Progression")
regtest(prog)

# combined subgroup plot
apathy_df <- complete_data %>% filter(exposure_class == "Apathy")
apathy_df <- apathy_df %>% filter(new_SMD != "NA")
apathy_subgroup <- apathy_df[, c("meta_label", "outcome_class", "new_SMD", "new_Vd", "standard_error", "year", "adjusted")]
apathy_subgroup <- apathy_subgroup %>%
  group_by(adjusted) %>%
  arrange(year)

apathy_subgroup$year <- as.numeric(apathy_subgroup$year)

# Create the forest plot with individual study labels and outcome class labels
forest_plot <- ggplot(apathy_subgroup, aes(x = new_SMD, y = reorder(meta_label, -year), label = rownames(apathy_subgroup))) +
  geom_point(aes(size = (1/new_Vd), fill = adjusted), shape = 21, color = "black") +  # Set shape and color
  geom_errorbarh(aes(xmin = new_SMD - 1.96 * standard_error, xmax = new_SMD + 1.96 * standard_error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("green","orange", "blue", "purple")) +  # Define custom colors
  labs(title = "The Effect of Apathy on Parkinson's Disease Outcomes",
       x = "Standardized Mean Difference (SMD)",
       y = "",
       size = "Weight",
       fill = "Outcome Class") +  # Update legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0),
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines"))

# Add facet_grid with borders and outcome class labels
apathy_forest_plot_faceted <- forest_plot +
  facet_grid(outcome_class ~ ., scales = "free_y", space = "free_y") +
  theme(panel.border = element_rect(color = "black", fill = NA),  # Add borders around facets
        strip.background = element_rect(fill = "grey"),  # Grey box background
        strip.text = element_text(size = 12, face = "bold", color = "white"), # Format strip labels
        plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 20, 0)),  # Add space above and below the title
        plot.margin = margin(40, 40, 40, 40)  # Add white space around the plot
  )  

print(apathy_forest_plot_faceted)

# there are NO >=2 study exp/out pairs for adjusted subgroup analyses 

# ------------------ anxiety ------------------------ #

anx_df <- complete_data %>% filter(exposure_class == "Anxiety")
anx_df <- anx_df %>% filter(new_SMD != "NA")
table(anx_df$outcome_class)

# Subgroup analysis for "Cognitive impairment " - 2 studies
cog <- rma(yi = new_SMD, vi = new_Vd, data = anx_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-anxiety on Cognition", cog), header="Author(s) and Year")
print(cog)
funnel(cog, main = "Funnel Plot of Studies Examining the Impact of Anxiety on Cognition")
regtest(cog)

# Subgroup analysis for "Disease progression"
prog <- rma(yi = new_SMD, vi = new_Vd, data = anx_df,
            subset = (outcome_class == "Disease progression"),
            slab = paste(meta_label))
forest(prog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-anxiety on disease progression", prog), header="Author(s) and Year")
print(prog)
funnel(prog, main = "Funnel Plot of Studies Examining the Impact of Anxiety on Disease Progression")
regtest(prog)
ggsave("anx_prog_forest.pdf", plot = psychosis_forest_plot_faceted, width = 10, height = 20, units = "in")

# combined subgroup plot
anxiety_subgroup <- anx_df[, c("meta_label", "outcome_class", "new_SMD", "new_Vd", "standard_error", "year", "adjusted")]
anxiety_subgroup <- anxiety_subgroup %>%
  filter(outcome_class != "Falls/fractures")
anxiety_subgroup$new_SMD <- as.numeric(anxiety_subgroup$new_SMD)
anxiety_subgroup <- anxiety_subgroup %>%
  arrange(year)

# Create the forest plot with individual study labels and outcome class labels
forest_plot <- ggplot(anxiety_subgroup, aes(x = new_SMD, y = reorder(meta_label, -year), label = rownames(anxiety_subgroup))) +
  geom_point(aes(size = (1/new_Vd), fill = adjusted), shape = 21, color = "black") +  # Set shape and color
  geom_errorbarh(aes(xmin = new_SMD - 1.96 * standard_error, xmax = new_SMD + 1.96 * standard_error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("green","orange", "blue", "purple")) +  # Define custom colors
  labs(title = "The Effect of Anxiety on Parkinson's Disease Outcomes",
       x = "Standardized Mean Difference (SMD)",
       y = "",
       size = "Weight",
       fill = "Outcome Class") +  # Update legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0),
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines"))

# Add facet_grid with borders and outcome class labels
anx_forest_plot_faceted <- forest_plot +
  facet_grid(outcome_class ~ ., scales = "free_y", space = "free_y") +
  theme(panel.border = element_rect(color = "black", fill = NA),  # Add borders around facets
        strip.background = element_rect(fill = "grey"),  # Grey box background
        strip.text = element_text(size = 12, face = "bold", color = "white"), # Format strip labels
        plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 20, 0)),  # Add space above and below the title
        plot.margin = margin(40, 40, 40, 40)  # Add white space around the plot
  )  

print(anx_forest_plot_faceted)
ggsave("combined_anx.pdf", plot = anx_forest_plot_faceted, width = 10, height = 18, units = "in")


# ------------------ icbs ------------------------ #

icb_df <- complete_data %>% filter(exposure_class == "ICB")
icb_df <- icb_df %>% filter(new_SMD != "NA")
table(icb_df$outcome_class)


# Subgroup analysis for "Cognitive impairment "
cog <- rma(yi = new_SMD, vi = new_Vd, data = icb_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of ICBs on Cognition", cog), header="Author(s) and Year")
print(cog)
funnel(cog, main = "Funnel Plot of Studies Examining the Impact of ICBs on Cognition")
regtest(cog)

# Subgroup analysis for "Disease progression"
prog <- rma(yi = new_SMD, vi = new_Vd, data = icb_df,
            subset = (outcome_class == "Disease progression"),
            slab = paste(meta_label))
forest(prog, cex=0.75, mlab=mlabfun("RE Model for the Impact of ICBs on disease progression", prog), header="Author(s) and Year")
print(prog)
funnel(prog, main = "Funnel Plot of Studies Examining the Impact of ICBs on Disease Progression")
regtest(prog)

# combined subgroup plot
icb_subgroup <- icb_df[, c("meta_label", "outcome_class", "new_SMD", "new_Vd", "standard_error", "year", "adjusted")]
icb_subgroup <- icb_subgroup %>%
  group_by(adjusted) %>%
  arrange(year)

# Create the forest plot with individual study labels and outcome class labels
forest_plot <- ggplot(icb_subgroup, aes(x = new_SMD, y = reorder(meta_label, -year), label = rownames(icb_subgroup))) +
  geom_point(aes(size = (1/new_Vd), fill = adjusted), shape = 21, color = "black") +  # Set shape and color
  geom_errorbarh(aes(xmin = new_SMD - 1.96 * standard_error, xmax = new_SMD + 1.96 * standard_error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("green","orange", "blue", "purple")) +  # Define custom colors
  labs(title = "The Effect of ICBs on Parkinson's Disease Outcomes",
       x = "Standardized Mean Difference (SMD)",
       y = "",
       size = "Weight",
       fill = "Outcome Class") +  # Update legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0),
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines"))

# Add facet_grid with borders and outcome class labels
icb_forest_plot_faceted <- forest_plot +
  facet_grid(outcome_class ~ ., scales = "free_y", space = "free_y") +
  theme(panel.border = element_rect(color = "black", fill = NA),  # Add borders around facets
        strip.background = element_rect(fill = "grey"),  # Grey box background
        strip.text = element_text(size = 12, face = "bold", color = "white"), # Format strip labels
        plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 20, 0)),  # Add space above and below the title
        plot.margin = margin(40, 40, 40, 40)  # Add white space around the plot
  )  

print(icb_forest_plot_faceted)

# ------------------ BPAD ------------------------ #
# only 1 study for 3 
# ------------------ psychosis ------------------------ #

psychosis_df <- complete_data %>% filter(exposure_class == "Psychosis")
psychosis_df <- psychosis_df %>% filter(new_SMD != "NA")
table(psychosis_df$outcome_class)
psychosis_df$adjusted <- ifelse(!is.na(psychosis_df$Vd_adjusted), "adjusted", "unadjusted")
psychosis_df <- psychosis_df %>%
  arrange(new_SMD)

# Subgroup analysis for "Cognitive impairment "
cog <- rma(yi = new_SMD, vi = new_Vd, data = psychosis_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-psychosis on Cognition", cog), header="Author(s) and Year")
print(cog)
funnel(cog, main = "Funnel Plot of Studies Examining the Impact of Psychosis on Cognition")
regtest(cog)

# Subgroup analysis for "Death"
death <- rma(yi = new_SMD, vi = new_Vd, data = psychosis_df,
             subset = (outcome_class == "Death "),
             slab = paste(meta_label))
forest(death, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-psychosis on death", death), header="Author(s) and Year")
print(death)
funnel(death, main = "Funnel Plot of Studies Examining the Impact of Psychosis on Death")
regtest(death)

# Subgroup analysis for "Disease progression"
prog <- rma(yi = new_SMD, vi = new_Vd, data = psychosis_df,
            subset = (outcome_class == "Disease progression"),
            slab = paste(meta_label))
forest(prog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-psychosis on disease progression", prog), header="Author(s) and Year")
print(prog)
funnel(prog, main = "Funnel Plot of Studies Examining the Impact of Psychosis on Disease Progression")
regtest(prog)

# Subgroup analysis for "Falls/fractures"
falls <- rma(yi = new_SMD, vi = new_Vd, data = psychosis_df,
            subset = (outcome_class == "Falls/fractures"),
            slab = paste(meta_label))
forest(falls, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-psychosis on falls/fractures", falls), header="Author(s) and Year")
print(falls)
funnel(falls, main = "Funnel Plot of Studies Examining the Impact of Psychosis on Falls/Fractures")
regtest(falls)

# Subgroup analysis for "Residential home"
care <- rma(yi = new_SMD, vi = new_Vd, data = psychosis_df,
            subset = (outcome_class == "Residential home"),
            slab = paste(meta_label))
forest(care, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-psychosis on carehome", care), header="Author(s) and Year")
print(care)
funnel(care, main = "Funnel Plot of Studies Examining the Impact of Psychosis on Carehome Placement")
regtest(care)

# plot for psychosis subgroup analysis --------------------------
psych_subgroup <- psychosis_df[, c("meta_label", "outcome_class", "new_SMD", "new_Vd", "standard_error", "adjusted", "year")]
psych_subgroup <- psych_subgroup %>%
  group_by(adjusted) %>%
  arrange(year)

# Create the forest plot with individual study labels and outcome class labels
forest_plot <- ggplot(psych_subgroup, aes(x = new_SMD, y = reorder(meta_label, -year), label = rownames(psych_subgroup))) +
  geom_point(aes(size = (1/new_Vd), fill = adjusted), shape = 21, color = "black") +  # Set shape and color
  geom_errorbarh(aes(xmin = new_SMD - 1.96 * standard_error, xmax = new_SMD + 1.96 * standard_error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("green","orange", "blue", "purple")) +  # Define custom colors
  labs(title = "The Impact of Psychosis on Parkinson's Disease Outcomes",
       x = "Standardized Mean Difference (SMD)",
       y = "",
       size = "Weight",
       fill = "Outcome Class") +  # Update legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0),
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines"))

# Add facet_grid with borders and outcome class labels
psychosis_forest_plot_faceted <- forest_plot +
  facet_grid(outcome_class ~ ., scales = "free_y", space = "free_y") +
  theme(panel.border = element_rect(color = "black", fill = NA),  # Add borders around facets
        strip.background = element_rect(fill = "grey"),  # Grey box background
        strip.text = element_text(size = 10, face = "bold", color = "white"), # Format strip labels
        plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 20, 0)),  # Add space above and below the title
        plot.margin = margin(30, 30, 30, 30)  # Add white space around the plot
  )  +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -1.5),
    fill = "white", color = NA
  )
  

print(psychosis_forest_plot_faceted)
ggsave("combined_psychosis.pdf", plot = psychosis_forest_plot_faceted, width = 10, height = 20, units = "in")


# ------------------ depression ------------------------ #

dep_df <- complete_data %>% filter(exposure_class == "Depression")
table(dep_df$outcome_class)
dep_df <- dep_df %>% filter(new_SMD != "NA")
dep_df <- dep_df %>%
  arrange(new_SMD)

# Subgroup analysis for "Cognitive impairment "
cog <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on Cognition", cog), header="Author(s) and Year")
print(cog)
funnel(cog, main = "Funnel Plot of Studies Examining the Impact of Depression on Cognition")
regtest(cog)

# Subgroup analysis for "Death"
death <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
             subset = (outcome_class == "Death "),
             slab = paste(meta_label))
forest(death, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on death", death), header="Author(s) and Year")
print(death)
funnel(death, main = "Funnel Plot of Studies Examining the Impact of Depression on Death")
regtest(death)

# Subgroup analysis for "Disease progression"
prog <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
            subset = (outcome_class == "Disease progression"),
            slab = paste(meta_label))
forest(prog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on disease progression", prog), header="Author(s) and Year")
print(prog)
funnel(prog, main = "Funnel Plot of Studies Examining the Impact of Depression on Disease Progression")
regtest(prog)

# Subgroup analysis for "Falls/fractures"
falls <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
             subset = (outcome_class == "Falls/fractures"),
             slab = paste(meta_label))
forest(falls, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on falls/fractures", falls), header="Author(s) and Year")
print(falls)
funnel(falls, main = "Funnel Plot of Studies Examining the Impact of Depression on Falls/Fractures")
regtest(falls)

# Subgroup analysis for "Disability"
dis <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
            subset = (outcome_class == "Disability "),
            slab = paste(meta_label))
forest(dis, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on Disability", care), header="Author(s) and Year")
print(dis)
funnel(dis, main = "Funnel Plot of Studies Examining the Impact of Depression on Disability")
regtest(dis)

# plot for depression subgroup analysis --------------------------
dep_subgroup <- dep_df[, c("meta_label", "outcome_class", "new_SMD", "new_Vd", "standard_error", "adjusted", "year")]
dep_subgroup <- dep_subgroup %>%
  filter(outcome_class != "Residential home")
dep_subgroup <- dep_subgroup %>%
  group_by(adjusted) %>%
  arrange(year)

# Create the forest plot with individual study labels and outcome class labels
forest_plot <- ggplot(dep_subgroup, aes(x = new_SMD, y = reorder(meta_label, -year), label = rownames(dep_subgroup))) +
  geom_point(aes(size = (1/new_Vd), fill = adjusted), shape = 21, color = "black") +  # Set shape and color
  geom_errorbarh(aes(xmin = new_SMD - 1.96 * standard_error, xmax = new_SMD + 1.96 * standard_error), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("green","orange", "blue", "purple")) +  # Define custom colors
  labs(title = "The Impact of Depression on Parkinson's Disease Outcomes",
       x = "Standardized Mean Difference (SMD)",
       y = "",
       size = "Weight",
       fill = "Outcome Class") +  # Update legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(hjust = 0),
        axis.title.y = element_blank(),
        panel.spacing = unit(1, "lines"))


# Add facet_grid with borders and outcome class labels

dep_forest_plot_faceted <- forest_plot +
facet_grid(outcome_class ~ ., scales = "free_y", space = "free_y") +
  theme(panel.border = element_rect(color = "black", fill = NA),  # Add borders around facets
        strip.background = element_rect(fill = "grey"),  # Grey box background
        strip.text = element_text(size = 10, face = "bold", color = "white"), # Format strip labels
        plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 20, 0)),  # Add space above and below the title
        plot.margin = margin(30, 30, 30, 30)  # Add white space around the plot
  )  +
  geom_rect(
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -1.5),
    fill = "white", color = NA
  )


dep_forest_plot_faceted
ggsave("combined_dep.pdf", plot = dep_forest_plot_faceted, width = 10, height = 20, units = "in")

# ------------------ adjustsed effect sizes ------------------------ #

adj_df <- complete_data %>% filter(!is.na(SMD_adjusted))
cross_tab <- table(adj_df$outcome_class, adj_df$exposure_class)

# depression
dep_df <- adj_df %>% filter(exposure_class == "Depression")
table(dep_df$outcome_class)
dep_df <- dep_df %>% filter(new_SMD != "NA")

# i) dep/cog
cog <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on Cognition", cog), header="Author(s) and Year")
print(cog)

# ii) dep/death
death <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
             subset = (outcome_class == "Death "),
             slab = paste(meta_label))
forest(death, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on death", death), header="Author(s) and Year")
print(death)

# iii) dep/dis
dis <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
           subset = (outcome_class == "Disability "),
           slab = paste(meta_label))
forest(care, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on Disability", care), header="Author(s) and Year")
print(dis)

# iv) dep/prog
prog <- rma(yi = new_SMD, vi = new_Vd, data = dep_df,
            subset = (outcome_class == "Disease progression"),
            slab = paste(meta_label))
forest(prog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-depression on disease progression", prog), header="Author(s) and Year")
print(prog)

# psychosis
psychosis_df <- adj_df %>% filter(exposure_class == "Psychosis")
psychosis_df <- psychosis_df %>% filter(new_SMD != "NA")
table(psychosis_df$outcome_class)

# i) psych/cog 
cog <- rma(yi = new_SMD, vi = new_Vd, data = psychosis_df,
           subset = (outcome_class == "Cognitive impairment "),
           slab = paste(meta_label))
forest(cog, cex=0.75, mlab=mlabfun("RE Model for the Impact of PD-psychosis on Cognition", cog), header="Author(s) and Year")
print(cog)

# ------------------ heatmap figure ------------------------ #

library(ggplot2)
library(dplyr)

# Given matrix data
matrix_data <- matrix(c(
  0.45, 0.46, 0.58, 0.75, 0.18,
  0.48, 0.40, 0.36, -0.28, 0.18,
  0.47, -0.17, NA, NA, NA,
  NA, 0.5, NA, NA, NA,
  1.06, NA, NA, NA, NA,
  0.54, 0.26, NA, NA, NA
), nrow = 6, byrow = TRUE)

rownames(matrix_data) <- c("Cognitive Impairment", "Disease Progression", "Falls/Fractures", "Disability", "Carehome", "Death")
colnames(matrix_data) <- c("Psychosis", "Depression", "Apathy", "Anxiety", "ICBs")

# Convert matrix to a tidy format for ggplot2
tidy_data <- as.data.frame(as.table(matrix_data)) %>%
  rename(Exposure = Var2, Outcome = Var1, EffectSize = Freq)


heatmap_plot <- ggplot(tidy_data, aes(x = Outcome, y = reorder(Exposure, -as.numeric(Exposure)), fill = EffectSize)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       limits = c(-0.7, 1.06),
                       breaks = c(-0.4, -0.15, 0.15, 0.4, 0.75),  # Specify breaks
                       labels = c("-0.4 (Medium)", "-0.15 (Small)", "0.15 (Small)", "0.4 (Medium)", "0.75 (Large)")) +  # Specify labels
  geom_text(aes(label = ifelse(is.na(EffectSize), "NA", round(EffectSize, 2))), color = "black", size = 5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Helvetica"),
        axis.text = element_text(size = 12, family = "Helvetica"),
        axis.title = element_blank(),
        legend.text = element_text(size = 12, family = "Helvetica"),
        legend.title = element_text(size = 14, family = "Helvetica"),
        legend.key.height = unit(1.5, "cm"),  # Increase the legend key height
        plot.margin = margin(2, 4, 2, 4, "cm")) +
  scale_y_discrete(limits = rev(levels(tidy_data$Exposure))) +
  labs(title = "An heatmap of the impact of psychiatric exposures on Parkinson's disease outcomes",
       x = "Outcomes", y = "Exposures",
       fill = "Effect Size")

print(heatmap_plot)
ggsave("heatmap.pdf", plot = heatmap_plot, width = 17, height = 11, units = "in")

# ------------------ SES calculations and conversions ------------------------ #
# this was initially the first step, but calculated SMDs are now online in the extraction table 
data <- data %>%
  mutate(
    n_exposed = as.numeric(n_exposed),
    n_unexposed = as.numeric(n_unexposed),
    exposed_outcome_mean = as.numeric(exposed_outcome_mean),
    unexposed_outcome_mean = as.numeric(unexposed_outcome_mean),
    exposed_outcome_sd = as.numeric(exposed_outcome_sd),
    unexposed_outcome_sd = as.numeric(unexposed_outcome_sd),
    SMD_unadjusted = as.numeric(SMD_unadjusted),
    Vd_unadjusted = as.numeric(Vd_unadjusted)
  )

# Identify the rows for which you want to calculate SMD and VD
# selected_rows <- data$SMD_conversion == "calc_smd"

# Loop over the selected rows and calculate SMD and VD
for (row_index in which(selected_rows)) {
  n_exposed <- data$n_exposed[row_index]
  exposed_outcome_mean <- data$exposed_outcome_mean[row_index]
  exposed_outcome_sd <- data$exposed_outcome_sd[row_index]
  
  n_unexposed <- data$n_unexposed[row_index]
  unexposed_outcome_mean <- data$unexposed_outcome_mean[row_index]
  unexposed_outcome_sd <- data$unexposed_outcome_sd[row_index]
  
  # Calculate the pooled standard deviation
  pooled_sd <- sqrt((((n_exposed - 1) * (exposed_outcome_sd^2)) + ((n_unexposed - 1) * (unexposed_outcome_sd^2))) 
                    / ((n_exposed + n_unexposed) - 2)) # correct & corroborated
  
  
  # Calculate the unadjusted SMD and VD
  smd_unadjusted <- (exposed_outcome_mean - unexposed_outcome_mean) / pooled_sd # correct formula 
  # vd_unadjusted <- (((exposed_outcome_sd^2) / n_exposed) + ((unexposed_outcome_sd^2) / n_unexposed)) # correct & corroborated
  
  vd_unadjusted <- (((n_exposed + n_unexposed) / (n_exposed*n_unexposed))
                    +
                      (smd_unadjusted / (2*(n_exposed+n_unexposed-2))))
  
  # Store the calculated values in the existing columns
  data$SMD_unadjusted[row_index] <- smd_unadjusted
  data$Vd_unadjusted[row_index] <- vd_unadjusted
  data$pooled_sd[row_index] <- pooled_sd
}

data

write.xlsx(data, file = "smds_2.xlsx")