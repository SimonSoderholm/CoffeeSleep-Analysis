
# Code for analysis 
# Project: Coffee consumption and sleep/sleepiness
# R-version used: 4.4.2

##############################################################################################################################
# Load R packages
# Note: package versions given below are based on R version 4.4.2
##############################################################################################################################

library(dplyr) # version 1.1.4
library(tidyverse) # version 2.0.0
library(ggplot2) # version 3.5.1
library(ggpubr) # version 0.6.0
library(gt) # version 0.11.1
library(gtsummary) # version 2.1.0
library(flextable) # version 0.9.7
library(officer) # version 0.6.8
library(cowplot) # version 1.1.3

library(data.table) # version 1.16.4
library(reshape2) # version 1.4.4

library(MASS) # version 7.3.64
library(ordinal) # version 2023.12.4.1
library(gofcat) # version 0.1.2
library(prettyglm) # version 1.0.1

library(effects) # version 4.2.2
library(ggeffects) # version 2.2.1


##############################################################################################################################
# Read SCAPIS phenotype data with adjusted variables
# Retain samples that are present in merged and QC-filtered .psam file
##############################################################################################################################

x <- read.table("SCAPIS_data/SCAPIS-DATA-PETITION-527-20230607_adj.txt", header = TRUE, sep = '\t', stringsAsFactors = FALSE, comment.char = "", quote = "")
QCsamples <- read.table("Data_merged/SCAPIS_merged_QCed.psam", header = TRUE, stringsAsFactors = FALSE, sep = '\t', comment.char = "")
x <- x[x$IID %in% QCsamples$IID,]

x_sub <- x %>% dplyr::select(IID, AgeAtVisitOne, Sex, BMI, Site, 
                      q5question2, q5question3, cqsh006,
                      cqsh007, cqsh008, cqsh009, cqsh010,
                      cqsh011, cqsh012, cqsh013, 
                      cqsle001, derived_smoke_status, cqsh001,
                      cqsh002, cqsh003, cqsh004, cqsh005, cqsh014, cqsh015,
                      cqpa012, cqsle001, cqme006)


##############################################################################################################################
# Use of sleep medications
##############################################################################################################################

patterns <- c("melatonin","mirtazapin","propavan","zolpidem","circadin","imovane","zopiklon")

matches <- x_sub[grepl(paste(patterns, collapse = "|"), x_sub$cqme006, ignore.case = TRUE),]

x_sub <- x_sub %>% 
  mutate(SleepMedication = case_when(IID %in% matches$IID ~ "YES", 
                                       !IID %in% matches$IID ~ "NO"))
x_sub$cqme006 <- NULL

x_sub$SleepMedication <- factor(x_sub$SleepMedication, levels = c("YES","NO"))

# percentage of participants using sleep medication:
nrow(matches)/nrow(x_sub)*100

##############################################################################################################################
# Investigating patterns of missingness
##############################################################################################################################

library(mice) # version 3.18.0
library(VIM) # version 6.2.6
library(naniar) # version 1.1.0
library(tidyverse) # version 2.0.0
library(gtsummary) # version 2.1.0

x_miss <- x_sub

x_miss <- x_miss %>% dplyr::rename(Stress = cqsle001,
                            Age = AgeAtVisitOne,
                            LeisurePhysActivityPast12 = cqpa012,
                            SleepTime = cqsh002,
                            SleepQuality = cqsh001,
                            DifficultFallAsleep = cqsh003,
                            WakingUp = cqsh004,
                            WakingUpEarly = cqsh005,
                            Reflux = cqsh014,
                            Snoring = cqsh015,
                            ESScrit1 = cqsh006,
                            ESScrit2 = cqsh007,
                            ESScrit3 = cqsh008,
                            ESScrit4 = cqsh009,
                            ESScrit5 = cqsh010,
                            ESScrit6 = cqsh011,
                            ESScrit7 = cqsh012,
                            ESScrit8 = cqsh013,
                            TeaConsumption = q5question3,
                            CoffeeConsumption = q5question2,
                            Smoking = derived_smoke_status
)


x_miss <- x_miss %>% 
  mutate(CoffeeConsumption = case_when(CoffeeConsumption == 1 ~ 9, 
                                       CoffeeConsumption == 2 ~ 8, 
                                       CoffeeConsumption == 3 ~ 7, 
                                       CoffeeConsumption == 4 ~ 6, 
                                       CoffeeConsumption == 5 ~ 5, 
                                       CoffeeConsumption == 6 ~ 4, 
                                       CoffeeConsumption == 7 ~ 3, 
                                       CoffeeConsumption == 8 ~ 2, 
                                       CoffeeConsumption == 9 ~ 1, 
                                       CoffeeConsumption == -98 ~ 0,))

x_miss <- x_miss %>% 
  mutate(TeaConsumption = case_when(TeaConsumption == 1 ~ 9, 
                                    TeaConsumption == 2 ~ 8, 
                                    TeaConsumption == 3 ~ 7, 
                                    TeaConsumption == 4 ~ 6, 
                                    TeaConsumption == 5 ~ 5, 
                                    TeaConsumption == 6 ~ 4, 
                                    TeaConsumption == 7 ~ 3, 
                                    TeaConsumption == 8 ~ 2, 
                                    TeaConsumption == 9 ~ 1, 
                                    TeaConsumption == -98 ~ 0,))

x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 0] <- "No coffee consumption"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 1] <- "1-3 times/month"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 2] <- "1-2 times/week"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 3] <- "3-4 times/week"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 4] <- "5-6 times/week"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 5] <- "1 time/day"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 6] <- "2 times/day"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 7] <- "3 times/day"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 8] <- "4 times/day"
x_miss$CoffeeConsumption_label[x_miss$CoffeeConsumption == 9] <- "5 times/day or more"

x_miss$CoffeeConsumption_label <- factor(x_miss$CoffeeConsumption_label, levels = c("No coffee consumption","1-3 times/month","1-2 times/week","3-4 times/week","5-6 times/week","1 time/day","2 times/day","3 times/day","4 times/day","5 times/day or more"))

x_miss$CoffeeConsumption <- NULL

x_miss$Smoking[x_miss$Smoking == "UNKNOWN"] <- NA
x_miss[x_miss == -99] <- NA

x_miss <- x_miss %>%
  mutate_if(is.character, as.factor)

summary(x_miss)
anyNA(x_miss)

prop_miss(x_miss) # The total percentage of missing values in the data
mvs <- as.data.frame(as.matrix(miss_var_summary(x_miss)))
write.table(as.data.frame(mvs), file = "REVISION/Missingness/Missing_per_variable.txt", row.names = FALSE, sep = '\t', quote = FALSE)

# Correlation

dummyNA <- x_miss[, colSums(is.na(x_miss)) > 1]

dummyNA <- as.data.frame(abs(is.na(dummyNA)))

corrMatrix <- cor(dummyNA) %>% round(digits = 2)
corrMatrix_long <- melt(corrMatrix)

missCorr <- ggplot(data = corrMatrix_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_viridis_c(option = "magma") +
  theme(axis.text.x = element_text(angle = 60, color = "black", size = 12, hjust = 1), axis.text.y = element_text(size = 12, color = "black"), axis.title = element_blank())

ggsave("Missingness_Correlation.svg",
       plot = missCorr,
       path = "REVISION/Missingness",
       width = 9.5,
       height = 7,
       units = "in",
       dpi = "retina")


mcar <- mcar_test(x_miss) # low p-value means that the missingness is not MCAR, but either MAR or MNAR



nadf_coffGroup <- x_miss %>%
  group_by(CoffeeConsumption_label) %>%
  summarise(across(2:25, ~mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = -CoffeeConsumption_label, names_to = "Column", values_to = "NA_Percent")

nadf_coffGroup <- nadf_coffGroup[!is.na(nadf_coffGroup$CoffeeConsumption_label),]
nadf_coffGroup$Column <- factor(nadf_coffGroup$Column)
colnames(nadf_coffGroup) <- c("Group","Column","NA_Percent")
nadf_coffGroup$Category <- "Coffee consumption"
nadf_coffGroup$Group <- as.factor(nadf_coffGroup$Group)

mplot <- ggplot(nadf_coffGroup, aes(x = Column, y = Group, fill = NA_Percent)) +
  geom_tile(color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", NA_Percent)), size = 3) +
  scale_fill_gradient(low = "white", high = "red", name = "NA %") +
  labs(y = "Coffee consumption") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.text.y = element_text(color = "black"), axis.title.x = element_blank())

ggsave("Variables_NAs_per_coffeeGroup2.svg",
       plot = mplot,
       path = "REVISION/Missingness",
       width = 9.5,
       height = 5,
       units = "in")


##############################################################################################################################
# Imputation
##############################################################################################################################

library(missMDA) # version 1.19
library(FactoMineR) # version 2.11

x_miss2 <- x_miss
rownames(x_miss2) <- x_miss2$IID
saved_IID <- x_miss2$IID
x_miss2$IID <- NULL

x_miss2$BMI_groups[x_miss2$BMI < 18.5] <- "Underweight"
x_miss2$BMI_groups[x_miss2$BMI >= 18.5 & x_miss2$BMI <= 24.9] <- "Normal weight"
x_miss2$BMI_groups[x_miss2$BMI >= 25 & x_miss2$BMI <= 29.9] <- "Overweight"
x_miss2$BMI_groups[x_miss2$BMI >= 30 & x_miss2$BMI <= 34.9] <- "Obesity class I"
x_miss2$BMI_groups[x_miss2$BMI >= 35 & x_miss2$BMI <= 39.9] <- "Obesity class II"
x_miss2$BMI_groups[x_miss2$BMI >= 40] <- "Obesity class III"

x_miss2$BMI <- x_miss2$BMI_groups
x_miss2$BMI_groups <- NULL

x_miss2$Age_groups[x_miss2$Age < 55] <- "50-54"
x_miss2$Age_groups[x_miss2$Age >= 55 & x_miss2$Age < 60] <- "55-59"
x_miss2$Age_groups[x_miss2$Age >= 60] <- "60-64"

x_miss2$Age <- x_miss2$Age_groups
x_miss2$Age_groups <- NULL

x_miss2 <- as.data.frame(lapply(x_miss2, as.factor))

set.seed(123)
nb <- estim_ncpMCA(x_miss2, ncp.max = 10)
mi100 <- MIMCA(x_miss2, verbose = TRUE, ncp = nb$ncp, nboot = 100)
saveRDS(mi100, file = "REVISION/Missingness/mi100.rds")

plot.MIMCA(mi100)

################################################################################
### Assess the robustness of the data imputation model

# Using the Overimpute() function to assess the quality of the imputation by comparing imputed values
# to observed values through cross-validation-like techniques. The method temporarily removed observed
# values, one at the time, and then imputing them using the MIPCA model. It then compares the imputed
# values to the true observed values.
################################################################################

library(missForest) # version 1.5

x_overimp <- as.data.frame(as.matrix(x_miss2))
orig_NA <- is.na(x_overimp)

set.seed(111)
mask_candidates <- which(!orig_NA, arr.ind = TRUE)
mask_indices <- sample(mask_candidates, size = floor(0.1 * length(mask_candidates)))

x_overimp_mat <- as.matrix(x_overimp)
x_overimp_mat[mask_indices] <- NA
x_overimp <- as.data.frame(x_overimp_mat)

rm(x_overimp_mat)

mi100_overimpute <- MIMCA(x_overimp, ncp = nb$ncp, nboot = 100, verbose = TRUE)
#x_imp <- missMDA::complete(mi100_overimpute, 1)

nboot <- length(mi100_overimpute$res.MI)
acc_list <- numeric(nboot)

for(i in 1:nboot) {
  x_imp <- as.matrix(mi100_overimpute$res.MI[[i]])
  acc_list[i] <- mean(x_imp[mask_indices] == as.matrix(x_miss2)[mask_indices], na.rm = TRUE)
}

mean_acc <- mean(acc_list)
sd_acc <- sd(acc_list)



vars <- names(x_miss2)
acc_per_var <- matrix(NA, nrow = nboot, ncol = length(vars))
colnames(acc_per_var) <- vars

for(b in 1:nboot) {
  x_imp <- mi100_overimpute$res.MI[[b]]
  
  for(j in seq_along(vars)) {
    masked_rows <- mask_candidates[sel, 1][mask_candidates[sel, 2] == j]
    
    if(length(masked_rows) > 0) {
      acc_per_var[b, j] <- mean(x_imp[masked_rows, j] == x_miss2[masked_rows, j])
    } else {
      acc_per_var[b, j] <- NA
    }
    
  }
  
}

mean_acc <- colMeans(acc_per_var, na.rm = TRUE)

baseline_acc <- sapply(seq_along(vars), function(j) {
  freq <- table(x_miss2[[j]])
  sum((freq / sum(freq))^2)
})

results <- data.frame(
  Variable = vars,
  MIMCA_Accuracy = mean_acc,
  Naive_Baseline = baseline_acc,
  Ratio = mean_acc / baseline_acc
)

print(results)

plot1_data <- results %>%
  arrange(desc(MIMCA_Accuracy)) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

acc_plot1 <- ggplot(plot1_data, aes(x = Variable, y = MIMCA_Accuracy)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = sprintf("%.2f",MIMCA_Accuracy)), hjust = -0.1, size = 4) +
  ylim(0,max(plot1_data$MIMCA_Accuracy) * 1.1) +
  ylab("Mean Accuracy") +
  theme_minimal(base_size = 13) +
  theme(axis.text = element_text(color = "black")) +
  coord_flip()

plot2_data <- results %>%
  arrange(desc(Ratio)) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

acc_plot2 <- ggplot(plot2_data, aes(x = Variable, y = Ratio)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = sprintf("%.2f",Ratio)), hjust = -0.1, size = 4) +
  ylim(0,max(plot2_data$Ratio) * 1.1) +
  ylab("Mean Accuracy Ratio") +
  theme_minimal(base_size = 13) +
  theme(axis.text = element_text(color = "black")) +
  coord_flip()

plot_grid(ncol = 2, acc_plot1, acc_plot2)

##############################################################################################################################
# Adjusting the data
# NOTE: functions have been defined in the end of the script
##############################################################################################################################

x_sub <- na.omit(x_sub)

x_sub <- AdjustCoffeeData(x_sub)
x_sub <- AdjustTeaData(x_sub)
x_sub <- AdjustBMIData(x_sub)
x_sub <- AdjustAgeData(x_sub)
x_sub <- AdjustSexData(x_sub)
x_sub <- AdjustESSData(x_sub)
x_sub <- AdjustSleepData(x_sub)
x_sub <- AdjustSmoking(x_sub)
x_sub <- AdjustStressData(x_sub)
x_sub <- AdjustPhysActData(x_sub)


##############################################################################################################################
# Table 1 - Coffee consumption - population characteristics
##############################################################################################################################

x_sub %>% 
  select(AgeAtVisitOne, Sex, BMI, derived_smoke_status, CoffeeConsumptionLevel, TeaConsumptionLevel, Stress, LeisurePhysActivityPast12, SleepMedication) %>%
  tbl_summary(
    by = CoffeeConsumptionLevel,                # What are the variables shown by/ Columns
    label = list(Sex ~ "Sex",
                 AgeAtVisitOne ~ "Age",             # Changing the names of variables shown as rows 
                 BMI ~ "BMI",
                 derived_smoke_status ~ "Smoking Status",
                 TeaConsumptionLevel ~ "Tea consumption",
                 Stress ~ "Stress",
                 LeisurePhysActivityPast12 ~ "Physical activity",
                 SleepMedication ~ "Sleep medication"
                 ),
    statistic = list(AgeAtVisitOne ~ "{mean} ({sd})",     # Detailing how certain variables values should be displayed
                     BMI ~ "{mean} ({sd})"),
    percent = "column") %>%                        # Adding percentages to rows
  add_p() %>%
  add_overall() %>%
  #add_q(method = "bonferroni") %>%
  modify_spanning_header(c("stat_1","stat_2","stat_3","stat_4") ~ "**Coffee consumption**") %>%
  bold_labels() %>%
  as_gt() %>%
  gt::gtsave(filename = "CoffeeVariablesTable.docx", path = "New_figures_20251201/Tables")


##############################################################################################################################
# Overview plot of coffee consumption
##############################################################################################################################

coffeeOverview <- ggplot(x_sub, aes(x = CoffeeConsumption_label)) +
  geom_bar(fill = "dodgerblue4") +
  theme_classic() +
  xlab("Coffee consumption") +
  ylab("") +
  #scale_fill_viridis_d(option = "cividis") +
  geom_text(aes(label = ..count.., size = 12) , stat = "count", position=position_dodge(width=0.9), vjust=-0.25) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(0,7000), expand = c(0,0))

coffeeOverview


coffeeSex <- ggplot(x_sub, aes(x = Sex, fill = CoffeeConsumption_label)) +
  geom_bar(position = "fill") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"))

cq2_coffee_sex <- chisq.test(with(x_sub, table(Sex, CoffeeConsumption_label)))

##############################################################################################################################
# Overview plots of coffee consumption vs. other variables
# Perform t-test and chi2-test to assess whether coffee consumption is statistically different between groups
##############################################################################################################################

coffee_Sex <- ggplot(x_sub, aes(x = Sex, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("Sex")

coffee_Sex
cq2_coffee_sex <- chisq.test(with(x_sub, table(Sex, CoffeeConsumption_label)))

coffee_BMI <- ggplot(x_sub, aes(x = BMI_groups, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("BMI")

coffee_BMI

coffee_Age <- ggplot(x_sub, aes(x = Age_groups, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("Age")

coffee_Age

coffee_Tea <- ggplot(x_sub, aes(x = TeaConsumptionLevel, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("Tea consumption")

coffee_Tea

coffee_Stress <- ggplot(x_sub, aes(x = Stress, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("Experience of stress")

coffee_Stress

coffee_PhysActivity <- ggplot(x_sub, aes(x = LeisurePhysActivityPast12, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol=1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("Physical activity")

coffee_PhysActivity

coffee_Smoking <- ggplot(x_sub, aes(x = derived_smoke_status, fill = CoffeeConsumptionLevel)) +
  geom_bar(position = "fill", color = "black") +
  theme_bw() +
  scale_fill_viridis_d(option = "cividis") +
  guides(fill=guide_legend(ncol = 1, title = "Coffee consumption")) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title = element_blank()) +
  ggtitle("Smoking status")

coffee_Smoking

legend_coffee <- get_plot_component(coffee_Smoking, "guide-box-right", return_all = TRUE)

row1 <- plot_grid(nrow = 1, align = "h", axis = "b",
          coffee_Age + theme(legend.position = "none"),
          coffee_Sex + theme(legend.position = "none"),
          coffee_BMI + theme(legend.position = "none"),
          coffee_Tea + theme(legend.position = "none")
)

row2 <- plot_grid(nrow = 1, align = "h", axis = "b",
                  coffee_Stress + theme(legend.position = "none"),
                  coffee_PhysActivity + theme(legend.position = "none"),
                  coffee_Smoking + theme(legend.position = "none"),
                  legend_coffee)

plot_grid(nrow = 2, align = "h", rel_heights = c(1,1.3),
          row1,
          row2)

##############################################################################################################################
# GWAS results
##############################################################################################################################

# --> Code for handling and plotting GWAS results are found in a separate R script file names "Soderholm_etal_CoffeeSleep_GWASplots.R"

##############################################################################################################################
# Table 2 - Coffee consumption - sleep characteristics
##############################################################################################################################

x_sub %>% 
  select(CoffeeConsumptionLevel, SleepTime_label, cqsh001_label, cqsh003_label,
         cqsh004_label, cqsh005_label, cqsh014_label, cqsh015_label, ESS_tot_ref) %>%
  tbl_summary(
    by = CoffeeConsumptionLevel,
    label = list(cqsh001_label ~ "Quality of sleep",
                 SleepTime_label ~ "Hours of sleep",             # Changing the names of variables shown as rows 
                 cqsh003_label ~ "Frequency of difficulty falling asleep",
                 cqsh004_label ~ "Frequency waking up several times",
                 cqsh005_label ~ "Frequency waking up too early",
                 cqsh014_label ~ "Frequency of reflux after going to bed",
                 cqsh015_label ~ "Frequency of loud snoring",
                 ESS_tot_ref ~ "ESS category"
    ),
    percent = "column") %>%
  #modify_table_styling(columns = matches("percent"), style_percent(x, digits = 1)) %>%
  #modify_fmt_fun(all_stat_cols() ~ function(x) {if (is.numeric(x)) style_percent(x, digits = 1) else x}) %>%
  add_p() %>%
  add_overall() %>%
  #add_q(method = "bonferroni") %>%
  modify_spanning_header(c("stat_1","stat_2","stat_3","stat_4") ~ "**Coffee consumption**") %>%
  bold_labels() %>%
  as_gt() %>% 
  gt::gtsave(filename = "SleepVariablesTable.docx", path = "Tables")

##############################################################################################################################
# Sleep score and ESS score distributions
##############################################################################################################################

SleepTot_mean <- round(mean(x_sub$Sleep_tot_new), digits = 1)
SleepTot_sd <- round(sd(x_sub$Sleep_tot_new), digits = 2)
ESStot_mean <- round(mean(x_sub$ESS_tot), digits = 1)
ESStot_sd <- round(sd(x_sub$ESS_tot), digits = 2)

SleepPlot_tot <- ggplot(x_sub, aes(x = Sleep_tot_new)) +
  geom_histogram(colour = "black", fill = "burlywood3", binwidth = 1) +
  #geom_density(alpha = 0.2, fill = "darkred") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,3500)) +
  theme(axis.text = element_text(size = 12, colour = "black"), legend.position = "none", axis.title = element_text(size = 12, colour = "black")) +
  ylab("Number of participants") +
  xlab("Sleep quality score") +
  geom_vline(xintercept = SleepTot_mean, linetype = "dashed", color = "black") +
  annotate(x = 0, y = +Inf, label = paste0("Mean: ", SleepTot_mean, "\nStandard deviation: ", SleepTot_sd), vjust = 1, hjust = 0, geom = "label", size = 5)

plot(SleepPlot_tot)

ESSPlot_tot <- ggplot(x_sub, aes(x = ESS_tot)) +
  geom_histogram(colour = "black", fill = "darkolivegreen4", binwidth = 1) +
  #geom_density(alpha = 0.2, fill = "darkred") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(0,3500)) +
  theme(axis.text = element_text(size = 12, colour = "black"), legend.position = "none", axis.title = element_text(size = 12, colour = "black")) +
  ylab("Number of participants") +
  xlab("ESS score") +
  geom_vline(xintercept = ESStot_mean, linetype = "dashed", color = "black") +
  annotate(x = 0, y = +Inf, label = paste0("Mean: ", ESStot_mean, "\nStandard deviation: ", ESStot_sd), vjust = 1, hjust = 0, geom = "label", size = 5)

plot(ESSPlot_tot)

plot_grid(ncol = 2, align = "h", axis = "b",
          SleepPlot_tot,
          ESSPlot_tot
)

##############################################################################################################################
# Correlation analysis
##############################################################################################################################

x_subCor <- x_sub
x_subCor

x_subCor$CoffeeConsumption_label <- factor(x_subCor$CoffeeConsumption_label, levels = c("No coffee consumption","1-3 times/month","1-2 times/week","3-4 times/week","5-6 times/week","1 time/day","2 times/day","3 times/day","4 times/day","5 times/day or more"), ordered = TRUE)
x_subCor$CoffeeConsumptionLevel <- factor(x_subCor$CoffeeConsumptionLevel, levels = c("NONE","LOW","MODERATE","HIGH"), ordered = TRUE)

CorrelationRes <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(CorrelationRes) <- c("Test","CoffeeVar","SleepESS","Correlation","Pval")

s_10g_sleep <- cor.test(as.numeric(x_subCor$CoffeeConsumption_label), x_subCor$Sleep_tot_new, method = "spearman")
k_10g_sleep <- cor.test(as.numeric(x_subCor$CoffeeConsumption_label), x_subCor$Sleep_tot_new, method = "kendall")

s_10g_ESS <- cor.test(as.numeric(x_subCor$CoffeeConsumption_label), x_subCor$ESS_tot, method = "spearman")
k_10g_ESS <- cor.test(as.numeric(x_subCor$CoffeeConsumption_label), x_subCor$ESS_tot, method = "kendall")

s_4g_sleep <- cor.test(as.numeric(x_subCor$CoffeeConsumptionLevel), x_subCor$Sleep_tot_new, method = "spearman")
k_4g_sleep <- cor.test(as.numeric(x_subCor$CoffeeConsumptionLevel), x_subCor$Sleep_tot_new, method = "kendall")

s_4g_ESS <- cor.test(as.numeric(x_subCor$CoffeeConsumptionLevel), x_subCor$ESS_tot, method = "spearman")
k_4g_ESS <- cor.test(as.numeric(x_subCor$CoffeeConsumptionLevel), x_subCor$ESS_tot, method = "kendall")


CorrelationRes <- rbind(CorrelationRes, c("spearman","10groups","SleepScore",s_10g_sleep$estimate,s_10g_sleep$p.value))
CorrelationRes <- rbind(CorrelationRes, c("kendall","10groups","SleepScore",k_10g_sleep$estimate,k_10g_sleep$p.value))

CorrelationRes <- rbind(CorrelationRes, c("spearman","10groups","ESS",s_10g_ESS$estimate,s_10g_ESS$p.value))
CorrelationRes <- rbind(CorrelationRes, c("kendall","10groups","ESS",k_10g_ESS$estimate,k_10g_ESS$p.value))

CorrelationRes <- rbind(CorrelationRes, c("spearman","4groups","SleepScore",s_4g_sleep$estimate,s_4g_sleep$p.value))
CorrelationRes <- rbind(CorrelationRes, c("kendall","4groups","SleepScore",k_4g_sleep$estimate,k_4g_sleep$p.value))

CorrelationRes <- rbind(CorrelationRes, c("spearman","4groups","ESS",s_4g_ESS$estimate,s_4g_ESS$p.value))
CorrelationRes <- rbind(CorrelationRes, c("kendall","4groups","ESS",k_4g_ESS$estimate,k_4g_ESS$p.value))

colnames(CorrelationRes) <- c("Test","CoffeeVar","SleepESS","Correlation","Pval")

write.table(CorrelationRes, file = "New_figures_20251201/CorrelationResults.txt", row.names = FALSE, sep = '\t', quote = FALSE)

##############################################################################################################################
# Ordinal Logistic regression analysis
# Quasi Poisson regression analysis for Sleep score and ESS
##############################################################################################################################

RegModels_Sleep <- list(
  "SleepTime" = clm(data = x_sub, formula = SleepTime_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "SleepQuality" = clm(data = x_sub, formula = cqsh001_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "DifficultFallAsleep" = clm(data = x_sub, formula = cqsh003_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "WakingUp" = clm(data = x_sub, formula = cqsh004_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "WakingUpEarly" = clm(data = x_sub, formula = cqsh005_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "Reflux" = clm(data = x_sub, formula = cqsh014_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "Snoring" = clm(data = x_sub, formula = cqsh015_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "SleepScore" = glm(data = x_sub, family = quasipoisson, formula = Sleep_tot_new ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication),
  "ESS" = glm(data = x_sub, family = quasipoisson, formula = ESS_tot ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002)
)

deviance(RegModels_Sleep$SleepScore) / df.residual(RegModels_Sleep$SleepScore)
deviance(RegModels_Sleep$ESS) / df.residual(RegModels_Sleep$ESS)

# Calculate Dispersion Statistics
# Info: https://fukamilab.github.io/BIO202/04-C-zero-data.html
# The closer to 1 the better

DisperStats <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(DisperStats) <- c("Model","DispersionStat")

for(d in 1:length(RegModels_Sleep)) {
  E2 <- resid(RegModels_Sleep[[d]], type = "pearson")
  N  <- nrow(x_sub)
  p  <- length(coef(RegModels_Sleep[[d]])) + 1  # '+1' is for variance parameter in NB
  dispVal <- sum(E2^2) / (N - p)
  DisperStats[nrow(DisperStats) + 1,] = list(names(RegModels_Sleep[d]), dispVal)
}

# Extract odds ratio, confidence intervals and p-values

RegStats_Sleep <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(RegStats_Sleep) <- c("CoffeeConsumptionLevel", "Estimate", "Std.Error", "t-value", "Pval", "OR", "CI_2.5", "CI_97.5", "SleepVar")

for(i in 1:length(RegModels_Sleep)) {
  modres <- as.data.frame(summary(RegModels_Sleep[[i]])$coefficients[c("CoffeeConsumptionLevelLOW","CoffeeConsumptionLevelMODERATE","CoffeeConsumptionLevelHIGH"),])
  modres$CoffeeConsumptionLevel <- rownames(modres)
  
  OR <- as.data.frame(round(exp(coef(RegModels_Sleep[[i]])), digits = 4))
  OR$CoffeeConsumptionLevel <- rownames(OR)
  colnames(OR) <- c("OR","CoffeeConsumptionLevel")
  OR <- OR[OR$CoffeeConsumptionLevel %in% c("CoffeeConsumptionLevelLOW","CoffeeConsumptionLevelMODERATE","CoffeeConsumptionLevelHIGH"),]
  
  CI <- as.data.frame(round(exp(confint(RegModels_Sleep[[i]])), digits = 4))
  CI$CoffeeConsumptionLevel <- rownames(CI)
  CI <- CI[CI$CoffeeConsumptionLevel %in% c("CoffeeConsumptionLevelLOW","CoffeeConsumptionLevelMODERATE","CoffeeConsumptionLevelHIGH"),]
  
  allstat <- merge(modres, OR, by = "CoffeeConsumptionLevel")
  allstat <- merge(allstat, CI, by = "CoffeeConsumptionLevel")
  allstat$SleepVar <- names(RegModels_Sleep[i])
  colnames(allstat) <- c("CoffeeConsumptionLevel", "Estimate", "Std.Error", "t-value", "Pval", "OR", "CI_2.5", "CI_97.5", "SleepVar")
  RegStats_Sleep <- rbind(RegStats_Sleep, allstat)
  print(paste0(i, "/", length(RegModels_Sleep)))
}


#with(RegModels_Sleep$SleepScore, cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail = FALSE)))

#lreg.or <- as.data.frame(round(exp(cbind(OR = coef(RegModels_Sleep[[i]]), confint(RegModels_Sleep[[i]]))), digits = 4))[c("CoffeeConsumptionLevelLOW","CoffeeConsumptionLevelMODERATE","CoffeeConsumptionLevelHIGH"),]
#regstats <- merge(modres, lreg.or, by = "row.names")
#regstats$SleepVar <- names(RegModels_Sleep[i])
#colnames(regstats) <- c("CoffeeConsumptionLevel", "Estimate", "Std.Error", "t-value", "Pval", "OR", "CI_2.5", "CI_97.5", "SleepVar")
#RegStats_Sleep <- rbind(RegStats_Sleep, regstats)


RegStats_Sleep$CoffeeConsumptionLevel <- gsub("CoffeeConsumptionLevel", "", as.character(RegStats_Sleep$CoffeeConsumptionLevel))
RegStats_Sleep$CoffeeConsumptionLevel <- factor(RegStats_Sleep$CoffeeConsumptionLevel, levels = c("LOW","MODERATE","HIGH"))
RegStats_Sleep$Psign[RegStats_Sleep$Pval >= 0.05] <- "NO"
RegStats_Sleep$Psign[RegStats_Sleep$Pval < 0.05] <- "YES"
RegStats_Sleep$SleepVar <- factor(RegStats_Sleep$SleepVar, levels = rev(unique(RegStats_Sleep$SleepVar)))
#levels(RegStats_Sleep$SleepVar) <- rev(levels(RegStats_Sleep$SleepVar))
#RegStats_Sleep <- RegStats_Sleep %>% arrange(SleepVar, CoffeeConsumptionLevel)
#RegStats_Sleep <- RegStats_Sleep[order(RegStats_Sleep$SleepVar, decreasing = TRUE),]

ggplot(RegStats_Sleep, aes(x = OR, y = SleepVar)) +
  geom_vline(aes(xintercept = 1), linewidth = 0.5, linetype = "dashed") +
  geom_errorbarh(aes(xmax = CI_97.5, xmin = CI_2.5), size = 0.5, height = 0.2, color = "black") +
  geom_point(aes(fill = Psign), colour = "black", pch = 21, size = 5) +
  scale_fill_manual(values = c("NO" = "black", "YES" = "orange")) +
  facet_wrap(CoffeeConsumptionLevel ~ .) +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"), legend.position = "none", strip.text = element_text(size = 11, color = "black"), axis.title.x = element_text(size = 14, color = "black"), axis.title.y = element_blank()) +
  xlab("Odds Ratio / Rate Ratio")


#broom::tidy(RegModels_Sleep[[1]])
#gtsummary::tbl_regression(RegModels_Sleep$DifficultFallAsleep, intercept = TRUE) %>% as_tibble()
#coef(RegModels_Sleep$DifficultFallAsleep)

#as.data.frame(summary(RegModels[[1]])$coefficients[c("CoffeeConsumptionLevelLOW","CoffeeConsumptionLevelMODERATE","CoffeeConsumptionLevelHIGH"),])
#as.data.frame(round(exp(cbind(OR = coef(RegModels[[1]]), confint(RegModels[[1]]))), digits = 4))[c("CoffeeConsumptionLevelLOW","CoffeeConsumptionLevelMODERATE","CoffeeConsumptionLevelHIGH"),]

##### Make tables of regression results

RegressionTable(RegModels_Sleep, "Basic")

##############################################################################################################################
# Diagnostics checks of models
##############################################################################################################################

##### Proportional odds (PO) assumption test #####

PO_results <- structure(list(outcome = character(), variable = character(), nt_p = numeric(), st_p = numeric()), class = "data.frame")

for(i in 1:length(RegModels_Sleep)) {
  
  mod_name <- names(RegModels_Sleep[i])
 
  if(mod_name %in% c("SleepScore","ESS")) {
    print(paste0("Not a clm model (", mod_name, "), skipped!"))
  } else {
    nt <- nominal_test(RegModels_Sleep[[i]])
    nt$variable <- rownames(nt)
    nt <- nt[!nt$variable == "<none>",]
    nt[c("Df","logLik","AIC","LRT")] <- NULL
    nt$nt_p <- nt$`Pr(>Chi)`
    nt$`Pr(>Chi)` <- NULL
    
    st <- scale_test(RegModels_Sleep[[i]])
    st$variable <- rownames(st)
    st <- st[!st$variable == "<none>",]
    st[c("Df","logLik","AIC","LRT")] <- NULL
    st$st_p <- st$`Pr(>Chi)`
    st$`Pr(>Chi)` <- NULL
    
    ntst <- merge(nt, st, by = "variable")
    ntst$outcome <- mod_name
    ntst <- ntst[,c("outcome","variable","nt_p","st_p")]
    
    PO_results <- rbind(PO_results, ntst)
    
  }
  
print(paste0(i,"/",length(RegModels_Sleep)))
  
}

PO_results <- PO_results %>%
  dplyr::mutate(
    Sign_nt = case_when(
      nt_p >= 0.05 ~ "",
      nt_p < 0.05 & nt_p >= 0.001 ~ "*",
      nt_p < 0.001 & nt_p >= 0.0001 ~ "**",
      nt_p < 0.0001 ~ "***"
    )
  )

PO_results <- PO_results %>%
  dplyr::mutate(
    Sign_st = case_when(
      st_p >= 0.05 ~ "",
      st_p < 0.05 & st_p >= 0.001 ~ "*",
      st_p < 0.001 & st_p >= 0.0001 ~ "**",
      st_p < 0.0001 ~ "***"
    )
  )

PO_results <- PO_results %>% 
  mutate(variable = recode(variable, AgeAtVisitOne = "Age", CoffeeConsumptionLevel = "Coffee consumption", derived_smoke_status = "Smoking", LeisurePhysActivityPast12 = "Physical activity", SleepMedication = "Sleep medication", TeaConsumption = "Tea consumption" 
))

PO_results$outcome <- factor(PO_results$outcome, levels = rev(c("SleepTime","SleepQuality","DifficultFallAsleep","WakingUp","WakingUpEarly","Reflux","Snoring")))

ntplot <- ggplot(PO_results, aes(x = variable, y = outcome, fill = -log10(nt_p))) +
  geom_tile(color = "black") +
  geom_text(aes(label = Sign_nt, size = 3)) +
  scale_fill_gradient2(low = "white", high = "indianred2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"), legend.position = "none") +
  ggtitle("Nominal test")

stplot <- ggplot(PO_results, aes(x = variable, y = outcome, fill = -log10(st_p))) +
  geom_tile(color = "black") +
  geom_text(aes(label = Sign_st, size = 3)) +
  scale_fill_gradient2(low = "white", high = "indianred2") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"), legend.position = "none") +
  ggtitle("Scale test")

plot_grid(ncol = 2,
          ntplot,
          stplot)

##### Run VGLM and extract threshold-specific ORs and plot #####

library(VGAM) # version 1.1.13
library(broom) # version 1.0.7


sleep_vars <- c("SleepTime_label", "cqsh001_label", "cqsh003_label", "cqsh004_label", "cqsh005_label", "cqsh014_label", "cqsh015_label")

sleep_vars_names <- c("SleepTime","SleepQuality","DifficultFallAsleep","WakingUp","WakingUpEarly","Reflux","Snoring")

confounders <- "AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication"

vgam_models <- list()
OR_plots <- list()
logit_plots <- list()


for (v in 1:length(sleep_vars)) {
  
  outcome <- sleep_vars[v]
  
  formula <- as.formula(paste(outcome, "~ CoffeeConsumptionLevel +", confounders))
  
  m_npo <- vglm(formula = formula, cumulative(parallel = FALSE ~ CoffeeConsumptionLevel + 1), data = x_sub)
  vgam_models[[sleep_vars_names[v]]] <- m_npo  
  
  coef_df <- data.frame(term = names(coef(m_npo)), estimate = coef(m_npo))
  
  coffee_effects <- coef_df %>%
    filter(grepl("Coffee", term)) %>%
    mutate(
      threshold = sub(".*:", "", term),
      predictor = sub(":.*", "", term),
      OR = exp(estimate)
    )
  
  se <- sqrt(diag(vcov(m_npo)))
  
  coffee_effects <- coffee_effects %>%
    mutate(
      SE = se[match(term, names(se))],
      conf.low = exp(estimate - 1.96 * SE),
      conf.high = exp(estimate + 1.96 * SE)
    )
  
  coffee_effects$predictor <- sub("CoffeeConsumptionLevel","", coffee_effects$predictor)
  coffee_effects$predictor <- factor(coffee_effects$predictor, levels = c("LOW","MODERATE","HIGH"))
  
  plt <- ggplot(coffee_effects, aes(x = as.numeric(threshold), y = OR, ymin = conf.low, ymax = conf.high, color = predictor)) +
    geom_pointrange(position = position_dodge(width = 0.4)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    labs(x = "Threshold index", y = "Threshold-specific OR", color = "Coffee consumption") +
    scale_color_manual(values = c("burlywood2","cadetblue4","coral2")) +
    theme_classic() +
    ggtitle(sleep_vars_names[v])
  
  OR_plots[[sleep_vars_names[v]]] <- plt
  
  
  
  
  print(paste0(v,"/",length(sleep_vars)))
  
}

ORlegend <- get_legend(OR_plots$SleepTime)

plot_grid(ncol = 4,
          OR_plots$SleepTime + theme(legend.position = "none"),
          OR_plots$SleepQuality + theme(legend.position = "none"),
          OR_plots$DifficultFallAsleep + theme(legend.position = "none"),
          OR_plots$WakingUp + theme(legend.position = "none"),
          OR_plots$WakingUpEarly + theme(legend.position = "none"),
          OR_plots$Reflux + theme(legend.position = "none"),
          OR_plots$Snoring + theme(legend.position = "none"),
          ORlegend)


##### Plot cumulative logit across threshold #####

newdat <- expand.grid(CoffeeConsumptionLevel = levels(x_sub$CoffeeConsumptionLevel),
                      AgeAtVisitOne = mean(x_sub$AgeAtVisitOne, na.rm = TRUE),
                      BMI = mean(x_sub$BMI, na.rm = TRUE),
                      Sex = levels(x_sub$Sex),
                      derived_smoke_status = levels(x_sub$derived_smoke_status),
                      TeaConsumption = mean(x_sub$TeaConsumption, na.rm = TRUE),
                      Stress = levels(x_sub$Stress),
                      LeisurePhysActivityPast12 = levels(x_sub$LeisurePhysActivityPast12),
                      SleepMedication = levels(x_sub$SleepMedication),
                      stringsAsFactors = FALSE)

for(m in 1:length(vgam_models)) {
  
  outcome <- names(vgam_models[m])
  
  pred_link <- predict(vgam_models[[m]], newdata = newdat, type = "link", se.fit = TRUE)
  fit_mat <- pred_link$fitted.values
  se_mat <- pred_link$se.fit
  
  pred_long <- as.data.frame(fit_mat)
  pred_long$CoffeeConsumptionLevel <- newdat$CoffeeConsumptionLevel
  pred_long <- pivot_longer(pred_long, cols = -CoffeeConsumptionLevel, names_to = "threshold", values_to = "link")
  pred_long$thr_index <- sub(".*=", "", pred_long$threshold)
  pred_long$thr_index <- sub("])", "", pred_long$thr_index)
  pred_long$thr_index <- as.numeric(pred_long$thr_index)
  
  pred_long$CoffeeConsumptionLevel <- factor(pred_long$CoffeeConsumptionLevel, levels = c("NONE","LOW","MODERATE","HIGH"))
  
  nmax <- max(pred_long$thr_index)
  
  plt <- ggplot(pred_long, aes(x = thr_index, y = link, color = CoffeeConsumptionLevel)) +
    geom_line() + geom_point(position = position_jitter(seed = 122)) +
    labs(x = "Threshold index", y = "Cumulative logit (log-odds of Y <= j)", color = "Coffee consumption") +
    scale_color_manual(values = c("black","burlywood2","cadetblue4","coral2")) +
    scale_x_continuous(breaks = seq(1, nmax, 1)) +
    theme_classic() +
    theme(axis.text = element_text(size = 10, color = "black")) +
    ggtitle(outcome)
  
  logit_plots[[outcome]] <- plt
  
  print(paste0(m,"/",length(vgam_models)))
  
}

Logitlegend <- get_legend(logit_plots$SleepTime)

plot_grid(ncol = 4,
          logit_plots$SleepTime + theme(legend.position = "none"),
          logit_plots$SleepQuality + theme(legend.position = "none"),
          logit_plots$DifficultFallAsleep + theme(legend.position = "none"),
          logit_plots$WakingUp + theme(legend.position = "none"),
          logit_plots$WakingUpEarly + theme(legend.position = "none"),
          logit_plots$Reflux + theme(legend.position = "none"),
          logit_plots$Snoring + theme(legend.position = "none"),
          Logitlegend)









#pred_link <- predict(m_npo, newdata = newdat, type = "link", se.fit = TRUE)
#fit_mat <- pred_link$fitted.values
#se_mat <- pred_link$se.fit

#pred_long <- as.data.frame(fit_mat)
#pred_long$CoffeeConsumptionLevel <- newdat$CoffeeConsumptionLevel
#pred_long <- pivot_longer(pred_long, cols = -CoffeeConsumptionLevel, names_to = "threshold", values_to = "link")
#pred_long$thr_index <- sub(".*=", "", pred_long$threshold)
#pred_long$thr_index <- sub("])", "", pred_long$thr_index)
#pred_long$thr_index <- as.numeric(pred_long$thr_index)

#pred_long$CoffeeConsumptionLevel <- factor(pred_long$CoffeeConsumptionLevel, levels = c("NONE","LOW","MODERATE","HIGH"))

#nmax <- max(pred_long$thr_index)

#ggplot(pred_long, aes(x = thr_index, y = link, color = CoffeeConsumptionLevel)) +
#  geom_line() + geom_jitter() +
#  labs(x = "Threshold index", y = "Cumulative logit (log-odds of Y <= j)", color = "Coffee consumption") +
#  scale_color_manual(values = c("black","burlywood2","cadetblue4","coral2")) +
#  scale_x_continuous(breaks = seq(1, nmax, 1)) +
#  theme_classic() +
#  theme(axis.text = element_text(size = 10, color = "black"))



##### Plot category probabilities and absolute differences #####

prob_plots <- list()
prob_data <- list()
prob_diffs <- list()

factor_levels <- list(
  "SleepTime" = c("5 hours","6 hours","7 hours", "8 hours", "9 hours", "10 hours or more"),
  "SleepQuality" = c("Good","Somewhat good","Bad", "Very bad"),
  "DifficultFallAsleep" = c("Less than once per week","1-2 times/week","3-6 times/week", "Almost every night"),
  "WakingUp" = c("Less than once per week","1-2 times/week","3-6 times/week", "Almost every night"),
  "WakingUpEarly" = c("Less than once per week","1-2 times/week","3-6 times/week", "Almost every night"),
  "Reflux" = c("Less than once per week","1-2 times/week","3-6 times/week", "Almost every night"),
  "Snoring" = c("Seldom","Sometimes","Often", "Very often")
)



for(m in 1:length(vgam_models)) {
  
  outcome <- names(vgam_models[m])
  
  pred_probs <- ggpredict(vgam_models[[m]], term = "CoffeeConsumptionLevel")
  df <- as.data.frame(pred_probs)
  df$response.level <- sub(".*= ","", df$response.level)
  df$response.level <- sub("]","", df$response.level)
  
  df$response.level <- factor(df$response.level, levels = factor_levels[[m]])
  
  prob_data[[outcome]] <- df
  
  plt <- ggplot(df, aes(x = x, y = predicted, color = response.level)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.3)) +
    labs(x = "Coffee consumption", y = "Predicted probability", color = outcome) +
    guides(color = guide_legend(ncol = 2)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10, color = "black"), axis.text.y = element_text(size = 10, color = "black"), legend.position = "bottom", legend.title = element_blank()) +
    ggtitle(outcome)
  
  if(m == 1) {
    plt <- plt + scale_color_manual(values = c("coral","coral1","coral2","coral3","coral4","black"))
  } else if (m == 2) {
    plt <- plt + scale_color_manual(values = c("cadetblue1","cadetblue2","cadetblue3","cadetblue4"))
  } else if (m %in% c(3,4,5,6)) {
    plt <- plt + scale_color_manual(values = c("burlywood1","burlywood2","burlywood3","burlywood4"))
  } else {
    plt <- plt + scale_color_manual(values = c("darkolivegreen1","darkolivegreen2","darkolivegreen3","darkolivegreen4"))
  }
  
  prob_plots[[outcome]] <- plt
  
  print(paste0(m,"/",length(vgam_models)))
  
}

problegend1 <- get_legend(prob_plots$SleepTime)
problegend2 <- get_legend(prob_plots$SleepQuality)
problegend3 <- get_legend(prob_plots$DifficultFallAsleep)
problegend4 <- get_legend(prob_plots$Snoring)

legends <- plot_grid(ncol = 1,
                     problegend1,
                     problegend2,
                     problegend3,
                     problegend4)


plot_grid(ncol = 4,
          prob_plots$SleepTime + theme(legend.position = "none"),
          prob_plots$SleepQuality + theme(legend.position = "none"),
          prob_plots$DifficultFallAsleep + theme(legend.position = "none"),
          prob_plots$WakingUp + theme(legend.position = "none"),
          prob_plots$WakingUpEarly + theme(legend.position = "none"),
          prob_plots$Reflux + theme(legend.position = "none"),
          prob_plots$Snoring + theme(legend.position = "none"),
          problegend1)



plot_grid(ncol = 2,
          prob_plots$SleepTime,
          prob_plots$SleepQuality,
          prob_plots$DifficultFallAsleep,
          prob_plots$Snoring
)




pred_probs <- ggpredict(vgam_models$SleepTime, term = "CoffeeConsumptionLevel")
df <- as.data.frame(pred_probs)
df$response.level <- sub(".*= ","", df$response.level)
df$response.level <- sub("]","", df$response.level)

df$response.level <- factor(df$response.level, levels = factor_levels$SleepTime)



##### Compute pairwise absolute risk differences (ARDs) for clm models ##### 

DiffData <- structure(list(response.level = character(), variable = character(), value = numeric(), outcome = character()), class = "data.frame")

for(d in 1:length(prob_data)) {
  
  outcome <- names(prob_data[d])
  
  dftmp <- prob_data[[d]]
  
  df_wide <- dftmp %>%
    dplyr::select("response.level", "x", "predicted") %>%
    tidyr::pivot_wider(
      names_from = x,
      values_from = predicted
    )
 
  df_wide <- df_wide %>%
    mutate(
      "LOW-NONE" = abs(LOW - NONE),
      "MODERATE-NONE" = abs(MODERATE - NONE),
      "HIGH-NONE" = abs(HIGH - NONE),
      "HIGH-LOW" = abs(HIGH - LOW),
      "HIGH-MODERATE" = abs(HIGH - MODERATE),
      "MODERATE-LOW" = abs(MODERATE - LOW)
    )
  
  df_wide[,c("NONE","LOW","MODERATE","HIGH")] <- NULL
  
  df_long <- melt(df_wide, id.vars = "response.level")
  
  df_long$outcome <- outcome
  
  DiffData <- rbind(DiffData, df_long)
}

DiffData$percent <- round(DiffData$value*100, digits = 2)

ggplot(DiffData, aes(x = response.level, y = variable, fill = percent)) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(aes(label = paste0(percent,"%")), size = 3.5, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "lightskyblue3") +
  facet_wrap(~outcome, scales = "free_x", nrow = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black", size = 12), axis.title = element_blank(), axis.text.y = element_text(color = "black", size = 12), legend.position = "none", strip.text.x = element_text(size = 12, color = "black"))


##### Compute pairwise signed risk differences for clm models ##### 

DiffData_signed <- structure(list(response.level = character(), variable = character(), value = numeric(), outcome = character()), class = "data.frame")
DiffData_signed_full <- data.frame()

for(d in 1:length(prob_data)) {
  
  outcome <- names(prob_data[d])
  
  dftmp <- prob_data[[d]]
  
  df_wide <- dftmp %>%
    dplyr::select("response.level", "x", "predicted") %>%
    tidyr::pivot_wider(
      names_from = x,
      values_from = predicted
    )
  
  df_wide <- df_wide %>%
    mutate(
      "LOW-NONE" = LOW - NONE,
      "MODERATE-NONE" = MODERATE - NONE,
      "HIGH-NONE" = HIGH - NONE,
      "HIGH-LOW" = HIGH - LOW,
      "HIGH-MODERATE" = HIGH - MODERATE,
      "MODERATE-LOW" = MODERATE - LOW
    )
  
  df_wide_tmp <- df_wide
  df_wide_tmp$outcome <- outcome
  DiffData_signed_full <- rbind(DiffData_signed_full, df_wide_tmp)
  
  df_wide[,c("NONE","LOW","MODERATE","HIGH")] <- NULL
  
  df_long <- melt(df_wide, id.vars = "response.level")
  
  df_long$outcome <- outcome
  
  DiffData_signed <- rbind(DiffData_signed, df_long)
}

DiffData_signed$percent <- round(DiffData_signed$value*100, digits = 2)
DiffData_signed$variable <- factor(DiffData_signed$variable, levels = rev(c("HIGH-NONE","MODERATE-NONE","LOW-NONE","HIGH-MODERATE","HIGH-LOW","MODERATE-LOW")))

DiffData_signed_small <- DiffData_signed[DiffData_signed$variable %in% c("HIGH-NONE","MODERATE-NONE","LOW-NONE"),]
DiffData_signed_small$variable <- factor(DiffData_signed_small$variable, levels = rev(c("HIGH-NONE","MODERATE-NONE","LOW-NONE")))

ggplot(DiffData_signed_small, aes(x = response.level, y = variable, fill = percent)) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(aes(label = paste0(percent,"%")), size = 3.5, fontface = "bold") +
  scale_fill_gradientn(colours = c("indianred", "white", "lightskyblue3"), rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  facet_wrap(~outcome, scales = "free_x", nrow = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black", size = 12), axis.title = element_blank(), axis.text.y = element_text(color = "black", size = 12), legend.position = "none", strip.text.x = element_text(size = 12, color = "black"))


## Make separate plots for later plotting together with SleepScore and ESS: 

DiffData_signed_small_1 <- DiffData_signed_small[DiffData_signed_small$outcome %in% c("SleepTime","SleepQuality","DifficultFallAsleep","WakingUp"),]
DiffData_signed_small_2 <- DiffData_signed_small[DiffData_signed_small$outcome %in% c("WakingUpEarly","Reflux","Snoring"),]

DiffData_signed_small_1$outcome <- factor(DiffData_signed_small_1$outcome, levels = c("SleepTime","SleepQuality","DifficultFallAsleep","WakingUp"))
DiffData_signed_small_2$outcome <- factor(DiffData_signed_small_2$outcome, levels = c("WakingUpEarly","Reflux","Snoring"))

r1 <- ggplot(DiffData_signed_small_1, aes(x = response.level, y = variable, fill = percent)) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(aes(label = paste0(percent,"%")), size = 3.5, fontface = "bold") +
  scale_fill_gradientn(colours = c("indianred", "white", "lightskyblue3"), rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  facet_wrap(~outcome, scales = "free_x", nrow = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black", size = 12), axis.title = element_blank(), axis.text.y = element_text(color = "black", size = 12), legend.position = "none", strip.text.x = element_text(size = 12, color = "black"))


r2 <- ggplot(DiffData_signed_small_2, aes(x = response.level, y = variable, fill = percent)) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(aes(label = paste0(percent,"%")), size = 3.5, fontface = "bold") +
  scale_fill_gradientn(colours = c("indianred", "white", "lightskyblue3"), rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  facet_wrap(~outcome, scales = "free_x", nrow = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black", size = 12), axis.title = element_blank(), axis.text.y = element_text(color = "black", size = 12), legend.position = "none", strip.text.x = element_text(size = 12, color = "black"))






#df_wide <- df %>%
#  dplyr::select("response.level", "x", "predicted") %>%
#  tidyr::pivot_wider(
#    names_from = x,
#    values_from = predicted
#  )

#df_wide <- df_wide %>%
#  mutate(
#    "LOW-NONE" = abs(LOW - NONE),
#    "MODERATE-NONE" = abs(MODERATE - NONE),
#    "HIGH-NONE" = abs(HIGH - NONE),
#    "HIGH-LOW" = abs(HIGH - LOW),
#    "HIGH-MODERATE" = abs(HIGH - MODERATE),
#    "MODERATE-LOW" = abs(MODERATE - LOW)
#)

#df_wide[,c("NONE","LOW","MODERATE","HIGH")] <- NULL

#df_long <- melt(df_wide, id.vars = "response.level")


#ggplot(df_long, aes(x = variable, y = response.level, fill = value)) +
#  geom_tile(color = "black") +
#  geom_text(aes(label = round(value, digits = 4)), size = 3) +
#  scale_fill_gradient(low = "white", high = "lightskyblue3") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"), legend.position = "none")






ggplot(df_long, aes(x = response.level, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(0.75), width = 0.5, color = "black") +
  labs(
    y = "Absolute probability difference",
    x = "SleepTime"
  ) +
  scale_fill_manual(values = c("cadetblue3","brown","cornsilk2","gold2","olivedrab4","gray30")) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12, color = "black"), axis.text.y = element_text(size = 12, color = "black")) +
  scale_y_continuous(expand = c(0,0))




ggplot(df_wide, aes(x = response.level, y = absdiff_low_none)) +
  geom_col() +
  labs(
    y = "Absolute probability difference (Low vs None)",
    x = "Outcome category"
  ) +
  theme_classic()


##############################################################################################################################
# Check for non-linearity
# packages: ordinal, effects, MASS, splines
##############################################################################################################################

library(MASS) # version 7.3.64
library(splines) # version 4.4.2
library(ggeffects) # version 2.2.1
library(performance) # version 0.15.2

x_subTest <- x_sub

x_subTest$coffee_num <- with(x_subTest, case_when(
  CoffeeConsumption_label == "No coffee consumption" ~ 0,
  CoffeeConsumption_label == "1-3 times/month" ~ 0.07,
  CoffeeConsumption_label == "1-2 times/week" ~ 0.21,
  CoffeeConsumption_label == "3-4 times/week" ~ 0.5,
  CoffeeConsumption_label == "5-6 times/week" ~ 0.8,
  CoffeeConsumption_label == "1 time/day" ~ 1,
  CoffeeConsumption_label == "2 times/day" ~ 2,
  CoffeeConsumption_label == "3 times/day" ~ 3,
  CoffeeConsumption_label == "4 times/day" ~ 4,
  CoffeeConsumption_label == "5 times/day or more" ~ 5
))


# QAIC helper for quasi-Poisson models
qaic <- function(model) {
  fam <- tryCatch(family(model)$family, error = function(e) NA)
  
  if (!is.na(fam) && fam == "quasipoisson") {
    dev <- deviance(model)
    df <- df.residual(model)
    disp <- dev/df
    k <- length(coef(model))
    return(dev / disp + 2 * k)
  } else {
    return(NA)
  }
} 


get_metrics <- function(m) {
  
  fam <- tryCatch(family(m)$family, error = function(e) NA)
  
  dev <- tryCatch(deviance(m), error = function(e) NA)
  if (is.null(dev)) dev <- NA_real_
  
  data.frame(
    model = deparse(formula(m)[1]),
    AIC = suppressWarnings(tryCatch(AIC(m), error = function(e) NA)),
    BIC = suppressWarnings(tryCatch(BIC(m), error = function(e) NA)),
    QAIC = qaic(m),
    Deviance = dev,
    R2 = suppressWarnings(tryCatch(performance::r2(m)$R2, error = function(e) NA))
  )
}


fit_models <- function(outcome, data, confounders, type) {
  form_4cat <- as.formula(paste(outcome, "~ CoffeeConsumptionLevel +", confounders))
  form_10cat <- as.formula(paste(outcome, "~ CoffeeConsumption_label +", confounders))
  form_lin <- as.formula(paste(outcome, "~ coffee_num +", confounders))
  form_spline <- as.formula(paste(outcome, "~ ns(coffee_num, df = 3) +", confounders))
  
  if (type == "cat") {
    m1 <- clm(data = data, formula = form_4cat)
    m2 <- clm(data = data, formula = form_10cat)
    m3 <- clm(data = data, formula = form_lin)
    m4 <- clm(data = data, formula = form_spline)
  } else {
    m1 <- glm(data = data, family = quasipoisson, formula = form_4cat)
    m2 <- glm(data = data, family = quasipoisson, formula = form_10cat)
    m3 <- glm(data = data, family = quasipoisson, formula = form_lin)
    m4 <- glm(data = data, family = quasipoisson, formula = form_spline)
  }
  
  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4)
  
}


save_effect_plot <- function(model, outcome_name, model_name, out_dir = "New_figures_20251201/non-linearity", type) {
  
  if (model_name == "m1") {
    
    pred <- tryCatch({
      ggpredict(model = model, terms = "CoffeeConsumptionLevel [all]")
    }, error = function(e) NULL)
    
    if (outcome_name %in% sleep_vars) {
      lvls <- levels(x_subTest[, outcome_name])
      
      pred$response.level <- factor(pred$response.level,
                                        levels = levels(factor(pred$response.level)),
                                        labels = lvls)
    }
    
    p <- plot(pred) +
      ggtitle(paste0(outcome_name, "-", model_name)) +
      theme_classic(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1, color = "black"))
    
  } else if (model_name == "m2") {
    
    pred <- tryCatch({
      ggpredict(model = model, terms = "CoffeeConsumption_label [all]")
    }, error = function(e) NULL)
    
    if (outcome_name %in% sleep_vars) {
      lvls <- levels(x_subTest[, outcome_name])
      
      pred$response.level <- factor(pred$response.level,
                                    levels = levels(factor(pred$response.level)),
                                    labels = lvls)
    }
    
    p <- plot(pred) +
      ggtitle(paste0(outcome_name, "-", model_name)) +
      theme_classic(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1, color = "black"))
    
  } else {
    
    pred <- tryCatch({
      ggpredict(model = model, terms = "coffee_num [all]")
    }, error = function(e) NULL)
    
    if (outcome_name %in% sleep_vars) {
      lvls <- levels(x_subTest[, outcome_name])
      
      pred$response.level <- factor(pred$response.level,
                                    levels = levels(factor(pred$response.level)),
                                    labels = lvls)
    }
    
    p <- plot(pred) +
      ggtitle(paste0(outcome_name, "-", model_name)) +
      theme_classic(base_size = 13) +
      theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(color = "black"))
  }
  
  if (is.null(pred)) return(NULL)

  filename <- file.path(out_dir, paste0(outcome_name, "_", model_name, ".svg"))
  
  if (type == "cat") {
    ggsave(filename,
           plot = p,
           width = 8,
           height = 8,
           units = "in",
           dpi = "retina")
  } else if (type == "score") {
    ggsave(filename,
           plot = p,
           width = 4,
           height = 4,
           units = "in",
           dpi = "retina")
  }
  

  
  return(p)
}


compare_models <- function(models, outcomer_type) {
  
  if (outcomer_type == "cat") {
    
    comp1 <- anova(models$m1, models$m2)
    comp2 <- anova(models$m3, models$m4)
    p1 <- comp1$`Pr(>Chisq)`[2]
    p2 <- comp2$`Pr(>Chisq)`[2]
    
  } else {
    
    comp1 <- anova(models$m1, models$m2, test = "F")
    comp2 <- anova(models$m3, models$m4, test = "F")
    p1 <- comp1$`Pr(>F)`[2]
    p2 <- comp2$`Pr(>F)`[2]
    
  }
  
  # Extract AIC/BIC and pseudo-R2 (Nagelkerke)
 
  model_fits <- lapply(models, get_metrics)
   
#  model_fits <- lapply(models, function(m) {
#    
#    data.frame(
#      AIC = tryCatch(AIC(m), error = function(e) NA),
#      BIC = tryCatch(BIC(m), error = function(e) NA),
#      R2 = tryCatch(performance::r2(m)$R2, error = function(e) NA)
#    )
#    
#  })
  
  fits_df <- bind_rows(model_fits, .id = "model")
  
  list(
    comparisons = data.frame(comp = c("M1 vs M2","M3 vs M4"), p_value = c(p1, p2)), fits = fits_df
  )
  
}


sleep_vars <- c("SleepTime_label", "cqsh001_label", "cqsh003_label", "cqsh004_label", "cqsh005_label", "cqsh014_label", "cqsh015_label")
score_vars <- c("Sleep_tot_new","ESS_tot")

confounders <- "AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication"

results_list <- list()
fits_all <- list()
plots <- list()

for (v in sleep_vars) {
  mods <- fit_models(outcome = v, data = x_subTest, confounders = confounders, type = "cat")
  
  for (m_name in names(mods)) {
    p <- save_effect_plot(mods[[m_name]], outcome_name = v, model_name = m_name, type = "cat")
    plots[[paste(v, m_name, sep = "_")]] <- p
  }
  
  res <- compare_models(mods, outcomer_type = "cat")
  results_list[[v]] <- res$comparisons
  fits_all[[v]] <- mutate(res$fits, outcome = v)
}

for (v in score_vars) {
  
  if (v == "ESS_tot") {
    mods <- fit_models(outcome = v, data = x_subTest, confounders = paste0(confounders, " + cqsh002"), type = "score")
  } else {
    mods <- fit_models(outcome = v, data = x_subTest, confounders = confounders, type = "score")
  }

  for (m_name in names(mods)) {
    p <- save_effect_plot(mods[[m_name]], outcome_name = v, model_name = m_name, type = "score")
    plots[[paste(v, m_name, sep = "_")]] <- p
  }
  
  res <- compare_models(mods, outcomer_type = "score")
  results_list[[v]] <- res$comparisons
  fits_all[[v]] <- mutate(res$fits, outcome = v)
}


results <- bind_rows(lapply(names(results_list), function(v) {
  mutate(results_list[[v]], outcome = v)
}))

fits_summary <- bind_rows(fits_all)

results_table <- results %>%
  mutate(sig = ifelse(p_value < 0.05, "*", "")) %>%
  pivot_wider(names_from = comp, values_from = p_value) %>%
  arrange(outcome)

fits_summary_mean <- fits_summary %>%
  group_by(outcome) %>%
  summarise(across(c(AIC, BIC, R2, QAIC, Deviance), mean, na.rm = TRUE))

summary_table <- left_join(results_table, fits_summary_mean, by = "outcome")

print(summary_table)

write.table(as.data.frame(summary_table), file = "New_figures_20251201/non-linearity/Summary_table.txt", row.names = FALSE, sep = '\t', quote = FALSE)

### Make plots

p_sleeptime <- plot_grid(ncol = 4,
                         plots$SleepTime_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("Original categories") + ylab("SleepTime"),
                         plots$SleepTime_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("Aggregated categories"),
                         plots$SleepTime_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("Numeric midpoint-Linear model"),
                         plots$SleepTime_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("Numeric midpoint-Spline model")
)

p_sleepquality <- plot_grid(ncol = 4,
                         plots$cqsh001_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("SleepQuality"),
                         plots$cqsh001_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                         plots$cqsh001_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                         plots$cqsh001_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_difffallasleep <- plot_grid(ncol = 4,
                         plots$cqsh003_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("DifficultFallAsleep"),
                         plots$cqsh003_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                         plots$cqsh003_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                         plots$cqsh003_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_wakingup <- plot_grid(ncol = 4,
                         plots$cqsh004_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("WakingUp"),
                         plots$cqsh004_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                         plots$cqsh004_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                         plots$cqsh004_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_wakingupearly <- plot_grid(ncol = 4,
                        plots$cqsh005_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("WakingUpEarly"),
                        plots$cqsh005_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                        plots$cqsh005_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                        plots$cqsh005_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_reflux <- plot_grid(ncol = 4,
                      plots$cqsh014_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("Reflux"),
                      plots$cqsh014_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                      plots$cqsh014_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                      plots$cqsh014_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_snoring <- plot_grid(ncol = 4,
                       plots$cqsh015_label_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("Snoring"),
                       plots$cqsh015_label_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                       plots$cqsh015_label_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                       plots$cqsh015_label_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_sleepscore <- plot_grid(ncol = 4,
                       plots$Sleep_tot_new_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("SleepScore"),
                       plots$Sleep_tot_new_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                       plots$Sleep_tot_new_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                       plots$Sleep_tot_new_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_ESSscore <- plot_grid(ncol = 4,
                          plots$ESS_tot_m2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + ggtitle("") + ylab("ESS"),
                          plots$ESS_tot_m1 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                          plots$ESS_tot_m3 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle(""),
                          plots$ESS_tot_m4 + theme(axis.text.x = element_blank(), axis.title = element_blank()) + ggtitle("")
)

p_all <- plot_grid(ncol = 1, rel_heights = c(1.2,1,1,1,1,1,1,0.5,0.5),
                   p_sleeptime,
                   p_sleepquality,
                   p_difffallasleep,
                   p_wakingup,
                   p_wakingupearly,
                   p_reflux,
                   p_snoring,
                   p_sleepscore,
                   p_ESSscore)

plot(p_all)








#model_spline <- clm(data = x_subTest, formula = cqsh001_label ~ ns(coffee_num, df = 3) + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12)
#summary(model_spline)

#plot(allEffects(model_spline), "coffee_num")

# Plot predicted probabilities
#pred <- ggpredict(model_spline, terms = "coffee_num [all]")
#plot(pred)

# Test whether there are "significant" U-shape
#sq_linear <- clm(data = x_subTest, formula = cqsh001_label ~ coffee_num + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12)
#sq_spline <- clm(data = x_subTest, formula = cqsh001_label ~ ns(coffee_num, df = 3) + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12)
#sq_old <- clm(data = x_sub, formula = cqsh001_label ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12)
#sq_old_all <- clm(data = x_sub, formula = cqsh001_label ~ CoffeeConsumption_label + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12)

#cpmtest <- anova(sq_linear, sq_spline)

#pred_old <- ggpredict(sq_old, terms = "CoffeeConsumptionLevel [all]")
#lvls <- levels(x_sub$cqsh001_label)

#pred_old$response.level <- factor(pred_old$response.level,
#                         levels = c("1","2","3","4","5"),
#                         labels = lvls)

#plot(pred_old)

#pred_old_all <- ggpredict(sq_old_all, terms = "CoffeeConsumption_label [all]")
#plot(pred_old_all)

#anova(sq_old, sq_old_all)
#anova(sq_old_all, sq_linear)
#anova(sq_old_all, sq_spline)


##############################################################################################################################
# Interaction analysis
##############################################################################################################################

interaction_mods <- list()

for (v in sleep_vars) {
  m_main <- clm(data = x_sub, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
  m_age <- clm(data = x_sub, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
  m_sex <- clm(data = x_sub, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * Sex + AgeAtVisitOne + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
  m_bmi <- clm(data = x_sub, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * BMI + AgeAtVisitOne + Sex + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
  
  interaction_mods[[v]] <- list(m_main = m_main, m_age = m_age, m_sex = m_sex, m_bmi = m_bmi)
}

for (v in score_vars) {
  
  if (v == "ESS_tot") {
    
    m_main <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002")))
    m_age <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002")))
    m_sex <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * Sex + AgeAtVisitOne + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002")))
    m_bmi <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * BMI + AgeAtVisitOne + Sex + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002")))
    
  } else {
    
    m_main <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
    m_age <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
    m_sex <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * Sex + AgeAtVisitOne + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
    m_bmi <- glm(data = x_sub, family = quasipoisson, formula = as.formula(paste(v, "~ CoffeeConsumptionLevel * BMI + AgeAtVisitOne + Sex + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication")))
    
  }
  
  interaction_mods[[v]] <- list(m_main = m_main, m_age = m_age, m_sex = m_sex, m_bmi = m_bmi)
  
}

int_results <- structure(list(outcome = character(), int_age_p = numeric(), int_sex_p = numeric(), int_bmi_p = numeric()), class = "data.frame")

for (i in 1:length(interaction_mods)) {
  
  if (names(interaction_mods[i]) %in% score_vars) {
    
    comp_age <- anova(interaction_mods[[i]]$m_main, interaction_mods[[i]]$m_age, test = "F")
    comp_sex <- anova(interaction_mods[[i]]$m_main, interaction_mods[[i]]$m_sex, test = "F")
    comp_bmi <- anova(interaction_mods[[i]]$m_main, interaction_mods[[i]]$m_bmi, test = "F")
    
    int_results <- rbind(int_results, data.frame(outcome = names(interaction_mods[i]), int_age_p = comp_age$`Pr(>F)`[2], int_sex_p = comp_sex$`Pr(>F)`[2], int_bmi_p = comp_bmi$`Pr(>F)`[2]))
    
  } else {
    
    comp_age <- anova(interaction_mods[[i]]$m_main, interaction_mods[[i]]$m_age)
    comp_sex <- anova(interaction_mods[[i]]$m_main, interaction_mods[[i]]$m_sex)
    comp_bmi <- anova(interaction_mods[[i]]$m_main, interaction_mods[[i]]$m_bmi)
    
    int_results <- rbind(int_results, data.frame(outcome = names(interaction_mods[i]), int_age_p = comp_age$`Pr(>Chisq)`[2], int_sex_p = comp_sex$`Pr(>Chisq)`[2], int_bmi_p = comp_bmi$`Pr(>Chisq)`[2]))

  }
  
}

colnames(int_results) <- c("Outcome","Coffee*Age","Coffee*Sex","Coffee*BMI")

int_results$Outcome[int_results$Outcome == "SleepTime_label"] <- "SleepTime"
int_results$Outcome[int_results$Outcome == "cqsh001_label"] <- "SleepQuality"
int_results$Outcome[int_results$Outcome == "cqsh003_label"] <- "DifficultFallAsleep"
int_results$Outcome[int_results$Outcome == "cqsh004_label"] <- "WakingUp"
int_results$Outcome[int_results$Outcome == "cqsh005_label"] <- "WakingUpEarly"
int_results$Outcome[int_results$Outcome == "cqsh014_label"] <- "Reflux"
int_results$Outcome[int_results$Outcome == "cqsh015_label"] <- "Snoring"
int_results$Outcome[int_results$Outcome == "Sleep_tot_new"] <- "SleepScore"
int_results$Outcome[int_results$Outcome == "ESS_tot"] <- "ESS"

intres_long <- melt(int_results)

intres_long$Outcome <- factor(intres_long$Outcome, levels = rev(unique(intres_long$Outcome)))
intres_long <- intres_long %>% mutate(Sign = if_else(value < 0.05, "YES", "NO"))

ggplot(intres_long, aes(x = variable, y = Outcome, fill = Sign)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, digits = 3)), size = 3) +
  scale_fill_manual(values = c("white","lightskyblue2")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"), legend.position = "none")


# A closer look at the significant interactions

DifficultFallAsleep_BMI <- ggpredict(interaction_mods$cqsh003_label$m_bmi, terms = c("BMI", "CoffeeConsumptionLevel"))
WakingUp_Sex <- ggpredict(interaction_mods$cqsh004_label$m_sex, terms = c("CoffeeConsumptionLevel","Sex"))
ESS_Age <- ggpredict(interaction_mods$ESS_tot$m_age, terms = c("AgeAtVisitOne","CoffeeConsumptionLevel"))

DifficultFallAsleep_BMI$response.level <- factor(DifficultFallAsleep_BMI$response.level,
                              levels = levels(factor(DifficultFallAsleep_BMI$response.level)),
                              labels = levels(x_sub[, "cqsh003_label"]))

WakingUp_Sex$response.level <- factor(WakingUp_Sex$response.level,
                                      levels = levels(factor(WakingUp_Sex$response.level)),
                                      labels = levels(x_sub[, "cqsh004_label"]))



dfa_bmi <- plot(DifficultFallAsleep_BMI) +
  ggtitle("Difficulty falling asleep") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(color = "black"), legend.position = "bottom") +
  guides(color=guide_legend(title = "Coffee consumption: ")) +
  ylab("Predicted probability") +
  facet_wrap(~response.level, nrow = 1)

wus <- plot(WakingUp_Sex) +
  ggtitle("Waking up") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), legend.position = "bottom") +
  guides(color=guide_legend(title = "Sex: ")) +
  ylab("Predicted probability") +
  xlab("Coffee consumption") +
  facet_wrap(~response.level, nrow = 1)

essa <- plot(ESS_Age) +
  ggtitle("ESS") +
  theme_classic(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(color = "black")) +
  ylab("Predicted counts") +
  xlab("Age")

intplots1 <- plot_grid(ncol = 2, rel_widths = c(1,0.5),
          dfa_bmi,
          essa + theme(legend.position = "none"))

plot_grid(ncol = 1,
          intplots1,
          wus)


##############################################################################################################################
# Compute and visualize predicted probabilities
# package ggeffects
##############################################################################################################################


Pred_List <- list(
  "SleepTime" = ggpredict(RegModels_Sleep$SleepTime, terms = c("CoffeeConsumptionLevel")),
  "SleepQuality" = ggpredict(RegModels_Sleep$SleepQuality, terms = c("CoffeeConsumptionLevel [all]")),
  "DifficultFallAsleep" = ggpredict(RegModels_Sleep$DifficultFallAsleep, terms = c("CoffeeConsumptionLevel [all]")),
  "WakingUp" = ggpredict(RegModels_Sleep$WakingUp, terms = c("CoffeeConsumptionLevel [all]")),
  "WakingUpEarly" = ggpredict(RegModels_Sleep$WakingUpEarly, terms = c("CoffeeConsumptionLevel [all]")),
  "Reflux" = ggpredict(RegModels_Sleep$Reflux, terms = c("CoffeeConsumptionLevel [all]")),
  "Snoring" = ggpredict(RegModels_Sleep$Snoring, terms = c("CoffeeConsumptionLevel [all]")),
  "SleepScore" = ggpredict(RegModels_Sleep$SleepScore, terms = c("CoffeeConsumptionLevel [all]")),
  "ESS" = ggpredict(RegModels_Sleep$ESS, terms = c("CoffeeConsumptionLevel [all]"))
)

Pred_List$SleepTime$response.level <- factor(Pred_List$SleepTime$response.level,
                                                 levels = levels(factor(Pred_List$SleepTime$response.level)),
                                                 labels = levels(x_sub[, "SleepTime_label"]))

Pred_List$SleepQuality$response.level <- factor(Pred_List$SleepQuality$response.level,
                                             levels = levels(factor(Pred_List$SleepQuality$response.level)),
                                             labels = levels(x_sub[, "cqsh001_label"]))

Pred_List$DifficultFallAsleep$response.level <- factor(Pred_List$DifficultFallAsleep$response.level,
                                                levels = levels(factor(Pred_List$DifficultFallAsleep$response.level)),
                                                labels = levels(x_sub[, "cqsh003_label"]))

Pred_List$WakingUp$response.level <- factor(Pred_List$WakingUp$response.level,
                                                       levels = levels(factor(Pred_List$WakingUp$response.level)),
                                                       labels = levels(x_sub[, "cqsh004_label"]))

Pred_List$WakingUpEarly$response.level <- factor(Pred_List$WakingUpEarly$response.level,
                                            levels = levels(factor(Pred_List$WakingUpEarly$response.level)),
                                            labels = levels(x_sub[, "cqsh005_label"]))

Pred_List$Reflux$response.level <- factor(Pred_List$Reflux$response.level,
                                                 levels = levels(factor(Pred_List$Reflux$response.level)),
                                                 labels = levels(x_sub[, "cqsh014_label"]))

Pred_List$Snoring$response.level <- factor(Pred_List$Snoring$response.level,
                                          levels = levels(factor(Pred_List$Snoring$response.level)),
                                          labels = levels(x_sub[, "cqsh015_label"]))


shades1 <- as.data.frame(Pred_List$SleepTime) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )

shades2 <- as.data.frame(Pred_List$SleepQuality) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )

shades3 <- as.data.frame(Pred_List$DifficultFallAsleep) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )

shades4 <- as.data.frame(Pred_List$WakingUp) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )

shades5 <- as.data.frame(Pred_List$WakingUpEarly) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )

shades6 <- as.data.frame(Pred_List$Reflux) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )

shades7 <- as.data.frame(Pred_List$Snoring) %>%
  distinct(response.level) %>%
  arrange(response.level) %>%
  mutate(
    rl_num = as.numeric(as.factor(response.level)),
    xmin = rl_num - 0.5,
    xmax = rl_num + 0.5,
    shade = rl_num %% 2 == 0
  )


predplot1 <- ggplot(Pred_List$SleepTime, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades1, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank(), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size = 12, color = "black")) +
  ylab("Predicted probability") + ggtitle("Sleep time")

predplot2 <- ggplot(Pred_List$SleepQuality, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades2, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank()) +
  ylab("Predicted probability") + ggtitle("Sleep quality")

predplot3 <- ggplot(Pred_List$DifficultFallAsleep, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades3, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank()) +
  ylab("Predicted probability") + ggtitle("Difficulty falling asleep")

predplot4 <- ggplot(Pred_List$WakingUp, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades4, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank()) +
  ylab("Predicted probability") + ggtitle("Frequency waking up")

predplot5 <- ggplot(Pred_List$WakingUpEarly, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades5, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank()) +
  ylab("Predicted probability") + ggtitle("Waking up too early")

predplot6 <- ggplot(Pred_List$Reflux, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades6, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank()) +
  ylab("Predicted probability") + ggtitle("Frequency of reflux")

predplot7 <- ggplot(Pred_List$Snoring, aes(x = response.level, y = predicted)) +
  geom_rect(data = filter(shades7, shade),
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey95") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.5), position = position_dodge(width = 1)) +
  geom_point(size = 4, position = position_dodge(width = 1), aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank()) +
  ylab("Predicted probability") + ggtitle("Frequency of loud snoring")

predplot8 <- ggplot(Pred_List$SleepScore, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.25), position = position_dodge(width = 1)) +
  geom_point(size = 4, aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank(), legend.position = "none") +
  ylab("Predicted counts") + ggtitle("Sleep score")

predplot9 <- ggplot(Pred_List$ESS, aes(x = x, y = predicted)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, colour = x, width = 0.25), position = position_dodge(width = 1)) +
  geom_point(size = 4, aes(fill = x), colour = "black", pch = 21) +
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x, color = NULL), alpha = 0.15) +
  scale_fill_manual(values = c("white","lightskyblue","lightblue4","black")) +
  scale_color_manual(values = c("black","black","black","black")) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"), axis.text.x = element_text(size = 12, angle = 60, hjust = 1, color = "black"), axis.title.x = element_blank(), legend.position = "none") +
  ylab("Predicted counts") + ggtitle("ESS score")


predplots_main <- plot_grid(ncol = 3, align = "hv",
          predplot1 + theme(legend.position = "none"),
          predplot2 + theme(legend.position = "none"),
          predplot3 + theme(legend.position = "none"),
          predplot4 + theme(legend.position = "none"),
          predplot5 + theme(legend.position = "none"),
          predplot6 + theme(legend.position = "none"),
          predplot7 + theme(legend.position = "none"),
          predplot8,
          predplot9
)

plot_grid(nrow = 2, rel_heights = c(1,0.05),
          predplots_main,
          get_legend(predplot1))

##### Compute pairwise absolute risk differences (ARDs) for poisson models #####

DiffData_poisson <- structure(list(outcome = character(), variable = character(), value = numeric()), class = "data.frame")


SC_ARDs <- as.data.frame(Pred_List$SleepScore) %>%
  dplyr::select("x", "predicted") %>%
  tidyr::pivot_wider(
    names_from = x,
    values_from = predicted
)

SC_ARDs <- SC_ARDs %>%
  mutate(
    "LOW-NONE" = LOW - NONE,
    "MODERATE-NONE" = MODERATE - NONE,
    "HIGH-NONE" = HIGH - NONE,
    "HIGH-LOW" = HIGH - LOW,
    "HIGH-MODERATE" = HIGH - MODERATE,
    "MODERATE-LOW" = MODERATE - LOW
)

SC_ARDs_full <- SC_ARDs
SC_ARDs_full$outcome <- "SleepScore"

SC_ARDs[,c("NONE","LOW","MODERATE","HIGH")] <- NULL

SC_ARDs$outcome <- "SleepScore"

SC_ARDs_long <- melt(SC_ARDs, id.vars = "outcome")

DiffData_poisson <- rbind(DiffData_poisson, SC_ARDs_long)


ESS_ARDs <- as.data.frame(Pred_List$ESS) %>%
  dplyr::select("x", "predicted") %>%
  tidyr::pivot_wider(
    names_from = x,
    values_from = predicted
  )

ESS_ARDs <- ESS_ARDs %>%
  mutate(
    "LOW-NONE" = LOW - NONE,
    "MODERATE-NONE" = MODERATE - NONE,
    "HIGH-NONE" = HIGH - NONE,
    "HIGH-LOW" = HIGH - LOW,
    "HIGH-MODERATE" = HIGH - MODERATE,
    "MODERATE-LOW" = MODERATE - LOW
  )

ESS_ARDs_full <- ESS_ARDs
ESS_ARDs_full$outcome <- "ESS"

ESS_ARDs[,c("NONE","LOW","MODERATE","HIGH")] <- NULL
ESS_ARDs$outcome <- "ESS"

ESS_ARDs_long <- melt(ESS_ARDs, id.vars = "outcome")

DiffData_poisson <- rbind(DiffData_poisson, ESS_ARDs_long)

DiffData_poisson$variable <- factor(DiffData_poisson$variable, levels = rev(c("HIGH-NONE","MODERATE-NONE","LOW-NONE","HIGH-MODERATE","HIGH-LOW","MODERATE-LOW")))

DiffData_poisson$value_rounded <- round(DiffData_poisson$value, digits = 2)

DiffData_poisson$outcome <- factor(DiffData_poisson$outcome, levels = c("SleepScore","ESS"))

DiffData_poisson_small <- DiffData_poisson[DiffData_poisson$variable %in% c("HIGH-NONE","MODERATE-NONE","LOW-NONE"),]
DiffData_poisson_small$variable <- factor(DiffData_poisson_small$variable, levels = rev(c("HIGH-NONE","MODERATE-NONE","LOW-NONE")))

poisplot <- ggplot(DiffData_poisson_small, aes(x = outcome, y = variable, fill = value_rounded)) +
  geom_tile(color = "black", linewidth = 0.25) +
  geom_text(aes(label = value_rounded), size = 3.5, fontface = "bold") +
  scale_fill_gradientn(colours = c("indianred", "white", "lightskyblue3"), rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  facet_wrap(~outcome, scales = "free_x", nrow = 1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title = element_blank(), axis.text.y = element_text(color = "black", size = 12), legend.position = "none", strip.text.x = element_text(size = 12, color = "black"))

## Plot together with risk differences from clm models

ARDs_row2 <- plot_grid(nrow = 1, align = "h", axis = "b", rel_widths = c(1, 0.20), r2, poisplot + theme(axis.text.y = element_blank()))

plot_grid(nrow = 2, r1, ARDs_row2)


## Prepare for supplementary table with predicted probabilities and absolute differences

ARD_ALL <- rbind(SC_ARDs_full, ESS_ARDs_full)

dd_tmp <- DiffData_signed_full[,c(2:12,1)]

ARD_ALL$response.level <- NA
ARD_ALL <- rbind(dd_tmp, ARD_ALL)
ARD_ALL[,c("HIGH-LOW","MODERATE-LOW","HIGH-MODERATE")] <- NULL


ARD_ALL_2 <- ARD_ALL %>%
  dplyr::group_by(outcome) %>%
  mutate(
    has_sub = any(!is.na(response.level))
  ) %>%
  ungroup()

ARD_table <- ARD_ALL_2 %>%
  mutate(
    row_label = case_when(
      has_sub & !is.na(response.level) ~ paste0("  ", response.level),
      has_sub & is.na(response.level) ~ NA_character_,
      !has_sub ~ outcome
    ),
    row_group = if_else(has_sub, outcome, "Sleep and sleepiness scores")
  ) %>%
  filter(!is.na(row_label)) %>%
  dplyr::select(row_group, row_label, "NONE":"HIGH", "LOW-NONE":"HIGH-NONE")


gt_table <- ARD_table %>%
  gt(
    rowname_col = "row_label",
    groupname_col = "row_group"
  ) %>%
  tab_spanner(
    label = "Predicted probabilities",
    columns = "NONE":"HIGH"
  ) %>%
  tab_spanner(
    label = "Signed absolute differences",
    columns = "LOW-NONE":"HIGH-NONE"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 3
  )

gt_table <- gt_table %>%
  cols_width(
    stub() ~ px(140),
    "NONE":"HIGH-NONE" ~ px(90)
  )

gt_table <- gt_table %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

gt_table <- gt_table %>%
  tab_style(
    style = cell_text(color = "grey30"),
    locations = cells_stub(rows = starts_with("  "))
  )

gt_table <- gt_table %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  )

gt_table <- gt_table %>%
  cols_align(
    align = "center",
    columns = "NONE":"HIGH-NONE"
  )

gt_table <- gt_table %>%
  tab_options(
    column_labels.font.size = px(14),
    stub.font.size = px(14),
    row_group.font.size = px(14),
    table.font.size = px(14)
  )

print(gt_table)

##############################################################################################################################
# Sensitivity analysis for ESS model
##############################################################################################################################

library(MASS) # version 7.3.64

ESS_robust_lin <- rlm(ESS_tot ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002, data = x_sub)
ESS_neg_bino <- glm.nb(ESS_tot ~ CoffeeConsumptionLevel + AgeAtVisitOne + Sex + BMI + derived_smoke_status + TeaConsumption + Stress + LeisurePhysActivityPast12 + SleepMedication + cqsh002, data = x_sub)

anova(RegModels_Sleep$ESS, ESS_robust_lin, test = "F")
anova(RegModels_Sleep$ESS, ESS_neg_bino, test = "F")

pred_ESS_rlm <- ggpredict(ESS_robust_lin, terms = c("CoffeeConsumptionLevel [all]"))
pred_ESS_nb <- ggpredict(ESS_neg_bino, terms = c("CoffeeConsumptionLevel [all]"))

ESS_poi_p <- plot(Pred_List$ESS) + ggtitle("Predicted ESS counts\n(quasi-Poisson model)") + ylim(c(2,5)) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12, color = "black"), axis.text.y = element_text(size = 12, color = "black"), axis.title = element_text(size = 12, color = "black"))
ESS_rlm_p <- plot(pred_ESS_rlm) + ggtitle("Predicted ESS counts\n(robust linear model)") + ylim(c(2,5)) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12, color = "black"), axis.text.y = element_text(size = 12, color = "black"), axis.title = element_text(size = 12, color = "black"))
ESS_nb_p <- plot(pred_ESS_nb) + ggtitle("Predicted ESS counts\n(negative binomial model)") + ylim(c(2,5)) + theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12, color = "black"), axis.text.y = element_text(size = 12, color = "black"), axis.title = element_text(size = 12, color = "black"))

plot_grid(nrow = 1, ESS_poi_p, ESS_nb_p, ESS_rlm_p)

##############################################################################################################################
# Prepare supplementary tables for prediction results
# vgam models for sleep variables
# quasi-poisson model for SleepScore and ESS
##############################################################################################################################

CompletePredRes_1 <- data.frame()
CompletePredRes_2 <- data.frame()

for(d in 1:length(prob_data)) {
  
  outcome <- names(prob_data[d])
  
  dftmp <- as.data.frame(prob_data[[d]])
  
  dftmp$outcome <- outcome
  
  CompletePredRes_1 <- rbind(CompletePredRes_1, dftmp)
  
}


for(d in 1:length(Pred_List)) {
  
  outcome <- names(Pred_List[d])
  
  if(outcome %in% c("SleepScore","ESS")) {
      dftmp <- as.data.frame(Pred_List[[d]])
  
      dftmp$outcome <- outcome
  
      CompletePredRes_2 <- rbind(CompletePredRes_2, dftmp)
  }
}

write.table(CompletePredRes_1, file = "New_figures_20251201/PredictionData_SleepVariables_VGAMmodels", row.names = FALSE, quote = FALSE, sep = '\t')
write.table(CompletePredRes_2, file = "New_figures_20251201/PredictionData_SleepScoreESS_Poissonmodels", row.names = FALSE, quote = FALSE, sep = '\t')

##############################################################################################################################
# Variable importance
##############################################################################################################################

##### Sleep variables excluding Sleep Time

ImpRes_SleepVars <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(ImpRes_SleepVars) <- c("DF","Chisq","Pval","Term","SleepVar")

for(i in 1:length(RegModels_Sleep[!names(RegModels_Sleep) %in% c("SleepTime","SleepScore","ESS")])) {
  anova_results <- as.data.frame(car::Anova(RegModels_Sleep[!names(RegModels_Sleep) %in% c("SleepTime","SleepScore","ESS")][[i]], type = "II"))
  #anova_results <- as.data.frame(anova(RegModels_Sleep[!names(RegModels_Sleep) %in% c("SleepTime","SleepScore","ESS")][[i]]))
  colnames(anova_results) <- c("DF","Chisq","Pval")
  anova_results$Term <- rownames(anova_results)
  anova_results$SleepVar <- names(RegModels_Sleep[!names(RegModels_Sleep) %in% c("SleepTime","SleepScore","ESS")][i])
  ImpRes_SleepVars <- rbind(ImpRes_SleepVars, anova_results)
}

ImpRes_SleepVars$Term <- factor(ImpRes_SleepVars$Term, levels = rev(unique(ImpRes_SleepVars$Term)))
ImpRes_SleepVars <- ImpRes_SleepVars %>%
  dplyr::mutate(
    P_label = case_when(
      Pval >= 0.05 ~ "",
      Pval < 0.05 & Pval >= 0.001 ~ "*",
      Pval < 0.001 & Pval >= 0.0001 ~ "**",
      Pval < 0.0001 ~ "***"
    )
  )


imp1 <- ggplot(ImpRes_SleepVars, aes(x = SleepVar, y = Term, fill = Chisq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = P_label), size = 3) +
  scale_fill_gradient(low = "white", high = "palegreen4", name = "Importance\nWald Chi²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"))

##### Sleep Time

#anova_res_sleeptime <- as.data.frame(anova(RegModels_Sleep$SleepTime))
anova_res_sleeptime <- as.data.frame(car::Anova(RegModels_Sleep$SleepTime, type = "II"))
colnames(anova_res_sleeptime) <- c("DF","Chisq","Pval")
anova_res_sleeptime$Term <- rownames(anova_res_sleeptime)
anova_res_sleeptime$SleepVar <- "SleepTime"
anova_res_sleeptime$Term <- factor(anova_res_sleeptime$Term, levels = rev(anova_res_sleeptime$Term))
anova_res_sleeptime <- anova_res_sleeptime %>%
  dplyr::mutate(
    P_label = case_when(
      Pval >= 0.05 ~ "",
      Pval < 0.05 & Pval >= 0.001 ~ "*",
      Pval < 0.001 & Pval >= 0.0001 ~ "**",
      Pval < 0.0001 ~ "***"
    )
  )

imp2 <-ggplot(anova_res_sleeptime, aes(x = SleepVar, y = Term, fill = Chisq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = P_label), size = 3) +
  scale_fill_gradient(low = "white", high = "palegreen4", name = "Importance\nWald Chi²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"))

##### Sleep score

ImpRes_SleepScore <- as.data.frame(car::Anova(RegModels_Sleep$SleepScore, type = "II"))
colnames(ImpRes_SleepScore) <- c("DF","Chisq","Pval")
ImpRes_SleepScore$Term <- rownames(ImpRes_SleepScore)
ImpRes_SleepScore$SleepVar <- "SleepScore"
ImpRes_SleepScore$Term <- factor(ImpRes_SleepScore$Term, levels = rev(ImpRes_SleepScore$Term))
ImpRes_SleepScore <- ImpRes_SleepScore %>%
  dplyr::mutate(
    P_label = case_when(
      Pval >= 0.05 ~ "",
      Pval < 0.05 & Pval >= 0.001 ~ "*",
      Pval < 0.001 & Pval >= 0.0001 ~ "**",
      Pval < 0.0001 ~ "***"
    )
  )

imp3 <-ggplot(ImpRes_SleepScore, aes(x = SleepVar, y = Term, fill = Chisq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = P_label), size = 3) +
  scale_fill_gradient(low = "white", high = "palegreen4", name = "Importance\nWald Chi²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"))

##### ESS

ImpRes_ESSscore <- as.data.frame(car::Anova(RegModels_Sleep$ESS, type = "II"))
colnames(ImpRes_ESSscore) <- c("DF","Chisq","Pval")
ImpRes_ESSscore$Term <- rownames(ImpRes_ESSscore)
ImpRes_ESSscore$Term[ImpRes_ESSscore$Term == "cqsh002"] <- "SleepTime"
ImpRes_ESSscore$SleepVar <- "ESS"
ImpRes_ESSscore$Term <- factor(ImpRes_ESSscore$Term, levels = rev(ImpRes_ESSscore$Term))
ImpRes_ESSscore <- ImpRes_ESSscore %>%
  dplyr::mutate(
    P_label = case_when(
      Pval >= 0.05 ~ "",
      Pval < 0.05 & Pval >= 0.001 ~ "*",
      Pval < 0.001 & Pval >= 0.0001 ~ "**",
      Pval < 0.0001 ~ "***"
    )
  )



imp4 <-ggplot(ImpRes_ESSscore, aes(x = SleepVar, y = Term, fill = Chisq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = P_label), size = 3) +
  scale_fill_gradient(low = "white", high = "palegreen4", name = "Importance\nWald Chi²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "black"), axis.title = element_blank(), axis.text.y = element_text(color = "black"))


##### Combine plots

imp234 <- plot_grid(ncol = 3,
                    imp2, imp3, imp4)

impfull <- plot_grid(ncol = 1, imp1, imp234)

ggsave("New_figures_20251201/VariableImportance_WaldChi2_tmp.svg", impfull, width = 10)


##############################################################################################################################
# FUNCTIONS
# Defined functions used in above analyses
# Note: these function needs to be run before they can be used in the above code.
##############################################################################################################################

# Data adjustment

AdjustCoffeeData <- function(DataSet) {
  
  DataSet <- DataSet %>% 
    mutate(CoffeeConsumption = case_when(q5question2 == 1 ~ 9, 
                                         q5question2 == 2 ~ 8, 
                                         q5question2 == 3 ~ 7, 
                                         q5question2 == 4 ~ 6, 
                                         q5question2 == 5 ~ 5, 
                                         q5question2 == 6 ~ 4, 
                                         q5question2 == 7 ~ 3, 
                                         q5question2 == 8 ~ 2, 
                                         q5question2 == 9 ~ 1, 
                                         q5question2 == -98 ~ 0,))
  
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 0] <- "No coffee consumption"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 1] <- "1-3 times/month"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 2] <- "1-2 times/week"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 3] <- "3-4 times/week"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 4] <- "5-6 times/week"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 5] <- "1 time/day"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 6] <- "2 times/day"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 7] <- "3 times/day"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 8] <- "4 times/day"
  DataSet$CoffeeConsumption_label[DataSet$CoffeeConsumption == 9] <- "5 times/day or more"
  
  DataSet$CoffeeConsumption_label <- factor(DataSet$CoffeeConsumption_label, levels = c("No coffee consumption","1-3 times/month","1-2 times/week","3-4 times/week","5-6 times/week","1 time/day","2 times/day","3 times/day","4 times/day","5 times/day or more"))
  
  DataSet <- DataSet %>% mutate(CoffeeConsumptionLevel = 
                                      case_when(
                                        CoffeeConsumption >= 8 ~ "HIGH",
                                        CoffeeConsumption < 8 & CoffeeConsumption > 4 ~ "MODERATE",
                                        CoffeeConsumption < 5 & CoffeeConsumption > 0 ~ "LOW",
                                        CoffeeConsumption < 1 ~ "NONE",
                                      ))
  
  DataSet$CoffeeConsumptionLevel <- factor(DataSet$CoffeeConsumptionLevel, levels = c("NONE","LOW","MODERATE","HIGH"))
  
  return(DataSet)
  
}

AdjustTeaData <- function(DataSet) {
  
  DataSet <- DataSet %>% 
    mutate(TeaConsumption = case_when(q5question3 == 1 ~ 9, q5question3 == 2 ~ 8, 
                                      q5question3 == 3 ~ 7, q5question3 == 4 ~ 6, 
                                      q5question3 == 5 ~ 5, q5question3 == 6 ~ 4, 
                                      q5question3 == 7 ~ 3, q5question3 == 8 ~ 2, 
                                      q5question3 == 9 ~ 1, q5question3 == -98 ~ 0,))
  
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 0] <- "No tea consumption"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 1] <- "1-3 times/month"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 2] <- "1-2 times/week"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 3] <- "3-4 times/week"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 4] <- "5-6 times/week"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 5] <- "1 time/day"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 6] <- "2 times/day"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 7] <- "3 times/day"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 8] <- "4 times/day"
  DataSet$TeaConsumption_label[DataSet$TeaConsumption == 9] <- "5 times/day or more"
  
  DataSet$TeaConsumption_label <- factor(DataSet$TeaConsumption_label, levels = c("No tea consumption","1-3 times/month","1-2 times/week","3-4 times/week","5-6 times/week","1 time/day","2 times/day","3 times/day","4 times/day","5 times/day or more"))
  
  DataSet <- DataSet %>% mutate(TeaConsumptionLevel = 
                                  case_when(
                                    TeaConsumption >= 8 ~ "HIGH",
                                    TeaConsumption < 8 & TeaConsumption > 4 ~ "MODERATE",
                                    TeaConsumption < 5 & TeaConsumption > 0 ~ "LOW",
                                    TeaConsumption < 1 ~ "NONE",
                                  ))
  
  DataSet$TeaConsumptionLevel <- factor(DataSet$TeaConsumptionLevel, levels = c("NONE","LOW","MODERATE","HIGH"))
  
  return(DataSet)
  
}

AdjustESSData <- function(DataSet) {
  
  ESSList <- c("cqsh006","cqsh007","cqsh008","cqsh009","cqsh010","cqsh011","cqsh012","cqsh013")
  
  negative_row <- apply(DataSet[ESSList], 1, function(x) any(x < 0))
  DataSet <- DataSet[!negative_row,]
  
  DataSet$ESS_tot <- rowSums(DataSet[ESSList], na.rm = TRUE)
  
  DataSet$ESS_tot_5g <- DataSet$ESS_tot
  
  DataSet$ESS_tot_5g[DataSet$ESS_tot %in% c(0:4)] <- 1
  DataSet$ESS_tot_5g[DataSet$ESS_tot %in% c(5:9)] <- 2
  DataSet$ESS_tot_5g[DataSet$ESS_tot %in% c(10:14)] <- 3
  DataSet$ESS_tot_5g[DataSet$ESS_tot %in% c(15:19)] <- 4
  DataSet$ESS_tot_5g[DataSet$ESS_tot %in% c(20:24)] <- 5
  
  DataSet$ESS_tot_ref[DataSet$ESS_tot %in% c(0:5)] <- "Lower Normal DS"
  DataSet$ESS_tot_ref[DataSet$ESS_tot %in% c(6:10)] <- "Higher Normal DS"
  DataSet$ESS_tot_ref[DataSet$ESS_tot %in% c(11:12)] <- "Mild Excessive DS"
  DataSet$ESS_tot_ref[DataSet$ESS_tot %in% c(13:15)] <- "Moderate Excessive DS"
  DataSet$ESS_tot_ref[DataSet$ESS_tot %in% c(16:24)] <- "Severe Excessive DS"
  
  DataSet$ESS_tot_ref <- factor(DataSet$ESS_tot_ref, levels = c("Lower Normal DS","Higher Normal DS","Mild Excessive DS","Moderate Excessive DS","Severe Excessive DS"))
  
  DataSet$ESS_tot_char <- as.character(DataSet$ESS_tot)
  DataSet$ESS_tot_5g_char <- as.character(DataSet$ESS_tot_5g)
  
  return(DataSet)
  
}

AdjustSleepData <- function(DataSet) {
  
  SleepList <- c("cqsh001","cqsh003","cqsh004","cqsh005","cqsh014","cqsh015")
  SleepList2 <- c("cqsh001","cqsh002","cqsh003","cqsh004","cqsh005","cqsh014","cqsh015")
  
  negative_row <- apply(DataSet[SleepList2], 1, function(x) any(x < 0))
  DataSet <- DataSet[!negative_row,]
  
  DataSet$SleepTime_label[DataSet$cqsh002 == 0] <- "4 hours or less"
  DataSet$SleepTime_label[DataSet$cqsh002 == 1] <- "5 hours"
  DataSet$SleepTime_label[DataSet$cqsh002 == 2] <- "6 hours"
  DataSet$SleepTime_label[DataSet$cqsh002 == 3] <- "7 hours"
  DataSet$SleepTime_label[DataSet$cqsh002 == 4] <- "8 hours"
  DataSet$SleepTime_label[DataSet$cqsh002 == 5] <- "9 hours"
  DataSet$SleepTime_label[DataSet$cqsh002 == 6] <- "10 hours or more"
  DataSet$SleepTime_label <- factor(DataSet$SleepTime_label, levels = c("4 hours or less","5 hours","6 hours","7 hours","8 hours","9 hours","10 hours or more"), ordered = TRUE)
  
  DataSet$cqsh001_label[DataSet$cqsh001 == 0] <- "Very good"
  DataSet$cqsh001_label[DataSet$cqsh001 == 1] <- "Good"
  DataSet$cqsh001_label[DataSet$cqsh001 == 2] <- "Somewhat good"
  DataSet$cqsh001_label[DataSet$cqsh001 == 3] <- "Bad"
  DataSet$cqsh001_label[DataSet$cqsh001 == 4] <- "Very bad"
  DataSet$cqsh001_label <- factor(DataSet$cqsh001_label, levels = c("Very good","Good","Somewhat good","Bad","Very bad"), ordered = TRUE)
  
  DataSet$cqsh003_label[DataSet$cqsh003 == 0] <- "Never or very seldom"
  DataSet$cqsh003_label[DataSet$cqsh003 == 1] <- "Less than once per week"
  DataSet$cqsh003_label[DataSet$cqsh003 == 2] <- "1-2 times/week"
  DataSet$cqsh003_label[DataSet$cqsh003 == 3] <- "3-6 times/week"
  DataSet$cqsh003_label[DataSet$cqsh003 == 4] <- "Almost every night"
  DataSet$cqsh003_label <- factor(DataSet$cqsh003_label, levels = c("Never or very seldom","Less than once per week","1-2 times/week","3-6 times/week","Almost every night"), ordered = TRUE)
  
  DataSet$cqsh004_label[DataSet$cqsh004 == 0] <- "Never or very seldom"
  DataSet$cqsh004_label[DataSet$cqsh004 == 1] <- "Less than once per week"
  DataSet$cqsh004_label[DataSet$cqsh004 == 2] <- "1-2 times/week"
  DataSet$cqsh004_label[DataSet$cqsh004 == 3] <- "3-6 times/week"
  DataSet$cqsh004_label[DataSet$cqsh004 == 4] <- "Almost every night"
  DataSet$cqsh004_label <- factor(DataSet$cqsh004_label, levels = c("Never or very seldom","Less than once per week","1-2 times/week","3-6 times/week","Almost every night"), ordered = TRUE)
  
  DataSet$cqsh005_label[DataSet$cqsh005 == 0] <- "Never or very seldom"
  DataSet$cqsh005_label[DataSet$cqsh005 == 1] <- "Less than once per week"
  DataSet$cqsh005_label[DataSet$cqsh005 == 2] <- "1-2 times/week"
  DataSet$cqsh005_label[DataSet$cqsh005 == 3] <- "3-6 times/week"
  DataSet$cqsh005_label[DataSet$cqsh005 == 4] <- "Almost every night"
  DataSet$cqsh005_label <- factor(DataSet$cqsh005_label, levels = c("Never or very seldom","Less than once per week","1-2 times/week","3-6 times/week","Almost every night"), ordered = TRUE)
  
  DataSet$cqsh014_label[DataSet$cqsh014 == 0] <- "Never or very seldom"
  DataSet$cqsh014_label[DataSet$cqsh014 == 1] <- "Less than once per week"
  DataSet$cqsh014_label[DataSet$cqsh014 == 2] <- "1-2 times/week"
  DataSet$cqsh014_label[DataSet$cqsh014 == 3] <- "3-6 times/week"
  DataSet$cqsh014_label[DataSet$cqsh014 == 4] <- "Almost every night"
  DataSet$cqsh014_label <- factor(DataSet$cqsh014_label, levels = c("Never or very seldom","Less than once per week","1-2 times/week","3-6 times/week","Almost every night"), ordered = TRUE)
  
  DataSet$cqsh015_label[DataSet$cqsh015 == 0] <- "Never"
  DataSet$cqsh015_label[DataSet$cqsh015 == 1] <- "Seldom"
  DataSet$cqsh015_label[DataSet$cqsh015 == 2] <- "Sometimes"
  DataSet$cqsh015_label[DataSet$cqsh015 == 3] <- "Often"
  DataSet$cqsh015_label[DataSet$cqsh015 == 4] <- "Very often"
  DataSet$cqsh015_label <- factor(DataSet$cqsh015_label, levels = c("Never","Seldom","Sometimes","Often","Very often"), ordered = TRUE)
  
  
  #SleepVarList2 <- SleepVarList[!SleepVarList == "cqsh002"]
  #DataSet$Sleep_tot <- rowSums(DataSet[SleepVarList2], na.rm = TRUE)
  DataSet$Sleep_tot <- rowSums(DataSet[SleepList], na.rm = TRUE)
  
  DataSet <- DataSet %>%
    mutate(Sleep_tot_new = case_when(
      SleepTime_label %in% c("10 hours or more","9 hours","8 hours","7 hours") ~ Sleep_tot,
      SleepTime_label == "6 hours" ~ Sleep_tot + 1,
      SleepTime_label == "5 hours" ~ Sleep_tot + 2,
      SleepTime_label == "4 hours or less" ~ Sleep_tot + 3,
      TRUE ~ NA_real_
    ))
  
  DataSet$Sleep_tot_groups[DataSet$Sleep_tot %in% c(0:6)] <- "Good"
  DataSet$Sleep_tot_groups[DataSet$Sleep_tot %in% c(7:12)] <- "Intermediate"
  DataSet$Sleep_tot_groups[DataSet$Sleep_tot %in% c(13:18)] <- "Bad"
  DataSet$Sleep_tot_groups[DataSet$Sleep_tot %in% c(19:24)] <- "Very bad"
  DataSet$Sleep_tot_groups <- factor(DataSet$Sleep_tot_groups, levels = c("Good","Intermediate","Bad","Very bad"))
  
  
  return(DataSet)
  
}

AdjustBMIData <- function(DataSet) {
  DataSet$BMI_groups[DataSet$BMI < 18.5] <- "Underweight"
  DataSet$BMI_groups[DataSet$BMI >= 18.5 & DataSet$BMI <= 24.9] <- "Normal weight"
  DataSet$BMI_groups[DataSet$BMI >= 25 & DataSet$BMI <= 29.9] <- "Overweight"
  DataSet$BMI_groups[DataSet$BMI >= 30 & DataSet$BMI <= 34.9] <- "Obesity class I"
  DataSet$BMI_groups[DataSet$BMI >= 35 & DataSet$BMI <= 39.9] <- "Obesity class II"
  DataSet$BMI_groups[DataSet$BMI >= 40] <- "Obesity class III"
  
  DataSet$BMI_groups <- factor(DataSet$BMI_groups, levels = c("Underweight","Normal weight","Overweight","Obesity class I","Obesity class II","Obesity class III"))
  
  return(DataSet)
}

AdjustSexData <- function(DataSet) {
  DataSet$Sex[DataSet$Sex == "FEMALE"] <- "Female"
  DataSet$Sex[DataSet$Sex == "MALE"] <- "Male"
  
  DataSet$Sex <- factor(DataSet$Sex, levels = c("Female","Male"))
  
  return(DataSet)
}

AdjustAgeData <- function(DataSet) {
  DataSet$Age_groups[DataSet$AgeAtVisitOne < 55] <- "50-54"
  DataSet$Age_groups[DataSet$AgeAtVisitOne >= 55 & DataSet$AgeAtVisitOne < 60] <- "55-59"
  DataSet$Age_groups[DataSet$AgeAtVisitOne >= 60] <- "60-64"
  
  DataSet$Age_groups <- factor(DataSet$Age_groups, levels = c("50-54","55-59","60-64"))
  
  DataSet$Age_char <- factor(gsub("\\..*","", as.character(DataSet$AgeAtVisitOne)), levels = as.character(50:64))
  
  return(DataSet)
}

AdjustSmoking <- function(DataSet) {
  
  DataSet <- DataSet[!DataSet$derived_smoke_status == "UNKNOWN",]
  DataSet$derived_smoke_status[DataSet$derived_smoke_status == "CURRENT"] <- "Current"
  DataSet$derived_smoke_status[DataSet$derived_smoke_status == "EX_SMOKER"] <- "Ex-smoker"
  DataSet$derived_smoke_status[DataSet$derived_smoke_status == "NEVER"] <- "Never"
  
  DataSet$derived_smoke_status <- factor(DataSet$derived_smoke_status, levels = c("Never","Ex-smoker","Current"))
  
  return(DataSet)
}

AdjustStressData <- function(DataSet) {
  
  DataSet <- DataSet[!DataSet$cqsle001 == -99,]
  
  DataSet <- DataSet %>% 
    mutate(Stress = case_when(
      cqsle001 == 0 ~ "Never",
      cqsle001 == 1 ~ "Once",
      cqsle001 == 2 ~ "Once the past five years",
      cqsle001 == 3 ~ "Constant the past year",
      cqsle001 == 4 ~ "Constant the past five years"
      ))
  
  DataSet$Stress <- factor(DataSet$Stress, levels = c("Never","Once","Once the past five years","Constant the past year","Constant the past five years"))
  
  return(DataSet)
}

AdjustPhysActData <- function(DataSet) {
  
  DataSet <- DataSet[!DataSet$cqpa012 == -99,]
  
  DataSet <- DataSet %>% 
    mutate(LeisurePhysActivityPast12 = case_when(
      cqpa012 == 0 ~ "Sedentary",
      cqpa012 == 1 ~ "Moderate exercise",
      cqpa012 == 2 ~ "Moderate but regular exercise",
      cqpa012 == 3 ~ "Regular exercise and training"
    ))
  
  DataSet$LeisurePhysActivityPast12 <- factor(DataSet$LeisurePhysActivityPast12, levels = c("Sedentary","Moderate exercise","Moderate but regular exercise","Regular exercise and training"))
  
  return(DataSet)
}

RegressionTable <- function(ModelList, Name) {

  # Create Word doc
  
  doc <- read_docx()
  
  # Loop through models
  
  for (i in seq_along(ModelList)) {
    
    model <- ModelList[[i]]
    model_type <- class(model)[1]  # "clm" or "glm"
    
    # Title for each model
    
    heading <- fpar(
      ftext(paste0("Model ", i, ": Coffee consumption vs ", names(ModelList[i])), prop = fp_text(font.size = 14, bold = TRUE)),
      fp_p = fp_par(text.align = "center")
    )
    
    doc <- doc %>%
      body_add_fpar(heading) %>%
      #body_add_par(paste0("Model ", i, ": Coffee consumption vs ", names(RegModels_Sleep[i])), style = "heading 1") %>%
      body_add_par("", style = "Normal")
    
    # Create regression table (exponentiated)
    
    reg_table <- tbl_regression(model, exponentiate = TRUE)
    
    doc <- doc %>%
      body_add_flextable(as_flex_table(reg_table)) %>%
      body_add_par("", style = "Normal")
    
    # Model summary
    
    if (model_type == "clm") {
      
      # Ordinal model summary
      
      summary_stats <- tibble::tibble(
        Statistic = c("Log-likelihood", "AIC", "Number of thresholds"),
        Value = c(logLik(model)[1], AIC(model), length(model$alpha))
      )
      
    } else if (model_type == "glm") {
      
      # Quasipoisson model summary
      
      dispersion <- round(deviance(model) / df.residual(model), digits = 2)
      
      summary_stats <- tibble::tibble(
        Statistic = c("Deviance", "Degrees of Freedom", "Dispersion"),
        Value = c(round(deviance(model), digits = 2), round(df.residual(model), digits = 2), dispersion)
      )
    }
    
    # Add summary table
    
    heading2 <- fpar(
      ftext("Model Summary", prop = fp_text(font.size = 14, bold = TRUE)),
      fp_p = fp_par(text.align = "center")
    )
    
    doc <- doc %>%
      body_add_fpar(heading2) %>%
      #body_add_par("Model Summary", style = "heading 2") %>%
      body_add_par("", style = "Normal") %>%
      body_add_flextable(
        flextable(summary_stats) %>%
          autofit() %>%
          width(j = 1:2, width = 3)) %>%
      body_add_par("", style = "Normal") %>%
      body_add_break()
    
    print(paste0(i, "/", length(ModelList)))
    
  }
  
  # Save the full document
  
  print(doc, target = paste0("New_figures_20251201/Tables/Regression_model_reports_", Name,".docx"))
  
}