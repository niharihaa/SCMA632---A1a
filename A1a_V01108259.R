#set the working directory
setwd('C:\\Users\\nihar\\OneDrive\\Desktop\\Bootcamp\\SCMA 632\\Assignments\\A1a\\Data')
getwd()

#Install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library (package, character.only = TRUE)
  }
}

# List of required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "glue")
# Apply the function to the list of libraries
lapply(libraries, install_and_load)

# Load the dataset into R
data <- read.csv("NSSO68.csv")

#Filtering for Maharasthra
df <- data %>%
  filter(state_1 == "MH")

#Dataset Information Display
cat("Dataset Information: \n")
print(names(df))
print (head(df))
print (dim(df))

#Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information: \n")
print (missing_info)

#Sub-setting the data - Set 1
mhgrains <- df %>%
  select(state_1, District, Region, Sector, ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(mhgrains)))

# Finding outliers and removing them
remove_outliers <- function(df,ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v) {
  Q1 <- quantile(df[[ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v]], 0.25)
  Q3 <- quantile(df[[ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v]] >= lower_threshold & df[[ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricetotal_v", "wheattotal_v", "jowarp_v", "barleyp_v", "maizep_v", "maida_v", "suji_v", "bajrap_v", "milletp_v")
for (col in outlier_columns) {
  mhgrains <- remove_outliers(mhgrains, col)
}

# Summarize consumption
 mhgrains$total_consumption <- rowSums(mhgrains[, c("ricetotal_v", "wheattotal_v", "jowarp_v", "barleyp_v", "maizep_v", "maida_v", "suji_v", "bajrap_v", "milletp_v")], na.rm = TRUE)

# Summarize and display top and bottom consuming districts and regions
 summarize_consumption <- function(group_col) {
   summary <- mhgrains %>%
     group_by(across(all_of(group_col))) %>%
     summarise(total = sum(total_consumption)) %>%
     arrange(desc(total))
   return(summary)
 }
 
 district_summary <- summarize_consumption("District")
 region_summary <- summarize_consumption("Region")

 cat("Top 3 Consuming Districts:\n")
 print(head(district_summary, 3)) 
 cat("Bottom 3 Consuming Districts:\n")
 print(tail(district_summary, 3))
 cat("Region Consumption Summary:\n")
 print(region_summary)
 
 # Rename districts and sectors
 district_mapping <- c("21" = "Thane", "22" = "Mumbai (Suburban) an", "25" = "Pune")
 sector_mapping <- c("2" = "URBAN", "1" = "RURAL")
 district_mapping <- c("10" = "Bhandara", "22" = "Gadchiroli", "6" = "Washim")
 
 mhgrains$District <- as.character(mhgrains$District)
 mhgrains$Sector <- as.character(mhgrains$Sector)
 mhgrains$District <- ifelse(mhgrains$District %in% names(district_mapping), district_mapping[mhgrains$District], mhgrains$District)
 mhgrains$Sector <- ifelse(mhgrains$Sector %in% names(sector_mapping), sector_mapping[mhgrains$Sector], mhgrains$Sector)
 
 # Test for differences in mean consumption between urban and rural
 rural <- mhgrains %>%
   filter(Sector == "RURAL") %>%
   select(total_consumption)
 
 urban <- mhgrains %>%
   filter(Sector == "URBAN") %>%
   select(total_consumption)
 
 mean_rural <- mean(rural$total_consumption)
 mean_urban <- mean(urban$total_consumption)
 
 # Perform z-test
 z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
 
 # Generate output based on p-value
 if (z_test_result$p.value < 0.05) {
   cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we reject the null hypothesis.\n"))
   cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
   cat(glue::glue("The mean consumption in Rural areas is {mean_rural} and in Urban areas its {mean_urban}\n"))
 } else {
   cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we fail to reject the null hypothesis.\n"))
   cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
   cat(glue::glue("The mean consumption in Rural area is {mean_rural} and in Urban area its {mean_urban}\n"))
 }
 
 #Sub-setting the data - set 2
 mhfruits <- df %>%
   select(state_1, District, Region, Sector, bananano_v, jackfruit_v, watermel_v, pineaplno_v, guava_v, papayar_v, sighara_v, cocogno_v, mango_v, kharbooz_v, pears_v, berries_v, leechi_v, apple_v, grapes_v)
 
 # Check for missing values in the subset
 cat("Missing Values in Subset:\n")
 print(colSums(is.na(mhfruits)))
 
 # Finding outliers and removing them
 remove_outliers <- function(df,bananano_v, jackfruit_v, watermel_v, pineaplno_v, guava_v, papayar_v, sighara_v, cocogno_v, mango_v, kharbooz_v, pears_v, berries_v, leechi_v, apple_v, grapes_v) {
   Q1 <- quantile(df[[bananano_v, jackfruit_v, watermel_v, pineaplno_v, guava_v, papayar_v, sighara_v, cocogno_v, mango_v, kharbooz_v, pears_v, berries_v, leechi_v, apple_v, grapes_v]], 0.25)
   Q3 <- quantile(df[[bananano_v, jackfruit_v, watermel_v, pineaplno_v, guava_v, papayar_v, sighara_v, cocogno_v, mango_v, kharbooz_v, pears_v, berries_v, leechi_v, apple_v, grapes_v]], 0.75)
   IQR <- Q3 - Q1
   lower_threshold <- Q1 - (1.5 * IQR)
   upper_threshold <- Q3 + (1.5 * IQR)
   df <- subset(df, df[[bananano_v, jackfruit_v, watermel_v, pineaplno_v, guava_v, papayar_v, sighara_v, cocogno_v, mango_v, kharbooz_v, pears_v, berries_v, leechi_v, apple_v, grapes_v]] >= lower_threshold & df[[ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v]] <= upper_threshold)
   return(df)
 }
 
 outlier_columns <- c("bananano_v", "jackfruit_v", "watermel_v", "pineaplno_v", "guava_v", "papayar_v", "sighara_v", "cocogno_v", "mango_v", "kharbooz_v", "pears_v", "berries_v", "leechi_v", "apple_v", "grapes_v")
 for (col in outlier_columns) {
   mhfruits <- remove_outliers(mhfruits, col)
 }
 
 # Summarize consumption
 mhfruits$tot_consumption <- rowSums(mhfruits[, c("bananano_v", "jackfruit_v", "watermel_v", "pineaplno_v", "guava_v", "papayar_v", "sighara_v", "cocogno_v", "mango_v", "kharbooz_v", "pears_v", "berries_v", "leechi_v", "apple_v", "grapes_v")], na.rm = TRUE)
 
 # Summarize and display top and bottom consuming districts and regions
 summarize_consumptionfruits <- function(group_col) {
   summary <- mhfruits %>%
     group_by(across(all_of(group_col))) %>%
     summarise(total = sum(mhfruits$tot_consumption)) %>%
     arrange(desc(total))
   return(summary)
 }
 
 district_summary <- summarize_consumptionfruits("District")
 region_summary <- summarize_consumptionfruits("Region")
 
 cat("Top 3 Consuming Districts:\n")
 print(head(district_summary, 3)) 
 cat("Bottom 3 Consuming Districts:\n")
 print(tail(district_summary, 3))
 cat("Region Consumption Summary:\n")
 print(region_summary)
 
 # Rename districts and sectors
 district_mapping <- c("1" = "Manudurbar", "2" = "Dhule", "3" = "Jalgaon")
 sector_mapping <- c("2" = "URBAN", "1" = "RURAL")
 district_mapping <- c("33" = "Sindhudurg", "34" = "Kolhapur", "35" = "Sangli")
 
 mhfruits$District <- as.character(mhfruits$District)
 mhfruits$Sector <- as.character(mhfruits$Sector)
 mhfruits$District <- ifelse(mhfruits$District %in% names(district_mapping), district_mapping[mhfruits$District], mhfruits$District)
 mhfruits$Sector <- ifelse(mhfruits$Sector %in% names(sector_mapping), sector_mapping[mhfruits$Sector], mhfruits$Sector)
 
 # Test for differences in mean consumption between urban and rural
 ruralf <- mhfruits1 %>%
   filter(Sector == "RURAL") %>%
   select(total_consumption)
 
 urbanf <- mhfruits %>%
   filter(Sector == "URBAN") %>%
   select(total_consumption)
 
 mean_ruralf <- mean(ruralf$total_consumption)
 mean_urbanf <- mean(urbanf$total_consumption)
 
 # Perform z-test
 z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
 
 # Generate output based on p-value
 if (z_test_result$p.value < 0.05) {
   cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we reject the null hypothesis.\n"))
   cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
   cat(glue::glue("The mean consumption in Rural areas is {mean_ruralf} and in Urban areas its {mean_urbanf}\n"))
 } else {
   cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we fail to reject the null hypothesis.\n"))
   cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
   cat(glue::glue("The mean consumption in Rural area is {mean_ruralf} and in Urban area its {mean_urbanf}\n"))
 }
 
 #Sub-setting the data - Set 3
 mhmeat <- df %>%
   select(state_1, District, Region, Sector, eggsno_v, fishprawn_v, goatmeat_v, beef_v, pork_v, chicken_v, othrbirds_v )
 
 # Check for missing values in the subset
 cat("Missing Values in Subset:\n")
 print(colSums(is.na(mhmeat)))
 
 # Finding outliers and removing them
 remove_outliers <- function(df,eggsno_v, fishprawn_v, goatmeat_v, beef_v, pork_v, chicken_v, othrbirds_v) {
   Q1 <- quantile(df[[eggsno_v, fishprawn_v, goatmeat_v, beef_v, pork_v, chicken_v, othrbirds_v]], 0.25)
   Q3 <- quantile(df[[eggsno_v, fishprawn_v, goatmeat_v, beef_v, pork_v, chicken_v, othrbirds_v]], 0.75)
   IQR <- Q3 - Q1
   lower_threshold <- Q1 - (1.5 * IQR)
   upper_threshold <- Q3 + (1.5 * IQR)
   df <- subset(df, df[[eggsno_v, fishprawn_v, goatmeat_v, beef_v, pork_v, chicken_v, othrbirds_v]] >= lower_threshold & df[[ricetotal_v, wheattotal_v, jowarp_v, barleyp_v, maizep_v, maida_v, suji_v, bajrap_v, milletp_v]] <= upper_threshold)
   return(df)
 }
 
 outlier_columns <- c("eggsno_v", "fishprawn_v", "goatmeat_v", "beef_v", "pork_v", "chicken_v", "othrbirds_v")
 for (col in outlier_columns) {
   mhmeat <- remove_outliers(mhmeat, col)
 }
 
 # Summarize consumption
 mhmeat$total_cons <- rowSums(mhmeat[, c("eggsno_v", "fishprawn_v", "goatmeat_v", "beef_v", "pork_v", "chicken_v", "othrbirds_v")], na.rm = TRUE)
 
 # Summarize and display top and bottom consuming districts and regions
 summarize_consumption1 <- function(group_col) {
   summary <- mhmeat %>%
     group_by(across(all_of(group_col))) %>%
     summarise(total = sum(total_cons)) %>%
     arrange(desc(total))
   return(summary)
 }
 
 district_summary <- summarize_consumption1("District")
 region_summary <- summarize_consumption1("Region")
 
 cat("Top 3 Consuming Districts:\n")
 print(head(district_summary, 3)) 
 cat("Bottom 3 Consuming Districts:\n")
 print(tail(district_summary, 3))
 cat("Region Consumption Summary:\n")
 print(region_summary)
 
 # Rename districts and sectors
 district_mapping <- c("21" = "Thane", "22" = "Mumbai (Suburban) an", "25" = "Pune")
 sector_mapping <- c("2" = "URBAN", "1" = "RURAL")
 district_mapping <- c("10" = "Bhandara", "5" = "Akola", "16" = "Hingoli")
 
 mhmeat$District <- as.character(mhmeat$District)
 mhmeat$Sector <- as.character(mhmeat$Sector)
 mhmeat$District <- ifelse(mhmeat$District %in% names(district_mapping), district_mapping[mhmeat$District], mhmeat$District)
 mhmeat$Sector <- ifelse(mhmeat$Sector %in% names(sector_mapping), sector_mapping[mhmeat$Sector], mhmeat$Sector)
 
 # Test for differences in mean consumption between urban and rural
 ruralm <- mhmeat %>%
   filter(Sector == "RURAL") %>%
   select(total_cons)
 
 urbanm <- mhmeat %>%
   filter(Sector == "URBAN") %>%
   select(total_cons)
 
 mean_ruralm <- mean(rural$total_cons)
 mean_urbanm <- mean(urban$total_cons)
 
 # Perform z-test
 z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)
 
 # Generate output based on p-value
 if (z_test_result$p.value < 0.05) {
   cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we reject the null hypothesis.\n"))
   cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
   cat(glue::glue("The mean consumption in Rural areas is {mean_ruralm} and in Urban areas its {mean_urbanm}\n"))
 } else {
   cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we fail to reject the null hypothesis.\n"))
   cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
   cat(glue::glue("The mean consumption in Rural area is {mean_ruralm} and in Urban area its {mean_urbanm}\n"))
 }