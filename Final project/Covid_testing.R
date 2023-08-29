library(tidyverse)
library(gtsummary)
library(here)
library(dplyr)

# Descriptive statistics table (table 1)

tbl_summary(
	covid_testing,
	by = gender,
	include = c(age, pan_day, result, payor_group, demo_group, patient_class),
    label = list(
		pan_day ~ "Pandemic day",
		payor_group ~ "Payor",
		demo_group ~ "Demographic",
		patient_class ~ "Patient class",
		age ~ "Age",
		result ~ "Result"
	),
	missing_text = "Missing")

c2 <- covid_testing %>% mutate(Demographic = factor(demo_group, labels = c("client","patient", "other adult", "misc adult", "unidentified")),
			  Result = factor(result, labels = c("positive", "negative", "invalid")))

# univariate regression

tbl_uvregression(
       c2,
	y = age,
	include = c(Demographic, gender, result),
	method = lm)



# Create a figure - histogram for pandemic day

hist(covid_testing$pan_day)

# mean

mn <- function(x){
	m <- mean(x)
	return (m)
}
mn(c2$pan_day)

# Standard deviation after handling missing values

std_dev <- function(x){
		clean_data <- x[!is.na(x)]
		sd_value <- sd(clean_data)
		return (sd_value)}

std_dev(c2$pan_day)

install.packages("renv")
renv::init()


renv::snapshot()


