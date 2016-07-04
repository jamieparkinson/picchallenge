#!/usr/bin/env Rscript

library(ggplot2) # Plotting
library(reshape2) # Data handling
library(plyr) # General utilities

# Constants
columnTitles <- c("ID", # List of better titles
									"picScore",
									"achieveScore",
									"disadvantageScore",
									"inductiveTest",
									"numericTest",
									"uni",
									"stage",
									"vidScore",
									"gender",
									"ethnicity",
									"dob",
									"countryOfBirth",
									"townOfBirth",
									"nationality",
									"maritalStatus",
									"sexualOrientation",
									"religion",
									"disability",
									"homeDeprivation",
									"homeAccess",
									"region")
disabledTrueStr = "Please provide further details below, particularly if you will require reasonable adjustments to the recruitment process"
preferNotToSay = "Prefer not to say"
stageStrings = c("WITHDRAWN",
								 "REJECTED @ Online Testing",
								 "REJECTED @ Offer",
								 "REJECTED @ HireVue",
								 "REJECTED @ Assessment Day",
								 "REJECTED @ Application Review",
								 "Offer Accepted / Onboarding",
								 "In process AFTER role closed",
								 "Being considered for other opportunities within the business")
stageNames = c("Withdrawn",
							 "Online Testing",
							 "Offer",
							 "HireVue",
							 "Assessment Day",
							 "Application Review",
							 "Accepted",
							 "POST",
							 "Considered for Other Opportunities")

importClean <- function(fileName, cols = "ALL") {
	rawDF <- read.csv(fileName, col.names = columnTitles, na.strings = c(" ", "NA", "")) # Import data

	# Cleaning things up
	# disability column to boolean (or NA)
	disability2bool <- function(disabilityString) {
		disabledTrue = (disabilityString == disabledTrueStr)
		disabledNull = (disabilityString == preferNotToSay)
		if (disabledNull || is.na(disabilityString)) { return(NA) } # Return NA if preferred not to say or blank
		return(disabledTrue)
	}
	disability2bool = Vectorize(disability2bool)
	rawDF$disability = disability2bool(rawDF$disability)

	# Replace stage strings with nicer ones
	rawDF$stage = mapvalues(rawDF$stage, stageStrings, stageNames)

	if (cols == "ALL") {
		return(rawDF)
	} else {
		return(rawDF[,cols])
	}
}


