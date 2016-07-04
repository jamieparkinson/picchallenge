source("import.R")
library(jsonlite)

data <- importClean("raw-trimmed.csv", cols = "ALL")

# !!!These are in chronological order!!!
	stageNamesChron = c("Application Received",
											"Application Review",
											"Online Testing",
											"HireVue",
											"Assessment Day",
										  "Offer")

# Count number of people at each certain stage
countstage <- function(dat) {
	return( sapply(stageNamesChron, function(name) sum(dat$stage == name, na.rm = T) ) )
}

makeRiver <- function(inputData, filename = "applicantFlow.json") {
	nonWithdrawn = inputData[ ! inputData$stage %in% c("Withdrawn", "POST"), ] # Get rid of withdrawn applications
	nonWithdrawn$stage[nonWithdrawn$stage == "Considered for Other Opportunities" ] <- "Accepted" # Merge acceptances

	nums = data.frame(
										total = length(nonWithdrawn$stage) - cumsum(countstage(nonWithdrawn)),
										deprivedHome = sum(nonWithdrawn$homeDeprivation <= 50, na.rm = T) - cumsum(countstage(nonWithdrawn[nonWithdrawn$homeDeprivation <= 50, ])),
										stages = stageNamesChron									 
									 )
	nums$advantagedHome = nums$total - nums$deprivedHome

	# Retain ordering
	nums$stages <- factor(nums$stages, levels = stageNamesChron)
	nums = melt(nums)[,c(2,1,3)]
	levels(nums$variable) = c("All\nApplicants", "Applicants from\nDeprived Areas", "Applicants from\nLess Deprived Areas")

	alluvial_ts(nums, rankup = TRUE,
										col = c("#2ecc71", "#9b59b6", "#3498db"),
										lab.cex = 1.05,
										#xmargin = 0.2,
										axis.cex = 1.35,
										grid = FALSE,
										title = " ", xlab = " ", ylab = " ",
										leg.mode = FALSE, leg.x = 0, leg.y = 0)
}