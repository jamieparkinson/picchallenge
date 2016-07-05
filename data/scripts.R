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

makeRiverDisadvantage <- function(inputData) {
	nonWithdrawn = inputData[ ! inputData$stage %in% c("Withdrawn", "POST"), ] # Get rid of withdrawn applications
	nonWithdrawn$stage[nonWithdrawn$stage == "Considered for Other Opportunities" ] <- "Accepted" # Merge acceptances

	nums = data.frame(
										total = length(nonWithdrawn$stage) - cumsum(countstage(nonWithdrawn)),
										deprivedHome = sum(nonWithdrawn$disadvantageScore <= 50, na.rm = T) - cumsum(countstage(nonWithdrawn[nonWithdrawn$disadvantageScore <= 50, ])),
										stages = stageNamesChron									 
									 )
	nums$advantagedHome = nums$total - nums$disadvantageScore

	# Retain ordering
	nums$stages <- factor(nums$stages, levels = stageNamesChron)
	nums = melt(nums)[,c(2,1,3)]
	levels(nums$variable) = c("All\nApplicants", "Applicants from\nDisadvantaged Backgrounds", "Applicants from\nLess Disadvantaged Backgrounds")

	alluvial_ts(nums, rankup = TRUE,
										col = c("#2ecc71", "#9b59b6", "#3498db"),
										lab.cex = 1.05,
										#xmargin = 0.2,
										axis.cex = 1.35,
										grid = FALSE,
										title = " ", xlab = " ", ylab = " ",
										leg.mode = FALSE, leg.x = 0, leg.y = 0)
}

normalisedFreqs <- function(all, category, stage, allLevels = FALSE) {
	totals = na.omit(count(all, vars = category))
	specifics = na.omit(count(all[all$stage == stage,], vars = category))

	if (!allLevels) { # Delete "prefer not to say"
		totals = totals[totals[,category] != "Prefer not to say", ]
		specifics = specifics[specifics[,category] != "Prefer not to say", ]
	} 

	specifics$freq = 100 * specifics$freq / totals$freq
	specifics$factor = category
	colnames(specifics) <- c("variable", "value", "factor")

	return(specifics)
}

binarify <- function(inputData) {
	newData = inputData

	levels = levels(newData$ethnicity)
	levels[levels != "White"] <- "Ethnic Minority"
	levels(newData$ethnicity) <- levels

	levels = levels(newData$sexualOrientation)
	levels[levels != "Heterosexual" & levels != "Prefer not to say"] <- "LGBT"
	levels(newData$sexualOrientation) <- levels

	newData$disadvantageScore[newData$disadvantageScore <= 50] <- "Disadvantaged"
	newData$disadvantageScore[newData$disadvantageScore != "Disadvantaged"] <- "Less\nDisadvantaged"

	return(newData)
}

percentDropouts <- function(inputData) {
	binaryData = binarify(inputData)
	gender = normalisedFreqs(binaryData, "gender", "Withdrawn")

	ethnicity = normalisedFreqs(binaryData, "ethnicity", "Withdrawn")
	sexualOrientation = normalisedFreqs(binaryData, "sexualOrientation", "Withdrawn")
	disadvantage = normalisedFreqs(binaryData, "disadvantageScore", "Withdrawn")

	alldata = rbind.data.frame(gender, ethnicity, sexualOrientation, disadvantage)
	
	return(alldata)
}

plotDropouts <- function(categoryData) {
	categories = c(`gender` = "Gender", `ethnicity` = "Ethnicity", `sexualOrientation` = "Sexual Orientation", `disadvantageScore` = "Background")

	return( ggplot(categoryData, aes(x = variable, y = value, fill = variable)) +
						geom_bar(stat = "identity", position = "dodge") +
						facet_grid(. ~ factor, scales = "free", labeller = as_labeller(categories)) +
						theme_minimal() +
						theme(legend.position = "none",
									axis.title.x = element_blank(),
									axis.text.x = element_text(size = 15),
									axis.title.y = element_text(size = 15, face = "bold", margin = margin(0, 20, 0, 0)),
									strip.text.x = element_text(size = 15, face = "bold", margin = margin(0, 0, 25, 0))) +
						ylab("% Application Withdrawals") +
						scale_y_continuous(expand = c(-0.1, 0.1)) +
						scale_fill_manual(values = rep(c("#666666", "#CCCCCC"),4))
				)
}

pvals <- function(allData, category) {
	# Null hypothesis: no diff between male/female, white/non-white etc
	categoryData = percentDropouts(allData)
	percentages = categoryData[categoryData$factor == category, "value"]
	binaryData = binarify(allData)
	totals = na.omit(count(binaryData, vars = category))
	
	totals = totals[totals[,category] != "Prefer not to say", ]

	# z-statistic
	zdenom = sqrt(0.25*((1 / totals[1, "freq"]) + (1 / totals[2, "freq"])))
	z = (percentages[1] - percentages[2]) / (100*zdenom)

	# 2-sided p value
	p = 2*pnorm(-abs(z))
	return(p)
}