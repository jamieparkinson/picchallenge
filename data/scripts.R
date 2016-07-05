source("import.R")
library(jsonlite)
library(alluvial)

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

# Progression through stages
cumtotals <- function(dat, field, val, ineq = FALSE) {
  if (!ineq) {
    return(sum(dat[,field] == val, na.rm = T) - cumsum(countstage(dat[dat[,field] == val, ]))) 
  } else {
    return(sum(dat[,field] != val, na.rm = T) - cumsum(countstage(dat[dat[,field] != val, ])))
  }
}

totalAtStage <- function(dat, stage) {
  return(sum(dat[stage, sapply(dat[stage,], is.numeric)]))
}

makeRiver <- function(inputData) {
  nonWithdrawn = inputData[ ! inputData$stage %in% c("Withdrawn", "POST"), ] # Get rid of withdrawn applications and those still in process
  nonWithdrawn$stage[nonWithdrawn$stage == "Considered for Other Opportunities" ] <- "Accepted" # Merge acceptances

  nums = data.frame(
                    deprivedHome = sum(nonWithdrawn$disadvantageScore <= 50, na.rm = T) - cumsum(countstage(nonWithdrawn[nonWithdrawn$disadvantageScore <= 50, ])),
                    advantagedHome = sum(nonWithdrawn$disadvantageScore > 50, na.rm = T) - cumsum(countstage(nonWithdrawn[nonWithdrawn$disadvantageScore > 50, ])),
                    women = cumtotals(nonWithdrawn, "gender", "Female"),
                    men = cumtotals(nonWithdrawn, "gender", "Male"),
                    white = cumtotals(nonWithdrawn, "ethnicity", "White"),
                    nonwhite = cumtotals(nonWithdrawn, "ethnicity", "White", ineq = TRUE),
                    heterosexual = cumtotals(nonWithdrawn, "sexualOrientation", "Heterosexual"),
                    lgbt = cumtotals(nonWithdrawn, "sexualOrientation", "Heterosexual", ineq = TRUE),
                    stages = stageNamesChron                   
                   )

  # Relative percentages
  startTotal = totalAtStage(nums, stageNamesChron[1])
  scaleFactors = sapply(stageNamesChron, function(stage) startTotal/totalAtStage(nums, stage))
  nums[,1:(ncol(nums) - 1)] = nums[,1:(ncol(nums) - 1)] * scaleFactors

  # Retain ordering
  nums$stages <- factor(nums$stages, levels = stageNamesChron)
  nums = melt(nums)[,c(2,1,3)]

  #levels(nums$variable) = c("All\nApplicants", "Applicants from\nDisadvantaged Backgrounds", "Applicants from\nLess Disadvantaged Backgrounds")

  alluvial_ts(nums, rankup = FALSE,
                    col = gray.colors(8),
                    lab.cex = 1.05,
                    #xmargin = 0.2,
                    axis.cex = 1.35,
                    grid = FALSE,
                    title = " ", xlab = " ", ylab = " ",
                    leg.mode = FALSE, leg.x = 0, leg.y = 0)
}

makeEthRiver <- function(inputData) {
  nonWithdrawn = inputData[ ! inputData$stage %in% c("Withdrawn", "POST"), ] # Get rid of withdrawn applications and those still in process
  nonWithdrawn$stage[nonWithdrawn$stage == "Considered for Other Opportunities" ] <- "Accepted" # Merge acceptances

  # Corresponds to levels, simplifying mixed background etc
  ethSimplify = c("South Asian", "Black", "Black", "East Asian", "South Asian", "Other", "Other", "Mixed Background", "White", "South Asian", "Other", "White", "Mixed Background", "Mixed Background", "Mixed Background")
  levels(nonWithdrawn$ethnicity) <- ethSimplify

  ethCountStage <- function(stage) {
    stageSel = nonWithdrawn[nonWithdrawn$stage == stage, ]
    out = as.data.frame(table(stageSel$ethnicity))
    out$stage = rep(stage, nrow(out))
    colnames(out) <- c("ethnicity", "freq", "stage")
    return(out)
  }

  nums = lapply(stageNamesChron, ethCountStage)
  levels = unique(ethSimplify)
  received = data.frame(ethnicity = levels,
                        freq = rep(0, length(levels)),
                        stage = rep(stageNamesChron[1], length(levels))
                        )
  nums[1][[1]] = received

  nums = do.call(rbind, nums)

  totals = sapply(unique(ethSimplify), function(eth) sum(nonWithdrawn$ethnicity == eth, na.rm = T))
  totals = as.data.frame(t(replicate(length(levels), totals)))
  totals = totals[levels]

  nums = dcast(nums, stage ~ ethnicity, value.var = "freq")
  nums = nums[c("stage", levels)]
  nums[is.na(nums)] <- 0
  nums[,-1] = cumsum(nums[,-1])
  nums[,-1] = totals - nums[,-1]

  # Relative percentages
  scaleFactors = rowSums(n[,-1])
  scaleFactors = scaleFactors[1]/scaleFactors
  nums[,-1] = nums[,-1] * scaleFactors

  nums = melt(nums)[,c(2,1,3)]

  alluvial_ts(nums, rankup = FALSE,
                    col = gray.colors(length(levels)),
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