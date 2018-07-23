#======================================================================================================
# ICMPC Analysis
#--------------------------------------------------
source("scripts/cleanup.R") # 
#--------------------------------------------------
# Create Measurment Model 
str(semdata)
## Define Model 
measurement.model <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zMeanOspanPartialScore + zMeanSspanPartialScore + zTonePartial
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zSingBack23 + zHearOnceSingBack + zIdentifySpecial
gf ~~ wmc
'

## lavaan
measurment.model.fit <- cfa(measurement.model, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(measurment.model.fit, whatLabels = "std" , edge.label.cex = 2, layout = "tree2", rotation = 2)
summary(measurment.model.fit, standardized = TRUE)
# Remove some self report measures 

parameterEstimates(measurment.model.fit, standardized = TRUE, ci = FALSE)

semPaths(measurment.model.fit,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

fitMeasures(measurment.model.fit, c("df","chisq","pvalue","rmsea","cfi","tli","tli"))

#--------------------------------------------------
# Double Models - One with General Predict Beat and Melodic
#               - Two with Cognitive Predict Beat and Melodic 
#--------------------------------------------------
# Set Up Models for Improving Fits 
#--------------------------------------------------

model.A <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zTonePartial + zMeanOspanPartialScore + zMeanSspanPartialScore
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zSingBack23 + zHearOnceSingBack + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
zBeatPerception ~ wmc
zMelodicMemory ~ wmc
zBeatPerception ~ gf
zMelodicMemory ~ gf 
## Covariances
zMelodicMemory ~~ zBeatPerception
gf ~~ wmc
'
## Fits
model.fit.A <- sem(model.A, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.A, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.A)
fitMeasures(model.fit.A)

semPaths(model.fit.A,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.A, standardized = TRUE)
fitMeasures(model.fit.A, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))
#--------------------------------------------------

# Remove 35, 15, 38, 34, 27, 16, 37
# ExciteMotivate, HearBeat, EvokesMemories, PiecesEmotion, SingByMemory, HearTune, TalkEmotionsPiece
model.B <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zTonePartial + zMeanOspanPartialScore + zMeanSspanPartialScore
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
zBeatPerception ~ wmc
zMelodicMemory ~ wmc
zBeatPerception ~ gf
zMelodicMemory ~ gf 
## Covariances
zMelodicMemory ~~ zBeatPerception
gf ~~ wmc
'
## Fits
model.fit.B <- sem(model.B, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(model.fit.B, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.B)
fitMeasures(model.fit.B)

semPaths(model.fit.B,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.B, standardized = TRUE)
fitMeasures(model.fit.B, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.B, type = "cor")$cor

anova(model.fit.A, model.fit.B)

#--------------------------------------------------
model.C <- '
gf =~ zAdjustedNumberSeries + zRavensAvg
wmc =~ zTonePartial + zMeanOspanPartialScore + zMeanSspanPartialScore
zBeatPerception ~ wmc
zMelodicMemory ~ wmc
zBeatPerception ~ gf
zMelodicMemory ~ gf 
## Covariances
zMelodicMemory ~~ zBeatPerception
gf ~~ wmc
'
## Fits
model.fit.C <- sem(model.C, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.C, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.C)
fitMeasures(model.fit.C)

semPaths(model.fit.C,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.C, standardized = TRUE)
fitMeasures(model.fit.C, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.C, type = "cor")$cor

anova(model.fit.B, model.fit.C)

#--------------------------------------------------

model.D <- '
wmc =~ TonePartial + MeanOspanPartialScore + MeanSspanPartialScore
BeatPerception ~ wmc
MelodicMemory ~ wmc
BeatPerception ~ gf
MelodicMemory ~ gf
## Covariances
MelodicMemory ~~ BeatPerception
gf ~~ wmc
'

## Fits
model.fit.D <- sem(model.D, data = semdata, sample.nobs = 239, std.lv = TRUE)
semPaths(model.fit.D, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.D)
fitMeasures(model.fit.D)

semPaths(model.fit.D,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.D, standardized = TRUE)
fitMeasures(model.fit.D, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.D, type = "cor")$cor

anova(model.fit.C, model.fit.D)
#--------------------------------------------------
# Model 5

model.E <- '
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
## Covariances
zMelodicMemory ~~ zBeatPerception
'
## Fits
model.fit.E <- sem(model.E, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(model.fit.E, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.E)
fitMeasures(model.fit.E)

semPaths(model.fit.E,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.E, standardized = TRUE)
fitMeasures(model.fit.E, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.E, type = "cor")$cor

anova(model.fit.B, model.fit.E)

# Model 5.b | Drops Items 

model.E.b <- '
gen =~ zFreetime + zWriting + zSearchInternet + zAddiction + zComparePerf + zSelfTonal + zNeverComplimented + zNotConsiderSelf + zRegularPractice + zNoInstruments + zJoinIN + zSinginHarmony + zDontSingPubic + zSingBack23 + zHearOnceSingBack + zIdentifySpecial
zBeatPerception ~ gen
zMelodicMemory ~ gen
## Covariances
zMelodicMemory ~~ zBeatPerception
'
## Fits
model.fit.E.b <- sem(model.E.b, data = semdata, sample.nobs = 239, std.lv = FALSE)
semPaths(model.fit.E.b, whatLabels = "std", edge.label.cex = 2)
summary(model.fit.E.b)
fitMeasures(model.fit.E.b)

semPaths(model.fit.E.b,
         layout = "tree2",
         residuals = FALSE,
         allVars = FALSE, 
         intercepts = FALSE, 
         whatLabels = "std", 
         thresholds = FALSE, 
         nDigits = 2,
         rotation = 2)

modificationIndices(model.fit.E.b, standardized = TRUE)
fitMeasures(model.fit.E.b, fit.measures = c("df","pvalue","chisq", "cfi","rmsea","tli"))

residuals(model.fit.E.b, type = "cor")$cor

anova(model.fit.B, model.fit.E.b)


