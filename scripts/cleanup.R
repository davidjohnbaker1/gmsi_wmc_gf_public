library(GGally)
library(data.table)
library(psych)
library(ggplot2)
library(lavaan)
library(semPlot)
library(MVN)

#--------------------------------------------------
semdata <- fread("data/aggregated_data/AnalysisData-Deletion.csv")

names(semdata)

# Do it like paper OR do it with individual items? 
# GENERAL  =~ X2 + X6 + X7 + X9 + X11 + X18 +  X19 + X20 + X21 +  X22 + X25 + X26 + X27 + X28 + X29 + X30 + X32 + X33
semdata
2 + 38
adderVector <- c(2,6,7,9,11,18,19,20,21,22,25,26,27,28,29,30,32,33)

# THIS IS CORRECT GENERAL FACTOR, SWAP IN OTHER PAPER 
adderVectorCorrect <- c(1,2,4,6,13, 17,19,20,21,25,26,29,30,31,32,36)
length(adderVector)
forMeasurementModel <- adderVectorCorrect + 38
forMeasurementModel
names(semdata)

# Fix Name Problems
setnames(semdata,c("V39","V40","V41","V42","V43","V44","V45","V46",
                    "V47","V48","V49","V50","V51","V52","V53",
                    "V54","V55","V56","V57","V58","V59","V60",
                    "V61","V62","V63","V64","V65","V66","V67","V68",
                    "V69","V70","V71","V72","V73","V74","V75","V76"),
         c("Freetime",
           "Writing",
           "MusicalStyles",
           "SearchInternet",
           "SpendMoney",
           "Addiction",
           "KeepTrack",
           "LiveEvents",
           "ListenAttentively",
           "Singer",
           "HearFirstTime",
           "HardSpot",
           "ComparePerf",
           "SameSong",
           "HearBeat",
           "HearTune",
           "SelfTonal",
           "IDgenre",
           "NeverComplimented",
           "NotConsiderSelf",
           "RegularPractice",
           "PeakInterest",
           "MusicTheory",
           "Formal",
           "NoInstruments",
           "JoinIN",
           "SingByMemory",
           "HitRightNoteSingAlong",
           "SinginHarmony",
           "DontSingPubic",
           "SingBack23",
           "HearOnceSingBack",
           "ChooseMusic",
           "PiecesEmotion",
           "ExciteMotivate",
           "IdentifySpecial",
           "TalkEmotionslPiece",
           "EvokesMemories"))


#--------------------------------------------------
# Make Z Scores 
#--------------------------------------------------
semdata[, zAdjustedNumberSeries := scale(AdjustedNumberSeries)]
semdata[, zRavensAvg := scale(RavensAvg)]

semdata[, zMeanOspanPartialScore := scale(MeanOspanPartialScore)]
semdata[, zMeanSspanPartialScore := scale(MeanSspanPartialScore)]
semdata[, zTonePartial := scale(TonePartial)]

semdata[, zBeatPerception := scale(BeatPerception)]
semdata[, zMelodicMemory := scale(MelodicMemory)]

semdata[, zFreetime := scale(Freetime)]
semdata[, zWriting := scale(Writing)] 
semdata[, zMusicalStyles := scale(MusicalStyles)] 
semdata[, zSearchInternet := scale(SearchInternet)] 
semdata[, zSpendMoney := scale(SpendMoney)] 
semdata[, zAddiction := scale(Addiction)] 
semdata[, zKeepTrack := scale(KeepTrack)] 
semdata[, zLiveEvents := scale(LiveEvents)] 
semdata[, zListenAttentively := scale(ListenAttentively)] 
semdata[, zSinger := scale(Singer)] 
semdata[, zHearFirstTime := scale(HearFirstTime)] 
semdata[, zHardSpot := scale(HardSpot)] 
semdata[, zComparePerf := scale(ComparePerf)] 
semdata[, zSameSong := scale(SameSong)] 
semdata[, zHearBeat := scale(HearBeat)] 
semdata[, zHearTune := scale(HearTune)] 
semdata[, zSelfTonal := scale(SelfTonal)] 
semdata[, zIDgenre := scale(IDgenre)] 
semdata[, zNeverComplimented := scale(NeverComplimented)] 
semdata[, zNotConsiderSelf := scale(NotConsiderSelf)] 
semdata[, zRegularPractice := scale(RegularPractice)] 
semdata[, zPeakInterest := scale(PeakInterest)] 
semdata[, zMusicTheory := scale(MusicTheory)] 
semdata[, zFormal := scale(Formal)] 
semdata[, zNoInstruments := scale(NoInstruments)] 
semdata[, zJoinIN := scale(JoinIN)] 
semdata[, zSingByMemory := scale(SingByMemory)] 
semdata[, zHitRightNoteSingAlong := scale(HitRightNoteSingAlong)] 
semdata[, zSinginHarmony := scale(SinginHarmony)] 
semdata[, zDontSingPubic := scale(DontSingPubic)] 
semdata[, zSingBack23 := scale(SingBack23)] 
semdata[, zHearOnceSingBack := scale(HearOnceSingBack)] 
semdata[, zChooseMusic := scale(ChooseMusic)] 
semdata[, zPiecesEmotion := scale(PiecesEmotion)] 
semdata[, zExciteMotivate := scale(ExciteMotivate)] 
semdata[, zIdentifySpecial := scale(IdentifySpecial)] 
semdata[, zTalkEmotionslPiece := scale(TalkEmotionslPiece)] 
semdata[, zEvokesMemories := scale(EvokesMemories)] 
#--------------------------------------------------
      
# Check for Normality
#--------------------------------------------------
ofInterest <- semdata[, .(Freetime , Writing , SearchInternet , Addiction , ComparePerf , SelfTonal , NeverComplimented , NotConsiderSelf , RegularPractice , NoInstruments , JoinIN , SinginHarmony , DontSingPubic , SingBack23 , HearOnceSingBack , IdentifySpecial)]

descriptiveOfInterest <- describe(ofInterest)

descriptiveOfInterest.dt <- data.table(describe(ofInterest))
descriptiveOfInterest.dt[order(skew)]
  