suppressWarnings(library(tidyverse))
suppressWarnings(library(ggplot2))
suppressWarnings(library(forcats))

base_feature_descriptions <- c(
  "abs_int_range"	= "Size of largest interval",
  "art_range"=	"Range of articulation",
  "cdpcx_density_1" = "Relative frequency of chordal roots",
  "cdpcx_density_10" = "Relative frequency of chordal sharp thirds",
  "cdpcx_density_3" = "Relative frequency of chordal thirds",
  "cdpcx_density_5" = "Relative frequency of chordal fifths",
  "cdpcx_density_6" = "Relative frequency of chordal sixths",
  "cdpcx_density_b2" = "Relative frequency of flat ninths",
  "cdpcx_density_T" = "Relative frequency of sharp elevenths",
  "durclass_abs_entropy" = "Entropy of absolute duration classes",
  "durclass_abs_hist_01_very_short" = "Relative frequency of very short tones (< 16th notes in 120 bpm)",
  "expressive" = "Relative frequency of expressive midlevel units",
  "f0_median_dev" = "Median deviation from nominal 12-TET pitch",
  "f0_abs_median_dev" = "Mean absolute median deviation from nominal 12-TET pitch",
  "f0_sd_median_dev" = "Standard deviation of median deviation from nominal 12-TET pitch",
  "fragment" = "Relative frequency of fragment midlevel units",
  "int_bigram_entropy" = "Entropy of interval combinations",
  "lick" = "Relative frequency of lick midlevel units",
  "line" = "Relative frequency of line midlevel units",
  "loudness_sd" = "Standard deviation of tone loudness",
  "pitch_entropy" = "Entropy of pitch distribution",
  "pitch_range" = "Pitch range",
  "pitch_std" = "Mean distance from mean pitch.",
  "rhythm" = "Relative frequency of rhythm midlevel units",
  "total_duration" = "Total duration of solo (in sec)",
  "void" = "Relative frequency of void midlevel units",
  "theme" = "Relative frequency of theme midlevel units",
  "quote" = "Relative frequency of quote midlevel units",
  "melody" = "Relative frequency of melody midlevel units"
)
#get_feature_description<-function(feature_name){
#  desc <- feature_descriptions[feature_name]
#  if(is.na(desc)){
#    desc <- "Sorry, no description available yet"
#  }
#  desc
#}
