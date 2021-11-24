
cpenn_annotations <- read.csv("data/ia_annotations/C_penn_Set1_MH.csv")

pimp_annotations <- read.csv("data/ia_annotations/P_imparis_Set1_MB.csv")
# note - fix NC annotations
pimp_annotations <- rbind(pimp_annotations, read.csv("data/ia_annotations/P_imparis_Set1_NC.csv"))

sinv_annotations <- read.csv("data/ia_annotations/S_invicta_Set1_JI.csv")
sinv_annotations <- rbind(sinv_annotations, read.csv("data/ia_annotations/S_invicta_Set1_JO.csv"))

# get prop of nests
sum(cpenn_annotations$is_nest == "Y") / nrow(cpenn_annotations)
sum(sinv_annotations$is_nest == "Y") / nrow(sinv_annotations)
sum(pimp_annotations$is_nest == "Y") / nrow(pimp_annotations)
# s invicta has a ton of nests, others not so much

# get prop of males or queens
sum(cpenn_annotations$is_male_or_queen == "Y") / nrow(cpenn_annotations)
sum(sinv_annotations$is_male_or_queen == "Y") / nrow(sinv_annotations)
sum(pimp_annotations$is_male_or_queen == "Y") / nrow(pimp_annotations)

# get is night
sum(cpenn_annotations$is_night == "Y") / nrow(cpenn_annotations)
sum(sinv_annotations$is_night == "Y") / nrow(sinv_annotations)
sum(pimp_annotations$is_night == "Y") / nrow(pimp_annotations)

# get is flagged
sum(cpenn_annotations$is_flagged == "Y") / nrow(cpenn_annotations)
sum(sinv_annotations$is_flagged == "Y") / nrow(sinv_annotations)
sum(pimp_annotations$is_flagged == "Y") / nrow(pimp_annotations)

# get is multiple
sum(cpenn_annotations$is_multiple == "Y") / nrow(cpenn_annotations)
sum(sinv_annotations$is_multiple == "Y") / nrow(sinv_annotations)
sum(pimp_annotations$is_multiple == "Y") / nrow(pimp_annotations)
#annotations <- rbind(annotations, read.csv("data/ia_annotations/C_penn_Set1_MH.csv"))

