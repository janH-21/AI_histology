##### ALL PARTICIPANTS #####
# ectract data
gen_1_all <- imageClassificationDataLong_2 %>% filter(trainingBase=="B0") %>% filter(response=="correct") %>% nrow
gen_0_all <- imageClassificationDataLong_2 %>% filter(trainingBase=="B0") %>% filter(response=="incorrect") %>% nrow
A3_1_all <- imageClassificationDataLong_2 %>% filter(trainingBase=="F3") %>% filter(response=="correct") %>% nrow
A3_0_all <- imageClassificationDataLong_2 %>% filter(trainingBase=="F3") %>% filter(response=="incorrect") %>% nrow
A15_1_all <- imageClassificationDataLong_2 %>% filter(trainingBase=="F15") %>% filter(response=="correct") %>% nrow
A15_0_all <- imageClassificationDataLong_2 %>% filter(trainingBase=="F15") %>% filter(response=="incorrect") %>% nrow

# A3
TP_3_all <- A3_1_all
FP_3_all <- gen_0_all
TN_3_all <- gen_1_all
FN_3_all <- A3_0_all
all_3_allpos <- TP_3_all + FN_3_all
all_3_allneg <- TN_3_all + FP_3_all
sensitivity_ci_3_all <- binconf(TP_3_all, all_3_allpos, method="wilson")
specificity_ci_3_all <- binconf(TN_3_all, all_3_allneg, method="wilson")
accuracy_ci_3_all <- binconf(TP_3_all+TN_3_all, TP_3_all+FP_3_all+TN_3_all+FN_3_all, method="wilson")

# A15
TP_15_all <- A15_1_all
FP_15_all <- gen_0_all
TN_15_all <- gen_1_all
FN_15_all <- A15_0_all
all_15_allpos <- TP_15_all + FN_15_all
all_15_allneg <- TN_15_all + FP_15_all
sensitivity_ci_15_all <- binconf(TP_15_all, all_15_allpos, method="wilson")
specificity_ci_15_all <- binconf(TN_15_all, all_15_allneg, method="wilson")
accuracy_ci_15_all <- binconf(TP_15_all+TN_15_all, TP_15_all+FP_15_all+TN_15_all+FN_15_all, method="wilson")


##### NAIVE PARTICIPANTS #####
# ectract data
gen_1_nav <- imageClassificationDataLong_2 %>% filter(trainingBase=="B0") %>% filter(response=="correct") %>% filter(Image_seen_before=="no") %>% nrow
gen_0_nav <- imageClassificationDataLong_2 %>% filter(trainingBase=="B0") %>% filter(response=="incorrect") %>% filter(Image_seen_before=="no") %>% nrow
A3_1_nav <- imageClassificationDataLong_2 %>% filter(trainingBase=="F3") %>% filter(response=="correct") %>% filter(Image_seen_before=="no") %>% nrow
A3_0_nav <- imageClassificationDataLong_2 %>% filter(trainingBase=="F3") %>% filter(response=="incorrect") %>% filter(Image_seen_before=="no") %>% nrow
A15_1_nav <- imageClassificationDataLong_2 %>% filter(trainingBase=="F15") %>% filter(response=="correct") %>% filter(Image_seen_before=="no") %>% nrow
A15_0_nav <- imageClassificationDataLong_2 %>% filter(trainingBase=="F15") %>% filter(response=="incorrect") %>% filter(Image_seen_before=="no") %>% nrow

# A3
TP_3_nav <- A3_1_nav
FP_3_nav <- gen_0_nav
TN_3_nav <- gen_1_nav
FN_3_nav <- A3_0_nav
nav_3_allpos <- TP_3_nav + FN_3_nav
nav_3_allneg <- TN_3_nav + FP_3_nav
sensitivity_ci_3_nav <- binconf(TP_3_nav, nav_3_allpos, method="wilson")
specificity_ci_3_nav <- binconf(TN_3_nav, nav_3_allneg, method="wilson")
accuracy_ci_3_nav <- binconf(TP_3_nav+TN_3_nav, TP_3_nav+FP_3_nav+TN_3_nav+FN_3_nav, method="wilson")

# A15
TP_15_nav <- A15_1_nav
FP_15_nav <- gen_0_nav
TN_15_nav <- gen_1_nav
FN_15_nav <- A15_0_nav
nav_15_allpos <- TP_15_nav + FN_15_nav
nav_15_allneg <- TN_15_nav + FP_15_nav
sensitivity_ci_15_nav <- binconf(TP_15_nav, nav_15_allpos, method="wilson")
specificity_ci_15_nav <- binconf(TN_15_nav, nav_15_allneg, method="wilson")
accuracy_ci_15_nav <- binconf(TP_15_nav+TN_15_nav, TP_15_nav+FP_15_nav+TN_15_nav+FN_15_nav, method="wilson")

##### EXPERT PARTICIPANTS #####
# ectract data
gen_1_exp <- imageClassificationDataLong_2 %>% filter(trainingBase=="B0") %>% filter(response=="correct") %>% filter(Image_seen_before=="yes") %>% nrow
gen_0_exp <- imageClassificationDataLong_2 %>% filter(trainingBase=="B0") %>% filter(response=="incorrect") %>% filter(Image_seen_before=="yes") %>% nrow
A3_1_exp <- imageClassificationDataLong_2 %>% filter(trainingBase=="F3") %>% filter(response=="correct") %>% filter(Image_seen_before=="yes") %>% nrow
A3_0_exp <- imageClassificationDataLong_2 %>% filter(trainingBase=="F3") %>% filter(response=="incorrect") %>% filter(Image_seen_before=="yes") %>% nrow
A15_1_exp <- imageClassificationDataLong_2 %>% filter(trainingBase=="F15") %>% filter(response=="correct") %>% filter(Image_seen_before=="yes") %>% nrow
A15_0_exp <- imageClassificationDataLong_2 %>% filter(trainingBase=="F15") %>% filter(response=="incorrect") %>% filter(Image_seen_before=="yes") %>% nrow

# A3
TP_3_exp <- A3_1_exp
FP_3_exp <- gen_0_exp
TN_3_exp <- gen_1_exp
FN_3_exp <- A3_0_exp
exp_3_allpos <- TP_3_exp + FN_3_exp
exp_3_allneg <- TN_3_exp + FP_3_exp
sensitivity_ci_3_exp <- binconf(TP_3_exp, exp_3_allpos, method="wilson")
specificity_ci_3_exp <- binconf(TN_3_exp, exp_3_allneg, method="wilson")
accuracy_ci_3_exp <- binconf(TP_3_exp+TN_3_exp, TP_3_exp+FP_3_exp+TN_3_exp+FN_3_exp, method="wilson")

# A15
TP_15_exp <- A15_1_exp
FP_15_exp <- gen_0_exp
TN_15_exp <- gen_1_exp
FN_15_exp <- A15_0_exp
exp_15_allpos <- TP_15_exp + FN_15_exp
exp_15_allneg <- TN_15_exp + FP_15_exp
sensitivity_ci_15_exp <- binconf(TP_15_exp, exp_15_allpos, method="wilson")
specificity_ci_15_exp <- binconf(TN_15_exp, exp_15_allneg, method="wilson")
accuracy_ci_15_exp <- binconf(TP_15_exp+TN_15_exp, TP_15_exp+FP_15_exp+TN_15_exp+FN_15_exp, method="wilson")

##### PLOT RESULTS #####
val <- 1:18
est <- rbind(sensitivity_ci_3_all, specificity_ci_3_all, accuracy_ci_3_all,
             sensitivity_ci_15_all, specificity_ci_15_all, accuracy_ci_15_all,
             sensitivity_ci_3_nav, specificity_ci_3_nav, accuracy_ci_3_nav,
             sensitivity_ci_15_nav, specificity_ci_15_nav, accuracy_ci_15_nav,
             sensitivity_ci_3_exp, specificity_ci_3_exp, accuracy_ci_3_exp,
             sensitivity_ci_15_exp, specificity_ci_15_exp, accuracy_ci_15_exp)

dats <- cbind(val, est) %>% as.data.frame()

ggplot(dats, aes(x=val, y=PointEst)) +
  geom_point(size=5, shape=16) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=0, size=1) +
  labs(y= "Respones [%]", x = "") +
  plot_theme
