##### Response with outliers removed
# --------------- define outliers as <> 1.5*IQR ---------------
tlim<- boxplot.stats(imageClassificationDataLong_2$time)$stats[c(1, 5)]
imageClassificationDataLong_2_outliersRemoved <- imageClassificationDataLong_2[imageClassificationDataLong_2$time >= tlim[1] &
                                                                                 imageClassificationDataLong_2$time <= tlim[2],]

# --------------- response time vs. correctness ---------------
wilcox.test(imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$response=="correct"],
            imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$response=="incorrect"], paired=FALSE)

ggplot(imageClassificationDataLong_2_outliersRemoved, aes(fill=response, x=response, y=time)) +
  geom_violin(width=0.5)+
  ylim(0,20) +
  ylab("Response Time [s]") +
  scale_color_manual(values = c("#03c7ff", "#fc7303")) +
  plot_theme +
  stat_summary(fun=median, geom="crossbar")


# --------------- response time vs. experience ---------------
wilcox.test(imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$Image_seen_before=="yes"],
            imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$Image_seen_before=="no"], paired=FALSE)

ggplot(imageClassificationDataLong_2_outliersRemoved, aes(fill=Image_seen_before, x=Image_seen_before, y=time)) +
  geom_violin(width=0.5)+
  ylim(0,20) +
  ylab("Response Time [s]") +
  scale_color_manual(values = c("#03c7ff", "#fc7303")) +
  plot_theme +
  stat_summary(fun=median, geom="crossbar")


# --------------- prior experience <> classification correctness ---------------
imageClassificationDataLong_2_outliersRemoved <- imageClassificationDataLong_2_outliersRemoved %>%
  mutate(
    plotGroup = case_when(
      response == "correct" & Image_seen_before == "no" ~ "naive, correct answer",
      response == "incorrect" & Image_seen_before == "no" ~ "naive, incorrect answer",
      response == "correct" & Image_seen_before == "yes" ~ "expert, correct answer",
      response == "incorrect" & Image_seen_before == "yes" ~ "expert, incorrect answer",
    )
  )
group1 <- imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$plotGroup == "naive, correct answer"]
group2 <- imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$plotGroup == "naive, incorrect answer"]
group3 <- imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$plotGroup == "expert, correct answer"]
group4 <- imageClassificationDataLong_2_outliersRemoved$time[imageClassificationDataLong_2_outliersRemoved$plotGroup == "expert, incorrect answer"]
p1 <- wilcox.test(group1, group2, paired=FALSE)
p2 <- wilcox.test(group1, group3, paired=FALSE)
p3 <- wilcox.test(group1, group4, paired=FALSE)
p4 <- wilcox.test(group2, group3, paired=FALSE)
p5 <- wilcox.test(group2, group4, paired=FALSE)
p6 <- wilcox.test(group3, group4, paired=FALSE)
p_vals <- c(p1$p.value, p2$p.value, p3$p.value, p4$p.value, p5$p.value, p6$p.value)
p_adj <- p_vals * length(p_vals)
p_adj[p_adj > 1] <- 1
p_adj

ggplot(imageClassificationDataLong_2_outliersRemoved, aes(fill=plotGroup, x=plotGroup, y=time)) +
  geom_violin(width=0.5)+
  ylim(0,20) +
  ylab("Response Time [s]") +
  scale_color_manual(values = c("#03c7ff", "#fc7303")) +
  plot_theme +
  stat_summary(fun=median, geom="crossbar")


# --------------- participant-wise response times ---------------
# preallocate
pairwise_data_length <- length(unique(imageClassificationDataLong_2_outliersRemoved$ID)) *
  length(unique(imageClassificationDataLong_2_outliersRemoved$response))
pairwise_data <- data.frame(no=rep(NA,pairwise_data_length),
                            id=rep(NA,pairwise_data_length),
                            experience=rep(NA,pairwise_data_length),
                            response=rep(NA,pairwise_data_length),
                            time_mean=rep(NA,pairwise_data_length))


counter = 1
for(x in unique(imageClassificationDataLong_2_outliersRemoved$ID)){
  for(y in unique(imageClassificationDataLong_2_outliersRemoved$response)){
    tmp_data <- imageClassificationDataLong_2_outliersRemoved %>% subset(ID == x & response == y)
    pairwise_data$no[counter] <- counter
    pairwise_data$id[counter] <- tmp_data$ID[1]
    pairwise_data$experience[counter] <- tmp_data$Image_seen_before[1]
    pairwise_data$response[counter] <- tmp_data$response[1]
    pairwise_data$time_mean[counter] <- tmp_data$time %>% mean
    counter = counter + 1
  }
}

na_ind <- which(is.na(pairwise_data$time_mean))

# add odd inds for even ones & vice versa
na_ind_addOdd <- na_ind[which(na_ind %% 2 == 0)] - 1
na_ind_addEven <- na_ind[which(na_ind %% 2 != 0)] + 1
na_ind_all <- c(na_ind, na_ind_addOdd, na_ind_addEven)
na_ind_all <- sort(setdiff(na_ind_all, na_ind_all[duplicated(na_ind_all)]))   # unique IDs after exclusion

# non-paired vs. paired data
nonpaired_data <- pairwise_data[na_ind_all,] # correct answers only
paired_data <- pairwise_data[-na_ind_all,]   # correct and incorrect answers
na_ind_all
# n participant (2 entries / participant)
length(na_ind_all)/2

# by experience
tbl_all_corrrect <- table(nonpaired_data$experience[nonpaired_data$response=="correct"])
tbl_all_corrrect

ggplot(paired_data, aes(fill=response, x=response, y=time_mean)) +
  geom_violin(width=0.5)+
  ylim(0,20) +
  ylab("Mean Response Time [s]") +
  scale_color_manual(values = c("#03c7ff", "#fc7303")) +
  plot_theme +
  stat_summary(fun=median, geom="crossbar")

wilcox.test(paired_data$time_mean[paired_data$response == "correct"],
            paired_data$time_mean[paired_data$response == "incorrect"], paired=TRUE)
