dats <- cbind(as.numeric(imageClassificationDataLong_2$stimNo), as.numeric(imageClassificationDataLong_2$response_i)) %>% as.data.frame()
colnames(dats) <- c("stimNo", "response")
dats2 <- dcast(dats, stimNo~response) 
colnames(dats2) <- c("stimNo", "false", "correct")
dats2$pCorr <- dats2$correct / (dats2$false + dats2$correct)
dats2$trainingBase <- c(2,3,1,1,
                        3,1,1,2,
                        3,3,1,2,
                        1,2,1,1) # 1 - genuine, 2 - A15, 3 - A3

ggplot(dats2, aes( x=stimNo, y=pCorr, fill = trainingBase)) +
  geom_bar(stat="Identity") +
  plot_theme +
  ylim(0,1)

## Chi-2 of independence across categories
dats2_long <- melt(dats2, id.vars = "trainingBase", measure.vars = c("false", "correct"))
contingency_table <- xtabs(value ~ trainingBase + variable, data = dats2_long)
contingency_table
chisq.test(contingency_table)
chisq.test(contingency_table[c(1,2),])
chisq.test(contingency_table[c(2,3),])
chisq.test(contingency_table[c(1,3),])


## Chi-2 for homogeneiety within category
dats2 %>% filter(trainingBase==1) %>% .[,c(2,3)] %>% chisq.test()
dats2 %>% filter(trainingBase==2) %>% .[,c(2,3)] %>% chisq.test()
dats2 %>% filter(trainingBase==3) %>% .[,c(2,3)] %>% chisq.test()
