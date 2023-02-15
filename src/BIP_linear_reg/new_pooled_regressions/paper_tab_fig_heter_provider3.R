
lm_fe_provider3[[1]][["Estimate"]] <- as.numeric(levels(lm_fe_provider3[[1]][["Estimate"]]))[lm_fe_provider3[[1]][["Estimate"]]]
est <- as.data.frame(lm_fe_provider3[[1]][c(19:23), c(2,3,5)])
ftth <- rbind(est, lm_fe_provider3[[1]][["5 %"]][length(lm_fe_provider3[[1]][["5 %"]])])

lmfe_provider3_robust10 <- as.data.frame(lmfe_provider3_robust10[c(13:17), c(1,2,4)])
lmfe_provider3_robust10 <- rbind(lmfe_provider3_robust10, lmfe_provider3_robust10["Std. Error"][nrow(lmfe_provider3_robust10["Std. Error"]),])

match_ftth <- as.data.frame(provider3_match) 
match_ftth["tstat"] <- match_ftth$est/match_ftth$se
match_ftth["pval"] <- round(pt(match_ftth$tstat, match_ftth$wnobs), 3)
match_ftth["se"] <- round(match_ftth$se, 3)
match_ftth["est_round"] <- round(match_ftth$est, 3)
match_ftth <- match_ftth %>% mutate(est_sig = case_when(pval <= 0.01 ~ paste0(round(match_ftth$est, 3), "***"),
                                                        pval <= 0.05 &  pval > 0.01 ~  paste0(round(match_ftth$est, 3), "**"),
                                                        pval <= 0.1 &  pval > 0.05 ~  paste0(round(match_ftth$est, 3), "*"),
                                                        pval > 0.1 ~  paste0(round(match_ftth$est, 3))))
