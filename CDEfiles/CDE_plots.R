################# plot
library(ggplot2)
library(reshape2)

surv_diff1 <- data.frame(days_since_inclusion = 1:365,
                         No_Agreement = cdea0$CDE)
surv_diff2 <- data.frame(days_since_inclusion = 1:365,
                         CDE = cdea1$CDE - cdea0$CDE)

p1 <- ggplot(surv_diff1) + 
  geom_step(aes(days_since_inclusion, No_Agreement), size = 1.2) +
  theme_bw() + 
  theme(plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 25, colour = "black", angle = 0),
        strip.text.y = element_text(size = 25, colour = "black", angle = -90),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 25, colour = "black"),
        legend.text = element_text(size = 25, colour = "black"),
        legend.title = element_text(size = 25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        strip.background =element_rect(colour="white", fill="white")) +
  ggtitle("No agreement") + ylab("") + xlab("days since inclusion")

p2 <- ggplot(surv_diff2) + 
  geom_step(aes(days_since_inclusion, CDE), size = 1.2) +
  theme_bw() + 
  theme(plot.title = element_text(size = 25, hjust = 0.5),
        strip.text.x = element_text(size = 25, colour = "black", angle = 0),
        strip.text.y = element_text(size = 25, colour = "black", angle = -90),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 25, colour = "black"),
        legend.text = element_text(size = 25, colour = "black"),
        legend.title = element_text(size = 25, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        strip.background =element_rect(colour="white", fill="white")) +
  ggtitle("CDE") + ylab("") + xlab("days since inclusion") +
  geom_ribbon(aes(x = 1:365, 
                  ymin= 2 * CDE - quant_CDE[,2], 
                  ymax= 2 * CDE - quant_CDE[,1]), alpha=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)



gridExtra::grid.arrange(p1,p2,nrow=2)

