########### plots:
library(ggplot2)
library(reshape2)



surv_diff1 <- data.frame(days_since_inclusion = 1:365,
                         No_Agreement = Sa0)
surv_diff2 <- data.frame(days_since_inclusion = 1:365,
                         Total_effect = TE)
surv_diff3 <- data.frame(days_since_inclusion = 1:365,
                         AW_component = SE1)
surv_diff4 <- data.frame(days_since_inclusion = 1:365,
                         Mix_AGAW = SE2)



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
  ggtitle("No Agreement") + ylab("") + xlab("(a) days since inclusion")


p2 <- ggplot(surv_diff2) + 
  scale_y_continuous(limits = c(-0.08,0.02),
                     breaks = seq(-0.08,0.02, by = 0.02)) +
  geom_step(aes(days_since_inclusion, Total_effect), size = 1.2) +
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
  ggtitle("TE") + ylab("") + xlab("(b) days since inclusion") +
  geom_ribbon(aes(x = 1:365, 
                  ymin= Total_effect - 1.96 * sd_te, 
                  ymax= Total_effect + 1.96 * sd_te), alpha=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)


p3 <- ggplot(surv_diff3) + 
  scale_y_continuous(limits = c(-0.08,0.02),
                     breaks = seq(-0.08,0.02, by = 0.02)) +
  geom_step(aes(days_since_inclusion, AW_component), size = 1.2) +
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
  ggtitle("SE1") + ylab("") + xlab("(c) days since inclusion") +
  geom_ribbon(aes(x = 1:365, 
                  ymin= AW_component - 1.96 * sd_se1, 
                  ymax= AW_component + 1.96 * sd_se1), alpha=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) 


p4 <- ggplot(surv_diff4) + 
  scale_y_continuous(limits = c(-0.08,0.02),
                     breaks = seq(-0.08,0.02, by = 0.02)) +
  geom_step(aes(days_since_inclusion, Mix_AGAW), size = 1.2) +
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
  ggtitle("SE2") + ylab("") + xlab("(d) days since inclusion") +
  geom_ribbon(aes(x = 1:365, 
                  ymin= Mix_AGAW - 1.96 * sd_se2, 
                  ymax= Mix_AGAW + 1.96 * sd_se2), alpha=0.2) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1)




gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)













