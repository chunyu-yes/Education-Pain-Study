library(ggplot2)

#--------------------------------------------------------------------------------
#Calculate marginal prevalence and visualize our findings
#The model with interaction will be used based on finding from China; other countries may be different
ELSA_inter

ELSA_inter.rg <- ref.grid(ELSA_inter)
ELSA.pain_adjusted <- as.data.frame(lsmeans(ELSA_inter.rg, ~ high_edu + female, type = "response"))

ggplot(ELSA.pain_adjusted, aes(as.factor(high_edu), lsmean, color=as.factor(female))) +        
  geom_point(shape=18, size = 5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.5, size = 1, position = position_dodge(0.5)) +
  scale_y_continuous(limits = c(-0.01, 0.61), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 
					 label = c(0, 10, 20, 30, 40, 50, 60)) +
  scale_color_manual("female", values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("No", "Yes"))  + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
		axis.line = element_line(colour = "black"), axis.text = element_text(size=16), axis.title = element_text(size=16),
		legend.key.height = unit(0.75, "cm"), legend.text = element_text(size = 16), legend.position = "none",
		legend.key = element_blank(), legend.box="vertical", legend.title = element_text(size = 16),
		strip.text.x = element_text(size = 16), axis.title.x = element_blank(),
		legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
		plot.title = element_text(size = 18, hjust = 0.5)) +
  ylab("Prevalence of pain (%)") +
  scale_x_discrete(label = c("Low", "High")) +
  ggtitle("US (ELSA)")

#Do we also need figure to summarize all statistical analysis models across countries?


#The following code may be used to combine figures across countries
ELSA_q1 + ELSA_q2 + ELSA_q4 + ELSA_q5 + 
  mhas_q1 + mhas_q2 + mhas_q4 + mhas_q5 + 
  ELSA_q1 + ELSA_q2 + ELSA_q4 + ELSA_q5 + 
  lasi_q1 + lasi_q2 +   
  plot_layout(ncol = 4)