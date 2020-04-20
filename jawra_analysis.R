
# Load data ---------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

fn = "nhd_pdsi_analysis.csv"

library(tidyverse)
library(broom)

indat <- as.data.frame(read_csv(fn))
dpdsi <- indat$dPdsi
indat$dPdsi <- (indat$NhdPdsi + 100) - (indat$PtPdsi + 100)
write.csv(indat,"E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\data_release\\PRE\\nhd_pdsi_analysis.csv")


# Logisitc regression -----------------------------------------------------

indat$dPdsiLT0 <- ifelse(indat$dPdsi<0, indat$dPdsi, 0)
indat$dPdsiGT0 <- ifelse(indat$dPdsi>=0, indat$dPdsi, 0)
lr.spline.difsoint <- glm(Disagree ~ Category*dPdsiLT0 + Category*dPdsiGT0 + Category*as.factor(StrOrd), 
                          data=indat, family=binomial)

summary(lr.spline.difsoint)
res <- summary(lr.spline.difsoint)


# Plot logistic regression by stream order --------------------------------

model.data <- augment(lr.spline.difsoint) %>% mutate(index = 1:n())
model.data$pdsi_dif <- model.data$dPdsiLT0 + model.data$dPdsiGT0
score <- qnorm((0.95/2) + 0.5)
model.data$lwr <- plogis(model.data$.fitted-score*model.data$.se.fit)
model.data$upr <- plogis(model.data$.fitted+score*model.data$.se.fit)
#model.data$panelname <- paste("Stream Order:", model.data$StrOrd)
model.data$panelname <- paste("Stream Order:", model.data$as.factor.StrOrd.)

ggplot(model.data, aes(pdsi_dif, plogis(.fitted))) +
  geom_ribbon(aes(x=pdsi_dif, ymin=lwr, ymax=upr, group=Category), alpha = 0.2) + 
  geom_line(aes(color=Category)) + 
  geom_point(aes(pdsi_dif, Disagree, colour=Category), alpha=0.1) +
  scale_x_continuous(breaks=seq(-8,8,4)) +
  scale_color_manual(values=c("#fb0026", "#0c51fd")) +
  facet_wrap(~panelname, ncol=3) + 
  labs(x = "PDSI difference", y = "Probability of disagreement", color = "Observation type") +
  #ggtitle("Probability of Disagreement by Stream Order (NHDPlus-HR)") +
  theme(panel.grid.major = element_line(colour = 'gray60', size=0.01), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=14), axis.text=element_text(size=12),
        plot.title=element_text(size=16, face="bold", margin=margin(t=0, r=0, b=20, l=0)),
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0)),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        strip.text = element_text(size=12),
        strip.background = element_rect(fill="transparent"),
        legend.position =c(0.85,0.22), 
        legend.justification = c(1,1), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=12))

# Save figure -------------------------------------------------------------

ggsave("E:/konrad/Projects/usgs/prosper-nhd/figs/figs/lr_model_v2.png", plot = last_plot(), 
       width = 7, height = 4.5, units = "in", bg = "white", dpi=300)
