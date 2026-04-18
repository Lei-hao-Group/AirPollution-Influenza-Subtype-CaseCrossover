
#Taking the association between PM2.5 and the influenza virus infection as an example.

#############################################

library(survival)
library(splines)
library(dlnm)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(extrafont)

#############################################

#Associations between air pollution and influenza virus infection

#cpmputing the cross-basis
cb.pm25 <- crossbasis(
  data %>% select(starts_with("pm25p")),
  lag=6,
  argvar=list(fun = "lin"),
  arglag=list(fun = "ns", knots = exp((log(6))/(3)*1:2))
)

#run the model
modelpm25<-clogit(result~cb.pm25+ns(temp03,df=3)+ns(rh03,df=3)+strata(ID)+holiday,data=data,na.action = na.omit)

#predictions for per 10μg increment
predpm25<-crosspred(cb.pm25,modelpm25,cen = 0,at=10,cumul = T)


#############################################

#Linear and nonlinear dose–response curves

#Data up to the 99th percentile
p0<-quantile(data$pm25p0, probs = 0.99, na.rm = TRUE)
p1<-quantile(data$pm25p1, probs = 0.99, na.rm = TRUE)
p2<-quantile(data$pm25p2, probs = 0.99, na.rm = TRUE)
p3<-quantile(data$pm25p3, probs = 0.99, na.rm = TRUE)
p4<-quantile(data$pm25p4, probs = 0.99, na.rm = TRUE)
p5<-quantile(data$pm25p5, probs = 0.99, na.rm = TRUE)
p6<-quantile(data$pm25p6, probs = 0.99, na.rm = TRUE)
data <- data[data$pm25p0 <= p0 & data$pm25p1 <= p1 & data$pm25p2 <= p2 &data$pm25p3 <= p3 &data$pm25p4 <= p4 &data$pm25p5 <= p5 &data$pm25p6 <= p6 , ]

#Distributed lag linear models
cb.pm25 <- crossbasis(
  data %>% select(starts_with("pm25p")),
  lag=6,
  argvar=list(fun = "lin"),
  arglag=list(fun = "ns", knots = exp((log(6))/(3)*1:2))
)
modelpm25<-clogit(result~cb.pm25+ns(temp03,df=3)+ns(rh03,df=3)+strata(ID)+holiday,data=data,na.action = na.omit)
DLM<-crosspred(cb.pm25,modelpm25,cen = 0,by=1,cumul = T)

#Distributed lag nonlinear models
cb.pm25dlnm <- crossbasis(
  data %>% select(starts_with("pm25p")),
  lag=6,
  argvar=list(fun = "ns", df=3),
  arglag=list(fun = "ns", knots = exp((log(6))/(3)*1:2))
)
modelpm25dlnm<-clogit(result~cb.pm25dlnm+ns(temp03,df=3)+ns(rh03,df=3)+strata(ID)+holiday,data=data,na.action = na.omit)
DLNM<-crosspred(cb.pm25dlnm,modelpm25dlnm,cen = 0,by=1,cumul = T)

# plotting linear and nonlinear dose–response curves
plot_data <- data.frame(
  exposure = rep(DLNM$predvar, 2),
  RR = c(DLNM$allRRfit, DLM$allRRfit),
  low = c(DLNM$allRRlow, DLM$allRRlow),
  high = c(DLNM$allRRhigh, DLM$allRRhigh),
  model = rep(c("DLNM", "DLM"), each = length(DLNM$predvar))
)
color<-c("#F0776D","#29B4B6")
ggplot(plot_data, aes(x = exposure, y = RR, color = model, fill = model)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1.2) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = low, ymax = high), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values = color) +
  scale_fill_manual(values = color) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = seq(0, max(plot_data$exposure, na.rm = TRUE), by = 50)
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%.2f", x),
    limits = c(0.8, max(plot_data$high, na.rm = TRUE)),
    breaks = seq(1,1.6,by=0.2)
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 24, family = "sans"), 
    legend.key.size = unit(2, "lines"), 
    panel.border = element_rect(colour = "grey", fill = NA, size = 1.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 18, family = "sans"), 
    axis.text.y = element_text(size = 18, family = "sans"), 
    text = element_text(family = "sans"),
    plot.title = element_text(family = "sans")
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))
