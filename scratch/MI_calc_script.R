# CODE TO CALCULATE MI #

# I apologize for it is clunky, if you think of something more streamlined then go with it
# Essentially we use linear regression to identify the slope and intercept from the raw (or averaged) data
# Then we generate specific MI calc functions for each species
# Perhaps if we had more we'd need a better way to do this but for the few that we have I think ok
# I also am including easy data  (averaged by temp) but if we can get raw, we should use that and compare to the average
# The easy data I am generating here so its even easier lol

# LOAD DATA AND PACKAGES #####
library(tidyverse)

bsb.data = data.frame(Pcrit = c(4.13, 4.48, 4.58, 6.64, 7.95), 
                      Temp = c(12, 17, 22, 27, 30)) %>%
  mutate(inv.T =  1/(0.0000862*(Temp + 273)), 
         ln.Pcrit = log(Pcrit))
bsb.data

# RUN LM TO GET PARAMS #####
bsb.mod = lm(ln.Pcrit ~ inv.T, data = bsb.data)
shapiro.test(bsb.mod$residuals)
summary(bsb.mod)
# Intercept: 12.25391 (note: this is sometimes how it is reported in papers but the full transformation happens below;
# sometimes the transformed value is the one in papers; confusing I know)
# Slope: 0.26836

min(bsb.data$inv.T)
# 38.29 
max(bsb.data$inv.T)
# 40.71

inv.T = seq(38, 41, 0.1)

# CHECK PARAMS
bsb.mi.params.pred = data.frame(inv.T = inv.T) %>%
  mutate(pred = predict(bsb.mod, ., type = "response"), 
         se = predict(bsb.mod, ., type = "response", se.fit = T)$se,
         lower = pred - se, 
         upper = pred + se)
head(bsb.mi.params.pred)

a0.e0.plot.bsb = ggplot(bsb.mi.params.pred, aes(x = inv.T, y = pred)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = F) +
  geom_line(size = 1) +
  geom_point(data = bsb.data, aes(x = inv.T, y = ln.Pcrit), size = 3, color = "blue") +
  theme_bw() +
  theme(panel.grid = element_blank(), 
        strip.background = element_rect(fill = "white"),
        axis.text = element_text(size = 8, color = "black"), 
        axis.title = element_text(size = 8, color = "black"), 
        title = element_text(size = 8, color = "black"), 
        legend.text = element_text(size = 8, color = "black")) + 
  labs(y = expression(paste("ln(Pcrit)")), 
       x = expression(paste("Inv. Temp"))) 
a0.e0.plot.bsb

# GENERATE MI FUNCTION #####

MI.calc.function.bsb = function(Temp, Oxygen) {
  KB = 0.0000862
  A0 = 1/((exp(12.25391))/100) # sidebar: if you run this you get 4.77x10^-4 which is what is reported in my paper
  E0 = 0.26836 
  invKBT = 1/(KB*(Temp + 273))
  num = A0*(Oxygen/100)
  den = exp(-E0*invKBT)
  MI = num/den
  return(MI)
}

# check with values that make sense 
MI.calc.function.bsb(15, 21)
# 4.96
MI.calc.function.bsb(35, 21)
# 2.46
MI.calc.function.bsb(15, 5)
# 1.18

# looking good, sensitivity at very warm or very low oxygen levels