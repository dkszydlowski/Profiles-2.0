# compare manual chlorophyll to sonde chlorophyll

if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# for this, can use interpolated values for profiles to get as close to light depths as possible

# load in interpolated profiles
prof.clean.time = get(load(file = "./data/formatted data/cleaned profiles interpolated over depth and time.RData"))

# load in manual chl
chl.22.24 = get(load(file = "./data/formatted data/manual chlorophyll/routines chl 2024 and 2022.RData"))

# need to change names of chl.22.24 so that depth refers to the rounded depths
chl.22.24 = chl.22.24 %>% rename(depth.actual = depth, depth = depth.rounded)

sonde.man.chl = chl.22.24 %>% left_join(prof.clean.time, by = c("lake", "doy", "depth", "year"))


ggplot(sonde.man.chl %>% filter(log10(chl_ugL) != -Inf), aes(x = log10(manual.chl), y = log10(chl_ugL), fill = as.factor(ZID)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  facet_grid(lake~year)+
  #ylim(-1.5, 2.5)+
  #xlim(1, 3)+
  #scale_fill_gradient2()+
  theme_bw()+
  labs(y = "log10(sonde chl (ug/L))", x = "log10(manual chl (ug/L))", fill = "light depth")+
  geom_smooth(data = . %>% filter(lake == "L"), aes(group = 1), color = "black", se = FALSE)




ggplot(sonde.man.chl %>% filter(lake == "L" & chl_ugL > 0 & manual.chl > 0), aes(x = log10(manual.chl), y = log10(chl_ugL), fill = as.factor(year),
                                                                                 color = as.factor(year)))+
  geom_point(pch = 21, size = 3, alpha = 0.6, color = "black")+
  #facet_grid(lake~year)+
  theme_classic()+
  geom_smooth( se = FALSE, linetype = "dashed")+
  labs(x = "log10(manual chlorophyll ug/L)", y = "log10(sonde chlorophyll ug/L)", fill = "year", color = "year")+
  theme(axis.title = element_text(size = 14))+
  theme(axis.text = element_text(size = 14))
  
  


ggplot(sonde.man.chl %>% filter(lake == "R"), aes(x = log10(manual.chl), y = log10(chl_ugL), fill = as.factor(year)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  #facet_grid(lake~year)+
  theme_classic()+
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed")

ggplot(sonde.man.chl %>% filter(lake == "T"), aes(x = log10(manual.chl), y = log10(chl_ugL), fill = as.factor(year)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  #facet_grid(lake~year)+
  theme_classic()+
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed")





sonde.man.chl = sonde.man.chl %>% mutate(log.chl.ugl = replace(log.chl.ugl, log.chl.ugl == -Inf, NA))



ggplot(sonde.man.chl %>% filter(lake == "L" & chl_ugL > 0 & manual.chl > 0), aes(x = log.chl.ugl, y = log.man.chl, fill = as.factor(year)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  #facet_grid(lake~year)+
  theme_classic()+
  #geom_point(aes(x = log10(chl_ugL + 7.24), y= log10(manual.chl)))+
  geom_smooth(method = "lm", color = "black", se = FALSE, linetype = "dashed")




ggplot(sonde.man.chl %>% filter(lake == "L" & chl_ugL > 0 & manual.chl > 0), aes(x = log.man.chl, y = log.chl.ugl, fill = as.factor(year)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  #facet_grid(lake~year)+
  theme_classic()+
  #geom_point(aes(x = log10(chl_ugL + 7.24), y= log10(manual.chl)))+
  geom_smooth( color = "black", se = FALSE, linetype = "dashed")



ggplot(sonde.man.chl %>% filter(lake == "R" & chl_ugL > 0 & manual.chl > 0), aes(x = log.man.chl, y = log.chl.ugl, fill = as.factor(year)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  #facet_grid(lake~year)+
  theme_classic()
  #geom_point(aes(x = log10(chl_ugL + 7.24), y= log10(manual.chl)))+
  #geom_smooth( color = "black", se = FALSE, linetype = "dashed")


ggplot(sonde.man.chl %>% filter(lake == "R" & chl_ugL > 0 & manual.chl > 0), 
       aes(x = log.man.chl, y = log.chl.ugl, fill = as.factor(year))) +
  geom_point(pch = 21, size = 4, alpha = 0.7) +
  theme_classic() +
  geom_smooth(data = ~ subset(., year == 2022), 
              color = "black", se = FALSE, linetype = "dashed")

  
  
  ggplot(sonde.man.chl %>% filter(lake == "T" & chl_ugL > 0 & manual.chl > 0), aes(x = log.man.chl, y = log.chl.ugl, fill = as.factor(year)))+
  geom_point(pch = 21, size = 4, alpha = 0.7)+
  #facet_grid(lake~year)+
  theme_classic()
  #geom_point(aes(x = log10(chl_ugL + 7.24), y= log10(manual.chl)))+
  #geom_smooth( color = "black", se = FALSE, linetype = "dashed")


ggplot(sonde.man.chl %>% 
         filter(lake == "L" & chl_ugL > 0 & manual.chl > 0), 
       aes(x = log.man.chl, y = log.chl.ugl, fill = as.factor(year))) +
  geom_point(pch = 21, size = 4, alpha = 0.7) +
  theme_classic() +
  geom_smooth(
    method = "nls", 
    formula = y ~ a * exp(b * x), 
    method.args = list(start = list(a = -1, b = 0.2)), 
    color = "black", 
    se = FALSE, 
    linetype = "dashed"
  )

# Filter data for 2022 and 2024 for lake "L" and ensure chl_ugL > 0
sonde.man.chl.22 <- sonde.man.chl %>%
  filter(year == 2022 & lake == "L" & chl_ugL > 0)

sonde.man.chl.24 <- sonde.man.chl %>%
  filter(year == 2024 & lake == "L" & chl_ugL > 0)

# Fit linear models for each year
model_22 <- lm(log10(manual.chl) ~ log10(chl_ugL), data = sonde.man.chl.22)
model_24 <- lm(log10(manual.chl) ~ log10(chl_ugL), data = sonde.man.chl.24)

# Extract slopes and intercepts
slope_22 <- coef(model_22)[2]
intercept_22 <- coef(model_22)[1]
slope_24 <- coef(model_24)[2]
intercept_24 <- coef(model_24)[1]

# Transform 2022 data to align with 2024 relationship
sonde.man.chl.22 <- sonde.man.chl.22 %>%
  mutate(
    chl_ugL_transformed = 10^((log10(chl_ugL) * slope_24 / slope_22) + (intercept_24 - intercept_22))
  )

# Combine transformed 2022 data with original 2024 data for visualization
combined_data <- bind_rows(
  sonde.man.chl.22 %>% 
    select(chl_ugL_transformed, manual.chl) %>% 
    rename(chl_ugL = chl_ugL_transformed) %>% 
    mutate(year = "2022 (transformed)"),
  sonde.man.chl.24 %>% 
    select(chl_ugL, manual.chl) %>% 
    mutate(year = "2024 (untransformed)")
)

# Plot transformed 2022 chl vs. untransformed 2024 chl
library(ggplot2)
ggplot(combined_data, aes(x = log10(chl_ugL), y = log10(manual.chl), color = year)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    x = "log10(chl_ugL)", 
    y = "log10(manual.chl)", 
    color = "Year"
  ) +
  theme_minimal()

lm.chl.22 = lm(log10(manual.chl)~log10(chl_ugL), data = sonde.man.chl.22)
lm.chl.24 = lm(log10(manual.chl)~log10(chl_ugL), data = sonde.man.chl.24)

summary(lm.chl.22) # intercept of 1.575
summary(lm.chl.24) # intercept of 0.70597

# difference in intercepts 1.575-0.70597 = 0.86
# 10^0.86 = 7.24 ug/L
# because 2022 is higher, the sonde chlorophyll was lower compared to the manual
# so sondes in 2022 underestimated sonde chlorophyll by approximately 7.24 ug/L


ggplot(sonde.man.chl.22, aes(x = log10(manual.chl), y = log10(chl_ugL)))+
  geom_point(size = 3, fill = "violet", pch = 21)+
  geom_point(size = 3, fill = "blue", pch = 21, aes(x = log10(manual.chl), y = log10(chl_ugL * 1.31)))+
  theme_classic()

# Example dataset

# Fit the model
fit <- nls(log10(chl_ugL) ~ a * exp(-b * log10(manual.chl)) + c, data = all.chl.22, start = list(a = 1, b = 0.5, c = 0))

# View results
summary(fit)





#### try calculating the mean ratio of manual to sonde chlorophyll by year
avg.ratio = sonde.man.chl %>%
  group_by(year) %>% 
  summarize(mean.ratio = median(log10(manual.chl+0.1)/log10(chl_ugL+0.1), na.rm = TRUE))



### try curve-fitting #####
# Filter data for 2022 and 2024 for lake "L" and ensure chl_ugL > 0
sonde.man.chl.22 <- sonde.man.chl %>%
  filter(year == 2022 & lake == "L" & chl_ugL > 0)

sonde.man.chl.24 <- sonde.man.chl %>%
  filter(year == 2024 & lake == "L" & chl_ugL > 0)



sub.man.chl.sub



sonde.man.chl.sub <- subset(
  sonde.man.chl.sub,
  !is.na(log.man.chl) & !is.na(log.chl.ugl) &
    is.finite(log.man.chl) & is.finite(log.chl.ugl)
)


# Split data by year
data_split <- split(sonde.man.chl.sub, sonde.man.chl.sub$year)

# Create an empty list to store models
nls_models <- list()

# Loop through each year and fit the nls model
for (year in names(data_split)) {
  data_year <- data_split[[year]]
  
  # Fit the nls model
  nls_models[[year]] <- nls(
    log.man.chl ~ a + b * log.chl.ugl,
    data = data_year,
    start = list(a = 1, b = 1) # Initial guesses for parameters
  )
}

# Summarize results
lapply(nls_models, summary)






# Add fitted values to the dataset
sonde.man.chl.sub$fitted_2022 <- ifelse(sonde.man.chl.sub$year == 2022,
                                        1.48834 + 0.47455 * sonde.man.chl.sub$log.chl.ugl,
                                        NA)
sonde.man.chl.sub$fitted_2024 <- ifelse(sonde.man.chl.sub$year == 2024,
                                        0.92133 + 0.52463 * sonde.man.chl.sub$log.chl.ugl,
                                        NA)

# Create the plot
ggplot(sonde.man.chl.sub, aes(x = log.chl.ugl, y = log.man.chl, color = as.factor(year))) +
  #geom_point(alpha = 0.7) +
  geom_line(aes(y = fitted_2022), color = "red", linetype = "dashed", na.rm = TRUE) +
  geom_line(aes(y = fitted_2024), color = "blue", linetype = "dashed", na.rm = TRUE) +
  labs(
    title = "Fitted Models for 2022 and 2024",
    x = "log10(corrected.chl)",
    y = "log10(manual.chl)",
    color = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")






##### TRY FITTING CURVE TO JUST 2022 AND PAUL #######

curve.data.22 = sonde.man.chl.22


cube_root_model <- nls(
  log.chl.ugl ~ a + b * log.man.chl^(1/3),  # Define the cube root relationship
  data = sonde.man.chl.22,                             # Specify the dataset
  start = list(a = 0, b = 1)               # Provide initial guesses for parameters
)


summary(cube_root_model)



coefficients.22 <- coef(cube_root_model)

coefficients <- coef(cube_root_model)

# Generate a curve based on the fitted model
curve.data.22 <- curve.data.22 %>%
  mutate(
    fitted_values = coefficients["a"] + coefficients["b"] * log.man.chl^(1/3)
  )

# Plot the data and the fitted curve
ggplot(curve.data.22, aes(x = log.man.chl, y = log.chl.ugl)) +
  geom_point(size = 4, alpha = 0.7, color = "red") +        # Data points
  geom_line(aes(y = fitted_values), color = "red", size = 1) + # Fitted curve
  theme_classic() +
  labs(
    x = "Log(Manual Chlorophyll)",
    y = "Log(Chlorophyll ug/L)",
    title = "Cube Root Model Fit"
  )




### 2024 ###

curve.data.24 = sonde.man.chl.24


cube_root_model <- nls(
  log.chl.ugl ~ a + b * log.man.chl^(1/3),  # Define the cube root relationship
  data = sonde.man.chl.24,                             # Specify the dataset
  start = list(a = 0, b = 1)               # Provide initial guesses for parameters
)


summary(cube_root_model)



coefficients.24 <- coef(cube_root_model)

coefficients <- coef(cube_root_model)

# Generate a curve based on the fitted model
curve.data.24 <- curve.data.24 %>%
  mutate(
    fitted_values = coefficients["a"] + coefficients["b"] * log.man.chl^(1/3)
  )

# Plot the data and the fitted curve
ggplot(curve.data.24, aes(x = log.man.chl, y = log.chl.ugl)) +
  geom_point(size = 4, alpha = 0.7, color = "blue") +        # Data points
  geom_line(aes(y = fitted_values), color = "blue", size = 1) + # Fitted curve
  theme_classic() +
  labs(
    x = "Log(Manual Chlorophyll)",
    y = "Log(Chlorophyll ug/L)",
    title = "Cube Root Model Fit"
  )




# combine curve data
curve.data.22.24 = rbind(curve.data.22, curve.data.24)


ggplot(curve.data.22.24, aes(x = log.man.chl, y = log.chl.ugl, color = as.factor(year))) +
  geom_point(size = 4, alpha = 0.7) +        # Data points
  geom_line(aes(y = fitted_values), size = 1) + # Fitted curve
  theme_classic() +
  labs(
    x = "Log(Manual Chlorophyll)",
    y = "Log(Sensor Chlorophyll ug/L)",
    title = "Cube Root Model Fit"
  )








#### calculate values along the curves for the entire range of manual chl values

# predicted values
pred.values = data.frame(man.chl = seq(0, 3, by = 0.001))

pred.values = pred.values %>% mutate(pred.22 = coefficients.22["a"] + coefficients.22["b"] * man.chl^(1/3),
                                     pred.24 = coefficients.24["a"] + coefficients.24["b"] * man.chl^(1/3))


pred.values = pred.values %>% mutate(man.chl.unlog = 10^man.chl, pred.22.unlog = 10^pred.22, 
                                     pred.24.unlog = 10^pred.24)


## calculate the difference
pred.values = pred.values %>% mutate(diff.chl = pred.24.unlog - pred.22.unlog)


ggplot(pred.values, aes(x = man.chl.unlog, y = pred.24.unlog))+
  geom_line()+
  #geom_line(pred.values, aes(x = manual.chl.unlog, y = pred.22.unlog))+
  theme_classic()


# make along format version of the dataframe
pred.long = pred.values %>% pivot_longer(cols = c("pred.24.unlog", "pred.22.unlog"), names_to = "variable") %>% 
  select(man.chl.unlog, variable, value)


ggplot(pred.long %>% filter(man.chl.unlog <= 250), aes(x = man.chl.unlog, y = value, color = variable))+
  geom_line(size = 0.9)+
  labs(x = "manual chlorophyll", y = "sonde chlorophyll")+
  theme_minimal()







prof.clean.time = get(load(file = "./data/formatted data/cleaned profiles interpolated over depth and time.RData"))


# calculate estimated manual chlorophyll for all sonde values
prof.clean.time = prof.clean.time %>% mutate(est.man.chl = NA)

prof.clean.time = prof.clean.time %>% mutate(est.man.chl = replace(est.man.chl, year == 2022,
                                                                  ((log10(chl_ugL) - coefficients.22["a"]) / coefficients.22["b"])^3))

prof.clean.time = prof.clean.time %>% mutate(est.man.chl.unlog = 10^est.man.chl)
