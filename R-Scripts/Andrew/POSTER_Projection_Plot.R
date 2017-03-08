## This script calculates and plots 90% confidence interval projections for carbon sequestration by harvested wood products using 5-year and 10-year simple regression models on data from the WOODCARB3R package.

## woodcarb package install and load
devtools::install_github('benjones2/WOODCARB3R', build_vignettes = TRUE)
library(WOODCARB3R)

## actual contribution
years <- c(1901:2012)
final <- finalCarbonContribution(Years = years)
finalplot <- numeric()
finalplot[years] <- final

## last 10 years simple regression
last10 <- finalCarbonContribution(Years = c(2003:2012))
last10_years <- c(2003:2012)
last10_mod <- lm(last10 ~ last10_years)
new <- data.frame(last10_years = c(2013:2020))
last10_pred <- predict(last10_mod, newdata = new, interval = "confidence")
pred_last10_plot <- c(rep(NA, each = 2012), as.numeric(last10_pred[,1]))
low_last10_plot <- c(rep(NA, each = 2012), as.numeric(last10_pred[,2]))
up_last10_plot <- c(rep(NA, each = 2012), as.numeric(last10_pred[,3]))

## last 5 years simple regression
last5 <- finalCarbonContribution(Years = c(2008:2012))
last5_years <- c(2008:2012)
last5_mod <- lm(last5 ~ last5_years)
new <- data.frame(last5_years = c(2013:2020))
last5_pred <- predict(last5_mod, newdata = new, interval = "confidence")
pred_last5_plot <- c(rep(NA, each = 2012), as.numeric(last5_pred[,1]))
low_last5_plot <- c(rep(NA, each = 2012), as.numeric(last5_pred[,2]))
up_last5_plot <- c(rep(NA, each = 2012), as.numeric(last5_pred[,3]))

## plot
plot_years <- length(c(1901:2020))
plot_years_seq <- c(1:2020)
plot(finalplot, 
     type = "l", 
     col = "red", 
     lwd = 3,
     ylim = c(min(final, low_last10_plot, low_last5_plot, na.rm = TRUE), max(final, up_last10_plot, up_last5_plot, na.rm = TRUE)),
     xlim = c(1901,2020),
     main = "Projected Carbon Contribution", 
     xlab = "Year", 
     ylab = "Carbon Contribution (Thousand Metric Tons CO2)")
polygon(c(plot_years_seq, rev(plot_years_seq)),
        c(up_last5_plot, rev(low_last5_plot)),
        col = "orange",
        border = NA)
polygon(c(plot_years_seq, rev(plot_years_seq)),
        c(up_last10_plot, rev(low_last10_plot)),
        col = "lightskyblue",
        border = NA)
lines(pred_last5_plot,
      col = "darkorange3",
      lwd = 3)
lines(pred_last10_plot,
      col = "blue",
      lwd = 3)
