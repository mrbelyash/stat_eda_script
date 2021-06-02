library(ggplot2)
library(ggpubr)
library(magrittr)
library(dplyr)
library(foreach)
library(data.table)
library(gtools)

df15 <- read.csv("C:\\Users\\danii\\Downloads\\2015.csv")
df16 <- read.csv("C:\\Users\\danii\\Downloads\\2016.csv")

# Plotting Hapiness over Regions
png(filename="C:/Users/daniil/Downloads/h_vs_reg.png", res=300, height=1000, width=1500)
ggplot(df15, aes(y=reorder(Region, Happiness.Score), x=Happiness.Score, fill = Region)) +
  geom_point(aes(fill = Region), size = 2, shape = 23, position = position_jitterdodge(), alpha=0.6) +
  theme(text = element_text(size = 8),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#f5f6f5",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.6, linetype = 'solid',
                                        colour = "lightgray")
        ) +
          geom_boxplot(alpha = 0.7, size=0.4, col="black") + 
  labs(title="Happiness by Region", x="Hapiness Score", y="Region")
dev.off()

#Plotting total happiness distribution
ggplot(df15, aes(Happiness.Score))+
  geom_histogram(col='coral4', fill = 'coral', binwidth = 0.5)+
  labs(x = 'Happiness Score', y = 'Frequency', title = 'Happiness Scores Distribution')+
  geom_density(aes(y = ..count..), col="navy", size=0.7)+
  scale_y_continuous(sec.axis = sec_axis(~./20, name = "Scaled Density")) +
  theme(text = element_text(size = 8),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "#f5f6f5",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.6, linetype = 'solid',
                                        colour = "lightgray"))
ggsave("C:/Users/daniil/Downloads/happiness_dist.png")

# Plotting distributions with SW normality test
other_factors <- df15[c(4:12)]
i <- 0
plts <- vector() # Да, это плохо
pvals_sw <- vector()
colors <- c("darkslateblue", "blue", "black", "green", "navy", "darkcyan", "red", "darkorange4", "blue")
hue_cols <- c("blueviolet", "lightblue", "gray", "darkolivegreen1", "aquamarine", "coral2", "darkorange1", "darkorange1", "lightblue")
params <- names(df15)[c(4:12)]
foreach (i = 4:13, a = colors, b=hue_cols) %do% {
  print(i)
  cur_vec <- pull(df15, i)
  pvals_sw <- append(pvals_sw, shapiro.test(cur_vec)$p.value)
  fname <- paste("C:/Users/daniil/Downloads/", names(df15)[i], ".png", sep='')
  p <- ggqqplot(cur_vec, color=a, xlab="Theoretical Qs of Normal distribution", 
                ylab=paste("Qs of", names(df15)[i]), 
                title=paste("QQ-plot for ", names(df15)[i]))
  p$layers[[3]]$aes_params$fill <- b
  ggsave(fname, p, dpi=300)
}
pv_adj <- p.adjust(pvals_sw, method="BH")
sw_table <- data.frame(cbind(params, pvals_sw, pv_adj))

# LN data trying to see if it's normal that way (it's not)
log_pvs <- vector()
for (i in 4:12) {
  current_vector <- log(pull(df15, i) + 0.001) # pseudocount
  log_pvs <- append(log_pvs, shapiro.test(current_vector)$p.value)
}
log_pvs_adj <- p.adjust(log_pvs, method="BH")

# Plotting histograms
foreach (i = 4:12, a = colors, b=hue_cols) %do% {
  fname <- paste("C:/Users/danii/Downloads/", names(df15)[i], "hist.png", sep='')
  ggplot(df15, aes(x=df15[[i]])) + geom_histogram(fill=a, colour=b, bins=15) +
    labs(x = names(df15)[i], y = 'Frequency', title = paste(names(df15)[i], "distribution with KDE")) +
    geom_density(aes(y = ..count..), col=a, size=0.7)+
    scale_y_continuous(sec.axis = sec_axis(~./20, name = "Scaled Density"))
  ggsave(fname, dpi=300)
}

# Testing North/South difference with WMW U-test
north_regions <- c("Western Europe", "North America", "Australia and New Zealand", 
                  "Central and Eastern Europe")
south_regions <- setdiff(unique(df15$Region), north_regions)
north15 <- subset(df15, Region %in% north_regions)$Happiness.Score
south15 <- subset(df15, Region %in% south_regions)$Happiness.Score
t_res <- wilcox.test(north15, south15, paired=)

# Testing 2015/2016 difference with Wilcoxon signed-rank test (W-test)
w_df <- data.frame(cbind(df15$Happiness.Score[df15$Country %in% intersect(df15$Country, df16$Country)], 
              df16$Happiness.Score[df16$Country %in% intersect(df15$Country, df16$Country)]))
colnames(w_df) <- c("y2015", "y2016")
w_res <- wilcox.test(w_df$y2015, w_df$y2016, paired=T)

# Making a multiple linear regression model to see which of the parameters prevail
lm_df <- vector(length = 158)
for(i in 6:12) {       # for-loop over columns
  lm_df <- cbind(lm_df, as.numeric(quantcut(df15[ , i])))
}
lm_df <- data.frame(lm_df[, c(2:8)])
colnames(lm_df) <- names(df15)[6:12]
pure_happiness <- df15$Happiness.Score - df15$Dystopia.Residual
lm_df <- cbind(pure_happiness, lm_df)
model <- lm(pure_happiness ~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. +
              Freedom + Trust..Government.Corruption. + Generosity + 0, data=lm_df, )
param_to_coef <- model$coefficients
pvals_adjusted <- p.adjust(summary(model)$coefficients[,4], method="BH")

#Building a regression confidence interval
par_vec <- sample(c(1:4), 6, replace=T)
par_vec <- transpose(data.frame(par_vec))
colnames(par_vec) <- names(df15)[6:11]
ci <- predict(model, newdata = par_vec, interval = 'confidence', level=0.95)
avg <- ci[1]
half <- (ci[3]-ci[2])/2
print(paste(avg, "+-", half))

      