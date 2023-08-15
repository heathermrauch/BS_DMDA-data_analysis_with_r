library(ggplot2)
library(gridExtra)
library(Hmisc)
library(psych)
library(dplyr)
library(plot3D)
library(RColorBrewer)

outliers <- function(x) {
  # function to set the upper and lower limit for detecting outliers
  q1 <- quantile(x)[2] # first quantile
  q3 <- quantile(x)[4] # third quantile
  ll <- q1 - 1.5 * IQR(x) # lower limit
  ul <- q3 + 1.5 * IQR(x) # upper limit
  return(c(ll, ul))
}

hist_outliers <- function(data, x, binwidth = 1) {
  # function to produce a two-plot grid containing one histogram with the lower
  # and upper limits marked for outlier detection. The second plot is the same
  # as the first, except it is zoomed in to the non-outlier data.
  p1 <- ggplot(aes(x = x), data = data) +
    geom_histogram(binwidth = binwidth) +
    geom_vline(xintercept = outliers(x)[[1]], color = I('red')) +
    geom_vline(xintercept = outliers(x)[[2]], color = I('red'))
  p2 <- p1 + coord_cartesian(xlim = c(outliers(x)[[1]], outliers(x)[[2]]))
  return(grid.arrange(p1, p2, ncol = 2))
}

extract_legend <- function(p) {
  # function to extract the legend from a plot
  # https://statisticsglobe.com/add-common-legend-to-combined-ggplot2-plots-in-r#first
  step1 <- ggplot_gtable(ggplot_build(p))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == 'guide-box')
  step3 <- step1$grobs[[step2]]
  return(step3)
}

# Load the Data
wine <- read.csv('wineQualityWhites.csv')

# List the dimensions
dim(wine)

# Print the data structure
str(wine)

# Print the five number summary for each variable
summary(wine)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$fixed.acidity, binwidth = 0.1)

# Print the 5 number summary
summary(wine$fixed.acidity)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$volatile.acidity, binwidth = 0.01)

# Print the 5 number summary
summary(wine$volatile.acidity)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$citric.acid, binwidth = 0.01)

# Print the 5 number summary
summary(wine$citric.acid)

# Filter to the spike at 0.49
ca5 <- subset(wine, wine$citric.acid == 0.49)

# Compare the spike's 5 number summary to the entire dataset
summary(wine)
summary(ca5)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$residual.sugar, binwidth = 0.1)

# Plot the log10 transformed distribution with Q1-Q3 as vertical lines
ggplot(aes(x = residual.sugar), data = wine) +
  geom_histogram(binwidth = 0.05) +
  geom_vline(xintercept = quantile(wine$residual.sugar)[2]) +
  geom_vline(xintercept = median(wine$residual.sugar)) +
  geom_vline(xintercept = quantile(wine$residual.sugar)[4]) +
  scale_x_log10(breaks = c(0.55, 1, 1.7, 3, 5.5, 10, 17, 30, 55))

# Print the five number summary
summary(wine$residual.sugar)

# Create the sweetness variable based on residual.sugar
wine <- wine %>% 
  mutate(sweetness = case_when(residual.sugar < 4 ~ 'dry',
                               residual.sugar < 12 ~ 'medium dry',
                               residual.sugar < 45 ~ 'medium',
                               TRUE ~ 'sweet'))

# Turn sweetness into an ordered factor
wine$sweetness <- factor(wine$sweetness, 
                         levels = c('dry', 'medium dry', 'medium', 'sweet'),
                         ordered = TRUE)

# Plot the distribution
ggplot(aes(x = sweetness), data = wine) + geom_histogram(stat = 'count') 

# Plot the residual sugar distribution by sweetness group
ggplot(aes(x = residual.sugar), data = wine) + geom_histogram(binwidth = 0.1) +
  facet_wrap(~sweetness, ncol = 2, scales = 'free')

# Print the 5 number summary for each sweetness group
by(wine$residual.sugar, wine$sweetness, summary)

# Plot residual.sugar distribution for medium dry sweetness category
# Include Q1-Q3 as vertical lines
ggplot(aes(x = residual.sugar), 
       data = subset(wine, wine$sweetness == 'medium dry')) +
  geom_histogram(binwidth = 0.5) +
  scale_x_continuous(breaks = seq(4, 12, 1)) +
  geom_vline(
    xintercept = quantile(
      subset(wine, wine$sweetness == 'medium dry')$residual.sugar, 0.25
    )
  ) +
  geom_vline(
    xintercept = median(
      subset(wine, wine$sweetness == 'medium dry')$residual.sugar
    )
  ) +
  geom_vline(
    xintercept = quantile(
      subset(wine, wine$sweetness == 'medium dry')$residual.sugar, 0.75
    )
  )

# Print 5 number summary
summary(subset(wine, wine$sweetness == 'medium dry')$residual.sugar)

# Plot the residual sugar distribution for the medium sweetness group
hist_outliers(subset(wine, wine$sweetness == 'medium'), 
              subset(wine, wine$sweetness == 'medium')$residual.sugar,
              binwidth = 0.1)

# Print the 5 number summary
summary(subset(wine, wine$sweetness == 'medium')$residual.sugar)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$chlorides, binwidth = 0.001)

# Print the 5 number summary
summary(wine$chlorides)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$free.sulfur.dioxide)

# Print the 5 number summary
summary(wine$free.sulfur.dioxide)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$total.sulfur.dioxide)

# Print the 5 number summary
summary(wine$total.sulfur.dioxide)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$density, binwidth = 0.0002)

# Print the 5 number summary
summary(wine$density)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$pH, binwidth = 0.02)

# Print the 5 number summary
summary(wine$pH)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$sulphates, binwidth = 0.01)

# Print the 5 number summary
summary(wine$sulphates)

# Plot the distribution with and without outliers
hist_outliers(wine, wine$alcohol, binwidth = 0.2)

# Print the 5 number summary
summary(wine$alcohol)

# Plot the distribution with Q1-Q3 as vertical lines
ggplot(aes(x = alcohol), data = wine) +
  geom_histogram(binwidth = 0.2) +
  geom_vline(xintercept = quantile(wine$alcohol)[2]) +
  geom_vline(xintercept = median(wine$alcohol)) +
  geom_vline(xintercept = quantile(wine$alcohol)[4])+
  scale_x_continuous(breaks = seq(8, 14, 1))

# Print the 5 number summary
summary(wine$alcohol)

# Plot the distribution across the quality ratings
ggplot(aes(x = quality), data = wine) +
  geom_histogram(stat = 'count') +
  scale_x_binned(breaks = seq(0, 10, 1))

# Print the 5 number summary
summary(wine$quality)

# Subset the data to the variables of interest
wine.sub <- wine[, c("sweetness", "residual.sugar", "alcohol", "density",
                     "chlorides", "total.sulfur.dioxide", "quality")]

# Run the correlation matrix for numeric variables in subset
cormat <- rcorr(as.matrix(wine.sub[, 2:7]))$r

# Transform correlation matrix by creating a row for each row-column 
# correlation pair and filter to correlations stronger than +/- 0.3
ut <- upper.tri(cormat)
data.frame(
  row = rownames(cormat)[row(cormat)[ut]],
  column = rownames(cormat)[col(cormat)[ut]],
  cor = (cormat)[ut]
) %>%
  filter(., abs(cor) >= 0.3) %>%
  arrange(.,desc(abs(cor)))

# Display a matrix of correlations, histogram/density plots, and scatter plots
# with regression lines
pairs.panels(wine.sub[, 2:7], smooth = FALSE, scale = FALSE, ellipses = TRUE, 
             method = 'pearson', pch = '.', lm = TRUE, cor = TRUE, 
             jiggle = FALSE, hist.col = 5, stars = FALSE, ci = FALSE)

# Calculate the outliers for the density variable
do <- outliers(x = wine.sub$density)

# Created the base plot
p1 <- ggplot(aes(x = residual.sugar, y = density), data = wine.sub)

# Create the enhanced plot with scale transforms
p2 <- p1 + geom_point(alpha = 0.1, position = position_jitter(width = 0.02)) + 
  geom_smooth(formula = y ~ x, color = 'red') + 
  scale_x_log10(breaks = c(1, 3, 10, 30)) +
  scale_y_continuous(limits = c(do[[1]], do[[2]]))

# Add the basic plot to the base plot and display side-by-side
p1 <- p1 + geom_point()
grid.arrange(p1, p2, ncol = 2)

# Test the correlation between residual sugar and density
cor.test(wine.sub$residual.sugar, wine.sub$density)

# Boxplot of density by sweetness
ggplot(aes(x = sweetness, y = density), data = wine.sub) + geom_boxplot()

# Build the base plot of alcohol and density
p1 <- ggplot(aes(x = alcohol, y = density), data = wine.sub)

# Add enhanced plot with jitter, alpha, and scale transforms
p2 <- p1 + geom_jitter(alpha = 0.1, width = 0.05) + 
  geom_smooth(formula = y ~ x, color = 'red') + 
  scale_y_continuous(limits = c(do[[1]], do[[2]]))

# Add basic plot to base plot and display side-by-side
p1 <- p1 + geom_point()
grid.arrange(p1, p2, ncol = 2)

# Test the correlation between alcohol and density
cor.test(wine.sub$alcohol, wine.sub$density)

# Build the base plot of residual sugar and alcohol
p1 <- ggplot(aes(x = residual.sugar, y = alcohol), data = wine.sub)

# Add the enhanced plot with jitter, alpha and scale transform
p2 <- p1 + geom_point(alpha = 0.1, position = position_jitter(width = 0.02)) + 
  geom_smooth(formula = y ~ x, color = 'red') + scale_x_log10()

# Add the basic plot to the base plot and display side-by-side
p1 <- p1 + geom_point()
grid.arrange(p1, p2, ncol = 2)

# Test the correlation between residual sugar and alcohol
cor.test(wine.sub$residual.sugar, wine.sub$alcohol)

# boxplot of alcohol by sweetness
ggplot(aes(x = sweetness, y = alcohol), data = wine.sub) +
  geom_boxplot()

# Calculate outliers needed for plot zoom
yo <- outliers(x = wine.sub$total.sulfur.dioxide)

# Define portions of plot which will be reused
p <- ggplot(data = wine.sub) + 
  scale_y_continuous(limits = c(yo[[1]], yo[[2]]))

# Define aesthetics for each plot
map1 <- aes(x = density, y = total.sulfur.dioxide)
map2 <- aes(x = residual.sugar, y = total.sulfur.dioxide)
map3 <- aes(x = alcohol, y = total.sulfur.dioxide)

# Build and arrange the plots
p1 <- p + geom_point(mapping = map1, alpha = 0.1, 
                     position = position_jitter(width = 0.0001)) +
  geom_smooth(mapping = map1, formula = y ~ x, color = 'red') + 
  scale_x_continuous(limits = c(do[[1]], do[[2]]))
p2 <- p + geom_point(mapping = map2, alpha = 0.1,
                     position = position_jitter(width = 0.02, height = 0)) +
  geom_smooth(mapping = map2, formula = y ~ x, color = 'red') + 
  scale_x_log10()
p3 <- p + geom_point(mapping = map3, alpha = 0.1, 
                     position = position_jitter(width = 0.05)) +
  geom_smooth(mapping = map3, formula = y ~ x, color = 'red')
grid.arrange(p1, p2, p3, ncol = 1)

# Correlation tests
cor.test(wine.sub$density, wine.sub$total.sulfur.dioxide)
cor.test(wine.sub$residual.sugar, wine.sub$total.sulfur.dioxide)
cor.test(wine.sub$alcohol, wine.sub$total.sulfur.dioxide)

# Boxplot of total.sulfur.dioxide by sweetness
ggplot(aes(x = sweetness, y = total.sulfur.dioxide), data = wine.sub) + 
  geom_boxplot() + coord_cartesian(ylim = c(yo[[1]], yo[[2]]))

# Calculate chlorides outliers
co = outliers(x = wine.sub$chlorides)

# scatterplot with cloride outliers removed
ggplot(aes(x = alcohol, y = chlorides), data = wine.sub) +
  geom_point(position = position_jitter(width = 0.04), alpha = 0.1) +
  geom_smooth(formula = y ~ x, color = 'red') +
  scale_y_continuous(limits = c(co[[1]], co[[2]]))

# Correlation test between alcohol and chlorides
cor.test(wine.sub$alcohol, wine.sub$chlorides)

# Run correlation matrix of numeric variables against quality
cormat <- rcorr(as.matrix(wine.sub[, 2:7]))$r

# Transform each row-column correlation pair to row
# Filter to quality correlations
ut <- upper.tri(cormat)
data.frame(
  row = rownames(cormat)[row(cormat)[ut]],
  column = rownames(cormat)[col(cormat)[ut]],
  cor = (cormat)[ut]
) %>%
  filter(row == 'quality' | column == 'quality') %>%
  arrange(abs(cor))

# quality boxplots with all outliers removed
p1 <- ggplot(aes(x = factor(quality), y = residual.sugar), data = wine.sub) +
  geom_boxplot() + coord_cartesian(ylim = c(0, 20)) + xlab('quality')
p2 <- ggplot(aes(x = factor(quality), y = total.sulfur.dioxide), 
             data = wine.sub) +
  geom_boxplot() + coord_cartesian(ylim = c(yo[[1]], yo[[2]])) + xlab('quality')
p3 <- ggplot(aes(x = factor(quality), y = chlorides), data = wine.sub) +
  geom_boxplot() + coord_cartesian(ylim = c(co[[1]], co[[2]])) + xlab('quality')
p4 <- ggplot(aes(x = factor(quality), y = density), data = wine.sub) +
  geom_boxplot() + coord_cartesian(ylim = c(do[[1]], do[[2]])) + xlab('quality')
p5 <- ggplot(aes(x = factor(quality), y = alcohol), data = wine.sub) +
  geom_boxplot() + xlab('quality')
grid.arrange(p1, p2, p3, p4, p5, ncol = 2)

# Build reusable plot parts
p <- ggplot(aes(color = sweetness),
            data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  theme_dark() + scale_color_brewer(palette = 'YlGn') + 
  theme(legend.position = 'none')

# Build density plot by sweetness for each numeric variable in subset
p1 <- p + geom_density(aes(x = residual.sugar), size = 1) + scale_x_log10()
p2 <- p + geom_density(aes(x = alcohol), size = 1)
p3 <- p + geom_density(aes(x = density), size = 1) + 
  coord_cartesian(xlim = c(do[[1]], do[[2]]))
p4 <- p + geom_density(aes(x = chlorides), size = 1) +
  coord_cartesian(xlim = c(co[[1]], co[[2]]))
p5 <- p + geom_density(aes(x = total.sulfur.dioxide), size = 1) +
  coord_cartesian(xlim = c(yo[[1]], yo[[2]]))

# Extract shared legend and arrange all items in grid
shared_legend <- extract_legend(p1 + theme(legend.position = 'top',
                                           legend.direction = 'vertical'))
grid.arrange(p1, p2, p3, p4, p5, shared_legend, ncol = 2)

# Scatterplot of residual sugar and density by sweetness
# x axis is log10 transformed, y axis zoomed to remove outliers
ggplot(aes(x = residual.sugar, y = density, color = sweetness),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  scale_color_brewer(palette = "YlGn") + theme_dark() +
  geom_point(position = position_jitter(width = 0.02)) + 
  geom_smooth(formula = y ~ x, color = 'darkgreen') +
  scale_x_log10() + coord_cartesian(ylim = c(do[[1]], do[[2]])) +
  theme(legend.position = 'bottom')

# Scatterplot of alcohol and density by sweetness
# y axis is zoomed to remove outliers
ggplot(aes(x = alcohol, y = density, color = sweetness), 
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  theme_dark() + theme(legend.position = 'bottom') +
  scale_color_brewer(palette = "YlGn") +
  geom_point(position = position_jitter(width = 0.05)) +
  geom_smooth(formula = y ~ x, color = 'darkgreen') +
  coord_cartesian(ylim = c(do[[1]], do[[2]]))

# Scatterplot of residual sugar and alcohol by sweetness
# x axis is log10 transformed
ggplot(aes(x = residual.sugar, y = alcohol, color = sweetness),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  geom_point(position = position_jitter(width = 0.02, height = 0.03)) + 
  geom_smooth(formula = y ~ x, color = 'darkgreen') +
  scale_color_brewer(palette = "YlGn") + theme_dark() + scale_x_log10() +
  theme(legend.position = 'bottom')

# Build reusable parts of plot
p <- ggplot(aes(y = total.sulfur.dioxide, color = sweetness),
            data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  theme_dark() + scale_color_brewer(palette = "YlGn") +
  coord_cartesian(ylim = c(yo[[1]], yo[[2]])) + theme(legend.position = 'none')

# density vs total.sulfur.dioxide scatterplot with outliers removed
p1 <- p + geom_point(aes(x = density)) +
  geom_smooth(aes(x = density), formula = y ~ x, color = 'darkgreen') +
  coord_cartesian(xlim = c(do[[1]], do[[2]]), ylim = c(yo[[1]], yo[[2]]))

# residual.sugar vs total.sulfur.dioxide scatterplot
# x axis log10 transformed and y axis outliers removed
p2 <- p + geom_point(aes(x = residual.sugar),
                     position = position_jitter(width = 0.02)) +
  geom_smooth(aes(x = residual.sugar), formula = y ~ x, color = 'darkgreen') + 
  scale_x_log10()

# alcohol vs total.sulful.dioxide scatterplot with outliers removed
p3 <- p + geom_point(aes(x = alcohol),
                     position = position_jitter(width = 0.04)) +
  geom_smooth(aes(x = alcohol), formula = y ~ x, color = 'darkgreen')

# Extract shared legend and arrange grid
shared_legend <- extract_legend(p1 + theme(legend.position = 'top'))
grid.arrange(shared_legend, arrangeGrob(p1, p2, p3, ncol = 1),
             nrow = 2, heights = c(1, 6))

# alcohol vs chlorides by sweetness scatterplot with outliers removed
ggplot(aes(x = alcohol, y = chlorides, color = sweetness), 
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  theme_dark() + theme(legend.position = 'bottom') +
  geom_point(position = position_jitter(height = 0.001, width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'darkgreen') +
  scale_color_brewer(palette = "YlGn") + 
  scale_y_continuous(limits = c(co[[1]], co[[2]]))

# residual.sugar vs density scatterplot colored by quality faceted by sweetness
# x axis log10 transformed
ggplot(aes(x = residual.sugar, y = density, color = factor(quality)),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  geom_point(position = position_jitter(width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'magenta') +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  scale_x_log10() + facet_wrap(~sweetness, scales = "free", ncol = 1)

# alcohol vs density scatterplot colored by quality faceted by sweetness
ggplot(aes(x = alcohol, y = density, color = factor(quality)),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  geom_point(position = position_jitter(width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'magenta') +
  facet_wrap(~sweetness, ncol = 1, scales = 'free')

# residual.sugar vs alcohol scatterplot colored by quality faceted by sweetness
ggplot(aes(x = residual.sugar, y = alcohol, color = factor(quality)),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  geom_point(position = position_jitter(height = 0.01, width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'magenta') + scale_x_log10() +
  facet_wrap(~sweetness, ncol = 1, scales = 'free')

# density vs total.sulfur.dioxide scatterplot colored by quality faceted by
# sweetness y axis zoomed to exclude outliers
ggplot(aes(x = density, y = total.sulfur.dioxide, color = factor(quality)),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  geom_point() + geom_smooth(formula = y ~ x, color = 'magenta') +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  coord_cartesian(ylim = c(yo[[1]], yo[[2]])) +
  facet_wrap(~sweetness, ncol = 1, scales = 'free')

# alcohol vs total.sulfur.dioxide scatterplot colored by quality faceted by 
# sweetenss y axis zoomed to exclude outliers
ggplot(aes(x = alcohol, y = total.sulfur.dioxide, color = factor(quality)),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  geom_point(position = position_jitter(width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'magenta') +
  coord_cartesian(ylim = c(yo[[1]], yo[[2]])) +
  facet_wrap(~sweetness, ncol = 1, scales = 'free')

# alcohol vs chlorides scatterplot colored by quality faceted by sweetness
# y axis zoomed to exclude outliers
ggplot(aes(x = alcohol, y = chlorides, color = factor(quality)),
       data = subset(wine.sub, wine.sub$sweetness != 'sweet')) +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  geom_point(position = position_jitter(width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'magenta') +
  coord_cartesian(ylim = c(co[[1]], co[[2]]))+
  facet_wrap(~sweetness, ncol = 1, scales = 'free')

# Subset data
wine.model <- subset(wine.sub, wine.sub$sweetness != 'sweet' &
                       wine.sub$density >= do[[1]] & 
                       wine.sub$density <= do[[2]] &
                       wine.sub$total.sulfur.dioxide >= yo[[1]] &
                       wine.sub$total.sulfur.dioxide <= yo[[2]] &
                       wine.sub$chlorides >= co[[1]] &
                       wine.sub$chlorides <= co[[2]])

# quality ~ residual.sugar - 0.01027
lm1 <- lm(formula = quality ~ residual.sugar, data = wine.model)
summary(lm1)

# quality ~ sweetness - 0.006507
lm1.5 <- lm(formula = quality ~ sweetness, data = wine.model)
summary(lm1.5)

# quality ~ total.sulfur.dioxide - 0.02525
lm2 <- lm(formula = quality ~ total.sulfur.dioxide, data = wine.model)
summary(lm2)

# quality ~ chlorides - 0.07935
lm3 <- lm(formula = quality ~ chlorides, data = wine.model)
summary(lm3)

# quality ~ density - 0.09911
lm4 <- lm(formula = quality ~ density, data = wine.model)
summary(lm4)

# quality ~ alcohol - 0.1911
lm5 <- lm(formula = quality ~ alcohol, data = wine.model)
summary(lm5)

# Build and summarize quality linear model
summary(lm(quality ~ alcohol + log10(residual.sugar), data = wine.model))

# Map linear model to 3D scatterplot
x <- log10(wine.model$residual.sugar)
y <- wine.model$alcohol
z <- wine.model$quality
fit <- lm(z ~ x + y)
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy),
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

# Build and display scatterplot
scatter3D(x, y, z, col = brewer.pal(7, "RdYlGn"), alpha = 0.7, bty = 'u', 
          col.panel = 'grey40', col.grid = 'grey0', expand = 0.5,
          pch = 18, cex = 0.85, theta = 60, phi = 15, ticktype = "detailed",
          xlab = "log10(residual.sugar)", ylab = "alcohol", zlab = "quality", 
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints), 
          main = "wine.model", clab = "quality")

# residual sugar distribution colored by sweetness x axis log10 transformed
ggplot(aes(x = residual.sugar, fill = sweetness), data = wine.model) +
  geom_histogram(binwidth = 0.05, color = 'black') +
  scale_x_log10(breaks = c(0.55, 1, 1.7, 3, 5.5, 10, 17)) +
  theme_dark() + scale_fill_brewer(palette = "YlGn") + 
  labs(title = "Histogram of residual sugar by sweetness", 
       subtitle = "Displayed on a log10 scale") + 
  xlab("residual sugar (g/dm^3)") +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))

# Scatterplot of residual.sugar and alcohol colored by quality faceted by
# sweetness. x axis log10 transformed
ggplot(aes(x = residual.sugar, y = alcohol, color = factor(quality)),
       data = wine.model) +
  scale_color_brewer(palette = "RdYlGn") + theme_dark() +
  geom_point(position = position_jitter(height = 0.02, width = 0.04)) +
  geom_smooth(formula = y ~ x, color = 'magenta') +
  scale_x_log10() +
  facet_wrap(~sweetness, scales = 'free_x') + 
  labs(title = 'scatter plot of residual sugar vs. alcohol by sweetness and quality', subtitle = "residual sugar displayed on a log10 scale") +
  xlab('residual sugar (g/dm^3)') + ylab('alcohol (% by volume)') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Correlation tests for log10 residual sugar and alcohol
# Performed for each sweetness group
cor.test(
  x = log10(subset(wine.model, wine.model$sweetness == 'dry')$residual.sugar),
  y = subset(wine.model, wine.model$sweetness == 'dry')$alcohol)
cor.test(
  x = log10(subset(wine.model, 
                   wine.model$sweetness == 'medium dry')$residual.sugar),
  y = subset(wine.model, wine.model$sweetness == 'medium dry')$alcohol
)
cor.test(
  x = log10(subset(wine.model, 
                   wine.model$sweetness == 'medium')$residual.sugar),
  y = subset(wine.model, wine.model$sweetness == 'medium')$alcohol
)

# Plot function
model.plot <- function(data, title) {
  # map data to dimensions
  x <- data$residual.sugar
  y <- data$alcohol
  z <- data$quality
  # fit dimensions to model
  fit <- lm(z ~ log(x) + y)
  # generate grid dimensions
  x.pred <- seq(min(x), max(x), length.out = grid.lines)
  y.pred <- seq(min(y), max(y), length.out = grid.lines)
  # build grid from dimensions
  xy <- expand.grid(x = x.pred, y = y.pred)
  # calculate z dimension of grid
  z.pred <- matrix(predict(fit, newdata = xy),
                   nrow = grid.lines, ncol = grid.lines)
  # used later to map points to grid
  fitpoints <- predict(fit)
  # configure plot settings
  grid.lines = 16
  col = brewer.pal(length(unique(data$quality)), "RdYlGn")
  pch = 18
  alpha = 0.7
  ticktype = "detailed"
  bty = 'u'
  col.panel = 'grey40'
  col.grid = 'grey30'
  expand = 0.5
  xlab = "log10 of residual sugar (g/dm^3)"
  ylab = "alcohol (% by volume)"
  zlab = "quality"
  clab = "quality"
  cex = 0.85
  theta = 30
  phi = 15
  # Return plot with all configurations mapped
  return(scatter3D(
    x=x, y=y, z=z, col=col, alpha=alpha, bty=bty, col.panel=col.panel,
    col.grid=col.grid, expand=expand, pch=pch, ticktype=ticktype, main=title,
    xlab=xlab, ylab=ylab, zlab=zlab, clab=clab, cex=cex, theta=theta,
    phi=phi, surf = list(x = x.pred, y = y.pred, z = z.pred, 
                         facets = NA, fit = fitpoints)
  ))
}

# Dry White Wine Quality Linear Model 3D Scatterplot
wine.dry <- subset(wine.model, wine.model$sweetness == 'dry')
formula <- with(wine.dry, quality ~ log10(residual.sugar) + alcohol)
model.plot(data = wine.dry, title = "Dry White Wine Quality Linear Model")

# Summarize Dry White Wine Quality Linear Model
summary(lm(formula))

# Medium Dry White Wine Quality Linear Model 3D Scatterplot
wine.med.dry <- subset(wine.model, wine.model$sweetness == 'medium dry')
formula <- with(wine.med.dry, quality ~ log10(residual.sugar) + alcohol)
model.plot(data = wine.med.dry, 
           title = "Medium Dry White Wine Quality Linear Model")

# Summarize Medium Dry White Wine Quality Linear Model
summary(lm(formula))

# Medium White Wine Quality Linear Model 3d Scatterplot
wine.med <- subset(wine.model, wine.model$sweetness == 'medium')
formula <- with(wine.med, quality ~ log10(residual.sugar) + alcohol)
model.plot(data = wine.med, title = "Medium White Wine Quality Linear Model")

# Summarize Medium White Wine Quality Linear Model
summary(lm(formula))


