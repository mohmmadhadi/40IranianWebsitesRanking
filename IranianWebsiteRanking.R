library(MCDM)
library(ggplot2)

#Reading the csv file
data <- read.csv("data.csv")


#normalizing data
data <- normalize_data(data)

#shannon entropy weights
weights <- shannon_entropy_weights(data)
View(weights)

#Preparing data
rownames(data) <- data[,1]
prepared_data <- data[,c(2:6)]
data_1 <- as.matrix(prepared_data)
cb <- c("max","max","max","max","min")
criterion <- colnames(data[2:6])
criteria_data <- data.frame(criterion, weights, cb)
websites <- row.names(data)

# Shanon weights bar plot
ggplot(criteria_data, aes(x = reorder(criterion, weights), y = weights, fill = cb)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flips the axes so the criteria are on the y-axis
  labs(title = "Criterion Weights",
       x = "Criterion",
       y = "Weight",
       fill = "Criterion Nature") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") # Colours for Maximize/Minimize


#VIKOR
V <- 0.5
vikor <- vikor(data_1,weights, cb, V)
View(vikor)

# Vikor Plot
ggplot(vikor, aes(x = Ranking, y = S, size = Q, color = Ranking)) +
  geom_point(alpha = 0.7) +
  labs(title = "VIKOR Rankings and Metrics", x = "Ranking", y = "S") +
  theme_minimal()

#Linear Topsis
topsislinear <- TOPSISLinear(data_1,weights,cb)
View(topsislinear)


ggplot(data = topsislinear, aes(x = R, y = Ranking, label = Alternatives)) +
  geom_point() +  # This adds the scatter plot points
  geom_text(aes(label=Alternatives), hjust=1, vjust=1) + # Adds labels to points
  scale_y_reverse() +  # This reverses the y-axis to have the best rank at the top
  labs(x = "Distance from Ideal Solution (R)", y = "Ranking") +
  ggtitle("Linear TOPSIS Model Rankings") +
  theme_minimal()  # A clean theme


#WASPAS
waspas <- WASPAS(data_1,weights,cb,0.5)
View(waspas)
waspas$Alternatives <- websites
View(waspas)

# Melt the data for plotting
waspas_long <- reshape2::melt(waspas, id.vars = c('Alternatives', 'Ranking'), variable.name = 'Metric', value.name = 'Value')
waspas_long$Alternatives <- websites


# plot for WASPAS
ggplot(waspas_long, aes(x = Alternatives, y = Value, fill = Metric)) +
  geom_bar(data = subset(waspas_long), aes(fill = Metric), stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("WSM" = "blue", "WPM" = "green", "Q" = "red")) +
  labs(x = "Alternatives", y = "WSM/WPM/Q Value", fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("WASPAS Model Visualization")






model_compare <- data.frame("Alternative"=data[,1],"VIKOR"=vikor[,5],
                            "TOPSIS"=topsislinear[,3], "WASPAS"=waspas[,5])
View(model_compare)

write.csv(model_compare,file="model_compare.csv")
