#Code for Journal of Risk and Financial Management

#Global Crypto Market Share
library(ggplot2)
library(dplyr)

crypto_data <- data.frame(
  Cryptocurrency = c("Bitcoin", "Ethereum", "USDT", "Ripple", "BNB", "Others"),
  MarketShare = c(60.52, 7.02, 5.71, 4.18, 3.23, 19.5)
)

crypto_data <- crypto_data %>%
  arrange(desc(Cryptocurrency)) %>%
  mutate(prop = MarketShare / sum(MarketShare),
         ypos = cumsum(prop) - 0.5 * prop)

map_colors <- c(
  "Bitcoin" = "#529be1",
  "Ethereum" = "lightgreen",
  "USDT" = "#984ea3",
  "Ripple" = "#ff7f00",
  "BNB" = "#e41a1c",
  "Others" = "grey"
)

ggplot(crypto_data, aes(x = 2, y = MarketShare, fill = Cryptocurrency)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = ifelse(Cryptocurrency == "Bitcoin", paste0(MarketShare, "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 6) +
  scale_fill_manual(values = map_colors) +
  xlim(0.5, 2.5) +
  theme_void() +
  labs(title = "Global Cryptocurrency Market Share") +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),   
    legend.title = element_blank(),
    legend.text = element_text(size = 15) 
  )



#Regression: Fixed Effects with Risk Variables
library(mice)
library(broom)
library(broom.mixed)
library(tidyverse)
library(plm)
library(dplyr)
library(lmtest)
library(sandwich)

Correlation_Analysis <- as.data.frame(Correlation_Analysis)
Correlation_Analysis$Country <- as.factor(Correlation_Analysis$Country)
Correlation_Analysis$Year <- as.numeric(Correlation_Analysis$Year)
Correlation_Analysis$Adoption <- as.numeric(Correlation_Analysis$Adoption)
Correlation_Analysis$Country_Code <- as.integer(factor(Correlation_Analysis$Country))

Correlation_Analysis$CPI <- Correlation_Analysis$CPI / 100
Correlation_Analysis$Internet_Penetration <- Correlation_Analysis$Internet_Penetration / 100

predMatrix <- make.predictorMatrix(Correlation_Analysis)
predMatrix[,] <- 1
predMatrix["Adoption", "Adoption"] <- 0 
predMatrix["Adoption", "Country_Code"] <- -2
predMatrix["Adoption", "Year"] <- 1

set.seed(123)
imputed_data <- mice(Correlation_Analysis, method = "pmm", predictorMatrix = predMatrix, m = 10, seed = 123)

averaged_data <- complete(imputed_data, action = "long") %>%
  group_by(Country, Year) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

scale_variables <- c("CPI", "Inflation", "UnemploymentRate", "Adoption", "GDP_per_capita", "Internet_Penetration",  "ExchangeRate", "ExchangeRate_Volatility", "Regulation")

for (var in scale_variables) {
  scaled_var <- paste0(var, "_scaled")
  averaged_data[[scaled_var]] <- as.numeric(scale(averaged_data[[var]]))
}

averaged_data$GDP_percapita_log <- log(averaged_data$GDP_per_capita)
averaged_data$GDP_percapita_scaled <- as.numeric(scale(averaged_data$GDP_percapita_log))

library(plm)
library(lmtest)
library(sandwich)

panel_model <- plm(Adoption_scaled ~ CPI_scaled + ExchangeRate_Volatility_scaled + Inflation_scaled + UnemploymentRate_scaled + Internet_Penetration_scaled + GDP_per_capita_scaled + Regulation_scaled, data = averaged_data, index = c("Country_Code", "Year"), model = "within")

driscoll_kraay_se <- coeftest(panel_model, vcov = vcovSCC(panel_model, type = "HC1", cluster = "group"))

print(driscoll_kraay_se)



#PCA Analysis
pca_vars <- averaged_data %>%
  ungroup() %>%
  select(GPR_country_scaled, CPI_scaled, GTI_scaled, Internet_Penetration_scaled,
         GDP_per_capita_scaled, Regulation_scaled)

pca_result <- prcomp(pca_vars, center = TRUE, scale. = TRUE)

summary(pca_result)

plot(pca_result, type = "l")

pca_scores <- as.data.frame(pca_result$x)
averaged_data$PC1 <- pca_scores$PC1
averaged_data$PC2 <- pca_scores$PC2

pca_model <- plm(Adoption_scaled ~ PC1 + PC2, data = averaged_data, index = c("Country_Code", "Year"), model = "within")
pca_model_results <- coeftest(pca_model, vcov = vcovSCC(pca_model, type = "HC1", cluster = "group"))

pca_model_df <- data.frame(
  Variable = rownames(pca_model_results),
  Estimate = pca_model_results[, 1],
  Std_Error = pca_model_results[, 2],
  t_value = pca_model_results[, 3],
  p_value = pca_model_results[, 4]
)

pca_vars <- averaged_data %>%
  ungroup() %>%
  select(CPI_scaled, ExchangeRate_Volatility_scaled,
         Inflation_scaled, UnemploymentRate_scaled, Internet_Penetration_scaled,
         GDP_per_capita_scaled, Regulation_scaled)

pca_result <- prcomp(pca_vars, center = TRUE, scale. = TRUE)

summary(pca_result)

plot(pca_result, type = "l")

pca_scores <- as.data.frame(pca_result$x)
averaged_data$PC1 <- pca_scores$PC1
averaged_data$PC2 <- pca_scores$PC2

pca_model <- plm(Adoption_scaled ~ PC1 + PC2, data = averaged_data, index = c("Country_Code", "Year"), model = "within")
pca_model_results <- coeftest(pca_model, vcov = vcovSCC(pca_model, type = "HC1", cluster = "group"))

pca_model_df <- data.frame(
  Variable = rownames(pca_model_results),
  Estimate = pca_model_results[, 1],
  Std_Error = pca_model_results[, 2],
  t_value = pca_model_results[, 3],
  p_value = pca_model_results[, 4]
)

loadings <- pca_result$rotation
print(round(loadings, 3))


library(knitr)
kable(round(loadings[, 1:2], 3), caption = "PCA Loadings for PC1 and PC2")



pca_model <- plm(Adoption_scaled ~ PC1 + PC2, data = averaged_data, index = c("Country_Code", "Year"), model = "within")
pca_model_results <- coeftest(pca_model, vcov = vcovSCC(pca_model, type = "HC1", cluster = "group"))

pca_model_df <- data.frame(
  Variable = rownames(pca_model_results),
  Estimate = pca_model_results[, 1],
  Std_Error = pca_model_results[, 2],
  t_value = pca_model_results[, 3],
  p_value = pca_model_results[, 4]
)
print(pca_model_df)


pca_summary <- summary(pca_result)

var_table <- data.frame(
  Component = colnames(pca_result$x),
  Std_Deviation = pca_result$sdev,
  Proportion_of_Variance = pca_summary$importance["Proportion of Variance", ],
  Cumulative_Proportion = pca_summary$importance["Cumulative Proportion", ]
)
print(var_table)


loadings_table <- round(pca_result$rotation, 3)

loadings_df <- as.data.frame(loadings_table)
loadings_df$Variable <- rownames(loadings_df)
loadings_df <- loadings_df[, c(ncol(loadings_df), 1:(ncol(loadings_df)-1))]
print(loadings_df)


write.csv(pca_model_df, "PCA_Model_Results.csv", row.names = FALSE)
write.csv(var_table, "Variance_Explained.csv", row.names = FALSE)
write.csv(loadings_df, "PCA_Loadings.csv", row.names = FALSE)



#World Map
library(countrycode)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(dplyr)


world <- ne_countries(scale = "medium", returnclass = "sf")

averaged_data$match_name <- countrycode(averaged_data$Country, origin = "country.name",
                                        destination = "country.name",
                                        custom_match = c("United States" = "United States of America"))

world_data <- left_join(world, averaged_data, by = c("admin" = "match_name"))

ggplot(world_data) +
  geom_sf(aes(fill = Adoption)) +
  scale_fill_viridis_c(option = "H", na.value = "#D3D3D3") +
  labs(
    title = "Raw Cryptocurrency Adoption by Country",
    fill = "Adoption (Raw)"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1A1A40"),
    legend.title = element_text(face = "bold", color = "#1A1A40"),
    panel.grid.major = element_line(color = "#B0B0B0"),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )



