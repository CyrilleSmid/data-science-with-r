library(ggplot2)
library(factoextra)
library(dplyr)
library(tidyverse)
library(lubridate)
library(pwt9)

df <- read.csv("filtered_exchange_rate.csv", sep=",")
df$RecordDate <- as.Date(df$RecordDate, format="%d/%m/%Y")

# Removing extreme values
'%!in%' <- function(x,y)!('%in%'(x,y))
df <- df[df$CountryCurrency %!in% c("ethiopia-birr"),]

# Scale rates relative to their first value
first_date <- min(df$RecordDate)
first_date_df <- df[df$RecordDate == first_date,]
df$ScaledExchangeRate <- df$ExchangeRate
for (currency in unique(first_date_df$CountryCurrency)) {
  first_exch_rate <- first_date_df[first_date_df$CountryCurrency == currency, "ExchangeRate"]
  df[df$CountryCurrency == currency,]$ScaledExchangeRate <- 
    df[df$CountryCurrency == currency,]$ExchangeRate / first_exch_rate
}

# Plot initial series
ggplot(df, aes(x=RecordDate, y=ExchangeRate, group=CountryCurrency)) + 
  geom_line()
# Plot scaled series
ggplot(df, aes(x=RecordDate, y=ScaledExchangeRate, group=CountryCurrency)) + 
  geom_line()

# Data frame must be in the wide format
df_wide <- df[, c("RecordDate", "CountryCurrency", "ScaledExchangeRate")] %>%
  pivot_wider(names_from = RecordDate, values_from = ScaledExchangeRate)  %>%
  mutate_at(vars(-CountryCurrency), as.numeric)

# Silhouette method
s_clust<-fviz_nbclust(select(df_wide, -CountryCurrency),kmeans,"silhouette")
plot(s_clust)

# Wrist method
w_clust<-fviz_nbclust(select(df_wide, -CountryCurrency),kmeans,"wss")
plot(w_clust)
# Wrist method is preferred and gives an optimal value of 3 clusters


# Kmeans method with a k=3
res <- kmeans(select(df_wide, -CountryCurrency), 3, iter.max = 10, nstart = 10)

# Saving the cluster's vector into a column and transform it to the "long" format
df_wide$Cluster <- res$cluster
df <- pivot_longer(df_wide, 
                   names_to = "RecordDate", 
                   values_to = "ScaledExchangeRate",
                   cols = sapply(unique(df[, "RecordDate"]), toString))

# Plotting the results
ggplot(df, aes(x=RecordDate,
               y=ScaledExchangeRate, 
               group=CountryCurrency,
               color=Cluster)) + 
  geom_line()


