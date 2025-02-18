library(ggplot2)

summary(ANT)

# Gráfico de barras de tasas por municipio
ggplot(df, aes(x = MUNICIPIO, y = tasa_nat)) + 
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Tasa de Natalidad por Municipio", x = "Municipio", y = "Tasa de Natalidad") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(df, aes(x = MUNICIPIO, y = tasa_def)) + 
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Tasa de Defunciones por Municipio", x = "Municipio", y = "Tasa de Defunciones") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))