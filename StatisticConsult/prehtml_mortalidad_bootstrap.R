
library(rsample)
library(ggplot2)
library(dplyr)
library(tidyverse)

Mortalidad_Materna<-read.csv(header=TRUE,"C:\\Users\\elektra\\OneDrive - Universidad Veracruzana\\CONSULTORIA\\PROYECTO FINAL\\BOOPSTRAP_NACIMIENTOS_2008_2020.csv")


Mortalidad_Materna %>%
  select(Anio, AGUASCALIENTES:ZACATECAS,BAJA_CALIFONIA:VERACRUZ) %>%
  pivot_longer(AGUASCALIENTES:VERACRUZ) %>%
  group_by(Anio, name) %>%
  summarise(prop = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(Anio, prop, color = name)) +
  geom_line(size = 1.2, show.legend = FALSE) +
  facet_wrap(vars(name),scales = "free") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "% de muertes maternas")

Mortalidad_Materna1<-dplyr::select(n_2017,-id,-X)
simple_mod <- lm(Anio ~ .,
                 data = n_20171
)



bootstraps(Mortalidad_Materna1, times = 1e3)

set.seed(123)
intervalos <- reg_intervals(Anio ~ AGUASCALIENTES+
                                     BAJA_CALIFONIA+
                                     BAJA_CALIFORNIA_SUR+
                                     CAMPECHE+
                                     COAHUILA+
                                     COLIMA+
                                     CHIAPAS+
                                     CHIHUAHUA+
                                     DF+
                                     DURANGO+
                                     GUANAJUATO+
                                     GUERRERO+
                                     HIDALGO+
                                     JALISCO+
                                     MICHOACAN+
                                     MORELOS+
                                     NAYARIT+
                                     NUEVO_LEON+
                                     OAXACA+
                                     PUEBLA+
                                     QUERETARO+
                                     QUINTANA_ROO+
                                     SAN_LUIS_POTOSI+
                                     SINALOA+
                                     SONORA+
                                     TABASCO+
                                     TAMAULIPAS+
                                     TLAXCALA+
                                     VERACRUZ+
                                     YUCATAN+
                                     ZACATECAS, data = n_20171,
                                   type = "percentile",
                                   keep_reps = TRUE
)

#youtube_intervals
intervalos %>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  ggplot(aes(.estimate, term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray80") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper),
                 size = 1.5, alpha = 0.5, color = "midnightblue"
  ) +
  geom_point(size = 3, color = "midnightblue") +
  labs(
    x = "Incremento de las Muertes Maternas",
    y = NULL
  )

intervalos %>%
  mutate(
    term = str_remove(term, "TRUE"),
    term = fct_reorder(term, .estimate)
  ) %>%
  unnest(.replicates) %>%
  ggplot(aes(estimate, fill = term)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(vars(term),scales = "free")+
  labs(title="Distribución de Intervalos de Confianza Bootstrap al 95% por Entidad Federativa",
       x="Estimaciones",y="")
