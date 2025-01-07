#' this project creates monthly temperature heatmap based on historical data
#' data obtained from "Meteogalicia" official portal:
#' https://www.meteogalicia.gal/observacion/estacionshistorico/historico.action
#' following search parameters used to choose a certain station
#' Provincia: 'Pontevedra', Consello: 'Vigo', Estación: 'Porto de Vigo'
#' package 'pheatmap' is used to create resulting plot

# importing necessary packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("reshape2")
install.packages("pheatmap")

library(tidyverse)
library(lubridate)
library(reshape2)
library(pheatmap)

# reading data from file
weather_dec_df <- read.csv("resultadoCSV_24.12.csv")

# presenting dataframe as matrix for heatmapping
temp_mat <- weather_dec_df %>%
	# provided data contains rows with value and parameter name, among other
	# e.g., first of all we need to filter rows with tempeature only
	filter(Código.parámetro == "TA_AVG_1.5m") %>%
	# data for midnight on last day of month presented as 1st day of next month
	# in this particular case it is also next year
	subset(., lubridate::year(Data) == "2024") %>%
	# we group temperature 10-minute records into 3 hour bins
	# calculating mean tempeature for this time period
	mutate(
		cuts = cut(lubridate::hour(Data),
		c(-Inf, 2, 5, 8, 11, 14, 17, 20, Inf))
	) %>%
	group_by(lubridate::day(Data), cuts) %>%
	summarize(mean_temp_by_3hrs = mean(Valor)) %>%
	# and final casting dataframe to matrix
	acast(
		.,
		c("0:00-3:00", "3:00-6:00", "6:00-9:00", "9:00-12:00",
		  "12:00-15:00", "15:00-18:00", "18:00-21:00", "21:00-0:00"
		) ~ .$"lubridate::day(Data)",
	value.var = "mean_temp_by_3hrs"
	)

pheatmap(
  temp_mat,  main="Monthly Temperature in Vigo, Spain (December 2024), Cº",
  treeheight_row=0, treeheight_col=0,
  cellwidth=15, cellheight=20,
  cluster_cols=FALSE, angle_col=0)