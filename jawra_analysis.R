
# Load data ---------------------------------------------------------------

rm(list=ls())
options(scipen=999)

setwd("E:\\konrad\\Projects\\usgs\\prosper-nhd\\data\\outputs\\csv")

fn = "nhd_pdsi_analysis.csv"

library(tidyverse)
library(broom)

indat <- as.data.frame(read_csv(fn))

