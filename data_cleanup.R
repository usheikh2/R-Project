install.packages("purrr")
install.packages("tidyverse")
library(tidyverse)
library(purrr)

#Replace with your directory 
file_path <- fs::dir_ls("~/Desktop/u/OR 568/OR Project/archive")

#Reads all csv files in that path 
file_path %>%
  map(function (path){
    read_csv(path)
    
  })
file_path  

#Read CSV data tables in file_path
aisles = read.csv(file_path[[1]])
departments = read.csv(file_path[[2]])
order_products__prior = read.csv(file_path[[3]])
order_products__train = read.csv(file_path[[4]])
orders = read.csv(file_path[[5]])
products = read.csv(file_path[[6]])

