Sys.setlocale("LC_TIME", "es_ES.UTF-8")

library(calendR)
library(tidyverse)


calendR(year = 2025,
        start = "M",
        special.days = c(9, 19, 56, 79, 102,  # Days to color
                         126, 257, 300, 342),
        special.col = "lightblue",            # Color of the specified days
        low.col = "white")                    # Background color of the rest of the days

calendR::calendR(
                 from = "2024-10-01",
                 to = "2024-12-31",
                 start = "M", 
                 special.days = sapply(c("2024-10-31", 
                                         str_c("2024-11-", c(5,7,12,14,19,21,26,28)), 
                                         str_c("2024-12-", c(10, 5,3))
                 ), lubridate::yday) - lubridate::yday("2024-09-30"), 
                 special.col = "red", 
                 text.col = c("blue")
                 )

