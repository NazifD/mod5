rm(list=ls())
#install.packages("fredr")
#install.packages("tidyverse")
#install.packages("zoo")
library(fredr)
library(tidyverse)


api_key <- "9943044600cc5fd157c7e72a2edb6eca"
fredr_set_key(api_key)

series <- c(
  "USAUCSFRCONDOSMSAMID", 
  "CSUSHPINSA", 
  "USSTHPI"
)

hget_data <- function(series_id){
  df <- fredr(series_id = series_id) |> 
    mutate(d_value = c(NA, diff(value)))
  dfA <- df |>  
    pivot_wider(names_from = series_id)
  colnames(dfA)[4] <- c(
    paste0("d_", series_id) 
  )
  dfA
}


df1 <- hget_data(series[1])
print(df1)

df2 <- hget_data(series[2])
print(df2)

df3 <- hget_data(series[3])
print(df3)

df4 <- hget_data("MEDSQUFEENY")
print(df4)

df <-full_join(df1,df2) |> full_join(df3) |> full_join(df4)
print(df)


str(df)

df <- df |>
  mutate(d_USSTHPI = zoo::na.approx(d_USSTHPI, na.rm =FALSE))

ggplot(df, aes(x=date)) + 
  #  lims(x=c(Sys.Date() - 3000, NA)) + 
  geom_line(aes(y=d_USAUCSFRCONDOSMSAMID/1000, color = 'green')) +
  geom_line(aes(y=d_CSUSHPINSA/3), color='red') +
  geom_line(aes(y=d_USSTHPI/10), color='black') +
  geom_point(aes(y=d_MEDSQUFEENY/10), color='blue', alpha=1/10)

plot(df$date,df$d_MEDSQUFEENY/10, col="red")
points(df$date, df$d_USSTHPI/10, col="blue")
points(df$date, df$d_USAUCSFRCONDOSMSAMID/1000, col="green")
