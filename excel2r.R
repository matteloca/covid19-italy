library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
library(data.table)
library(readxl)
library(XLConnect)

setwd("~/training/coronavirus")

df <- data.table()

wb <- loadWorkbook(filename = "covid-19_IT.xlsx")

sheetsname <- getSheets(wb)

for (i in sheetsname){
  temp.df <- as.data.table(read_xlsx("covid-19_IT.xlsx",sheet = i))
  temp.df[,sheetname:=paste0(i,'.2020')]
   # enter code here
  df <- rbind(df,temp.df)
  }

dfclean <- df

dfclean[is.na(dfclean)] = 0

dfclean <- dfclean %>% filter(Regione != 'Somma Totale') %>% 
  mutate(Regione = case_when(Regione == 'Emila Romagna' ~ 'Emilia Romagna',
                             T ~ Regione))

dfclean %>% filter(Regione %in% c('Emilia Romagna','Lombardia','Veneto')) %>% 
  plot_ly(x = ~ dmy(sheetname),y = ~ Tamponi,
          type = 'scatter',mode = 'marker+scatter',
          size = ~round(Tamponi/`Totale Positivi`,1),color = ~Regione,
          hoverinfo = 'text',
          text = ~paste0('</br> Data: ', sheetname,
                        '</br> Positivi: ', `Totale Positivi`,
                        '</br> Regione: ', Regione,
                        '</br> Tamponi: ', Tamponi,
                        '</br> Tamponi/Positivi: ', round(Tamponi/`Totale Positivi`,1))) %>% 
    layout(xaxis = list(title = 'Data'),
           title = 'Tamponi vs Casi positivi')
