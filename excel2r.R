library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
library(data.table)
#library(readxl)
#library(XLConnect)

## EXCEL-------

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


## CSV FROM GITHUB------

dfall <- data.table()

csvname <- c("23.02","24.02","25.02","26.02","27.02","28.02","29.02","01.03","02.03","03.03","04.03","05.03","06.03")

for (i in csvname) {
  print(i)
  if (dim(dfall)[1] == 0 ) {
    print('value 0')
    dfall = fread(paste0('https://raw.githubusercontent.com/carlotorniai/COVID-19-Italy/develop/data/csv/',i,'.csv')) %>% 
      mutate(giorno = paste0(i,'.2020'))
  } else {
    print('value 1')
    dfall <- rbind(dfall,
                   fread(paste0('https://raw.githubusercontent.com/carlotorniai/COVID-19-Italy/develop/data/csv/',i,'.csv')) %>% 
                     mutate(giorno = paste0(i,'.2020'))
                   ) 
  }
}

dfclean = dfall

dfclean[is.na(dfclean)] = 0

dfclean <- dfclean %>% filter(Regione != 'Somma Totale') %>% 
  mutate(Regione = case_when(Regione == 'Emila Romanga' ~ 'Emilia Romagna',
                             T ~ Regione))

dfclean %>% filter(Regione %in% c('Emilia Romagna','Lombardia','Veneto')) %>% 
  plot_ly(x = ~ dmy(giorno),y = ~ Tamponi,
          type = 'scatter',mode = 'marker+scatter',
          size = ~round(Tamponi/`Totale Positivi`,1),color = ~Regione,
          hoverinfo = 'text',
          text = ~paste0('</br> Data: ', giorno,
                         '</br> Positivi: ', `Totale Positivi`,
                         '</br> Regione: ', Regione,
                         '</br> Tamponi: ', Tamponi,
                         '</br> Tamponi/Positivi: ', round(Tamponi/`Totale Positivi`,1))) %>% 
  layout(xaxis = list(title = 'Data'),
         title = 'Tamponi vs Casi positivi')
