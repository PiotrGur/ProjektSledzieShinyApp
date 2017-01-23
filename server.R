
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

library(dplyr)
library(ggplot2)
library(tabplot)
library(caret)
library(datasets)
library(data.table)
require(zoo)
library (plyr)
library(reshape2)
library(reshape)



options(shiny.maxRequestSize=30*1024^2)

#Kod zapewniający powtarzalność wyników przy każdym uruchomieniu raportu na tych samych danych.
set.seed(127)

#Kod pozwalający wczytać dane z pliku.
wczytaneDane <- read.csv("sledzie.csv", stringsAsFactors = FALSE)


newColNames<-c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl")
colWithPlankt1<-c("dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl","plankt")
colnames(wczytaneDane)<-newColNames

mydata<-wczytaneDane

shinyServer(function(input, output) {

#Kod przetwarzający brakujące dane.
# find elements
idx <- mydata == "?"
# replace elements with NA
is.na(mydata) <- idx

przedC<-nrow(mydata)

output$przedCzyszczeniemInfo <- renderText({przedC})

good<-complete.cases(mydata)
mydata_cc<-mydata[good,]
badRow<-sum(!good)

output$wierszeZNA<-renderText({badRow})

poC<-nrow(mydata_cc)

output$poCzyszczeniuInfo<-renderText({poC})

dfCzyszczenieInfo = data.frame(c("Liczba_wierszy_przed_czyszczeniem",przedC),
                               c("Liczba_wierszy_z_NA",badRow),
                               c("Liczba_wierszy_po_czyszczeniu",poC))
output$dfCzyszczenieInfo<-renderDataTable(dfCzyszczenieInfo)

#Sekcję podsumowującą rozmiar zbioru i podstawowe statystyki.
output$wczytaneDaneTabelaAll <- renderDataTable(wczytaneDane, options = list(pageLength = 5))
output$daneWczytaneSummaryInfoPrzedCzyszczeniem<- renderDataTable(summary(wczytaneDane))

wczytaneDaneCharToNum<-
  transform(wczytaneDane, 
          #"gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2"
          gpl1g1 = as.numeric(gpl1g1), 
          gpl1g2 = as.numeric(gpl1g2),
          gpl2g1 = as.numeric(gpl2g1), 
          gpl2g2 = as.numeric(gpl2g2),
          gwidlg1 = as.numeric(gwidlg1), 
          gwidlg2 = as.numeric(gwidlg2),
          tempC = as.numeric(tempC)
          )
wczytaneCharToNumPlankt<-mutate(wczytaneDaneCharToNum, plankt=gpl1g1+gpl1g2+gpl2g1+gpl2g2+gwidlg1+gwidlg2)

columnsWithPlankt <- c(newColNames,"plankt")
bezNAcharToNumPlanktNORMALIZE<- wczytaneCharToNumPlankt %>% mutate_each_(funs(scale(.) %>% as.vector), vars=colWithPlankt1)

#wczytaneDaneOldCharAsChar<-wczytaneDane
#wczytaneDane<-wczytaneDaneCharToNum

wczytaneZPlankt<-wczytaneCharToNumPlankt
output$daneWczytaneSummaryInfoPrzedCzyszczeniemPoTransformAllToNumeric<- renderDataTable(summary(wczytaneZPlankt))


########START h4("Podsumowanie rozmiaru zbioru, wykresy")
columns1 = names(wczytaneZPlankt)
columns2 = names(wczytaneZPlankt)
columns3 = names(wczytaneZPlankt)

boxplot1AllNieNorm<-boxplot(wczytaneZPlankt[,columns1,drop=FALSE])

boxPlot2AllNieNorm<-ggplot(melt(
  wczytaneZPlankt[,columns2,drop=FALSE]
), aes(variable, value)) + geom_boxplot()

hist1AllNieNorm<-      ggplot(melt(
  wczytaneZPlankt[,columns3,drop=FALSE]
), aes(variable, value)) + geom_bar(
  stat="identity",
  position=
    "identity")#+geom_smooth()

hist2AllNorm<-      ggplot(melt(
  bezNAcharToNumPlanktNORMALIZE
), aes(variable, value)) + geom_bar(
  stat="identity",
  position=
    "identity")#+geom_smooth()

boxPlot3AllNorm<-ggplot(melt(
  bezNAcharToNumPlanktNORMALIZE
), aes(variable, value)) + geom_boxplot()+geom_smooth()


#to samo co w powyższych observerEvent (tam reakcja na zmianę, tu przy pierwszym uruchomieniu)
output$daneWczytaneBOXPlotPrzedCzyszczeniem<- renderPlot(boxplot(wczytaneZPlankt[,columns1,drop=FALSE]))
output$daneWczytane_BOXPlot_BezNA_PrzedCzyszczeniem_GGPlot<- renderPlot(boxPlot2AllNieNorm)

output$daneWczytaneHistBezNAPrzedCzyszcOutliersCzyszczenuGGPlot<- renderPlot(hist1AllNieNorm)

output$daneWczytaneHistPoCzyszczenuGGPlotNormalize<- renderPlot(hist2AllNorm)

output$daneWczytaneBOXPlotPrzedCzyszczeniemGGPlotNormalize<- renderPlot(boxPlot3AllNorm)

observeEvent(input$select1,{
  
  if (!is.null(input$select1)) {
    columns1 = input$select1
    
    output$daneWczytaneBOXPlotPrzedCzyszczeniem<- renderPlot(boxplot(wczytaneZPlankt[,columns1,drop=FALSE]))
    
  }
})
observeEvent(input$select2,{
  
  if (!is.null(input$select2)) {
    columns2 = input$select2
    
    output$daneWczytane_BOXPlot_BezNA_PrzedCzyszczeniem_GGPlot<- renderPlot(boxPlot2AllNieNorm)
  }
})
observeEvent(input$select3,{
  
  if (!is.null(input$select3)) {
    columns3 = input$select3
    
    output$daneWczytaneHistBezNAPrzedCzyszcOutliersCzyszczenuGGPlot<- renderPlot(hist1AllNieNorm)
  }
})
########END h4("Podsumowanie rozmiaru zbioru, wykresy")

######remove outliers IRQ
data_without_outliers <- bezNAcharToNumPlanktNORMALIZE

# wiersze do usunięcia
Outliers <- c()
outliersInfo <- data.frame(info=c("nazwa_col","liczba_out"))
# kolumny do usunięcia outlierów
for(i in colWithPlankt1){
  
  # min/max
  max <- quantile(data_without_outliers[,i],0.75, na.rm=TRUE) + (IQR(data_without_outliers[,i], na.rm=TRUE) * 1.5 )
  min <- quantile(data_without_outliers[,i],0.25, na.rm=TRUE) - (IQR(data_without_outliers[,i], na.rm=TRUE) * 1.5 )
  
  idx <- which(data_without_outliers[,i] < min | data_without_outliers[,i] > max)
  
  # Output the number of outliers in each variable
  outliersInfo[,i]  <- c(i,length(idx))
  # dodanie outlierów do listy
  
  Outliers <- c(Outliers, idx) 
}

# sortowanie
Outliers <- sort(Outliers)

# usunięcie outlierów
data_without_outliers <- data_without_outliers[-Outliers,]

output$outliers_Info<-renderDataTable(outliersInfo)
#######remove outliers IRQ
data_without_outliers_1 <- data_without_outliers
output$daneWczytaneBOXPlotBezNAOutliers1000_1<- renderPlot(boxplot(select(data_without_outliers,dl:plankt)))
######1remove outliers IRQ
#data_without_outliers <- bezNAcharToNumPlanktNORMALIZE

# wiersze do usunięcia
Outliers <- c()
outliersInfo1 <- data.frame(info=c("nazwa_col","liczba_out"))
# kolumny do usunięcia outlierów
for(i in colWithPlankt1){
  
  # min/max
  max <- quantile(data_without_outliers[,i],0.75, na.rm=TRUE) + (IQR(data_without_outliers[,i], na.rm=TRUE) * 1.5 )
  min <- quantile(data_without_outliers[,i],0.25, na.rm=TRUE) - (IQR(data_without_outliers[,i], na.rm=TRUE) * 1.5 )
  
  idx <- which(data_without_outliers[,i] < min | data_without_outliers[,i] > max)
  
  # Output the number of outliers in each variable
  outliersInfo1[,i]  <- c(i,length(idx))
  # dodanie outlierów do listy
  
  Outliers <- c(Outliers, idx) 
}

# sortowanie
Outliers <- sort(Outliers)

# usunięcie outlierów
data_without_outliers <- data_without_outliers[-Outliers,]
output$daneWczytaneBOXPlotBezNAOutliers1000_2<- renderPlot(boxplot(select(data_without_outliers_1,dl:plankt)))
output$outliers_Info1<-renderDataTable(outliersInfo1)
#######1remove outliers IRQ


########START without outliers 1000
data_without_outliers1000 <- data_without_outliers[sample(nrow(select(data_without_outliers,dl:plankt)),1000),]

columns11 = names(select(data_without_outliers1000,dl:plankt))

boxplot11AllNieNorm<-boxplot(data_without_outliers1000[,columns11,drop=FALSE])

boxPlot21AllNieNorm<-ggplot(melt(data_without_outliers1000[,columns11,drop=FALSE]
), aes(variable, value)) + geom_boxplot()


hist21AllNorm<-      ggplot(melt(
  data_without_outliers1000[,columns11,drop=FALSE]
), aes(variable, value)) + geom_bar(
  stat="identity",
  position=
    "identity")#+geom_smooth()


#to samo co w powyższych observerEvent (tam reakcja na zmianę, tu przy pierwszym uruchomieniu)
output$daneWczytaneBOXPlotBezNAOutliers1000<- renderPlot(boxplot(data_without_outliers[,columns11,drop=FALSE]))

output$daneWczytane_BOXPlot_BezNAOutliers_GGPlot1000<- renderPlot(boxPlot21AllNieNorm)

output$daneWczytaneHistBezNAOutliersGGPlotNormalize1000<- renderPlot(hist21AllNorm)

observeEvent(input$select11,{
  
  if (!is.null(input$select11)) {
    columns11 = input$select11
    
    output$daneWczytaneBOXPlotBezNAOutliers1000<- renderPlot(boxplot(data_without_outliers[,columns11,drop=FALSE]))
    
    boxPlot21AllNieNorm<-ggplot(melt(
      data_without_outliers1000[,columns11,drop=FALSE]
    ), aes(variable, value)) + geom_boxplot()
    output$daneWczytane_BOXPlot_BezNAOutliers_GGPlot1000<- renderPlot(boxPlot21AllNieNorm)
    
    
    hist21AllNorm<-      ggplot(melt(
      data_without_outliers1000[,columns11,drop=FALSE]
    ), aes(variable, value)) + geom_bar(
      stat="identity",
      position=
        "identity")#+geom_smooth()
    output$daneWczytaneHistBezNAOutliersGGPlotNormalize1000<- renderPlot(hist21AllNorm)

  }
})

########END without outliers 1000


output$daneBezNA <- renderDataTable(mydata_cc, options = list(
                                     pageLength = 5, lengthMenu = c(5, 10, 15, 20,50,100) ))

output$dataSummaryInfoPoCzyszczeniu<- renderDataTable(summary(mydata_cc))

mydata_cc_sample1000WithOut<- mydata_cc[sample(nrow(mydata_cc),1000),]

#output$dataInfoSample <- renderDataTable(summary(mydata_cc_sample1000WIthOut))

#wartości distinct dla wczytanych z NA i po oczyszczeniu  
uniqueDataWczytanezNA <- sapply(wczytaneDane, function(x)length(unique(x)))

dfWczytaneDane <- ldply (uniqueDataWczytanezNA, data.frame)

output$distinctPoWczytaniuJeszczeZNA <- renderDataTable(dfWczytaneDane)


uniqueData <- sapply(mydata_cc, function(x)length(unique(x)))

df <- ldply (uniqueData, data.frame)

output$distinctPoCzyszczeniuzNA <- renderDataTable(df)

dfUnique <- 
  data.frame(Nazwy_kolumn=dfWczytaneDane[,c(1)],Dane_przed_czyszczeniem=dfWczytaneDane[,c(2)],Dane_oczyszczone_z_NA = df[,c(2)])
output$dfUniqueTable<-
  renderDataTable(dfUnique)


  #Szczegółową analizę wartości atrybutów (np. poprzez prezentację rozkładów wartości).


output$plotAllCorrelatedHist<-renderPlot({
  ggplot(data = melt(data_without_outliers1000), mapping = aes(x = value)) + 
    geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
  
  })
  output$allTablePlot<-renderPlot({tableplot(data_without_outliers1000)})
  #ggplot(data_without_outliers1000,
   #                                          aes(x=nr)) +
    #geom_histogram(aes(y=dl))
    
    
    #geom_histogram(aes(y=dl), color="red") +
    #geom_histogram(aes(y=gpl1g1), color="blue")+
    #geom_histogram(aes(y=gpl1g2), color="green") +
    #geom_histogram(aes(y=gpl2g1), color="yellow") +
    #geom_histogram(aes(y=gpl2g2), color="orange") +
    #geom_histogram(aes(y=gwidlg1), color="grey") +
    #geom_histogram(aes(y=gwidlg2), color="brown") +
    #geom_histogram(aes(y=poz_nar), color="pink") +
    #geom_histogram(aes(y=roczny_nar), color="blue") +
    #geom_histogram(aes(y=lpoz_nar), color="orange") +
    #geom_histogram(aes(y=llzlowryb), color="green") +
    #geom_histogram(aes(y=tempC), color="brown") +
    #geom_histogram(aes(y=zasol), color="black") +
    #geom_histogram(aes(y=mies), color="grey") +
    #geom_histogram(aes(y=oscyl), color="yellow") +
    #geom_histogram(aes(y=plankt), color="blue") 
#})

#output$boxPlot1All<-ggplot(melt(mydata_cc), aes(variable, value)) + geom_boxplot()

#output$boxPlot2Sample<-ggplot(melt(mydata_cc_sample), aes(variable, value)) + geom_boxplot()


meltData <- melt(mydata_cc)
output$boxPlot1All<-renderPlot({boxplot(data=meltData, value~variable)})

meltData <- melt(mydata_cc_sample1000WithOut)
output$boxPlot2Sample<-
  renderPlot({boxplot(data=meltData, value~variable)})


  #Sekcję sprawdzającą korelacje między zmiennymi; sekcja ta powinna zawierać jakąś formę graficznej prezentacji korelacji.
#output$corrTable<-renderDataTable(cor(data_without_outliers1000[sample(nrow(data_without_outliers1000),100),]))

#output$plotAllCorrelated<-renderPlot({ggplot(data_without_outliers1000[sample(nrow(data_without_outliers1000),100),],
 #      aes(x=nr)) +
  #geom_point(aes(y=dl), color="red") +geom_smooth(y=dl)+
   # geom_point(aes(y=gpl1g1), color="blue") +geom_smooth(y=dl)+
    #geom_point(aes(y=gpl1g2), color="green")  +geom_smooth(y=dl)+
    #geom_point(aes(y=gpl2g1), color="yellow")  +geom_smooth(y=dl)+
    #geom_point(aes(y=gpl2g2), color="orange")  +geom_smooth(y=dl)+
  #geom_line(aes(y=gwidlg1), color="grey") +
  #geom_line(aes(y=gwidlg2), color="brown") +
  #geom_line(aes(y=poz_nar), color="pink") +
  #geom_line(aes(y=roczny_nar), color="blue") +
  #geom_line(aes(y=lpoz_nar), color="orange") +
  #geom_line(aes(y=llzlowryb), color="green") +
  #geom_line(aes(y=tempC), color="brown") +
  #geom_line(aes(y=zasol), color="black") +
  #geom_line(aes(y=mies), color="grey") +
  #geom_line(aes(y=oscyl), color="yellow") +
  #geom_line(aes(y=plankt), color="blue") 
#})

#output$plotAllCorrelated1<-renderPlot({
  #ggplot(data_without_outliers1000,
   #                                          aes(x=nr)) +
    #geom_point(aes(y=dl), color="red") +
    #geom_point(aes(y=gpl1g1), color="blue")+
    #geom_point(aes(y=gpl1g2), color="green") +
    #geom_point(aes(y=gpl2g1), color="yellow") +
    #geom_point(aes(y=gpl2g2), color="orange") +
    #geom_point(aes(y=gwidlg1), color="grey") +
    #geom_point(aes(y=gwidlg2), color="brown") +
    #geom_point(aes(y=poz_nar), color="pink") +
    #geom_point(aes(y=roczny_nar), color="blue") +
    #geom_point(aes(y=lpoz_nar), color="orange") +
    #geom_point(aes(y=llzlowryb), color="green") +
    #geom_point(aes(y=tempC), color="brown") +
    #geom_point(aes(y=zasol), color="black") +
    #geom_point(aes(y=mies), color="grey") +
    #geom_point(aes(y=oscyl), color="yellow") +
    #geom_point(aes(y=plankt), color="blue") 
#})


  #Interaktywny wykres lub animację prezentującą zmianę rozmiaru śledzi w czasie.

  

  #Sekcję próbującą stworzyć regresor przewidujący rozmiar śledzia (w tej sekcji należy wykorzystać wiedzę z pozostałych punktów oraz wykonać dodatkowe czynności, które mogą poprawić trafność predykcji); dobór parametrów modelu oraz oszacowanie jego skuteczności powinny zostać wykonane za pomocą techniki podziału zbioru na dane uczące, walidujące i testowe; trafność regresji powinna zostać oszacowana na podstawie miar R2R2 i RMSERMSE.
inTraining <- 
  createDataPartition(
    # atrybut do stratyfikacji
    y = select(data_without_outliers1000, dl:plankt, -(gpl1g1:gwidlg2))$llzlowryb,
    # procent w zbiorze uczącym
    p = .50,
    # chcemy indeksy a nie listę
    list = FALSE)

training <- Sonar[ inTraining,]
testingWalidating  <- Sonar[-inTraining,]

#inTesting <- 
 # createDataPartition(
    # atrybut do stratyfikacji
  #  y = select(testingWalidating)$llzlowryb,
    # procent w zbiorze uczącym
   # p = .25,
    # chcemy indeksy a nie listę
    #list = FALSE)
 
#testing <- Sonar[ inTesting,]
#walidating  <- Sonar[-inTesting,] 

  #Analizę ważności atrybutów najlepszego znalezionego modelu regresji. Analiza ważności atrybutów powinna stanowić próbę odpowiedzi na pytanie: co sprawia, że rozmiar śledzi zaczął w pewnym momencie maleć.

  
  output$distPlot <- 
    renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  
  output$distPlot1 <- 
    renderPlot({
      ggplot(data = melt(data_without_outliers1000), mapping = aes(x = value)) + 
        geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
      
    })
  
  
  
  
  
  
  datz <- zoo(mydata_cc_sample1000WithOut)
 
  
  output$distPlot2 <- 
    renderPlot({
    plot(datz)
    
  })
  
  

  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  observeEvent(input$checkboxPodsumowanieRozmiaruZbioruWykresy,{
    toggle(condition = input$checkboxPodsumowanieRozmiaruZbioruWykresy, selector = "#podsumowanieRozmiaruZbioruWykresy")
  })
  observeEvent(input$checkboxPodsumowanieRozmiaruZbioruWykresy1000,{
    toggle(condition = input$checkboxPodsumowanieRozmiaruZbioruWykresy1000, selector = "#podsumowanieRozmiaruZbioruWykresy1000")
  })
  observeEvent(input$checkboxPodsumowanieRozmiaruZbioruStatystyki,{
    toggle(condition = input$checkboxPodsumowanieRozmiaruZbioruStatystyki, selector = "#podsumowanieRozmiaruZbioruStatystyki")
  })

  
  
  
})

