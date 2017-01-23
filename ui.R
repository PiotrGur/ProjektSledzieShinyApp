
#Kod wyliczający wykorzystane biblioteki.

library(shiny)
library(shinyBS)
library(shinyjs)

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

#Kod wyliczający wykorzystane biblioteki.
#options(width = 400)


shinyUI(fluidPage(
  
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(
    id = "loading-content",
    h2("Loading...")
  ),
  
  # The main app code goes here
  hidden(
    div(
      id = "app-content",
      
      
      
      #Sekcję podsumowującą rozmiar zbioru i podstawowe statystyki.
      h3("Podsumowanie zbioru danych"),
      h4("Podsumowanie rozmiaru zbioru, podstawowe statystyki"),
      checkboxInput("checkboxPodsumowanieRozmiaruZbioruStatystyki", "Show|Hide", TRUE),
      div(id="podsumowanieRozmiaruZbioruStatystyki",
      tabsetPanel(
        tabPanel("Wczytane dane", 
                 dataTableOutput("wczytaneDaneTabelaAll")), 
        tabPanel("Summary - wczytane dane", 
                 dataTableOutput("daneWczytaneSummaryInfoPrzedCzyszczeniem")),
        tabPanel("Summary - wczytane dane jako numeryczne", 
                 dataTableOutput("daneWczytaneSummaryInfoPrzedCzyszczeniemPoTransformAllToNumeric"))
      )
      ),
      br(),
      
      h4("Podsumowanie rozmiaru zbioru, wykresy"),
      checkboxInput("checkboxPodsumowanieRozmiaruZbioruWykresy", "Show|Hide", TRUE),
      div(id="podsumowanieRozmiaruZbioruWykresy",
          
          tabsetPanel(
            tabPanel("BoxPlot", 
                     selectInput("select1", "Select columns to display", c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl"), multiple = TRUE),
                     plotOutput("daneWczytaneBOXPlotPrzedCzyszczeniem")), 
            tabPanel("BoxPlot GGPlot", 
                     selectInput("select2", "Select columns to display", c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl"), multiple = TRUE),
                     plotOutput("daneWczytane_BOXPlot_BezNA_PrzedCzyszczeniem_GGPlot")),
            tabPanel("Hist GGPlot", 
                     selectInput("select3", "Select columns to display", c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl"), multiple = TRUE),
                     plotOutput("daneWczytaneHistBezNAPrzedCzyszcOutliersCzyszczenuGGPlot")),
            tabPanel("Hist GGPlot Normalize", 
                     #selectInput("select3", "Select columns to display", c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl"), multiple = TRUE),
                     plotOutput("daneWczytaneHistPoCzyszczenuGGPlotNormalize")),
            tabPanel("BoxPlot GGPlot Normalize", 
                     #selectInput("select2", "Select columns to display", c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl"), multiple = TRUE),
                     plotOutput("daneWczytaneBOXPlotPrzedCzyszczeniemGGPlotNormalize"))
          )
        ),
      br(),
      #Kod przetwarzający brakujące dane.
      h3("Przetwarzanie brakujących danych"),
      tabsetPanel(
        tabPanel("Czyszczenie z NA - podsumowanie", 
                 h4("Liczba obserwacji przed czyszczeniem"),
                 textOutput("przedCzyszczeniemInfo"),
                 h4("Liczba obserwacji z NA"),
                 textOutput("wierszeZNA"), 
                 h4("Liczba obserwacji po czyszczenu"),
                 textOutput("poCzyszczeniuInfo")
        ), 
        tabPanel("Statystyka oczyszczania", 
                 h4("Statystyka oczyszczania"),
                 dataTableOutput("dfCzyszczenieInfo")
        ),
        tabPanel("Dane bez brakujących wartości", 
                 h4("Dane bez brakujących wartości"),
                 dataTableOutput("daneBezNA")
        ),
        tabPanel("Dane po czyszczeniu z NA", 
                 h4("Dane po czyszczeniu z NA"),
                 dataTableOutput("dataSummaryInfoPoCzyszczeniu")
        )
        #,
        #tabPanel("Dane po czyszczeniu z NA, próbka 1000", 
        #         h4("Dane po czyszczeniu z NA, próbka 1000"),
        #         dataTableOutput("dataInfoSample")
        #)
      ),
      br(),
      h4("Usuwanie oulierów - statystyka"),
      h5("Widać, że pozostało kilka outlierów"),
      plotOutput("daneWczytaneBOXPlotBezNAOutliers1000_1"),
      dataTableOutput("outliers_Info"),
      h5("Powtórne usuwanie nie tylko nie usuwa wszystkich ale pojawia się więcej outlierów"),
      plotOutput("daneWczytaneBOXPlotBezNAOutliers1000_2"),
      dataTableOutput("outliers_Info1"),
      br(),
      h4("Podsumowanie rozmiaru zbioru próbki 1000, wykresy"),
      checkboxInput("checkboxPodsumowanieRozmiaruZbioruWykresy1000", "Show|Hide", TRUE),
      div(id="podsumowanieRozmiaruZbioruWykresy1000",
          selectInput("select11", "Select columns to display", c("nr","dl","gpl1g1","gpl1g2","gpl2g1","gpl2g2","gwidlg1","gwidlg2","poz_nar","roczny_nar","lpoz_nar","llzlowryb","tempC", "zasol", "mies", "oscyl","plankt"), multiple = TRUE), 
          tabsetPanel(
            
            tabPanel("BoxPlot", 
                     plotOutput("daneWczytaneBOXPlotBezNAOutliers1000")), 
            tabPanel("BoxPlot GGPlot", 
                     plotOutput("daneWczytane_BOXPlot_BezNAOutliers_GGPlot1000")),
            #tabPanel("Hist GGPlot", 
            #         plotOutput("daneWczytaneHistBezNAOutliersGGPlot1000")),
            tabPanel("Hist GGPlot Normalize", 
                     plotOutput("daneWczytaneHistBezNAOutliersGGPlotNormalize1000"))#,
            #tabPanel("BoxPlot GGPlot Normalize", 
            #         plotOutput("daneWczytaneBOXPlotBezNAOutliersGGPlotNormalize1000"))
          )
      ),
      h4("Liczba unikalnych wartości dla poszczególnych kolumn"),
      tabsetPanel(
        tabPanel("Czyszczenie z NA - podsumowanie", 
                 fluidRow(
                   column(6,
                          dataTableOutput("distinctPoWczytaniuJeszczeZNA")
                   ),
                   
                   # Show a plot of the generated distribution
                   column(6,
                          dataTableOutput("distinctPoCzyszczeniuzNA")
                   )
                 )
        ), 
        tabPanel("Statystyka oczyszczania", 
                 dataTableOutput("dfUniqueTable")
        )
      ),
      br(),
      #Szczegółową analizę wartości atrybutów (np. poprzez prezentację rozkładów wartości).
      h3("Szczegółowa analiza wartości atrybutów"),
      plotOutput("plotAllCorrelatedHist"),
      plotOutput("allTablePlot"),
      
      
      plotOutput("output$boxPlot1All"),
      
      plotOutput("output$boxPlot2Sample"),
      br(),
      #Sekcję sprawdzającą korelacje między zmiennymi; sekcja ta powinna zawierać jakąś formę graficznej prezentacji korelacji.
      h3("Korelacje między atrybutami"),
      tableOutput("corrTable"),
      plotOutput("plotAllCorrelated"),
      plotOutput("plotAllCorrelated1"),
      br(),
      #Interaktywny wykres lub animację prezentującą zmianę rozmiaru śledzi w czasie.
      h3("Interaktywna prezentacja zmiany rozmiary śledzi"),  
      
      br(),
      #Sekcję próbującą stworzyć regresor przewidujący rozmiar śledzia (w tej sekcji należy wykorzystać wiedzę z pozostałych punktów oraz wykonać dodatkowe czynności, które mogą poprawić trafność predykcji); dobór parametrów modelu oraz oszacowanie jego skuteczności powinny zostać wykonane za pomocą techniki podziału zbioru na dane uczące, walidujące i testowe; trafność regresji powinna zostać oszacowana na podstawie miar R2R2 i RMSERMSE.
      h3("Regresor przewidujący rozmiar śledzia"),  
      
      br(),
      #Analizę ważności atrybutów najlepszego znalezionego modelu regresji. Analiza ważności atrybutów powinna stanowić próbę odpowiedzi na pytanie: co sprawia, że rozmiar śledzi zaczął w pewnym momencie maleć.
      h3("Analiza najlepszego modelu regresji"),   
      
      # Application title
      titlePanel("Old Faithful Geyser Data"),
      
      
      
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
      ),
      
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins1",
                      "Number of bins:",
                      min = 1,
                      max = 50000,
                      value = 30)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot1")
        )
        
        
        
      ),
      plotOutput("distPlot2"),
      sidebarLayout(
        sidebarPanel(
          
          checkboxGroupInput("choices1", label = h3("Wybierz Kolumny"), choices = NULL),
          # there is combobox to pick column
          selectInput("combobox", label = h3("(Histogram) Wybierz kolumne"),  choices = NULL)
          
        ),
        
        mainPanel(
          plotOutput("tb")
        )
        
      )
    )
    )
  )
  

)
