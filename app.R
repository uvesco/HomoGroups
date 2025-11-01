# Librerie ----------------------------------------------------------------

library(shiny)
library(readxl)
library(WriteXLS)
library(ggplot2)

# funzioni ----------------------------------------------------------------

arrotperc <- function(a){format(a*100, digits = 2)}

# parametri ---------------------------------------------------------------

# dataframe per il numero di api zav e il passo dello slider per la selezione della soglia
numApiPasso <- structure(c(300, 450, 900, 0.25, 0.2, 0.1), .Dim = 3:2, .Dimnames = list(
  NULL, c("numApi", "passoSlider")))

ui <- shinyUI(navbarPage("Formazione dei gruppi",
                         tabPanel("Dati",
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput('file1', 'Seleziona un file xls',
                                                accept = c(".xls"), buttonLabel = "Sfoglia...", 
                                                placeholder = "Nessun file selezionato"
                                                ),
                                      selectInput('numApi', "Numero di Api", numApiPasso[, 1],
                                                  selected = numApiPasso[1, 1]),
                                      br(),
                                      "E' possibile scaricare un file di prova da utilizzare come template qui: ",
                                      tags$a(href="dati_prova.xls", "dati_prova.xls")
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Tabella", tableOutput('tabAlveari')), 
                                        tabPanel("Sommario",
                                                 h2("Statistiche"),
                                                 strong("Numero di alveari: "), textOutput("numAlveari", inline = T),
                                                 br(),
                                                 strong("Infestazione media: "), textOutput("infMed", inline = T),
                                                 br(),
                                                 strong("Infestazione mediana: "), textOutput("infMedian", inline = T)
                                        ),
                                        tabPanel("Distribuzione",
                                                 plotOutput("grBox"),
                                                 plotOutput("grBar")
                                                 )
                                      )
                                    )
                                  )),
                         tabPanel("Selezione",
                                  sidebarLayout(
                                    sidebarPanel(
                                      checkboxInput("delOutliers", "Escludi gli outliers", F),
                                      br(),
                                      uiOutput("sliderSoglia"),
                                      #sliderInput("sogliaMin", "Soglia minima di infestazione", 0, 2, 0, 0.5),
                                      br(),
                                      uiOutput("manualExclude"),
                                      br(),
                                      "Alveari selezionati:", textOutput("nAlvPo", inline = T)
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Soglia minima",
                                                 plotOutput("selBar")
                                        ),
                                        tabPanel("Outliers",
                                                 strong("Outliers identificati: "), textOutput("idOutliers", inline = T), br(),
                                                 strong("Proporzione (%) degli outliers:"), textOutput("propOutliers", inline = T), br(),
                                                 strong("Infestazione media degli outliers:"), textOutput("medOutliers", inline = T), br(),
                                                 strong("Infestazione media con gli outliers:"), textOutput("medconOutliers", inline = T), br(),
                                                 strong("Infestazione media senza gli outliers:"), textOutput("medsenOutliers", inline = T), br(),
                                                 plotOutput("bplotsOutliers")
                                                 )
                                      )
                                    )
                                  )
                         ),
                         tabPanel("Simulazione",
                                  sidebarLayout(
                                    sidebarPanel(
                                      #"Alveari selezionati:", textOutput("nAlvPo", inline = T),
                                      uiOutput("inputTesi"),
                                      textOutput("checkTesi"),
                                      sliderInput("iterazioni", "Numero di iterazioni", 100, 5000, 500),
                                      numericInput("seme", "Seme del generatore di numeri casuali", 1),
                                      "Cambiare il seme e ricalcolare per variare la distribuzione e minimizzare l'indice di deriva",
                                      br(),
                                      br(),
                                      div("Attenzione: aprire il tab \'Selezione\' prima di calcolare", style = "color: red;"), #2do: spostarlo on error
                                      actionButton("calcButton", "Calcola"),
                                      textOutput("outputCalcButton"),
                                      br(),
                                      textOutput("numSim"),
                                      textOutput("sumDerIndex"),
                                      br(),
                                      br(),
                                      uiOutput("best25Choice"),
                                      br(),
                                      uiOutput("downloadButton")
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Grafici",
                                                 strong("Indice di deriva:"), textOutput("defDerIndex"),
                                                 br(),
                                                 textOutput("DEBUG"),
                                                 plotOutput("defBar"),
                                                 plotOutput("defDen")
                                                 ),
                                        tabPanel("Tabella",
                                                 tableOutput('tabDEF')
                                                 )
                                      )
                                      
          
                                      
                                      #textOutput("TEMP"),

                                    )
                                  )),
                         tabPanel("Help",
                                  fluidRow(
                                    column(12,
                                           h2("Guida all'utilizzo dell'applicazione"),
                                           
                                           h3("1. Funzionamento generale"),
                                           p("L'applicazione divide gli alveari in n gruppi omogenei per livello di infestazione da varroa, 
                                             minimizzando la deriva (passaggio di api e varroa) tra gruppi diversi posizionati su banchette adiacenti."),
                                           p("Il processo si articola in tre fasi: caricamento dati, selezione alveari e simulazione automatica 
                                             per la creazione dei gruppi."),
                                           
                                           h3("2. Tab 'Dati' - Caricamento e visualizzazione"),
                                           h4("File di Input (formato .xls)"),
                                           tags$ul(
                                             tags$li("Gli alveari devono essere disposti nel file nell'ordine della loro posizione fisica"),
                                             tags$li("Colonne richieste: idAlveare, idBanchetta (gruppo di alveari sulla stessa banchetta), 
                                                     idGruppo (fila di banchette), zav (numero assoluto di acari caduti, non percentuale)"),
                                             tags$li("La deriva viene calcolata considerando gli alveari disposti in una o più file e in banchette lineari"),
                                             tags$li("È disponibile un file di prova come template")
                                           ),
                                           h4("Numero di Api"),
                                           p("Selezionare il numero di api campionate durante il metodo dello zucchero a velo o lavaggio alcolico 
                                             (300, 450 o 900). Questo valore è necessario per calcolare la percentuale di infestazione."),
                                           h4("Visualizzazioni"),
                                           tags$ul(
                                             tags$li(strong("Tabella:"), " mostra tutti i dati caricati"),
                                             tags$li(strong("Sommario:"), " statistiche descrittive (numero alveari, media, mediana)"),
                                             tags$li(strong("Distribuzione:"), " boxplot e barplot dell'infestazione, con linea di riferimento al 5%")
                                           ),
                                           
                                           h3("3. Tab 'Selezione' - Filtri e criteri di esclusione"),
                                           p(strong("IMPORTANTE:"), " È necessario visitare questo tab prima di procedere alla simulazione 
                                             per attivare il dataset di lavoro."),
                                           h4("Escludi gli outliers"),
                                           p("Permette di escludere automaticamente gli alveari con valori di infestazione anomali (outliers statistici). 
                                             Il subtab 'Outliers' mostra:"),
                                           tags$ul(
                                             tags$li("Identificazione degli outliers"),
                                             tags$li("Proporzione e media degli outliers"),
                                             tags$li("Confronto tra distribuzioni con e senza outliers"),
                                             tags$li("Visualizzazioni comparative (boxplot e istogrammi)")
                                           ),
                                           h4("Soglia minima di infestazione"),
                                           p("Permette di escludere gli alveari con infestazione inferiore a una soglia (da 0 a 2%, regolabile). 
                                             Il subtab 'Soglia minima' mostra graficamente gli alveari selezionati."),
                                           h4("Escludi manualmente"),
                                           p("Permette di selezionare specifici alveari da escludere dall'analisi."),
                                           p("Il numero di alveari selezionati viene visualizzato nella barra laterale."),
                                           
                                           h3("4. Tab 'Simulazione' - Creazione automatica dei gruppi"),
                                           h4("Logica dell'algoritmo"),
                                           p("L'algoritmo opera in due fasi:"),
                                           tags$ol(
                                             tags$li(strong("Ordinamento:"), " gli alveari vengono ordinati per valore di infestazione. 
                                                     Per alveari con lo stesso valore, la posizione viene assegnata in modo pseudocasuale 
                                                     basato sul seme impostato."),
                                             tags$li(strong("Iterazioni:"), " vengono eseguite diverse iterazioni (100-5000 impostabili) 
                                                     per assegnare casualmente gli alveari di ciascun quantile alle n tesi. 
                                                     Le 25 migliori combinazioni che minimizzano l'affiancamento di alveari di tesi diverse 
                                                     sulla stessa banchetta vengono rese disponibili per la selezione.")
                                           ),
                                           h4("Parametri"),
                                           tags$ul(
                                             tags$li(strong("Numero di tesi:"), " numero di gruppi omogenei da creare (= numero di trattamenti). 
                                                     Il numero di alveari selezionati DEVE essere un multiplo del numero di tesi 
                                                     (es: 30 alveari con 3 gruppi OK, 30 alveari con 4 gruppi NO)."),
                                             tags$li(strong("Numero di iterazioni:"), " numero di permutazioni casuali (100-5000). 
                                                     Più iterazioni aumentano la probabilità di trovare soluzioni ottimali."),
                                             tags$li(strong("Seme del generatore:"), " seme di partenza per la generazione dei numeri casuali. 
                                                     Variare il seme per ottenere distribuzioni diverse e minimizzare l'indice di deriva.")
                                           ),
                                           h4("Indice di deriva"),
                                           p("L'indice di deriva rappresenta la frazione di accostamenti di alveari di tesi diverse 
                                             sulla stessa banchetta rispetto al totale degli accostamenti possibili. 
                                             Un indice più basso indica una migliore separazione fisica tra i gruppi, 
                                             riducendo il rischio di contaminazione tra trattamenti diversi."),
                                           p("Il programma propone le 25 migliori combinazioni per ciascuna distribuzione, 
                                             ordinate per indice di deriva crescente."),
                                           h4("Test del Chi quadrato"),
                                           p("Viene eseguito un test del Chi quadrato di Pearson sui dati raggruppati in 5 classi 
                                             (quintili di infestazione) per verificare l'omogeneità della distribuzione tra i gruppi. 
                                             Un p-value alto (> 0.05) indica che i gruppi hanno distribuzioni simili di infestazione."),
                                           h4("Visualizzazioni"),
                                           tags$ul(
                                             tags$li(strong("Grafici:"), " barplot colorato per tesi e grafico di densità per visualizzare 
                                                     la distribuzione dell'infestazione in ciascun gruppo"),
                                             tags$li(strong("Tabella:"), " mostra i risultati finali con l'assegnazione di ciascun alveare al gruppo (colonna grTesi)")
                                           ),
                                           h4("Download"),
                                           p("Il pulsante Download permette di scaricare un file .xls con la soluzione selezionata, 
                                             inclusa la colonna grTesi che indica l'appartenenza al gruppo."),
                                           
                                           h3("5. Procedura consigliata"),
                                           tags$ol(
                                             tags$li("Caricare il file .xls dei dati nel tab 'Dati' e verificare le statistiche"),
                                             tags$li("Selezionare il numero corretto di api campionate"),
                                             tags$li("Visitare il tab 'Selezione' ed eventualmente escludere outliers o impostare una soglia minima"),
                                             tags$li("Verificare che il numero di alveari selezionati sia compatibile con il numero di tesi desiderato"),
                                             tags$li("Nel tab 'Simulazione', impostare numero di tesi e iterazioni, poi cliccare 'Calcola'"),
                                             tags$li("Provare diversi valori del seme per minimizzare l'indice di deriva"),
                                             tags$li("Selezionare la combinazione migliore tra le 25 proposte"),
                                             tags$li("Verificare i grafici e il test Chi quadrato"),
                                             tags$li("Scaricare il file con i gruppi creati")
                                           ),
                                           
                                           h3("6. Note e limitazioni"),
                                           tags$ul(
                                             tags$li("È necessario visitare il tab 'Selezione' prima di calcolare i gruppi nel tab 'Simulazione'"),
                                             tags$li("L'algoritmo di valutazione della deriva non tiene conto degli spazi vuoti creati dagli alveari esclusi"),
                                             tags$li("La linea rossa di riferimento nei grafici indica il 5% di infestazione, 
                                                     valore critico per valutare la necessità di trattamento")
                                           ),
                                           
                                           br(),
                                           hr(),
                                           h4("Riferimenti"),
                                           p("Per maggiori dettagli metodologici sulla stima dell'infestazione:"),
                                           tags$ul(
                                             tags$li("Lee et al. (2010) American Bee Journal 150:1151-1155"),
                                             tags$li("Lee et al. (2010) Journal of Economic Entomology 103:1039-1050")
                                           )
                                    )
                                  )
                         )
))
  
  
server <- function(input, output) {
  # Importazione dati grezzi --------------------------------
  alveari <- reactive({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xls", sep=""))
    tempalv <- read_excel(paste(inFile$datapath, ".xls", sep=""), 1, col_names = c("idAlveare", "idBanchetta", "idGruppo", "zav"),
               col_types = c("text", "text", "text", "numeric"), skip = 1)
    tempalv$zav <- as.integer(tempalv$zav)
    nas <- which(is.na(tempalv$idBanchetta))
    tempalv$idBanchetta[nas] <- "999"
    nas <- which(is.na(tempalv$idGruppo))
    tempalv$idGruppo[nas] <- "999"
    tempalv
  })
  ### Altre elaborazioni ------------------------------
  # zav per 100 api
  tzav <- reactive({
    req(alveari())
    (alveari()$zav / as.integer(input$numApi))*100
  })
  # logical, is outlier
  isOutlier <- reactive({
    req(alveari())
    alveari()$zav %in% boxplot.stats(alveari()$zav)$out
  })
  # logical, is sotto soglia
  isSottosoglia <- reactive({
    req(tzav(), input$sogliaMin)
    tzav() < input$sogliaMin

  })
  # logical, is manually excluded
  isManExcluded <- reactive({
    req(alveari())
    if(is.null(input$manualExcluded)) {
      return(rep(FALSE, nrow(alveari())))
    }
    alveari()$idAlveare %in% input$manualExcluded
  })
  
  # alveari selezionati in base a parametri di outlier sì/no e slider soglia
 
    alveariPo <- reactive({
    req(alveari())
    if(input$delOutliers){
      alveari()[!isOutlier() & !isSottosoglia() & !isManExcluded(), ]
    }else{
      alveari()[!isSottosoglia() & !isManExcluded(), ]
    }
  })
  # tutti gli alveari, annullati i non selezionati
    alveariAn <- reactive({
      req(alveari())
      alveariAn <- alveari()
      alveariAn$zav[!(alveariAn$idAlveare %in% alveariPo()$idAlveare)] <- NA
      alveariAn
    })
  # tzavAn
    # zav per 100 api
    tzavAn <- reactive({
      req(alveariAn())
      (alveariAn()$zav / as.integer(input$numApi))*100
    })
  
  # numero di alveari in uso è un multiplo del numero di tesi?
    output$checkTesi <- renderText(
      {
      req(alveariPo(), input$tesi)
      if(dim(alveariPo())[1] %% input$tesi == 0){
        paste0("OK")
      }else{
        paste0("ERRORE: il numero di alveari (", dim(alveariPo())[1],
               ") non è multiplo del numero di gruppi (", input$tesi,
               "): aggiungere o togliere alveari.")
        #2do sarebbe bene rendere non cliccabile l'action button
        }
      }
    )
  # simulazione delle combinazioni
    
# TAB Dati ----------------------------------------------------------------

      # tabella generale da visualizzare
  output$tabAlveari <- renderTable({
    req(alveari())
    alveari()
  })
  # numero di alveari
  output$numAlveari <- renderText({
    req(alveari())
    dim(alveari())[1]
  })
  # infestazione media e range
  output$infMed <- renderText({
    req(alveari())
    media <- arrotperc(mean(alveari()$zav, na.rm = T) / as.integer(input$numApi))
    minima <- arrotperc(min(alveari()$zav, na.rm = T) / as.integer(input$numApi))
    massima <- arrotperc(max(alveari()$zav, na.rm = T) / as.integer(input$numApi))
    paste0(media, "% (range: ", minima, "-", massima, "%)")
    })
  # mediana
  output$infMedian <- renderText({
    req(tzav())
    boxplot.stats(tzav())$stats[3]
  })

  # boxplot dati grezzi
  output$grBox <- renderPlot({
    #tzav <- (alveari()$zav / as.integer(input$numApi))*100
    req(tzav())
    boxplot(tzav(), main = "Infestazione complessiva", ylab = "acari / 100 api"); abline(h=5, col = "red", lty=2)
  })
  # barplot dati grezzi
  distpreci <- reactive({ # distanze richiamate in tutti i barplot successivi
  req(alveari())
  distprec <- 0.15
  for(i in 2:dim(alveari())[1]){
    distprec[i] <- (alveari()$idBanchetta[i-1] != alveari()$idBanchetta[i]) + 
      (alveari()$idGruppo[i-1] != alveari()$idGruppo[i])*2 + 0.15
  }
  distprec
  })
  output$grBar <- renderPlot({
    req(tzav(), distpreci())
    barplot(tzav(), space = distpreci(), col = as.factor(alveari()$idGruppo),
            main = "Infestazione\n(raggruppati e colorati per gruppo\ne raggruppati per banchetta)", ylab = "acari / 100 api",
            names.arg = alveari()$idAlveare, cex.names = 0.9, las = 2); abline(h=5, col = "red", lty=2)
    # 2DO: aggiungere boxplot per gruppo, con analisi e density per banchetta
    })

# TAB Selezione -----------------------------------------------------------
  # Slider della soglia con passo dinamico a seconda del numero di api
  output$sliderSoglia <- renderUI({
    sliderInput("sogliaMin", "Soglia minima di infestazione", 0, 2, 0, numApiPasso[numApiPasso[, 1] == as.integer(input$numApi), 2], round = 1, animate = T, ticks=T)
    #sliderInput("slider", "Slider", min = 0,
    #            max = input$num, value = 0)
  })
  # Eliminazione manuale di alveari
  output$manualExclude  <- renderUI({
    req(alveari())
    selectInput("manualExcluded", "Escludi manualmente:",
                choices = alveari()$idAlveare, multiple = T)
  })
  
  ## Subtab Outliers
  # outliers identificati
  output$idOutliers <- renderText({
    req(alveari(), isOutlier())
    paste(alveari()$idAlveare[isOutlier()], sep = ", ")
    #alveari()[which(alveari()$zav %in% boxplot.stats(alveari()$zav)$out), 1]
  })
  # proporzione (%) degli outliers
  output$propOutliers <- renderText({
    req(isOutlier())
    paste0(format(100*sum(isOutlier())/length(isOutlier()),digits = 2), "%")
  })
  # media degli outliers
  output$medOutliers <- renderText({
    req(tzav(), isOutlier())
    paste0(format(mean(tzav()[isOutlier()]),digits = 2), "%")
  })
  # media con gli outliers
  output$medconOutliers <- renderText({
    req(tzav())
    paste0(format(mean(tzav()),digits = 2), "%")
  })
  # media senza gli outliers
  output$medsenOutliers <- renderText({
    req(tzav(), isOutlier())
    paste0(format(mean(tzav()[!isOutlier()]),digits = 2), "%")
  })
  # boxplot outliers tipo 
  # https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
  output$bplotsOutliers <- renderPlot({
    req(tzav(), isOutlier())
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(tzav(), main="Con outliers"); abline(h=5, col = "red", lty=2)
    hist(tzav(), main="Con outliers", xlab=NA, ylab=NA); abline(v=5, col = "red", lty=2)
    boxplot(tzav()[!isOutlier()], main="Senza outliers"); abline(h=5, col = "red", lty=2)
    hist(tzav()[!isOutlier()], main="Senza outliers", xlab=NA, ylab=NA); abline(v=5, col = "red", lty=2)
    par(mfrow=c(1, 1), oma=c(0,0,0,0))
  })
  ## SUBTAB Soglia minima
  # barplot selezione
  output$selBar <- renderPlot({
    req(alveariAn())
    distprec <- 0.15
    for(i in 2:dim(alveariAn())[1]){
      distprec[i] <- (alveariAn()$idBanchetta[i-1] != alveariAn()$idBanchetta[i]) + 
        (alveariAn()$idGruppo[i-1] != alveariAn()$idGruppo[i])*2 + 0.15
    }
    req(tzavAn(), tzav(), input$sogliaMin)
    barplot(tzavAn(), space = distprec, col = "khaki1",
          main = "Infestazione\n(alveari selezionati, raggruppati per banchetta)", 
          ylab = "acari / 100 api",           names.arg = alveariAn()$idAlveare, cex.names = 0.9, 
          las = 2, ylim = c(0, max(tzav()))); abline(h=5, col = "red", lty=2); abline(h = input$sogliaMin)
  })
  
  
    #conteggio alveari post selezione
    output$nAlvPo <- renderText({
    req(alveariPo())
    paste(dim(alveariPo())[1])
    #print(input$delOutliers)
  })

# TAB Analisi -------------------------------------------------------------
    #2do: per evitare tutti gli errori iniziali si potrebbe forzare il calcolo nella pagina precedente richiedendo per esempio
    #un dato dipendente dagli altri
    output$inputTesi <- renderUI({
      req(alveariPo())
      numericInput("tesi", "Numero di tesi", 2, min = 2, max = dim(alveariPo())[1])
    })
    
    # numero di clicks sul bottone
    output$outputCalcButton <- renderText({paste0("Calcolo eseguito ", input$calcButton, " volte.")})
    
    # combinazioni di migliori gruppi ( #2do si potrebbe anche allargare a quantili più ampi ma diventa
    # più complesssa la selezione, in compenso si potrebbe abbattere ancora la deriva)
    combinazioni <- eventReactive({
      #alveariPo()
      input$calcButton
      input$seme
      }, {
      req(alveariPo(), input$tesi)
      t(unique(sapply(1 : input$iterazioni, function(b){
        set.seed(input$seme + b)
        ranran <- rank(alveariPo()$zav, ties.method="random", na.last=F) #rango casuale
        # (eseguo per ciascuno degli alveari di ogni tesi (1:numht) l'estrazione casuale)[la riordino per rango casuale]
        unlist(lapply(1 : (dim(alveariPo())[1] / input$tesi), 
                      function(a){set.seed(input$seme + a)
                        sample(1:input$tesi, input$tesi, replace = F)}
                      ))[ranran]}), 
        MARGIN = 2))
    })
    # forza l'esecuzione dei calcoli del tab selezione anche non aperto permettendo l'esecuzione del precendente
    # outputOptions(output, "selBar", suspendWhenHidden = FALSE) # non funziona
    #2do known issue: la simulazione non funziona se prima non è stata visitata la pagina di selezione
    
    
    # numero di ipotesi uniche di gruppi
    output$numSim <- renderText({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(combinazioni())
      paste0("Numero di ipotesi uniche di gruppi: ", dim(combinazioni())[1])
      #paste0(combinazioni()[, 1])
      }
    })
    
    # calcolo della deriva
    # vicinanza su stessa banchetta: massima deriva teorica
    pos <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
        req(alveariPo())
        alveariPo()$idBanchetta[-1] == alveariPo()$idBanchetta[-length(alveariPo()$idBanchetta)]
      }
    })
    
    deriva <- function(pos, tesi){
      sum(pos & (tesi[-1] != tesi[-length(tesi)]))
    }
    
    derIndex <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(combinazioni(), pos())
      validate(need(sum(pos()) > 0, "Impossibile calcolare la deriva: nessuna banchetta consecutiva disponibile."))
      apply(combinazioni(), 1, function(a){deriva(pos(), a)}) / sum(pos())
      }
      })
    best25 <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
        req(combinazioni(), derIndex())
        head(combinazioni()[order(derIndex()), ], n = 25)
      }
    })  #traspone e prende i primi 25
    
    output$best25Choice <- renderUI({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
        req(best25())
        selectInput(
          "bestChosen",
          "Altre combinazioni con stessa distribuzione (deriva crescente)",
          choices = (1:(dim(best25())[1])),
          1
        )
      }
        })

    output$TEMP <- #renderText({input$bestChosen})
      renderText({
        if (input$calcButton == 0) {
          return(NULL)
        }else{
        req(best25())
        paste0(best25()[as.integer(input$bestChosen),])
        }
        })
    
    alveariAnDEF <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(alveariAn(), alveariPo(), best25(), tzav(), tzavAn())
      alveariPo2 <- alveariPo()
      alveariPo2$grTesi <- best25()[as.integer(input$bestChosen),]
      temp <- merge(alveariAn(), alveariPo2[, c(1, 5)], by = "idAlveare", all.x = T, sort = F)
      #temp$inf <- temp$zav / as.integer(input$numApi) *100 # infestazione percentuale
      temp <- temp[order(as.integer(temp$idAlveare)), ]
      temp <- cbind(temp, tzav(), tzavAn())
      temp
      }
    })
    
    # grafico a barre simulazione automatica
    output$defBar <- renderPlot({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      #par(mfrow=c(2,1))
      req(tzavAn(), distpreci(), alveariAnDEF(), tzav())
      barplot(tzavAn(), space = distpreci(), col = 1 + as.integer(alveariAnDEF()$grTesi),
              main = "Infestazione\n(colorati per tesi\ne raggruppati per banchetta)",
              ylab = "acari / 100 api",           names.arg = alveariAnDEF()$idAlveare, cex.names = 0.9,
              las = 2, ylim = c(0, max(tzav()))); abline(h=5, col = "red", lty=2)
      
      #boxplot(tzavAn()~alveariAnDEF()$grTesi, col = 1 + as.integer(levels(as.factor(alveariAnDEF()$grTesi))), 
      #                 ylab = "acari / 100 api", ylim = c(0, max(tzav()))); abline(h=5, col = "red", lty=2)
      #par(mfrow=c(1,1))
      }
    })
    
    # grafico densità simulazione automatica
    #2do non si aggiorna il grafico  della densità all'aggiornamento della ipotesi (idem boxplot)
    # potrebbe essere dovuto al sistema di generazione dei dati, che sposta solo tra uguali a parità di seme?
    # nel qual caso necessario cambiare le modalità di generazione (es. produrre i migliori 25 di 25 diversi semi)
    #alveariAnDEF2 <- eventReactive(input$bestChosen, {alveariAnDEF()})
    
    output$defDen <- renderPlot({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(alveariAnDEF())
      #boxplot(alveariAnDEF()$zav~alveariAnDEF()$grTesi)
      
      # boxplot(tzavAn()~alveariAnDEF()$grTesi, col = 1 + as.integer(levels(as.factor(alveariAnDEF()$grTesi))), 
      #         ylab = "acari / 100 api", ylim = c(0, max(tzav()))); abline(h=5, col = "red", lty=2)
      ggplot(alveariAnDEF(), aes_string(alveariAnDEF()$tzavAn,group=as.factor(alveariAnDEF()$grTesi),color=as.factor(alveariAnDEF()$grTesi)), environment=environment()) + geom_density()
      }
      })
    
    
    
    # indice di deriva della prova in analisi
    output$defDerIndex <- renderText({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(derIndex(), input$bestChosen)
      paste0(derIndex()[order(derIndex())][as.integer(input$bestChosen)])
      }
      })
      
    # sommario degli indici di deriva
    output$sumDerIndex <- renderText({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(derIndex())
      paste0("Indice di deriva minimo: ", min(derIndex()))
      }
      })
    

# Bins --------------------------------------------------------------------

    
    # analisi bins
    #https://heuristically.wordpress.com/2012/06/13/comparing-continuous-distributions-with-r/
    tabBinDef <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(alveariAnDEF(), combinazioni(), input$bestChosen)
      validate(need(sum(!is.na(alveariAnDEF()$tzavAn)) >= 2, "Dati insufficienti per il test sui quantili."))
      validate(need(length(unique(na.omit(alveariAnDEF()$tzavAn))) > 1,
                    "Valori di infestazione insufficienti per creare classi distinte."))
      (q<-quantile(as.numeric(alveariAnDEF()$tzavAn) , seq(0, 1, .2), na.rm = T))
      bin <- cut(alveariAnDEF()$tzavAn, breaks=q, include.lowest=T)
      tab <- with(alveariAnDEF(), table(bin, as.factor(combinazioni()[as.integer(input$bestChosen),])))
      chisq.test(tab)
      }
    })
    
    output$DEBUG <- renderText({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
        req(tabBinDef())
        paste0("Test del Chi quadrato di Pearson sui dati raggruppati in 5 classi. \n",
               "Chi quadrato = ", format(tabBinDef()$statistic, digits = 4), " p = ",
               format(tabBinDef()$p.value, digits = 3)
        )
      }

      #print(chisq.test(tabBinDef()))
      #prop.table(tabBinDef(), 2)
    })

    
# tabella finale ----------------------------------------------------------

    
    
    # tabella finale
    output$tabDEF <- renderTable({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(alveariAnDEF())
      alveariAnDEF()[,1:5]
      }
    })
    
    
    # download button
    output$downloadData <- downloadHandler(
      
      
      filename = function() {
        paste("gruppi_auto.xls", sep = "")
      },
      content = function(file) {
        req(alveariAnDEF())
        WriteXLS(alveariAnDEF()[,1:5], file, row.names = FALSE)}

      
    )
    
    
    
    output$downloadButton <- renderUI({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      req(alveariAnDEF())
      tagList(
        p("Download della soluzione attuale (gruppi creati nella colonna grTesi)"),
        downloadButton("downloadData", "Download")
      )
      }
    })
    

# Manuale -----------------------------------------------------------------

  alveariPoM <- reactive({ # farlo aggiornare solo su pressione di un bottone???
    req(alveariAnDEF())
    alveariAnDEF()[!is.na(alveariAnDEF()$grTesi), ]
  })

  output$radioGrid <- renderUI({
    req(alveariPoM())
# a=1
#   radioButtons(alveariPoM()$idAlveare[a], alveariPoM()$idAlveare[a], 1 : as.integer(input$tesi), selected = alveariPoM()$grTesi[a], inline = T)
  radioOutputList <- tagList()
      for(a in 1:length(alveariPoM()$zav)){
        radioOutputList[[a]] <- radioButtons(alveariPoM()$idAlveare[a], 
                                   # label = NULL,
                                   label = as.character(alveariPoM()$idAlveare[a]), 
                                   1 : as.integer(input$tesi), 
                                   selected = alveariPoM()$grTesi[a],
                                   inline = T)
      }
    return(radioOutputList)
  #as.character(do.call(tagList, unlist(radioOutputList, recursive = F)))
  
})
   


    
    
}


shinyApp(ui = ui, server = server)

# 2do
# traduzione multilingue: https://www.r-bloggers.com/another-take-on-building-a-multi-lingual-shiny-app/
# prevedere altri casi d'uso: 
# - spostamenti per cui ce ne si frega della deriva
# - valutazione di gruppi già caricati con il file xls
# l'algoritmo di valutazione della deriva non tiene conto dei buchi creati dagli alveari scartati! Se il buco è ampio conta meno!
# esportare i risultati
# nella pagina finale spostare tutto fino al selettore a sinistra
# aggiungere zav completo e mettere colore bianco ai non utilizzati (sarebbe neglio rivedere la strategia del tzav)
# valutare gruppi fatti per banchetta / per alveare manualmente?
# aggiungere testo per errore se si salta subito ad auto senza passare dalla selezione
