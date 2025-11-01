# Librerie ----------------------------------------------------------------

library(shiny)
library(readxl)
library(writexl)
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
                                      tags$a(href="http://uvesco.altervista.org/unaapi/dati_prova.xls", "dati_prova.xls")
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
                         tabPanel("Auto",
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
                                  ))#,
                         # tabPanel("Manuale",
                         #          sidebarLayout(
                         #            sidebarPanel(
                         #              uiOutput("radioGrid")
                         # 
                         #            ),
                         #            mainPanel(
                         #              tabsetPanel(
                         #                tabPanel("Grafici"
                         # 
                         #                         
                         #                ),
                         #                tabPanel("Tabella"
                         # 
                         #                         
                         #                )
                         #              )
                         #              
                         #              
                         #              
                         #              #textOutput("TEMP"),
                         #              
                         #            )
                         #          ))
                         
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
  tzav <- reactive({(alveari()$zav / as.integer(input$numApi))*100})
  # logical, is outlier
  isOutlier <- reactive({
    alveari()$zav %in% boxplot.stats(alveari()$zav)$out
  })
  # logical, is sotto soglia
  isSottosoglia <- reactive({
    tzav() < input$sogliaMin
    
  })
  # logical, is manually excluded
  isManExcluded <- reactive({
    alveari()$idAlveare %in% input$manualExcluded
  })
  
  # alveari selezionati in base a parametri di outlier sì/no e slider soglia
 
    alveariPo <- reactive({
    if(input$delOutliers){
      alveari()[!isOutlier() & !isSottosoglia() & !isManExcluded(), ]
    }else{alveari()[!isSottosoglia(), ]}
  })
  # tutti gli alveari, annullati i non selezionati
    alveariAn <- reactive({
      alveariAn <- alveari()
      alveariAn$zav[!(alveariAn$idAlveare %in% alveariPo()$idAlveare)] <- NA
      alveariAn
    })
  # tzavAn
    # zav per 100 api
    tzavAn <- reactive({(alveariAn()$zav / as.integer(input$numApi))*100})
  
  # numero di alveari in uso è un multiplo del numero di tesi?
    output$checkTesi <- renderText(
      if(dim(alveariPo())[1] %% input$tesi == 0){
        paste0("OK")
      }else{
        paste0("ERRORE: il numero di alveari (", dim(alveariPo())[1],
               ") non è multiplo del numero di gruppi (", input$tesi, 
               "): aggiungere o togliere alveari.")
        #2do sarebbe bene rendere non cliccabile l'action button
        }
    )
  # simulazione delle combinazioni
    
# TAB Dati ----------------------------------------------------------------

      # tabella generale da visualizzare
  output$tabAlveari <- renderTable({alveari() })
  # numero di alveari
  output$numAlveari <- renderText({
    dim(alveari())[1]
  })
  # infestazione media e range
  output$infMed <- renderText({
    media <- arrotperc(mean(alveari()$zav, na.rm = T) / as.integer(input$numApi))
    minima <- arrotperc(min(alveari()$zav, na.rm = T) / as.integer(input$numApi))
    massima <- arrotperc(max(alveari()$zav, na.rm = T) / as.integer(input$numApi))
    paste0(media, "% (range: ", minima, "-", massima, "%)")
    })
  # mediana
  output$infMedian <- renderText({
    boxplot.stats(tzav())$stats[3]
  })

  # boxplot dati grezzi
  output$grBox <- renderPlot({
    #tzav <- (alveari()$zav / as.integer(input$numApi))*100
    boxplot(tzav(), main = "Infestazione complessiva", ylab = "acari / 100 api"); abline(h=5, col = "red", lty=2)
  })
  # barplot dati grezzi
  distpreci <- reactive({ # distanze richiamate in tutti i barplot successivi
  distprec <- 0.15 
  for(i in 2:dim(alveari())[1]){
    distprec[i] <- (alveari()$idBanchetta[i-1] != alveari()$idBanchetta[i]) + 
      (alveari()$idGruppo[i-1] != alveari()$idGruppo[i])*2 + 0.15
  }
  distprec
  })
  output$grBar <- renderPlot({
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
    selectInput("manualExcluded", "Escludi manualmente:",
                choices = alveari()$idAlveare, multiple = T)
  })
  
  ## Subtab Outliers
  # outliers identificati
  output$idOutliers <- renderText({
    paste(alveari()$idAlveare[isOutlier()], sep = ", ")
    #alveari()[which(alveari()$zav %in% boxplot.stats(alveari()$zav)$out), 1]
  })
  # proporzione (%) degli outliers
  output$propOutliers <- renderText({
    paste0(format(100*sum(isOutlier())/length(isOutlier()),digits = 2), "%")
  })
  # media degli outliers
  output$medOutliers <- renderText({
    paste0(format(mean(tzav()[isOutlier()]),digits = 2), "%")
  })
  # media con gli outliers
  output$medconOutliers <- renderText({
    paste0(format(mean(tzav()),digits = 2), "%")
  })
  # media senza gli outliers
  output$medsenOutliers <- renderText({
    paste0(format(mean(tzav()[!isOutlier()]),digits = 2), "%")
  })
  # boxplot outliers tipo 
  # https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
  output$bplotsOutliers <- renderPlot({
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
    distprec <- 0.15
    for(i in 2:dim(alveari())[1]){
      distprec[i] <- (alveari()$idBanchetta[i-1] != alveari()$idBanchetta[i]) + 
        (alveari()$idGruppo[i-1] != alveari()$idGruppo[i])*2 + 0.15
    }
    barplot(tzavAn(), space = distprec, col = "khaki1", 
          main = "Infestazione\n(alveari selezionati, raggruppati per banchetta)", 
          ylab = "acari / 100 api",           names.arg = alveari()$idAlveare, cex.names = 0.9, 
          las = 2, ylim = c(0, max(tzav()))); abline(h=5, col = "red", lty=2); abline(h = input$sogliaMin)
  })
  
  
    #conteggio alveari post selezione
    output$nAlvPo <- renderText({
    paste(dim(alveariPo())[1])
    #print(input$delOutliers)
  })

# TAB Analisi -------------------------------------------------------------
    #2do: per evitare tutti gli errori iniziali si potrebbe forzare il calcolo nella pagina precedente richiedendo per esempio
    #un dato dipendente dagli altri
    output$inputTesi <- renderUI({
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
      alveariPo()$idBanchetta[-1] == alveariPo()$idBanchetta[-length(alveariPo()$idBanchetta)]}}) 
    
    deriva <- function(pos, tesi){
      sum(pos & (tesi[-1] != tesi[-length(tesi)]))
    }
    
    derIndex <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      apply(combinazioni(), 1, function(a){deriva(pos(), a)}) / sum(pos())
      }
      })
    best25 <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      head(combinazioni()[order(derIndex()), ], n = 25)
        }
      })  #traspone e prende i primi 25
    
    output$best25Choice <- renderUI({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      selectInput("bestChosen", "Altre combinazioni con stessa distribuzione (deriva crescente)", choices = (1 : (dim(best25())[1])), 1)
      }
        })
    
    output$TEMP <- #renderText({input$bestChosen})
      renderText({
        if (input$calcButton == 0) {
          return(NULL)
        }else{
        paste0(best25()[as.integer(input$bestChosen),])
        }
        })
    
    alveariAnDEF <- reactive({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
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
      paste0(derIndex()[order(derIndex())][as.integer(input$bestChosen)])
      }
      })
      
    # sommario degli indici di deriva
    output$sumDerIndex <- renderText({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
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
      alveariAnDEF()[,1:5]
      }
    })
    
    
    # download button
    output$downloadData <- downloadHandler(
      
      
      filename = function() {
        paste("gruppi_auto.xls", sep = "")
      },
      content = function(file) {
        write_xlsx(alveariAnDEF()[,1:5], file, col_names = TRUE)}

      
    )
    
    
    
    output$downloadButton <- renderUI({
      if (input$calcButton == 0) {
        return(NULL)
      }else{
      paste0("Download della soluzione attuale (gruppi creati nella colonna grTesi)")
      downloadButton("downloadData", "Download")
      }
    })
    

# Manuale -----------------------------------------------------------------

alveariPoM <- reactive({ # farlo aggiornare solo su pressione di un bottone???
  alveariAnDEF()[!is.na(alveariAnDEF()$grTesi), ]
})

output$radioGrid <- renderUI({
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
