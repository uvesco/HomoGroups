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
                                           
                                           h3("Introduzione"),
                                           p(strong("Varroa destructor"), " è un acaro parassita delle api mellifere che rappresenta una delle principali minacce 
                                             per la salute degli alveari. L'infestazione da varroa causa l'indebolimento delle colonie e può portare 
                                             al collasso dell'alveare se non controllata adeguatamente."),
                                           p("Questa applicazione è stata progettata per assistere ricercatori e apicoltori nella creazione di gruppi sperimentali 
                                             omogenei per testare l'efficacia di diversi trattamenti acaricidi, garantendo la validità statistica dei risultati."),
                                           
                                           h3("1. Funzionamento generale"),
                                           p("L'applicazione divide gli alveari in n gruppi omogenei per livello di infestazione da varroa, 
                                             minimizzando la deriva (passaggio di api e varroa) tra gruppi diversi posizionati su banchette adiacenti."),
                                           p(strong("La deriva"), " è il fenomeno per cui le api bottinatrici possono entrare in alveari diversi dal proprio, 
                                             soprattutto quando gli alveari sono disposti in fila sulla stessa banchetta. Questo può causare il trasferimento 
                                             di acari varroa tra alveari di gruppi sperimentali diversi, contaminando i risultati della sperimentazione."),
                                           p("Il processo si articola in tre fasi: caricamento dati, selezione alveari e simulazione automatica 
                                             per la creazione dei gruppi."),
                                           p(strong("Obiettivo:"), " creare gruppi con distribuzioni di infestazione statisticamente simili, 
                                             minimizzando al contempo il rischio di contaminazione da deriva tra gruppi adiacenti."),
                                           
                                           h3("2. Tab 'Dati' - Caricamento e visualizzazione"),
                                           
                                           h4("Stima dell'infestazione: metodi di campionamento"),
                                           p("L'infestazione viene stimata sulle api adulte utilizzando uno dei seguenti metodi standardizzati:"),
                                           tags$ul(
                                             tags$li(strong("Metodo dello zucchero a velo (sugar shaking):"), " le api vengono cosparse di zucchero a velo 
                                                     che fa cadere gli acari. Metodo non letale che preserva le api."),
                                             tags$li(strong("Lavaggio alcolico (alcohol washing):"), " le api vengono immerse in alcool per staccare gli acari. 
                                                     Metodo più accurato ma letale per le api campionate.")
                                           ),
                                           p("Il numero standard di api da campionare è di circa 300 api (mezzo bicchiere), ma è possibile utilizzare 
                                             campioni di 450 o 900 api per aumentare la precisione della stima."),
                                           
                                           h4("File di Input (formato .xls)"),
                                           p(strong("Struttura del file:")),
                                           tags$ul(
                                             tags$li(strong("idAlveare:"), " identificativo univoco di ogni alveare (es: A1, A2, B1, B2, ...)"),
                                             tags$li(strong("idBanchetta:"), " identificativo del gruppo di alveari sulla stessa banchetta fisica. 
                                                     Gli alveari sulla stessa banchetta hanno maggiore probabilità di deriva tra loro."),
                                             tags$li(strong("idGruppo:"), " identificativo della fila di banchette. Permette di gestire apiarie con più file parallele."),
                                             tags$li(strong("zav:"), " numero ASSOLUTO di acari caduti durante il campionamento (NON la percentuale). 
                                                     Ad esempio, se su 300 api sono stati trovati 15 acari, inserire 15.")
                                           ),
                                           p(strong("IMPORTANTE:"), " Gli alveari devono essere inseriti nel file seguendo l'ordine fisico di disposizione 
                                             nell'apiario (da sinistra a destra, fila per fila). Questo è essenziale per il calcolo corretto della deriva."),
                                           p(strong("Esempio pratico:")),
                                           tags$pre("idAlveare,idBanchetta,idGruppo,zav\nA1,1,1,12\nA2,1,1,15\nA3,2,1,8\nB1,3,2,20\nB2,3,2,18"),
                                           tags$ul(
                                             tags$li("La deriva viene calcolata considerando gli alveari disposti in banchette lineari"),
                                             tags$li("È disponibile un file di prova (dati_prova.xls) come template da scaricare e modificare")
                                           ),
                                           
                                           h4("Numero di Api"),
                                           p("Selezionare il numero di api campionate durante il metodo di campionamento (300, 450 o 900). 
                                             Questo valore è necessario per calcolare la percentuale di infestazione, che viene espressa come 
                                             'numero di acari per 100 api'."),
                                           p(strong("Formula:"), " % infestazione = (acari trovati / api campionate) × 100"),
                                           p(strong("Esempio:"), " 15 acari su 300 api = (15/300) × 100 = 5% di infestazione"),
                                           
                                           h4("Visualizzazioni"),
                                           tags$ul(
                                             tags$li(strong("Tabella:"), " mostra tutti i dati caricati con le informazioni di posizione e infestazione di ogni alveare"),
                                             tags$li(strong("Sommario:"), " statistiche descrittive aggregate (numero totale alveari, infestazione media, mediana e range). 
                                                     La mediana è particolarmente utile per identificare il valore centrale non influenzato da outliers."),
                                             tags$li(strong("Distribuzione:"), " boxplot (diagramma a scatola) e barplot dell'infestazione. 
                                                     La linea rossa orizzontale indica il 5% di infestazione, considerato il livello critico 
                                                     sopra il quale è necessario intervenire con un trattamento. Nel barplot, gli alveari sono 
                                                     raggruppati per banchetta e colorati per gruppo originale.")
                                           ),
                                           
                                           h3("3. Tab 'Selezione' - Filtri e criteri di esclusione"),
                                           p(strong("IMPORTANTE:"), " È necessario visitare questo tab prima di procedere alla simulazione 
                                             per attivare il dataset di lavoro. Anche se non si applicano filtri, questo passaggio è obbligatorio."),
                                           
                                           h4("Perché escludere alcuni alveari?"),
                                           p("In alcuni casi può essere necessario escludere alveari dalla sperimentazione per motivi scientifici:"),
                                           tags$ul(
                                             tags$li("Alveari con infestazione troppo bassa potrebbero non beneficiare del trattamento"),
                                             tags$li("Alveari con infestazione anomala (outliers) potrebbero distorcere i risultati"),
                                             tags$li("Alveari con problemi sanitari indipendenti da varroa (malattie, regina scarsa, ecc.)"),
                                             tags$li("Necessità di avere un numero di alveari multiplo del numero di gruppi sperimentali")
                                           ),
                                           
                                           h4("Escludi gli outliers"),
                                           p("Gli outliers sono valori statisticamente anomali, molto diversi dalla maggior parte dei dati. 
                                             L'applicazione utilizza il metodo dei quantili (IQR - Interquartile Range) per identificarli automaticamente."),
                                           p(strong("Criterio di identificazione:"), " un valore è considerato outlier se si trova oltre 1.5 volte 
                                             l'intervallo interquartile (IQR) dal primo o terzo quartile."),
                                           p("Il subtab 'Outliers' fornisce un'analisi dettagliata:"),
                                           tags$ul(
                                             tags$li(strong("Identificazione:"), " lista degli ID degli alveari identificati come outliers"),
                                             tags$li(strong("Proporzione:"), " percentuale di outliers sul totale (es: 2 outliers su 30 alveari = 6.7%)"),
                                             tags$li(strong("Media degli outliers:"), " infestazione media degli alveari anomali"),
                                             tags$li(strong("Confronto:"), " infestazione media con e senza outliers per valutare il loro impatto"),
                                             tags$li(strong("Visualizzazioni comparative:"), " boxplot e istogrammi che mostrano la distribuzione 
                                                     prima e dopo la rimozione degli outliers, aiutando a decidere se escluderli")
                                           ),
                                           p(strong("Quando escludere gli outliers?"), " Se la loro presenza altera significativamente la media 
                                             e potrebbero compromettere l'omogeneità dei gruppi o mascherare l'effetto del trattamento."),
                                           
                                           h4("Soglia minima di infestazione"),
                                           p("Permette di escludere gli alveari con infestazione inferiore a una soglia personalizzata (da 0 a 2%, regolabile con incrementi variabili 
                                             a seconda del numero di api campionate)."),
                                           p(strong("Esempio pratico:"), " Se si imposta una soglia di 1%, tutti gli alveari con meno dell'1% di infestazione 
                                             (meno di 3 acari su 300 api) verranno esclusi dalla sperimentazione."),
                                           p(strong("Razionale:"), " Gli alveari con infestazione molto bassa potrebbero non mostrare miglioramenti 
                                             apprezzabili dopo il trattamento, rendendo difficile valutare l'efficacia del prodotto testato."),
                                           p("Il subtab 'Soglia minima' mostra un barplot dove gli alveari esclusi appaiono come spazi vuoti, 
                                             permettendo di visualizzare immediatamente l'effetto della soglia impostata e la distribuzione 
                                             spaziale degli alveari selezionati."),
                                           
                                           h4("Escludi manualmente"),
                                           p("Permette di selezionare specifici alveari da escludere dall'analisi per motivi particolari 
                                             (es: problemi sanitari, regina scarsa, posizione isolata, ecc.)."),
                                           p("Selezionare uno o più ID dalla lista a discesa. Gli alveari selezionati saranno esclusi 
                                             in aggiunta a eventuali esclusioni automatiche (outliers o soglia)."),
                                           p(strong("Numero di alveari selezionati:"), " viene visualizzato in tempo reale nella barra laterale. 
                                             Questo numero deve essere un multiplo del numero di gruppi che si intende creare nella fase di simulazione."),
                                           
                                           h3("4. Tab 'Simulazione' - Creazione automatica dei gruppi"),
                                           
                                           h4("Obiettivo della simulazione"),
                                           p("L'obiettivo è creare gruppi sperimentali che siano:"),
                                           tags$ol(
                                             tags$li(strong("Omogenei:"), " ogni gruppo deve avere una distribuzione simile di livelli di infestazione"),
                                             tags$li(strong("Bilanciati:"), " stesso numero di alveari per gruppo"),
                                             tags$li(strong("Spazialmente separati:"), " minimizzare il numero di alveari di gruppi diversi 
                                                     posizionati uno accanto all'altro sulla stessa banchetta (per ridurre la deriva)")
                                           ),
                                           
                                           h4("Logica dell'algoritmo"),
                                           p("L'algoritmo opera in due fasi successive:"),
                                           tags$ol(
                                             tags$li(strong("Fase 1 - Ordinamento e stratificazione:"), " gli alveari vengono ordinati per valore di infestazione 
                                                     crescente, creando una graduatoria. Per alveari con lo stesso valore di infestazione, la posizione 
                                                     viene assegnata in modo pseudocasuale basandosi sul seme impostato dall'utente. Gli alveari vengono 
                                                     poi divisi in 'strati' (quantili), ciascuno contenente un numero di alveari pari al numero di gruppi 
                                                     da creare."),
                                             tags$li(strong("Fase 2 - Assegnazione iterativa ai gruppi:"), " vengono eseguite molte iterazioni (100-5000 impostabili). 
                                                     In ogni iterazione, gli alveari di ciascuno strato vengono assegnati casualmente ai gruppi, 
                                                     garantendo che ogni gruppo riceva un alveare da ogni strato. Questo assicura che tutti i gruppi 
                                                     abbiano una distribuzione simile di livelli di infestazione (basso, medio, alto). Delle migliaia 
                                                     di combinazioni generate, vengono selezionate le 25 migliori in base all'indice di deriva.")
                                           ),
                                           p(strong("Esempio pratico con 30 alveari e 3 gruppi:")),
                                           tags$ul(
                                             tags$li("Gli alveari vengono ordinati per infestazione: A1(2%), A2(3%), A3(4%), ... , A30(25%)"),
                                             tags$li("Vengono creati 10 strati, ciascuno con 3 alveari (30/3 = 10 strati)"),
                                             tags$li("Strato 1 (infestazione bassa): A1, A2, A3 → assegnati casualmente ai gruppi 1, 2, 3"),
                                             tags$li("Strato 2 (infestazione medio-bassa): A4, A5, A6 → assegnati casualmente ai gruppi"),
                                             tags$li("... e così via fino allo Strato 10 (infestazione alta): A28, A29, A30"),
                                             tags$li("Risultato: ogni gruppo ha alveari con bassa, media e alta infestazione")
                                           ),
                                           
                                           h4("Parametri"),
                                           tags$ul(
                                             tags$li(strong("Numero di tesi (gruppi):"), " numero di gruppi sperimentali omogenei da creare, 
                                                     che corrisponde al numero di trattamenti acaricidi diversi che si intende testare. 
                                                     VINCOLO IMPORTANTE: il numero di alveari selezionati DEVE essere un multiplo esatto del numero di tesi.",
                                                     tags$ul(
                                                       tags$li("Esempio CORRETTO: 30 alveari ÷ 3 gruppi = 10 alveari per gruppo ✓"),
                                                       tags$li("Esempio ERRATO: 30 alveari ÷ 4 gruppi = 7.5 alveari per gruppo ✗"),
                                                       tags$li("Se il numero non è multiplo, tornare al tab Selezione e aggiustare le esclusioni")
                                                     )),
                                             tags$li(strong("Numero di iterazioni:"), " numero di permutazioni casuali da generare (100-5000). 
                                                     Più iterazioni significano più combinazioni testate e maggiore probabilità di trovare 
                                                     soluzioni ottimali dal punto di vista della deriva. Valori consigliati:",
                                                     tags$ul(
                                                       tags$li("100-500 iterazioni: test rapido, sufficiente per piccole sperimentazioni"),
                                                       tags$li("500-2000 iterazioni: buon compromesso tra velocità e qualità"),
                                                       tags$li("2000-5000 iterazioni: massima accuratezza per sperimentazioni importanti")
                                                     )),
                                             tags$li(strong("Seme del generatore di numeri casuali:"), " numero intero che inizializza il generatore 
                                                     di numeri pseudocasuali. Cambiando il seme si ottengono distribuzioni diverse degli alveari 
                                                     nei gruppi. Si consiglia di:",
                                                     tags$ul(
                                                       tags$li("Provare diversi semi (es: 1, 2, 3, 10, 42, 100, ecc.)"),
                                                       tags$li("Per ogni seme, cliccare 'Calcola' e osservare l'indice di deriva"),
                                                       tags$li("Scegliere il seme che produce l'indice di deriva più basso"),
                                                       tags$li("Annotare il seme utilizzato per la riproducibilità della ricerca")
                                                     ))
                                           ),
                                           
                                           h4("Indice di deriva - Spiegazione dettagliata"),
                                           p(strong("Definizione:"), " L'indice di deriva è un numero tra 0 e 1 che quantifica il rischio 
                                             di contaminazione tra gruppi sperimentali dovuto alla deriva delle api."),
                                           p(strong("Formula:"), " Indice = (Numero di coppie di alveari adiacenti appartenenti a gruppi diversi) / 
                                             (Numero totale di coppie di alveari adiacenti sulla stessa banchetta)"),
                                           p(strong("Interpretazione:")),
                                           tags$ul(
                                             tags$li("Indice = 0: situazione ideale (impossibile in pratica). Tutti gli alveari adiacenti 
                                                     appartengono allo stesso gruppo sperimentale. Nessun rischio di deriva tra gruppi diversi."),
                                             tags$li("Indice = 0.1-0.3: ottima separazione. La maggior parte degli alveari adiacenti 
                                                     appartiene allo stesso gruppo."),
                                             tags$li("Indice = 0.3-0.5: separazione accettabile per molte sperimentazioni."),
                                             tags$li("Indice = 0.5-0.7: separazione moderata. Considerare se il rischio è accettabile."),
                                             tags$li("Indice > 0.7: scarsa separazione. Alto rischio di contaminazione tra gruppi.")
                                           ),
                                           p(strong("Esempio pratico:"), " Se ci sono 20 coppie di alveari adiacenti in totale, 
                                             e 6 di queste coppie hanno alveari di gruppi diversi, l'indice sarà 6/20 = 0.30 (30%)."),
                                           p("Il programma propone le 25 migliori combinazioni ordinate per indice di deriva crescente, 
                                             permettendo di scegliere tra diverse soluzioni con deriva simile ma distribuzione leggermente diversa."),
                                           
                                           h4("Test del Chi quadrato - Validazione statistica"),
                                           p("Il test del Chi quadrato di Pearson verifica l'ipotesi nulla che i gruppi abbiano distribuzioni 
                                             di infestazione statisticamente equivalenti."),
                                           p(strong("Procedura:")),
                                           tags$ol(
                                             tags$li("Gli alveari vengono suddivisi in 5 classi di infestazione (quintili: 0-20%, 20-40%, 40-60%, 60-80%, 80-100%)"),
                                             tags$li("Per ogni gruppo si conta quanti alveari cadono in ciascuna classe"),
                                             tags$li("Il test verifica se le frequenze osservate differiscono significativamente da quelle attese 
                                                     in caso di distribuzione uniforme")
                                           ),
                                           p(strong("Interpretazione del p-value:")),
                                           tags$ul(
                                             tags$li("p > 0.05: BUONO ✓ Non c'è evidenza di differenze significative tra i gruppi. 
                                                     Le distribuzioni sono statisticamente omogenee."),
                                             tags$li("p = 0.05-0.01: ACCETTABILE. Differenze lievi ma ancora nel range dell'accettabilità 
                                                     per molte sperimentazioni."),
                                             tags$li("p < 0.01: ATTENZIONE ⚠ I gruppi hanno distribuzioni significativamente diverse. 
                                                     Considerare di cambiare seme o modificare la selezione degli alveari.")
                                           ),
                                           p("Il valore del Chi quadrato e del p-value vengono visualizzati sopra i grafici."),
                                           
                                           h4("Visualizzazioni"),
                                           tags$ul(
                                             tags$li(strong("Barplot colorato:"), " mostra l'infestazione di ogni alveare, colorato in base 
                                                     al gruppo di appartenenza. Gli alveari sono raggruppati per banchetta, permettendo di 
                                                     visualizzare immediatamente la distribuzione spaziale dei gruppi e identificare eventuali 
                                                     zone critiche con alta probabilità di deriva."),
                                             tags$li(strong("Grafico di densità:"), " mostra la distribuzione statistica dell'infestazione 
                                                     per ciascun gruppo. Curve sovrapposte e simili indicano buona omogeneità."),
                                             tags$li(strong("Tabella:"), " mostra i risultati finali con tutte le informazioni: ID alveare, 
                                                     banchetta, gruppo originale, zav assoluto, e la nuova colonna 'grTesi' che indica 
                                                     l'assegnazione al gruppo sperimentale (1, 2, 3, ...)")
                                           ),
                                           
                                           h4("Download del risultato"),
                                           p("Il pulsante 'Download' permette di scaricare un file .xls (gruppi_auto.xls) con la soluzione selezionata. 
                                             Il file contiene tutte le colonne originali più la colonna 'grTesi' che indica il gruppo sperimentale 
                                             assegnato a ciascun alveare."),
                                           p("Questo file può essere utilizzato per:"),
                                           tags$ul(
                                             tags$li("Applicare i trattamenti sul campo secondo la disposizione ottimale"),
                                             tags$li("Documentare la randomizzazione per pubblicazioni scientifiche"),
                                             tags$li("Analizzare i risultati mantenendo la tracciabilità delle assegnazioni"),
                                             tags$li("Archivio per riproducibilità dell'esperimento")
                                           ),
                                           
                                           h3("5. Procedura consigliata - Guida passo-passo"),
                                           p("Seguire questa procedura per utilizzare l'applicazione in modo ottimale:"),
                                           tags$ol(
                                             tags$li(strong("Preparazione dei dati:"), " creare il file .xls con i dati di campionamento, 
                                                     assicurandosi che gli alveari siano elencati nell'ordine fisico di disposizione nell'apiario."),
                                             tags$li(strong("Caricamento:"), " nel tab 'Dati', caricare il file .xls e verificare che i dati 
                                                     siano stati importati correttamente visualizzando la tabella."),
                                             tags$li(strong("Configurazione:"), " selezionare il numero corretto di api campionate (300, 450 o 900) 
                                                     dal menu a tendina."),
                                             tags$li(strong("Analisi preliminare:"), " esaminare le statistiche nel subtab 'Sommario' e i grafici 
                                                     nel subtab 'Distribuzione' per avere un quadro generale dell'infestazione."),
                                             tags$li(strong("Selezione alveari:"), " OBBLIGATORIO - visitare il tab 'Selezione' anche se non si 
                                                     applicano filtri. Decidere se escludere outliers o impostare una soglia minima basandosi 
                                                     sugli obiettivi della ricerca."),
                                             tags$li(strong("Verifica compatibilità:"), " verificare che il numero di alveari selezionati 
                                                     (visualizzato nella barra laterale) sia compatibile con il numero di gruppi desiderato. 
                                                     Se necessario, aggiustare i filtri."),
                                             tags$li(strong("Configurazione simulazione:"), " nel tab 'Simulazione', impostare il numero di tesi 
                                                     (gruppi sperimentali) e il numero di iterazioni (consigliato: almeno 500 per buoni risultati)."),
                                             tags$li(strong("Esecuzione e ottimizzazione:"), " provare diversi valori del seme (es: 1, 2, 3, 5, 10, 42, 100). 
                                                     Per ogni seme, cliccare 'Calcola' e annotare l'indice di deriva ottenuto. Scegliere il seme 
                                                     che produce l'indice più basso."),
                                             tags$li(strong("Selezione della soluzione:"), " per il seme ottimale, esplorare le 25 combinazioni proposte 
                                                     nel menu a tendina. Tutte hanno deriva simile ma distribuzione leggermente diversa."),
                                             tags$li(strong("Validazione statistica:"), " verificare i grafici di densità (devono sovrapporsi) 
                                                     e il test Chi quadrato (p-value > 0.05 è ideale)."),
                                             tags$li(strong("Verifica visiva:"), " esaminare il barplot colorato per identificare eventuali zone 
                                                     critiche con molti alveari di gruppi diversi adiacenti."),
                                             tags$li(strong("Download e documentazione:"), " scaricare il file .xls con la soluzione finale e 
                                                     annotare il seme utilizzato per la documentazione scientifica dell'esperimento.")
                                           ),
                                           
                                           h3("6. Domande frequenti e risoluzione problemi"),
                                           
                                           h4("Problema: Errori o messaggi strani nel tab Simulazione"),
                                           p(strong("Causa:"), " Non è stato visitato il tab Selezione"),
                                           p(strong("Soluzione:"), " Visitare il tab Selezione anche senza applicare filtri, poi tornare a Simulazione e cliccare 'Calcola'"),
                                           
                                           h4("Problema: 'Il numero di alveari non è multiplo del numero di gruppi'"),
                                           p(strong("Causa:"), " Il numero di alveari selezionati non può essere diviso equamente tra i gruppi"),
                                           p(strong("Soluzione:"), " Tornare al tab Selezione e:"),
                                           tags$ul(
                                             tags$li("Escludere o includere manualmente alcuni alveari per raggiungere il multiplo corretto, oppure"),
                                             tags$li("Modificare il numero di gruppi nel tab Simulazione")
                                           ),
                                           p(strong("Esempi:")),
                                           tags$ul(
                                             tags$li("28 alveari → 2 gruppi ✓, 4 gruppi ✓, 3 gruppi ✗"),
                                             tags$li("30 alveari → 2 gruppi ✓, 3 gruppi ✓, 5 gruppi ✓, 4 gruppi ✗")
                                           ),
                                           
                                           h4("Problema: Indice di deriva molto alto (> 0.6)"),
                                           p(strong("Possibili cause:")),
                                           tags$ul(
                                             tags$li("Molti alveari sulla stessa banchetta rendono difficile la separazione"),
                                             tags$li("Seme non ottimale")
                                           ),
                                           p(strong("Soluzioni:")),
                                           tags$ul(
                                             tags$li("Provare molti semi diversi (anche valori alti come 1000, 5000)"),
                                             tags$li("Aumentare il numero di iterazioni per esplorare più combinazioni"),
                                             tags$li("Se possibile, ridisporre fisicamente gli alveari nell'apiario per ridurre il numero di alveari 
                                                     per banchetta")
                                           ),
                                           
                                           h4("Problema: Test Chi quadrato con p-value basso (< 0.05)"),
                                           p(strong("Significato:"), " I gruppi hanno distribuzioni statisticamente diverse"),
                                           p(strong("Soluzioni:")),
                                           tags$ul(
                                             tags$li("Cambiare seme e riprovare finché non si ottiene un p-value accettabile"),
                                             tags$li("Aumentare il numero di iterazioni"),
                                             tags$li("Verificare che non ci siano outliers non esclusi che distorcono la distribuzione"),
                                             tags$li("Considerare di modificare il numero di gruppi se la distribuzione originale è molto asimmetrica")
                                           ),
                                           
                                           h4("Domanda: Quante iterazioni devo usare?"),
                                           p(strong("Risposta:"), " Dipende dall'importanza della sperimentazione e dal tempo disponibile:"),
                                           tags$ul(
                                             tags$li("Test preliminari: 100-200 iterazioni (veloce, sufficiente per farsi un'idea)"),
                                             tags$li("Sperimentazioni standard: 500-1000 iterazioni (buon compromesso)"),
                                             tags$li("Pubblicazioni scientifiche: 2000-5000 iterazioni (massima accuratezza)")
                                           ),
                                           
                                           h4("Domanda: Devo sempre escludere gli outliers?"),
                                           p(strong("Risposta:"), " No, dipende dal contesto della ricerca:"),
                                           tags$ul(
                                             tags$li(strong("Escludere"), " se gli outliers sono dovuti a errori di campionamento o alveari con problemi particolari"),
                                             tags$li(strong("Includere"), " se rappresentano la naturale variabilità della popolazione e si vuole testare 
                                                     l'efficacia del trattamento anche su casi estremi"),
                                             tags$li("Analizzare sempre il subtab Outliers per valutare il loro impatto prima di decidere")
                                           ),
                                           
                                           h3("7. Note e limitazioni"),
                                           tags$ul(
                                             tags$li(strong("Prerequisito obbligatorio:"), " È necessario visitare il tab 'Selezione' prima di 
                                                     calcolare i gruppi nel tab 'Simulazione', anche se non si applicano filtri. Questo inizializza 
                                                     il dataset di lavoro."),
                                             tags$li(strong("Calcolo della deriva:"), " L'algoritmo considera solo alveari consecutivi sulla stessa 
                                                     banchetta. Non tiene conto degli spazi vuoti creati dagli alveari esclusi, che in realtà potrebbero 
                                                     ridurre la probabilità di deriva se lo spazio è sufficientemente ampio."),
                                             tags$li(strong("Soglia del 5%:"), " La linea rossa di riferimento nei grafici indica il 5% di infestazione, 
                                                     considerato il livello critico di intervento basato su evidenze scientifiche. Al di sopra di questa 
                                                     soglia, l'alveare necessita di trattamento per evitare danni alla colonia."),
                                             tags$li(strong("Limiti dell'omogeneità:"), " Con distribuzioni di infestazione molto asimmetriche o 
                                                     con pochi alveari, può essere difficile ottenere gruppi perfettamente omogenei. In questi casi, 
                                                     l'applicazione trova il migliore compromesso possibile."),
                                             tags$li(strong("Riproducibilità:"), " Usando lo stesso seme e gli stessi parametri, l'applicazione 
                                                     produce sempre lo stesso risultato. Questo è importante per la riproducibilità scientifica. 
                                                     Annotare sempre il seme utilizzato nella documentazione dell'esperimento."),
                                             tags$li(strong("Formato file:"), " L'applicazione accetta solo file .xls (Excel 97-2003). Se si ha un file 
                                                     .xlsx, salvarlo come .xls prima di caricarlo.")
                                           ),
                                           
                                           br(),
                                           hr(),
                                           h3("8. Riferimenti scientifici e ulteriori informazioni"),
                                           
                                           h4("Metodologia di campionamento"),
                                           p("I metodi standardizzati per la stima dell'infestazione da Varroa destructor sono descritti in:"),
                                           tags$ul(
                                             tags$li(strong("Lee, K.V., Reuter, G.S., Spivak, M. (2010)."), " Standardized Sampling Plan to Detect 
                                                     Varroa Density in Colonies and Apiaries. ", em("American Bee Journal"), " 150: 1151-1155."),
                                             tags$li(strong("Lee, K.V., Moon, R.D., Burkness, E.C., Hutchison, W.D., Spivak, M. (2010)."), 
                                                     " Practical sampling plans for Varroa destructor (Acari: Varroidae) in Apis mellifera 
                                                     (Hymenoptera: Apidae) colonies and apiaries. ", em("Journal of Economic Entomology"), 
                                                     " 103(4): 1039-1050. doi:10.1603/EC10037")
                                           ),
                                           
                                           h4("Livelli critici di infestazione"),
                                           p("La soglia del 5% di infestazione è stata identificata come livello critico di intervento in diversi studi. 
                                             Oltre questa soglia, il rischio di danno alla colonia aumenta significativamente se non si interviene 
                                             con un trattamento."),
                                           
                                           h4("Design sperimentale"),
                                           p("Per sperimentazioni rigorose sulla valutazione di trattamenti acaricidi, è fondamentale:"),
                                           tags$ul(
                                             tags$li("Utilizzare gruppi sperimentali omogenei per livello di infestazione"),
                                             tags$li("Minimizzare la contaminazione tra gruppi (deriva)"),
                                             tags$li("Documentare accuratamente la randomizzazione"),
                                             tags$li("Registrare tutti i parametri utilizzati per garantire la riproducibilità")
                                           ),
                                           
                                           h4("Informazioni di contatto"),
                                           p("Per segnalare problemi, suggerire miglioramenti o richiedere assistenza sull'utilizzo dell'applicazione, 
                                             visitare il [repository GitHub del progetto](https://github.com/uvesco/HomoGroups)."),
                                           
                                           br(),
                                           p(em("Versione dell'applicazione: 1.0 | Ultima revisione della guida: 2025")),
                                           p(em("Applicazione sviluppata per assistere la ricerca sulla gestione della varroasi nelle api mellifere"))
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
