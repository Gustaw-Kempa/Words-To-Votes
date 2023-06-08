rm(list = ls())

library(rvest)
library(dplyr)
library(lubridate)
library(stringr)
library(googleLanguageR)
gl_auth("/Users/gustawkempa/Desktop/Studia/Master/Master.Data/master-377713-d6f77365d167.json")





links <- c('https://web.archive.org/web/20201024034506/http://www.governo.it/it/articolo/coronavirus-dichiarazione-del-presidente-conte/14297',
           'https://web.archive.org/web/20201031130153/http://www.governo.it/it/articolo/dichiarazioni-del-presidente-conte/14361',
           'https://web.archive.org/web/20201031130153/http://www.governo.it/it/articolo/dichiarazioni-del-presidente-conte/14361',
           'https://web.archive.org/web/20201031130724/http://www.governo.it/it/articolo/covid-19-informativa-del-presidente-conte-al-senato/14500',
           'https://web.archive.org/web/20200926194019/http://www.governo.it/it/articolo/covid-19-informativa-del-presidente-conte-alla-camera/14550',
           'https://web.archive.org/web/20201030131712/http://www.governo.it/it/articolo/nuova-fase-legate-allemergenza-epidemiologica-da-covid-19-informativa-del-presidente-conte',
           'https://web.archive.org/web/20201026163449/http://www.governo.it/it/articolo/video-messaggio-del-presidente-conte-al-summit-globale-oil-su-covid-19-e-mondo-del-lavoro',
           'https://web.archive.org/web/20201027070748/http://www.governo.it/it/articolo/progettiamo-il-rilancio-l-intervento-introduttivo-del-presidente-conte/14746',
           'https://web.archive.org/web/20210101091333/http://www.governo.it/it/articolo/progettiamo-il-rilancio-lintervento-introduttivo-del-presidente-conte-della-quarta-giornata',
           'https://web.archive.org/web/20201026163213/http://www.governo.it/it/articolo/intervento-del-presidente-conte-alla-resentazione-del-soer-2020/14672',
           'https://web.archive.org/web/20201124172555/http://www.governo.it/it/articolo/intervento-del-presidente-conte-allinaugurazione-del-nuovo-ponte-autostradale-di-genova',
           'https://web.archive.org/web/20210127123655/http://www.governo.it/it/articolo/comunicazioni-del-presidente-conte-parlamento-intervento-al-senato/14992',
           'https://web.archive.org/web/20201129201743/http://www.governo.it/it/articolo/messaggio-del-presidente-conte-losservatorio-riparte-litalia/15260',
           'https://web.archive.org/web/20201204055814/http://www.governo.it/it/articolo/dpcm-18-ottobre-2020-informativa-del-presidente-conte-alla-camera/15483',
           'https://web.archive.org/web/20201125194541/http://www.governo.it/it/articolo/intervento-del-presidente-conte-allassemblea-annuale-dell-ania/15460',
           'https://web.archive.org/web/20201025173632/http://www.governo.it/it/articolo/senato-le-comunicazioni-del-presidente-conte-vista-del-consiglio-europeo-del-15-e-16',
           'https://web.archive.org/web/20201025173550/http://www.governo.it/it/articolo/conte-videoconferenza-al-festival-dello-sviluppo-sostenibile-2020/15355',
           'https://web.archive.org/web/20201129201743/http://www.governo.it/it/articolo/messaggio-del-presidente-conte-losservatorio-riparte-litalia/15260',
           'https://web.archive.org/web/20201126211622/http://www.governo.it/it/articolo/misure-lemergenza-covid-19-le-comunicazioni-di-conte-alla-camera-dei-deputati/15602',
           'https://web.archive.org/web/20210126211956/http://www.governo.it/it/articolo/intervento-di-conte-alla-presentazione-del-rapporto-svimez-2020/15772',
           'https://web.archive.org/web/20210210085014/http://www.governo.it/it/articolo/comunicazioni-al-senato-della-repubblica-replica-del-presidente-conte/16085',
           'https://web.archive.org/web/20210121211834/http://www.governo.it/it/articolo/intervento-del-presidente-conte-al-allevento-al-convegno-next-generation-italia/15906',
           'https://web.archive.org/web/20210121210015/http://www.governo.it/it/articolo/intervento-del-presidente-conte-al-rome-investment-forum-2020/15890'
)

# REMEMBER TO EXCLUDE Q&AS!!!!!
res <- tibble( titles = character(), dates = character(), speeches_IT = character(), links = character())

for (i in 1:length(links)) {
 res_temp <- matrix("", ncol = 4, nrow =1)
  res_temp[,1] <- links[i]
  page <- read_html(res_temp[,1])
 res_temp[,2] <- page %>% html_nodes('.title_large') %>% html_text()
 res_temp[,3] <-  page %>% html_nodes('.h6') %>% html_text()
  speech <- page %>% html_nodes('.body_intervista p') %>% html_text()
  res_temp[,4] <- paste(speech[-1], collapse = " ")
  res <- rbind(res, res_temp)
}

data_IT <- res
colnames(data_IT) <- c("links", "titles", "dates", "speeches_IT")


# manually adding unscrapable data
data_IT$speeches_IT[13] <- "Un saluto a tutte le Autorità presenti, un saluto al professor Luigi Balestra quale Presidente del Comitato di indirizzo Osservatorio “Riparte l’Italia”. È per me un piacere intervenire, anche se a distanza, a questo incontro da cui sono certo scaturiranno idee, proposte, opinioni e spunti di riflessione certamente preziosi anche per il nostro lavoro, nell’interesse del Paese, dei cittadini e delle imprese.
Con l’Osservatorio economico e sociale ‘Riparte l’Italia’ vi siete posti l’obiettivo di offrire un contributo, di indicare delle direttrici per migliorare la nostra vita collettiva ‘oltre l’emergenza’, come avete precisato. Io penso che queste iniziative siano necessarie per dare vita a stimoli, a impulsi di cui abbiamo bisogno per ripartire insieme.
Veniamo da mesi molto difficili, abbiamo combattuto e ancora stiamo affrontando una pandemia inattesa, sconosciuta. Negli scorsi giorni, autorevoli testate – penso a Bloomberg, al Financial Times – hanno riconosciuto all’Italia di aver gestito meglio di altri Paesi l’emergenza, tenendo sotto controllo l’epidemia. Da Presidente del Consiglio, lasciatemelo dire, sono orgoglioso di questi riconoscimenti. Dobbiamo tutti essere fieri del comportamento esemplare dimostrato da tutta la Comunità italiana.
Lo spirito di solidarietà e il senso di comunità che ci hanno unito nei mesi più difficili della pandemia oggi non vanno dispersi.
Non dobbiamo disunirci e sfilacciarci nella fase cruciale della ricostruzione. Credo che oltre alla sfida della resilienza possiamo vincere anche quella della ripartenza. È un impegno che prendiamo davanti alle generazioni future, non c’è quindi sfida più importante.
Il confronto continuo fra Stato, attori economici e sociali, cittadini e comunità tutta ci aiuta senz’altro a focalizzare con maggior decisione in che modo utilizzare la storica occasione dei 209 miliardi ottenuti in sede europea per far procedere speditamente il progetto di una ‘nuova Italia’, con la spinta del Recovery Plan.
Molte delle parole d’ordine del ‘manifesto’ del vostro Osservatorio sono le stesse poste alla base del lavoro che stiamo mettendo in campo in questi mesi: sviluppo, efficienza, sostenibilità, innovazione, solidarietà.
Sul fronte economico la pandemia ha spinto verso un maggior protagonismo dello Stato in tutto il mondo. Ed è un dovere – ritengo – dello Stato fare uno sforzo per proteggere cittadini, famiglie, imprese, e accompagnarle verso la crescita e verso uno sviluppo sostenibile. Nei mesi scorsi ci siamo mossi per creare le condizioni affinché gli investimenti si traducano realmente e rapidamente da ‘freddi numeri’ in bilancio in opere, cantieri, interventi che migliorino efficacemente, realmente la vita dei nostri cittadini e dei territori. Va in questo senso anche, in questa direzione il decreto Semplificazioni: lo considero un acceleratore per lo sviluppo per una maggiore efficienza della macchina amministrativa, che ci consentirà – insieme ad altri interventi che abbiamo in cantiere – di non disperdere risorse ed energie nei rivoli della burocrazia in un momento così cruciale. In questi giorni firmerò un decreto per individuare i cantieri che avranno un percorso accelerato e i relativi commissari.
Ma non può essere ‘velocità’ l’unica parola d’ordine della crescita e dello sviluppo di cui abbiamo bisogno per ripartire. Questo percorso deve essere anche sostenibile. E sostenibile innanzitutto a livello ambientale. Con il superbonus edilizia al 110%, ad esempio, diventa concreta la possibilità di produrre lavoro e occupazione nel settore dell’edilizia perseguendo però l’obiettivo dell’efficientamento energetico e dell’adeguamento sismico delle abitazioni. Intendiamo estendere questo strumento anche oltre il 2021. Con il Recovery Plan almeno il 37% delle risorse disponibili riguarderà investimenti green: quindi transizione energetica in settori strategici come l’automotive, il potenziamento della rete idrica e il contrasto al dissesto idrogeologico, l’efficientamento energetico degli edifici pubblici.
Altrettanto importante è la sostenibilità sociale: non c’è crescita se qualcuno rimane indietro e senza protezioni, se non c’è la stella polare della solidarietà e dell’inclusione a indicarci la strada giusta. Qualcuno ha classificato alcuni provvedimenti emergenziali del Governo come una ‘pioggia’ di bonus, di sussidi. In realtà erano l’ombrello con cui riparare le categorie più esposte alla tempesta. Questo ci impone di ragionare per il futuro su un welfare che metta sempre al centro la persona, la sua dignità, il diritto per i giovani ad avere gli strumenti per non cedere alla cultura dell’odio e della violenza e per abbracciare la speranza, il sogno di realizzarsi, il diritto di poter costruire una famiglia.
Parlando di giovani e di futuro non possiamo ignorare che l’Italia riparte solo se riparte la scuola: siamo in giorni cruciali, in queste prime settimane l’anno scolastico è ripreso in maniera ordinata, nel rispetto delle regole, simbolo anche questo – potremmo dire – di un’Italia che si rialza e riprende a correre. Contemporaneamente, però, su scuola, università e ricerca il nostro Paese non può accontentarsi della situazione pre pandemia, dobbiamo programmare e preparare da subito un nuovo slancio. Intendiamo trasformare tutte le classi italiane in luoghi innovativi per l’apprendimento; collegare le scuole superiori alle professioni del futuro, dando agli istituti gli strumenti e i mezzi per creare sin da subito ‘lavoratori digitali’. Investiamo su poli tematici di ricerca in ambiti come il fin-tech e agri-tech, promuoviamo le industrie strategiche nel settore aerospaziale. È nostro obiettivo anche sostenere interventi per la digitalizzazione della Sanità, per la telemedicina.
L’innovazione dovrà rimanere in cima a questa agenda per la ripresa. L’Italia del boom economico trovò nell’Autostrada del Sole la risorsa e il collegamento capace di unire il Paese e favorirne la crescita. Oggi acceleriamo sul fronte dell’autostrada del nostro futuro, un'infrastruttura digitale unica in banda ultralarga capace di proiettare velocemente il nostro Paese in avanti, recuperando il terreno perduto.
Investire sulla rete unica significa fornire agli studenti la possibilità di accedere a tutte le informazioni in maniera semplice e rapida; alla scuola di digitalizzarsi e rispondere alle aspettative dei nostri ragazzi. Significa anche rafforzare un rapporto diretto e veloce fra i cittadini e i servizi pubblici e privati; aprire la strada a nuove occasioni di sviluppo nelle aree depresse d’Italia dove i dati potranno finalmente viaggiare alla velocità degli obiettivi delle imprese innovative e dei sogni dei nostri giovani che sono troppo spesso costretti ad emigrare.
Nell’augurarvi buon lavoro intendo fare un auspicio per il nostro Paese: ripartire significa ritrovare la fiducia nel fatto che l’Italia ha un potenziale enorme. Dobbiamo rialzarci per accelerare, non per stare in piedi come prima. All’Italia non manca nulla per farcela, l’Italia non ha motivi per accontentarsi, l’Italia può, deve osare. Buon lavoro."


data_IT$speeches_IT[18] <- "Un saluto a tutte le Autorità presenti, un saluto al professor Luigi Balestra quale Presidente del Comitato di indirizzo Osservatorio “Riparte l’Italia”. È per me un piacere intervenire, anche se a distanza, a questo incontro da cui sono certo scaturiranno idee, proposte, opinioni e spunti di riflessione certamente preziosi anche per il nostro lavoro, nell’interesse del Paese, dei cittadini e delle imprese.
Con l’Osservatorio economico e sociale ‘Riparte l’Italia’ vi siete posti l’obiettivo di offrire un contributo, di indicare delle direttrici per migliorare la nostra vita collettiva ‘oltre l’emergenza’, come avete precisato. Io penso che queste iniziative siano necessarie per dare vita a stimoli, a impulsi di cui abbiamo bisogno per ripartire insieme.
Veniamo da mesi molto difficili, abbiamo combattuto e ancora stiamo affrontando una pandemia inattesa, sconosciuta. Negli scorsi giorni, autorevoli testate – penso a Bloomberg, al Financial Times – hanno riconosciuto all’Italia di aver gestito meglio di altri Paesi l’emergenza, tenendo sotto controllo l’epidemia. Da Presidente del Consiglio, lasciatemelo dire, sono orgoglioso di questi riconoscimenti. Dobbiamo tutti essere fieri del comportamento esemplare dimostrato da tutta la Comunità italiana.
Lo spirito di solidarietà e il senso di comunità che ci hanno unito nei mesi più difficili della pandemia oggi non vanno dispersi.
Non dobbiamo disunirci e sfilacciarci nella fase cruciale della ricostruzione. Credo che oltre alla sfida della resilienza possiamo vincere anche quella della ripartenza. È un impegno che prendiamo davanti alle generazioni future, non c’è quindi sfida più importante.
Il confronto continuo fra Stato, attori economici e sociali, cittadini e comunità tutta ci aiuta senz’altro a focalizzare con maggior decisione in che modo utilizzare la storica occasione dei 209 miliardi ottenuti in sede europea per far procedere speditamente il progetto di una ‘nuova Italia’, con la spinta del Recovery Plan.
Molte delle parole d’ordine del ‘manifesto’ del vostro Osservatorio sono le stesse poste alla base del lavoro che stiamo mettendo in campo in questi mesi: sviluppo, efficienza, sostenibilità, innovazione, solidarietà.
Sul fronte economico la pandemia ha spinto verso un maggior protagonismo dello Stato in tutto il mondo. Ed è un dovere – ritengo – dello Stato fare uno sforzo per proteggere cittadini, famiglie, imprese, e accompagnarle verso la crescita e verso uno sviluppo sostenibile. Nei mesi scorsi ci siamo mossi per creare le condizioni affinché gli investimenti si traducano realmente e rapidamente da ‘freddi numeri’ in bilancio in opere, cantieri, interventi che migliorino efficacemente, realmente la vita dei nostri cittadini e dei territori. Va in questo senso anche, in questa direzione il decreto Semplificazioni: lo considero un acceleratore per lo sviluppo per una maggiore efficienza della macchina amministrativa, che ci consentirà – insieme ad altri interventi che abbiamo in cantiere – di non disperdere risorse ed energie nei rivoli della burocrazia in un momento così cruciale. In questi giorni firmerò un decreto per individuare i cantieri che avranno un percorso accelerato e i relativi commissari.
Ma non può essere ‘velocità’ l’unica parola d’ordine della crescita e dello sviluppo di cui abbiamo bisogno per ripartire. Questo percorso deve essere anche sostenibile. E sostenibile innanzitutto a livello ambientale. Con il superbonus edilizia al 110%, ad esempio, diventa concreta la possibilità di produrre lavoro e occupazione nel settore dell’edilizia perseguendo però l’obiettivo dell’efficientamento energetico e dell’adeguamento sismico delle abitazioni. Intendiamo estendere questo strumento anche oltre il 2021. Con il Recovery Plan almeno il 37% delle risorse disponibili riguarderà investimenti green: quindi transizione energetica in settori strategici come l’automotive, il potenziamento della rete idrica e il contrasto al dissesto idrogeologico, l’efficientamento energetico degli edifici pubblici.
Altrettanto importante è la sostenibilità sociale: non c’è crescita se qualcuno rimane indietro e senza protezioni, se non c’è la stella polare della solidarietà e dell’inclusione a indicarci la strada giusta. Qualcuno ha classificato alcuni provvedimenti emergenziali del Governo come una ‘pioggia’ di bonus, di sussidi. In realtà erano l’ombrello con cui riparare le categorie più esposte alla tempesta. Questo ci impone di ragionare per il futuro su un welfare che metta sempre al centro la persona, la sua dignità, il diritto per i giovani ad avere gli strumenti per non cedere alla cultura dell’odio e della violenza e per abbracciare la speranza, il sogno di realizzarsi, il diritto di poter costruire una famiglia.
Parlando di giovani e di futuro non possiamo ignorare che l’Italia riparte solo se riparte la scuola: siamo in giorni cruciali, in queste prime settimane l’anno scolastico è ripreso in maniera ordinata, nel rispetto delle regole, simbolo anche questo – potremmo dire – di un’Italia che si rialza e riprende a correre. Contemporaneamente, però, su scuola, università e ricerca il nostro Paese non può accontentarsi della situazione pre pandemia, dobbiamo programmare e preparare da subito un nuovo slancio. Intendiamo trasformare tutte le classi italiane in luoghi innovativi per l’apprendimento; collegare le scuole superiori alle professioni del futuro, dando agli istituti gli strumenti e i mezzi per creare sin da subito ‘lavoratori digitali’. Investiamo su poli tematici di ricerca in ambiti come il fin-tech e agri-tech, promuoviamo le industrie strategiche nel settore aerospaziale. È nostro obiettivo anche sostenere interventi per la digitalizzazione della Sanità, per la telemedicina.
L’innovazione dovrà rimanere in cima a questa agenda per la ripresa. L’Italia del boom economico trovò nell’Autostrada del Sole la risorsa e il collegamento capace di unire il Paese e favorirne la crescita. Oggi acceleriamo sul fronte dell’autostrada del nostro futuro, un'infrastruttura digitale unica in banda ultralarga capace di proiettare velocemente il nostro Paese in avanti, recuperando il terreno perduto.
Investire sulla rete unica significa fornire agli studenti la possibilità di accedere a tutte le informazioni in maniera semplice e rapida; alla scuola di digitalizzarsi e rispondere alle aspettative dei nostri ragazzi. Significa anche rafforzare un rapporto diretto e veloce fra i cittadini e i servizi pubblici e privati; aprire la strada a nuove occasioni di sviluppo nelle aree depresse d’Italia dove i dati potranno finalmente viaggiare alla velocità degli obiettivi delle imprese innovative e dei sogni dei nostri giovani che sono troppo spesso costretti ad emigrare.
Nell’augurarvi buon lavoro intendo fare un auspicio per il nostro Paese: ripartire significa ritrovare la fiducia nel fatto che l’Italia ha un potenziale enorme. Dobbiamo rialzarci per accelerare, non per stare in piedi come prima. All’Italia non manca nulla per farcela, l’Italia non ha motivi per accontentarsi, l’Italia può, deve osare. Buon lavoro."





topics_EN <- gl_translate(t_string = data_IT$titles, target = "en",
                                  format = "text", model = "nmt")[1]
speeches_EN <- gl_translate(t_string = data_IT$speeches_IT, target = "en",
                                    format = "text", model = "nmt")[1]


months <- sub(".*,\\s(\\d{1,2})\\s(\\w+)\\s\\d{4}$", "\\2", data_IT$dates)
months <- gl_translate(t_string = months, target = "en",
                       format = "text", model = "nmt")[1]



data_IT$dates <-sub(".*,\\s(\\d{1,2})\\s(\\w+)\\s(\\d{4})$", "\\1 \\3", data_IT$dates)
data_IT$dates<- paste(months$translatedText,data_IT$dates )
data_IT$dates <- mdy(data_IT$dates)



data <- cbind(data_IT,topics_EN ,speeches_EN)
colnames(data) <- c("links", "topics", "dates", "speeches_IT", "topics_EN", "speeches_EN")
write.csv(data, 'data/speeches/speeches_IT_translated.csv')
