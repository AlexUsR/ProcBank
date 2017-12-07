
# ################################################################################################ #
# ################# Optimizaci?n de procesos de an?lisis de riesgos para Bankia ################## #
# ################################################################################################ #
# Autor: Alejandro Pedraza. Para NoesisAF, Madrid. Contacto: Alejandro.Pedraza@noesis.es
# Requiere Rtools instalado para funcionar!!! https://cran.r-project.org/bin/windows/Rtools/
# Este proceso utiliza la agenda de la base de datos de Killarney para obtener los festivos de la bolsa espa?ola por lo que esta tabla ha de estar actualizada
# Este proceso utiliza la tabla de divisas de la base de datos de Killarney para obtener el historico de los tipos de cambio

# Datos del cliente en el FTP  en NAFUSER11:  ACTPOS_FYYYYMMDD, 
# Datos nuestros: R:/Riesgos

# ######### Librer?as, rutas y funciones #### 
rm(list=ls())
FechaHoy <- Sys.Date()

Cambio_A_EUR <- function( Cotizaciones = Hist[3,5:ncol(Hist)] ,From = "GBP", To = "EUR"){
    # funcion que actualiza el historico de cotizaciones a EUR segun el historico 
    if(From == "EUR" || is.na(Cotizaciones[1,3]) ){
        return( Cotizaciones )
    }else{
        if(From == "GBp"){ # en peniques en vez de en libras
            From <- "GBP"
            Cotizaciones <- Cotizaciones/100
        }
        
        HcoDiv <- HcoCotizDivisas[ HcoCotizDivisas$FromDivisaID == IdDivisas[IdDivisas$CodigoISO == From,"Id"] &
                                       HcoCotizDivisas$ToDivisaID == IdDivisas[IdDivisas$CodigoISO == To,"Id"], c("FechaValor", "ExchangeRate")]
        
        for(l in 1:length(Cotizaciones)){ # comprobar que siempre haya cotizacion l<-1
            m <- 1
            NumCotiz <- which( HcoDiv$FechaValor %in% as.Date(colnames(Cotizaciones)[l]) )
            while( length(  NumCotiz  )==0 ){ NumCotiz <- which( HcoDiv$FechaValor %in% as.Date(colnames(Cotizaciones)[l+m]) );m <- m + 1 }# si falla el match por alguna fecha, voy a la siguiente
            Cotizaciones[1,l] <- Cotizaciones[1,l]*HcoDiv[ NumCotiz,"ExchangeRate"]
        }
        return( Cotizaciones )
    }
}
LimpiaNAs <- function( Tabla = HcoXaTxt[ ,2:ncol(HcoXaTxt) ] ){ 
    for(ii in 1:nrow(Tabla)){
        for(jj in 1:ncol(Tabla)){
            kk <- 0
            if( is.na(Tabla[ii,jj]) ){
                while( is.na(Tabla[ii,jj+kk]) && (jj+kk)<(ncol(Tabla)) ){
                    kk + 1 -> kk
                }
                Tabla[ii, jj] <- Tabla[ii, jj+kk]
                # }else if(Tabla[ii,jj]==100 && Tabla[ii,jj-1]==100){ # para ajuste del VaR
                #     Tabla[ii,jj]<-100.1
            }
        }    
    }
    return(Tabla)
}
CruceProxy <- function( TablaP = FiltroProxy ){
    Datos <- unique( TablaP$CodBNProxy[which(!is.na(TablaP$CodBNProxy))] )
    T_Dats<- list()
    for(ii in 1:length(Datos)){
        T_Dats[[ii]] <- bdh(Datos[ii],"PX_LAST", Sys.Date()-385,Sys.Date() )
        names(T_Dats)[[ii]] <- Datos[ii]
        if ( nrow(T_Dats[[ii]]) == 0 ){ # a?ade index si no estaba ya
            Datos[ii]<-paste0(Datos[ii]," Index")
            T_Dats[[ii]] <- bdh(Datos[ii],"PX_LAST", Sys.Date()-385,Sys.Date() )
        }
        
    }
    for(ii in 1:nrow(TablaP)){
        if( !is.na( TablaP$CodBNProxy[ii] ) ){
            ColPivot <-which( colnames(TablaP) %in% TablaP$FechaProxy[ii] )
            # calculo proxy por cross-multiplication respecto de la serie proxy indicada
            b <- as.numeric(TablaP[ii,ColPivot-1])
            c <- T_Dats[[ TablaP$CodBNProxy[ii] ]][ T_Dats[[TablaP$CodBNProxy[ii]]]$date == colnames(TablaP)[ColPivot-1] , 2]
            for(jj in ColPivot:(ncol(TablaP)-1)){
                a <- T_Dats[[ TablaP$CodBNProxy[ii] ]][ T_Dats[[TablaP$CodBNProxy[ii]]]$date == colnames(TablaP)[jj] , 2]
                
                if(length(a*b/c) != 0){ TablaP[ii,jj] <- a*b/c }
            }    
        }
        
    }
    return(TablaP)
}
# librerias arreglo txt
ArregFila <- function( Tabla ){
    for(j in 1:ncol(Tabla)){
        Tabla[1,j] <- gsub(";","", Tabla[1,j] )
        if(j > 2){
            while( nchar(Tabla[1,j]) < 18 ){Tabla[1,j] <- paste0(Tabla[1,j],"-")}
        }
    }   
    return(Tabla[1,])
} 
ArregCol  <- function( Tabla=HcoXaTxt[ 2:nrow(HcoXaTxt) , 1:2 ] ){
    for(i in 1:nrow(Tabla)){
        j <- 1
        # Tabla[i,j] <- gsub( "\\*"," ", Tabla[i,j] )
        while( nchar(Tabla[i,j]) < 30 ){Tabla[i,j] <- paste0(Tabla[i,j]," ")}
        j <- 2
        while( nchar(Tabla[i,j]) < 15 ){Tabla[i,j] <- paste0(Tabla[i,j]," ")}
    }
    return(Tabla)
    # ArregCol  <- function( Tabla ){
    #     for(i in 1:nrow(Tabla)){
    #         for(j in 1:ncol(Tabla)){
    #             Tabla[i,j] <- gsub( "\\*"," ", Tabla[i,j] )
    # k <- 1
    # while( substring(Tabla[i,j],k,k) == 0 ){ 
    #     substring(Tabla[i,j],k,k) <- " "  
    #     k <- k+1
    #     }
    #         }    
    #     }
    #     return(Tabla)
    # }   
}    
ArregDatos <- function( Tabla ){
    for(i in 1:nrow(Tabla)){
        for(j in 1:ncol(Tabla)){
            Tabla[i,j] <- zeroPad( Tabla[i,j] )
        }    
    }
    return(Tabla)
}    
zeroPad <- function(x, L=9, R =8 ){
    x <- as.character(x)
    if( !grepl("\\.",x) ){
        x <- paste0(x,".")
    }
    PosPunt <- gregexpr(pattern = "\\.",x)[[1]][1]
    if(PosPunt == -1 ){x <- paste0(x,".")}
    PosPunt <- gregexpr(pattern = "\\.",x)[[1]][1]
    Tama?o  <- nchar(x)
    num.zeros<-L-PosPunt+1
    if( ( (Tama?o-PosPunt) > R ) ){
        x<-substr(x, 1, PosPunt + R)
        Tama?o  <- nchar(x)
    }
    x <- paste0(paste(rep(" ", num.zeros), collapse = ""), x) # lado izq del punto
    num.zeros<-R-Tama?o+PosPunt
    x <- paste0(x, paste(rep("0", num.zeros), collapse = ""))
    return(x)
} 



list.of.packages <- c( "RODBC", "data.table", "openxlsx", "RCurl", "readxl","Rblpapi")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)  
Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") # Para openxlsx 
Sys.setlocale("LC_TIME","C")  # Sys.setlocale("LC_TIME","Spanish_Spain.1252") # Para calendario, d?as en ingl?s 
options("scipen"=100, "digits"=7) # normalmente 0 y 7 respectivamente

# bloomberg connection
con <- blpConnect(host = getOption("blpHost", "localhost"),
                  port = getOption("blpPort", 8194L), default = TRUE) 
channel<-odbcDriverConnect('driver={SQL Server};server=KILLARNEY;database=Instrumentos;uid=super;pwd=super')
# Para divisas
HcoCotizDivisas <- sqlQuery(channel, paste0( "SELECT * FROM [Instrumentos].[bankia].[HcoExchangeRates] order by FechaValor desc" ),stringsAsFactors=FALSE,as.is=TRUE,dec=".")
IdDivisas <- sqlQuery(channel, paste0( "SELECT  [Id],[CodigoISO],[Nombre_ES],[Unidades] FROM [Instrumentos].[bankia].[Divisa]"),stringsAsFactors=FALSE,as.is=TRUE,dec=".")
HcoCotizDivisas$FechaValor <- as.Date(HcoCotizDivisas$FechaValor)
HcoCotizDivisas$ExchangeRate<-as.numeric(HcoCotizDivisas$ExchangeRate)
Festivos <- sqlQuery(channel, paste0( "SELECT fecha, nombre, periodo, codPais FROM [Instrumentos].[macro].[NAFWEEK_DATOS] where nombre like 'Fest%' and codPais = 'ESP' and fecha > GETDATE() - 480" ),stringsAsFactors=FALSE,as.is=TRUE,dec=".")
Festivos$fecha <- as.Date( Festivos$fecha )
Caso16Mas5 <- sqlQuery(channel, paste0( "SELECT [Fecha],[Cierre] FROM [Instrumentos].[dbo].[Cotizaciones] where InstrumentoID = 135 order by Fecha desc" ),stringsAsFactors=FALSE,as.is=TRUE,dec=".")
Caso17Mab2 <- sqlQuery(channel, paste0( "SELECT [Fecha],[Cierre] FROM [Instrumentos].[dbo].[Cotizaciones] where InstrumentoID = 137 order by Fecha desc" ),stringsAsFactors=FALSE,as.is=TRUE,dec=".")
Caso10Ibex <- sqlQuery(channel, paste0( "SELECT [Fecha],[Cierre] FROM [Instrumentos].[dbo].[Cotizaciones] where InstrumentoID = 117 order by Fecha desc" ),stringsAsFactors=FALSE,as.is=TRUE,dec=".")
Caso16Mas5$Fecha <- as.Date(Caso16Mas5$Fecha)
Caso16Mas5$Cierre<- as.numeric(Caso16Mas5$Cierre)
Caso17Mab2$Fecha <- as.Date(Caso17Mab2$Fecha)
Caso17Mab2$Cierre<- as.numeric(Caso17Mab2$Cierre)
Caso10Ibex$Fecha <- as.Date(Caso10Ibex$Fecha)
Caso10Ibex$Cierre<- as.numeric(Caso10Ibex$Cierre)
FeedsBankiaVamp  <- sqlQuery(channel, paste0( "SELECT * FROM [Instrumentos].[bankia].[CotizacionFeed] where OrigenID = 7"  ),stringsAsFactors=FALSE,as.is=TRUE,dec=".") # [dbo].[FeedsDatosOrigen]
FeedsBankiaVamp$FechaCotizacion <- as.Date(FeedsBankiaVamp$FechaCotizacion)
FeedsBankiaVamp$Precio  <-  as.numeric(FeedsBankiaVamp$Precio)
FeedsBankiaVamp$Isin <- toupper( (FeedsBankiaVamp$Isin) )

# FTP ----

# Direcci?n: descargas.noesis.es
# Usuario: nafuserdev
# Contrase?a: ecceh0m0rR!    # tb BN
# Ruta: /widgets/apps/Ichimoku

# User <- "Nafuser11"
# Password <- "_nafus3r11"
# Ruta <- "/AF/"
# ftpUpload(paste0(RutaInvertia, NombreXML[3]), paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,NombreXML[3],"")) # ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")

FTPServer <- "descargas.noesis.es"
User <- "nafuserdev"
Password <- "ecceh0m0rR!"
Ruta <- "/NAFUSER11/"
FicherosFTP <- getURL(url= paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,""),
                      ftp.use.epsv = T,dirlistonly = TRUE) 
FicherosFTP <- paste(strsplit( FicherosFTP, "\r*\n")[[1]], sep = "")

# primero guardamos una copia de los archivos del ftp. De todos menos de duracion, porque ocupa mucho y no se utiliza. Los precios tampoco, as? que habr? que limpiar de vez en cnd
Archivo_PRECIOS<- paste0("F_PRECIOS_F",format(FechaHoy-1,format="%Y%m%d"),".TXT") # rutas ultimos archivos del ftp, precios no se usa
Archivo_PARAM  <- paste0("F_PARAM_F",   format(FechaHoy-1,format="%Y%m%d"),".TXT")
Archivo_Altae  <- paste0("carteras_altae.txt")
Archivo_c_exp  <- paste0("carteras_bbp_exp.txt")
Archivo_ACTFL  <- paste0("ACTFL_F",format(FechaHoy,format="%Y%m%d"),".TXT")
Archivo_ACTPOS <- paste0("ACTPOS_F",format(FechaHoy,format="%Y%m%d"),".TXT") # Archivo_ACTPOS <- paste0("ACTPOS_F",20171111,".TXT")

if( Archivo_PRECIOS %in% FicherosFTP ){
    download.file(url = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_PRECIOS,""),
                  destfile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/PRECIOS/",Archivo_PRECIOS),
                  quiet = T)
}else{}# alerta falta archivo
if( Archivo_PARAM %in% FicherosFTP ){
    download.file(url = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_PARAM,""),
                  destfile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/PARAM/",Archivo_PARAM),
                  quiet = T)
}else{}# alerta falta archivo
if( Archivo_Altae %in% FicherosFTP ){
    download.file(url = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_Altae,""),
                  destfile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/carteras/carteras_altae_",format(FechaHoy,format="%Y%m%d"),".txt"),
                  quiet = T)
}else{}# alerta falta archivo
if( Archivo_c_exp %in% FicherosFTP ){
    download.file(url = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_c_exp,""),
                  destfile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/carteras/carteras_bbp_exp_",format(FechaHoy,format="%Y%m%d"),".txt"),
                  quiet = T)
}else{}# alerta falta archivo
if( Archivo_ACTFL %in% FicherosFTP ){
    download.file(url = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_ACTFL,""),
                  destfile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/ACTFL/",Archivo_ACTFL),
                  quiet = T)
}else{}# alerta falta archivo
if( Archivo_ACTPOS %in% FicherosFTP ){
    download.file(url = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_ACTPOS,""),
                  destfile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/ACTPOS/",Archivo_ACTPOS),
                  quiet = T)
}else{}# alerta falta archivo


# Cargo tablas ----

ACTPOS   <-   read.csv2(file = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_ACTPOS,""), sep = ";", header = FALSE, stringsAsFactors = F)
ACTPOS[,1] <- gsub(" ","",ACTPOS[,1])
ACTPOS[,2] <- gsub(" ","",ACTPOS[,2])
ACTPOS <- ACTPOS[,1:2]
colnames( ACTPOS ) <- c("Codigo Interno", "ISIN")

# Comparo con ACTPOS anteriores para ver que ha salido y que ha entrado

ACTPOS_Anterior   <-   read.csv2(file = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/ACTPOS/ACTPOS_F",format(Sys.Date()-6,"%Y%m%d"),".TXT"), sep = ";", header = FALSE, stringsAsFactors = F)
ACTPOS_Anterior[,1] <- gsub(" ","",ACTPOS_Anterior[,1])
ACTPOS_Anterior[,2] <- gsub(" ","",ACTPOS_Anterior[,2])
ACTPOS_Anterior <- ACTPOS_Anterior[,1:2]
colnames( ACTPOS_Anterior ) <- c("Codigo Interno", "ISIN")
setdiff(ACTPOS$ISIN,ACTPOS_Anterior$ISIN) # los que han entrado "LU0650148827" "LU0650147852" "LU0329573587" "LU0273157635" "LU0144644332"
setdiff(ACTPOS_Anterior$ISIN,ACTPOS$ISIN) # los que han salido "US69343P1057" "XS1531330774" "LU0390137031" "LU0309468808" "LU0255978776" "LU0190162189" "LU0010009420" "ES0133443152" "BRVALEACNPA3"

# junto con los historicos

Histo  <- openxlsx::read.xlsx(xlsxFile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HISTNOESIS 30112017.xlsx"), colNames = T, rowNames = F)
colnames(Histo)[4:ncol((Histo))] <-as.character( as.Date( as.numeric(colnames(Histo)[4:ncol((Histo))] ) , origin = "1899-12-30") )
Histo$ISIN <- gsub(" ","",Histo$ISIN)
setorder(Histo, ISIN)

merge.data.frame(ACTPOS, Histo[,3:ncol(Histo)], by = "ISIN", all.x = T) -> TablaCarga

TablaCarga[,c(2,1,3:ncol(TablaCarga))] -> TablaCarga



T_Anterior <-  openxlsx::read.xlsx(xlsxFile = "R:/RIESGOS/Procesos_Automaticos_Bankia/Inputs/Hco_ACTPOS.xlsx", sheet = "Casos", startRow = 1, colNames = TRUE,
                                   rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                                   skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
T_Maestra <- merge.data.frame( ACTPOS , 
                               T_Anterior[,1:7 ], by = "ISIN", all.x = TRUE) 
T_Maestra <- T_Maestra[!duplicated(T_Maestra$ISIN),c("ISIN","CodB","Nombre/descripci?n", "Caso","Nombre_Altae_Final", "Div","UltPrecio")]
for(i in 8:(252+8)){
    T_Maestra[1,i] <-NA
}
# carga Calendario de la bolsa espa?ola 
CalendarioESP <- c();j<-1
for(i in 1:465){
    if( (format( FechaHoy-i+1, format = "%A") == "Saturday") || (format( FechaHoy-i+1, format = "%A") == "Sunday") || length(which(FechaHoy-i+1 == Festivos$fecha)) > 0 ){next}else{
        CalendarioESP[j] <- FechaHoy-i+1
        j <- j +1 
    }
}
CalendarioESP <- as.Date(CalendarioESP, origin = "1970-01-01")
colnames( T_Maestra )[8:(251+8)] <- if(format(FechaHoy,"%A")=="Saturday"){as.character(CalendarioESP[1:252])}else{as.character(CalendarioESP[2:253])} # <---------- ejec sabado ----


T_Maestra -> Tab

# + Feed JM e histo ----
HistoFeed  <- read.csv2(file = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/Inputs/HISTNOESIS_F20171121.TXT"), sep = ";", header = T, stringsAsFactors = FALSE)
colnames(HistoFeed) <- gsub("\\.","",colnames(HistoFeed))
colnames(HistoFeed)[3:ncol((HistoFeed))] <-as.character( as.Date(colnames(HistoFeed)[3:ncol((HistoFeed))] , format = "X%Y%m%d") )
HistoFeed$ISIN <- gsub(" ","",HistoFeed$ISIN)
for(i in 1:nrow(Tab)){
    if( length( which(HistoFeed$ISIN %in% Tab$ISIN[i]) ) == 1 ){
        which(HistoFeed$ISIN %in% Tab$ISIN[i]) -> NumFilaHist
        for(j in 8:(ncol(Tab))){
            if( length( which( colnames(HistoFeed) %in% colnames(Tab)[j] ) ) == 1 ){ 
                which( colnames(HistoFeed) %in% colnames(Tab)[j] ) -> NumColHist
                Tab[i , j] <-  HistoFeed[NumFilaHist, NumColHist]
            }
            
        }
    }
}

# <---- Hist 30/11/2017 ----
Histo30nov  <- openxlsx::read.xlsx(xlsxFile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HISTNOESIS 30112017.xlsx"), colNames = T, rowNames = F)
colnames(Histo30nov)[4:ncol((Histo30nov))] <-as.character( as.Date( as.numeric(colnames(Histo30nov)[4:ncol((Histo30nov))] ) , origin = "1899-12-30") )
Histo30nov$ISIN <- gsub(" ","",Histo30nov$ISIN)
for(i in 1:nrow(Tab)){
    if( length( which(Histo30nov$ISIN %in% Tab$ISIN[i]) ) == 1 ){
        which(Histo30nov$ISIN %in% Tab$ISIN[i]) -> NumFilaHist
        for(j in 8:(ncol(Tab))){
            if( length( which( colnames(Histo30nov) %in% colnames(Tab)[j] ) ) == 1 ){ 
                which( colnames(Histo30nov) %in% colnames(Tab)[j] ) -> NumColHist
                Tab[i , j] <-  Histo30nov[NumFilaHist, NumColHist]
            }
            
        }
    }
}

# para comprobar si esta desde historico marta

Tab[ which(Tab$ISIN %in% Histo30nov$ISIN[2374:2377]) ,] -> look

for(i in 2374:2377){ # VLP a HistMArta
    for(j in 8:(ncol(Tab))){
        if( length( which( colnames(Histo30nov) %in% colnames(Tab)[j] ) ) == 1 ){ 
            which( colnames(Histo30nov) %in% colnames(Tab)[j] ) -> NumColHist
            Tab[ which( Tab$ISIN %in% Histo30nov$ISIN[i] ), j] ->  Histo30nov[i, NumColHist]
        }
        
    }
}

opt <- c("periodicityAdjustment"="CALENDAR","CDR"="SP","periodicitySelection"="DAILY" )
faltan <- c(); k <- 1
for(i in 2378:2410){
    Carga <- bdh(paste0(Histo30nov$ISIN[i]," Equity"), "PX_LAST", FechaHoy-385, FechaHoy,options = opt )
    if( (is.na(Carga[1,1])) || (as.character(Carga[1,1]) == "") || (is.null(Carga[1,1])) ){ 
        faltan[j] <- Histo30nov$ISIN[i]
        k <- k + 1
        next 
    }
    setorder(Carga, -date)
    
    for(j in 1:( nrow(Carga) ) ){
        if( length( which( colnames(Tab) %in% as.character(Carga$date[j]) ) ) == 1 ){
            Tab[ which(Tab$ISIN %in% Histo30nov$ISIN[i]), which( colnames(Tab) %in% as.character(Carga$date[j]) ) ] <- Carga$PX_LAST[j] 
        }
    }
    for(j in 1:( nrow(Carga) ) ){
        if( length( which( colnames(Histo30nov) %in% as.character(Carga$date[j]) ) ) == 1 ){
            Histo30nov[ i, which( colnames(Histo30nov) %in% as.character(Carga$date[j]) ) ] <- Carga$PX_LAST[j] 
        }
    }
}
faltanBl <- c(); k <- 1
for(i in 2411:2878){
    Carga <- bdh(paste0(Histo30nov$Bloom[i]), "PX_LAST", FechaHoy-385, FechaHoy,options = opt )
    if( (is.na(Carga[1,1])) || (as.character(Carga[1,1]) == "") || (is.null(Carga[1,1])) ){ 
        faltan[j] <- Histo30nov$ISIN[i]
        k <- k + 1
        next 
    }
    setorder(Carga, -date)
    
    for(j in 1:( nrow(Carga) ) ){
        if( length( which( colnames(Tab) %in% as.character(Carga$date[j]) ) ) == 1 ){
            Tab[ which(Tab$ISIN %in% Histo30nov$ISIN[i]), which( colnames(Tab) %in% as.character(Carga$date[j]) ) ] <- Carga$PX_LAST[j] 
        }
    }
    for(j in 1:( nrow(Carga) ) ){
        if( length( which( colnames(Histo30nov) %in% as.character(Carga$date[j]) ) ) == 1 ){
            Histo30nov[ i, which( colnames(Histo30nov) %in% as.character(Carga$date[j]) ) ] <- Carga$PX_LAST[j] 
        }
    }
}


wb <- openxlsx::loadWorkbook( file = "R:/RIESGOS/Procesos_Automaticos_Bankia/HISTNOESIS 30112017.xlsx" )
# Tab <- openxlsx::readWorkbook(xlsxFile = wb , sheet = "MAPEADOS" , startRow = 1 , colNames = TRUE ,
#                               rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                               skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
openxlsx::writeData(wb, sheet=1, Histo30nov, startCol = 1, startRow = 2, xy = NULL,
                    colNames = F, rowNames = FALSE, headerStyle = NULL                    )
openxlsx::saveWorkbook(wb,file = "R:/RIESGOS/Procesos_Automaticos_Bankia/HISTNOESIS 30112017.xlsx" , overwrite = T)



Histo30nov  <- openxlsx::read.xlsx(xlsxFile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HISTNOESIS 30112017.xlsx"), colNames = T, rowNames = F)
colnames(Histo30nov)[4:ncol((Histo30nov))] <-as.character( as.Date( as.numeric(colnames(Histo30nov)[4:ncol((Histo30nov))] ) , origin = "1899-12-30") )
Histo30nov$ISIN <- gsub(" ","",Histo30nov$ISIN)

# Del Hist?rico a nuestra tabla del ACTPOS
for(i in 1:2377){ # VLP a HistMArta
    for(j in 8:(ncol(Tab))){
        if( length( which( colnames(Histo30nov) %in% colnames(Tab)[j] ) ) == 1 ){ 
            which( colnames(Histo30nov) %in% colnames(Tab)[j] ) -> NumColHist
            Tab[ which( Tab$ISIN %in% Histo30nov$ISIN[i] ), j] ->  Histo30nov[i, NumColHist]
        }
        
    }
}

Tab[ which(Tab$ISIN %in% setdiff(ACTPOS$ISIN,Histo30nov$ISIN) ),] -> look
openxlsx::write.xlsx(look,    file = "R:/RIESGOS/Procesos_Automaticos_Bankia/Lo que falta 30112017.xlsx", colNames = TRUE)


for(i in 1:2377){ # VLP a HistMArta
    for(j in 8:(ncol(Tab))){
        if( length( which( colnames(Histo30nov) %in% colnames(Tab)[j] ) ) == 1 ){ 
            which( colnames(Histo30nov) %in% colnames(Tab)[j] ) -> NumColHist
            Tab[ which( Tab$ISIN %in% Histo30nov$ISIN[i] ), j] ->  Histo30nov[i, NumColHist]
        }
        
    }
}
for(i in 1:nrow(Histo30nov)){
    if( length( which(VLP_Hco$ISIN %in% Histo30nov$ISIN[i]) ) == 1 ){
        which(VLP_Hco$ISIN %in% Histo30nov$ISIN[i]) -> NumFilaHist
        for(j in 4:(ncol(Histo30nov))){
            if( length( which( colnames(VLP_Hco) %in% colnames(Histo30nov)[j] ) ) == 1 ){ 
                which( colnames(VLP_Hco) %in% colnames(Histo30nov)[j] ) -> NumColHist
                Histo30nov[i , j] <-  VLP_Hco[NumFilaHist, NumColHist]
            }
            
        }
    }
}
for(i in 1:nrow(Histo30nov)){ 
    if(is.na(Histo30nov[i,11])){
        for(j in 4:11){
            if( j %% 2 == 0 ){
                Histo30nov[i,j] <- Histo30nov[i,12]
            }else{
                Histo30nov[i,j] <- Histo30nov[i,13]
            }
        }
    }
}
Histo30nov[is.na(Histo30nov[,11]),c(4:13)] -> look

Histo30nov -> TabCarga



Histo30nov  <- openxlsx::read.xlsx(xlsxFile = paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HISTNOESIS 30112017.xlsx"), colNames = T, rowNames = F)
colnames(Histo30nov)[4:ncol((Histo30nov))] <-as.character( as.Date( as.numeric(colnames(Histo30nov)[4:ncol((Histo30nov))] ) , origin = "1899-12-30") )
Histo30nov$ISIN <- gsub(" ","",Histo30nov$ISIN)
setorder(Histo30nov, ISIN)

merge.data.frame(ACTPOS, Histo30nov[,3:ncol(Histo30nov)], by = "ISIN", all.x = T) -> TablaCarga

TablaCarga[,c(2,1,3:ncol(TablaCarga))] -> TablaCarga


















# T_Maestra$CodB[ is.na(T_Maestra$CodB) ] <- CodsB$CodB[  match(T_Maestra$ISIN[is.na(T_Maestra$CodB)],CodsB$ISIN)  ]
# which( unique(T_Con_Casos$ISIN) %in% unique(FeedsBankiaVamp$Isin) )
# which( unique(Tab$ISIN) %in% unique(HistoFeed$ISIN) )
# de Vamp
for(i in 1:nrow(Tab)){
    if( length( which(FeedsBankiaVamp$Isin %in% Tab$ISIN[i]) ) != 0 ){ # aqui va diferente, hay un monton de filas por isin
        which(FeedsBankiaVamp$Isin %in% Tab$ISIN[i]) -> NumFilasHist
        for(k in 1:length(NumFilasHist) ){
            if( length( which( colnames(Tab) %in% as.character(FeedsBankiaVamp$FechaCotizacion[ NumFilasHist[k] ]) ) ) == 1 ){ 
                which( colnames(Tab) %in% as.character(FeedsBankiaVamp$FechaCotizacion[ NumFilasHist[k] ]) ) -> NumColTab
                Tab[i , NumColTab] <-  FeedsBankiaVamp[NumFilasHist[k] , "Precio"]
            }
        }
    }
}


# + Hco VLPs ----
VLP_Hco <- openxlsx::read.xlsx("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/VLP/VLP_Hco.xlsx", colNames = T, rowNames = F)
# Coger archivo de hoy y cargarlo al historico. Tambien guarda copia del txt por si acaso
VLP_temp <- read.csv2(file = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,"VLPs/F_VLP_NOESIS",""), sep = ";", header = FALSE, stringsAsFactors = FALSE)
write.table(  VLP_temp  , file= paste0("R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/VLP/hist?rico VLPs/",format(Sys.Date(),"%Y%m%d")," F_VLP_NOESIS.txt") , col.names = FALSE , row.names = FALSE , sep = ";" , quote = FALSE)
fechas <- as.Date( unique( VLP_temp$V1 ), format = "%d/%m/%Y")
for(i in 1:length(fechas)){ # a?ado nuevas fechas
    if( !( as.character(fechas[i]) %in% colnames(VLP_Hco) ) ){
        VLP_Hco[1,length(VLP_Hco)+1]<- NA
        colnames(VLP_Hco)[length(VLP_Hco)] <- as.character(fechas[i])
    }
}
Nuevos_IsinsVLP <- setdiff(unique(VLP_temp$V2),unique(VLP_Hco$ISIN)) #  si nuevo ISIN, hay que a?adirlo
if( length( Nuevos_IsinsVLP ) > 0 ){
    for(i in 1:length(Nuevos_IsinsVLP)){
        VLP_Hco[nrow(VLP_Hco)+1,] <- NA
        VLP_Hco[nrow(VLP_Hco),"ISIN"] <- Nuevos_IsinsVLP[i]
    }
}
VLP_Hco[ , order(names(VLP_Hco), decreasing = TRUE)] -> VLP_Hco
# VLP_Hco[ order((VLP_Hco$ISIN)) , ] -> VLP_Hco
for(i in 1:nrow(VLP_temp)){ # cargo datos
    if( length( VLP_Hco[ which( VLP_Hco$ISIN %in% as.character(VLP_temp$V2[i]) ) , which(colnames(VLP_Hco) %in% as.character(as.Date(VLP_temp$V1[i],format = "%d/%m/%Y")) )  ] ) == 1 ){
        VLP_Hco[ which( VLP_Hco$ISIN %in% as.character(VLP_temp$V2[i]) ) , which(colnames(VLP_Hco) %in% as.character(as.Date(VLP_temp$V1[i],format = "%d/%m/%Y")) )  ] <- VLP_temp$V3[i]
    }
}
# LimpiaNAs(Tabla = VLP_Hco) -> VLP_Hco
# para a?adir historico de nuevos
# temp <- openxlsx::read.xlsx("L:/AlejandroP/Mapeo.xlsx", colNames = T, rowNames = F, detectDates = T)
# temp[,c(5,3,4)] -> temp
# for(i in 1:nrow(temp)){
#     VLP_Hco[which(VLP_Hco$ISIN=="ES0113387015") ,which(colnames(VLP_Hco)==as.character(temp[i,1]))] <- temp$ES0113387015[i]
#     VLP_Hco[which(VLP_Hco$ISIN=="ES0113387007") ,which(colnames(VLP_Hco)==as.character(temp[i,1]))] <- temp$ES0113387007[i]
# }
openxlsx::write.xlsx(VLP_Hco, file = "R:/RIESGOS/Procesos_Automaticos_Bankia/HistoricosFTP/VLP/VLP_Hco.xlsx",
                     sheetName = "Historico", colNames = TRUE)

VLP_Hco[which(VLP_Hco$ISIN %in% c("ES0105578035",
"ES0110544030",
"ES0113256004",
"ES0113385001",
"ES0113385019",
"ES0117184038",
"ES0125726036",
"ES0155921036",
"ES0156980031",
"ES0158013005",
"ES0158965030",
"ES0158971038",  
"ES0158976037", 
"ES0158986036", 
"ES0159037037",
"ES0159037045",
"ES0159084039",
"ES0159178005",
"ES0159178039",
"ES0171888037",
"ES0185373034")),3] 


# Carga en tabla
for(i in 1:nrow(Tab)){
    if( length( which(VLP_Hco$ISIN %in% Tab$ISIN[i]) ) == 1 ){
        which(VLP_Hco$ISIN %in% Tab$ISIN[i]) -> NumFilaHist
        for(j in 8:(ncol(Tab))){
            if( length( which( colnames(VLP_Hco) %in% colnames(Tab)[j] ) ) == 1 ){ 
                which( colnames(VLP_Hco) %in% colnames(Tab)[j] ) -> NumColHist
                Tab[i , j] <-  VLP_Hco[NumFilaHist, NumColHist]
            }
            
        }
    }
}


# + C de Altae ----
Altae  <- read.csv2(file = paste0("ftp://",User,":",Password,"@",FTPServer,"/",Ruta,Archivo_Altae,""), sep = ";", header = FALSE, stringsAsFactors = F)
UltCierre <- as.Date(Altae[1,2],format="%d.%m.%Y")
ISINs_Altae <- (( unique(Altae$V4)[2:length(unique(Altae$V4))] ))
ISINs_Altae <- ISINs_Altae[!(grepl("NULL",ISINs_Altae))] 
ISINs_Altae <- ISINs_Altae[ which(as.numeric(ISINs_Altae[1:30]) > 500000)[1] : length(ISINs_Altae)] # Posici?n del primer numero mayor que 500000
Altae <- unique(Altae[Altae$V4 %in% ISINs_Altae,c("V4","V8","V13","V14")], by = c("V4"))
Altae <- Altae[!duplicated(Altae$V4),]
Altae <- Altae[order(Altae$V4),]
colnames(Altae) <- c(format(UltCierre, format="%d/%m/%Y" ),"","","")
ISINs_Altae <- ((Altae[ grepl("[A-z]",Altae[,1]) , 1] ))
ISINs_Altae <- gsub(" ","",ISINs_Altae) 
row.names(Altae) <- 1:nrow(Altae)
Altae[,1] <- gsub(" ","",Altae[,1])

Tab -> Tabla
for(i in 1:nrow(Tab)){
    if( length( which(Altae[,1] %in% Tab$ISIN[i]) ) == 1 ){
        which(Altae[,1] %in% Tab$ISIN[i]) -> NumFilaHist
        
        # for(j in 9:(ncol(Tab))){
        # if( length( which( colnames(Altae[1]) %in% colnames(Tab)[9] ) ) == 1 ){
        j <- 8 # <--- -----
        # which( colnames(VLP_Hco) %in% colnames(Tab)[j] ) -> NumColHist
        Tab[i , j] <-  Altae[NumFilaHist, 3]
        # }
        
        # }
    }
}

# + Proxys ----
ProxyT <- openxlsx::read.xlsx(xlsxFile = "R:/RIESGOS/ALTAE/Bkia Noesis MAPPING+VAR100.xlsx" , sheet = "MAPEADOS" , startRow = 1 , colNames = TRUE ,
                              rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                              skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)

# Esta tabla se va a modificar para a?adir fecha pivote y codigo BN. La tabla se construye de la siguiente manera
Tab[ is.na(Tab[,255]) & !is.na(Tab[,155]) ,] -> FiltroProxy # <------- mejor forma contando blanks ----
NA -> FiltroProxy[,ncol(FiltroProxy)+1]
colnames(FiltroProxy)[ncol(FiltroProxy)] <- "FechaProxy"
FiltroProxy <- FiltroProxy[,c(1:3,ncol(FiltroProxy),4:(ncol(FiltroProxy)-1))]
NA -> FiltroProxy[,ncol(FiltroProxy)+1]
colnames(FiltroProxy)[ncol(FiltroProxy)] <- "CodBNProxy"
FiltroProxy <- FiltroProxy[,c(1:4,ncol(FiltroProxy),5:(ncol(FiltroProxy)-1))]
for(i in 1:nrow(FiltroProxy)){
    colnames(FiltroProxy)[ which(is.na(FiltroProxy[i,152:ncol(FiltroProxy)]))[1]+151 ] -> FiltroProxy[i,4] 
    if( FiltroProxy$ISIN[i] %in% ProxyT$COD_ISIN ){ FiltroProxy[i,5] <- ProxyT[which( ProxyT$COD_ISIN %in% FiltroProxy$ISIN[i] ),4][1] } # [1] en caso de que haya ISIN repetidos
}
# LimpiaNAs(FiltroProxy[,10:(ncol(FiltroProxy))]) -> FiltroProxy[,10:(ncol(FiltroProxy))]
CruceProxy(TablaP = FiltroProxy) -> FiltroProxyCalculado

openxlsx::write.xlsx( list("SinProxy" = FiltroProxy,"ConProxy" = FiltroProxyCalculado) , file = "R:/RIESGOS/Procesos_Automaticos_Bankia/Outputs/TablaProxys2.xlsx", colNames = TRUE)

# Carga en tabla
for(i in 1:nrow(Tab)){
    if( length( which( FiltroProxyCalculado$ISIN %in% Tab$ISIN[i]) ) != 0 ){
        which( FiltroProxyCalculado$ISIN %in% Tab$ISIN[i])[1] -> NumFilaHist
        for(j in 8:(ncol(Tab))){
            if( length( which( colnames(FiltroProxyCalculado) %in% colnames(Tab)[j] ) ) == 1 ){ 
                which( colnames(FiltroProxyCalculado) %in% colnames(Tab)[j] ) -> NumColHist
                Tab[i , j] <-  FiltroProxyCalculado[NumFilaHist, NumColHist]
            }
            
        }
    }
}




# + Otros Excels para cods B  ----
xls <- "R://RIESGOS//Altae_BLOOMBERG.xls"
BD1 <- read_xls(path = xls, sheet="BLOOMBERG", range = "B5:Q1000", col_names = TRUE) # solo nos interesa esta hoja
BD1 <- BD1[complete.cases(BD1[,c("NOMBRE BLOOMBERG","ISIN NOESIS")]),]
DeAltaeB <- BD1[,c("NOMBRE BLOOMBERG","COD BLOOMBERG","ISIN NOESIS")]
colnames(DeAltaeB) <- c("Nombre", "CodB", "ISIN")
DeAltaeB <- DeAltaeB[!duplicated(DeAltaeB$ISIN),]


CodsB <- openxlsx::read.xlsx("R:/RIESGOS/Procesos_Automaticos_Bankia/Inputs/Origen_datos/CodsBloom.xlsx", colNames = FALSE)
colnames(CodsB) <- c("ISIN","CodB")
CodsB <- CodsB[order(CodsB$ISIN),]
row.names(CodsB) <- 1:nrow(CodsB)
# CodsB <- CodsB[unique(CodsB$ISIN),]
CodsB <- CodsB[!duplicated(CodsB$ISIN),]



# + HistoricoAltaeFinal Lectura historico carteras ----
# para comprobar que ACTPOS respecto de la cartera esta  actualizado pasado un d?a
# el que usamos nosotros, pesta?a NuevoHist
Ruta_Excel_Datos <- "R:/RIESGOS/ALTAE/HistoricoAltaeFinal.xlsm" # el que usamos nosotros, pesta?a NuevoHist
Hist <- openxlsx::read.xlsx(xlsxFile = Ruta_Excel_Datos, sheet = "NuevoHist", startRow = 1, colNames = TRUE,
                            rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                            skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
# arreglo formato de fechas  (Pero manteniendo categoria de character)
PosFinFecha <- which(grepl("^X",colnames(Hist)))[1]-1
names(Hist)[5:PosFinFecha] <-  as.character( as.Date( as.numeric( colnames(Hist)[5:PosFinFecha] ), origin = "1899-12-30") ) # origin = "1900-01-01"

HistAux <- openxlsx::read.xlsx(xlsxFile = Ruta_Excel_Datos, sheet = "CodBBDD", startRow = 1, colNames = TRUE,
                               rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                               skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)

names(HistAux)[3:length(HistAux)] <-  as.character( as.Date( as.numeric( colnames(HistAux)[3:length(HistAux)] ), origin = "1899-12-30") ) # origin = "1900-01-01"
names(HistAux)[1]  <-  "Caso"


# carga algunos codigos bloom de otros excels 
for(i in 1:nrow(Tab)){
    if( is.na( Tab$CodB[i] ) ){
        if( length( which( DeAltaeB$ISIN %in% Tab$ISIN[i] ) ) != 0 ){
            Tab$CodB[i] <- DeAltaeB$CodB[ which( DeAltaeB$ISIN %in% Tab$ISIN[i] ) ]
        }else if( length( which( CodsB$ISIN %in% Tab$ISIN[i] ) ) != 0 ){
            Tab$CodB[i] <- CodsB$CodB[ which( CodsB$ISIN %in% Tab$ISIN[i] ) ]
        }
    }
}


T_Maestra <- Tab[, 1:(ncol(Tab)-1)]
# ->>> carga datos bloomberg <- ----

opt <- c("periodicityAdjustment"="CALENDAR","CDR"="SP","periodicitySelection"="DAILY" )
faltan <- c(); j <- 1
for(i in 1:nrow(T_Maestra)){# i <- 1
    Valor_Actual_PX<- bdp( T_Maestra[i, 2], "PX_LAST")
    if ( (is.na(Valor_Actual_PX)) || (Valor_Actual_PX == "") || (is.null(Valor_Actual_PX)) ){ 
        faltan[j] <- T_Maestra[i, 2]
        j <- j + 1
        next 
        # write(paste0("No hay dato en Bloomberg de :",Macro_Series_Calendario[i, "codSerie"],", con Cod_Bloom: ",Cod_Bloom),
        #       file=paste0(ruta_carga,"//Alerta_Falta_Dato_Bloom.txt"),append=TRUE)
        # si el codigo Bloomberg no devuelve nada o si esta marcado como inactivo en Calendario
    }
    T_Maestra[i,3] <- bdp(T_Maestra[i, 2],"NAME")
    T_Maestra[i,6] <- bdp(T_Maestra[i, 2],"CRNCY")
    T_Maestra[i,7] <- Valor_Actual_PX
    
    # Carga <- bdh(T_Maestra[i, 3], "PX_LAST", FechaHoy-365, FechaHoy)
    # opt <- c("periodicityAdjustment"="CALENDAR","periodicitySelection"="MONTHLY" ,"currency"="USD")
    # opt <-  c("DAYS"="W", "CDR"="SP")
    # Carga <- bdh("RSTRIOE LX Equity", "PX_LAST", FechaHoy-385, FechaHoy,options = opt)
    Carga <- bdh(T_Maestra[i, 2], "PX_LAST", FechaHoy-385, FechaHoy,options = opt)
    setorder(Carga, -date)
    
    for(k in 1:( nrow(Carga) ) ){
        if( length( which( colnames(T_Maestra) %in% as.character(Carga$date[k]) ) ) == 1 ){
            T_Maestra[i, which( colnames(T_Maestra) %in% as.character(Carga$date[k]) ) ] <- Carga$PX_LAST[k] 
        }
    }
    # l <- 9
    # for(k in 1:( nrow(Carga) ) ){# k recorro carga, l es posicion teorica del siguiente dato, sirve para ajustar los dias que son festivos en la bolsa correspondiente pero laboral en Espa?a
    #     if(  length( 5+which(  Carga[k,1] == as.Date(colnames(T_Maestra)[6:length(T_Maestra)]) ) ) == 0 ){ # no hay match porque fue festivo en Espa?a y no en el pais de la accion, por tanto es como si ...[k]<l, me lo salto
    #         l <- l + 1
    #     }else if( (5+ which(  Carga[k,1] == as.Date(colnames(T_Maestra)[6:length(T_Maestra)]) )) == l ){ # coinciden calendarios, lo normal
    #     # system.time( replicate(10000, for(k in 1:nrow(Carga)){ match(  Carga[k,1], as.Date(colnames(T_Maestra)[6:250]) )    } ))
    #         T_Maestra[i, l] <-  Carga[k, 2]
    #         l <- l + 1
    #     }else if( (5+ which(  Carga[k,1] == as.Date(colnames(T_Maestra)[6:length(T_Maestra)]) )) > l ){ # ...[k] > l (ha habido un Festivo USA y no ESP)   alternativa con match--> ( length(which( Carga[k,1] == as.Date(colnames(T_Maestra)[6:250]) )) > 0 )
    #         # en este caso, adem?s de cargar el dato correspondiente, hay que cargar el dato (o datos) anterior con el dato del dia de antes
    #         while (l < ( 5+which(  Carga[k,1] == as.Date(colnames(T_Maestra)[6:length(T_Maestra)]) )) ) { 
    #             if( !is.na(Carga[k+1, 2]) ){ T_Maestra[i,l] <-  Carga[k+1, 2] }else{ T_Maestra[i,l] <-  Carga[k, 2] }
    #             l <- l + 1
    #         }
    #         T_Maestra[i, l] <-  Carga[k, 2]
    #         l <- l + 1
    #     }
    # }
    
}

# Divisas ----
T_Pasada_A_Eur <- T_Maestra
# setdiff(unique(T_MaestraV0$CRNCY),IdDivisas$CodigoISO)
for(i in 1:length(T_Maestra)){
    T_Pasada_A_Eur[i,8:length(T_Pasada_A_Eur)] <- Cambio_A_EUR(Cotizaciones = T_Maestra[i,8:length(T_Maestra)], From = T_Maestra[i,"Div"], To="EUR")
}


# Casos ----
T_Con_Casos <- T_Pasada_A_Eur
# a?ado columna para casos

# ActualizaDatosNormales en excel Historico Altae final
for(i in 1:nrow(T_Con_Casos)){
    if( length( which(Hist$ISIN %in% T_Con_Casos$ISIN[i]) ) == 1 ){
        which(Hist$ISIN %in% T_Con_Casos$ISIN[i]) -> NumFilaHist
        Hist$ES[NumFilaHist] -> T_Con_Casos[i, "Caso"]
        Hist$NOMBRE[NumFilaHist] -> T_Con_Casos[i, "Nombre_Altae_Final"]
        if(  Hist[NumFilaHist, "ES"] == 15  ){
            for(j in 8:(length(T_Con_Casos))){
                if( j %% 2 == 0 ){ # si es par o imparr 100 o 1
                    T_Con_Casos[i, j ] <- 100
                }else{T_Con_Casos[i, j ] <- 1}
            }
        }else if( Hist[NumFilaHist, "ES"] == 16 ){
            for(j in 8:(length(T_Con_Casos))){
                k <- 1
                NumFil <- which(format(Caso16Mas5$Fecha,"%Y-%m-%d") %in% colnames(T_Con_Casos)[j])
                while( length(NumFil)==0 ){ NumFil <- which(format(Caso16Mas5$Fecha,"%Y-%m-%d") %in% colnames(T_Con_Casos)[j+k]);k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
                T_Con_Casos[i, j ] <- Caso16Mas5$Cierre[NumFil] # si mete varios match, da warning, error en la primera columna
            }
            # for(j in 8:(length(T_Con_Casos))){
            #     k <- 1
            #     NumColAux <- which(colnames(HistAux) %in% colnames(T_Con_Casos)[j])
            #     while( length(NumColAux)==0 ){ NumColAux <- which(colnames(HistAux) %in% colnames(T_Con_Casos)[j+k]);k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
            #     T_Con_Casos[i, j ] <- HistAux[15,NumColAux] # si mete varios match, da warning, error en la primera columna
            # }
        }else if( Hist[NumFilaHist, "ES"] == 17 ){
            for(j in 8:(length(T_Con_Casos))){
                k <- 1
                NumFil <- which(format(Caso17Mab2$Fecha,"%Y-%m-%d") %in% colnames(T_Con_Casos)[j])
                while( length(NumFil)==0 ){ NumFil <- which(format(Caso17Mab2$Fecha,"%Y-%m-%d") %in% colnames(T_Con_Casos)[j+k]);k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
                T_Con_Casos[i, j ] <- Caso17Mab2$Cierre[NumFil] # si mete varios match, da warning, error en la primera columna
            }
            # for(j in 8:(length(T_Con_Casos))){
            #     k <- 1
            #     NumColAux <- which(colnames(HistAux) %in% colnames(T_Con_Casos)[j])
            #     while( length(NumColAux)==0 ){ NumColAux <- which(colnames(HistAux) %in% colnames(T_Con_Casos)[j+k]);k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
            #     T_Con_Casos[i, j ] <- HistAux[16,NumColAux] # si mete varios match, da warning, error en la primera columna
            # }
        }else if( Hist[NumFilaHist, "ES"] == 10 ){
            for(j in 8:(length(T_Con_Casos))){
                k <- 1
                NumFil <- which(format(Caso10Ibex$Fecha,"%Y-%m-%d") %in% colnames(T_Con_Casos)[j])
                while( length(NumFil)==0 ){ NumFil <- which(format(Caso10Ibex$Fecha,"%Y-%m-%d") %in% colnames(T_Con_Casos)[j+k]);k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
                T_Con_Casos[i, j ] <- Caso10Ibex$Cierre[NumFil] # si mete varios match, da warning, error en la primera columna
            }
            # for(j in 8:(length(T_Con_Casos))){
            #     k <- 1
            #     NumColAux <- which(colnames(HistAux) %in% colnames(T_Con_Casos)[j])
            #     while( length(NumColAux)==0 ){ NumColAux <- which(colnames(HistAux) %in% colnames(T_Con_Casos)[j+k]);k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
            #     T_Con_Casos[i, j ] <- HistAux[10,NumColAux] # si mete varios match, da warning, error en la primera columna
            # }
        }else if( Hist[NumFilaHist, "ES"] == 19 ){ # los VLPs, abajo para comprobar que todos esten en el Historico
            # setdiff(T_Con_Casos$ISIN[T_Con_Casos$Caso==19 & !is.na(T_Con_Casos$Caso)], VLP_Hco$ISIN)
            SerieCarga <- VLP_Hco[ VLP_Hco$ISIN %in% T_Con_Casos$ISIN[i],2:(length(VLP_Hco))]
            if( nrow(SerieCarga)==1 ){
                for(j in 8:(length(T_Con_Casos))){
                    k <- 1
                    NumCol <- which( colnames(SerieCarga) %in% colnames(T_Con_Casos)[j] )
                    while( length(NumCol)==0 ){ NumCol <- which( colnames(SerieCarga) %in% colnames(T_Con_Casos)[j+k] );k <- k + 1 }# si falla el match por alguna fecha, voy a la siguiente
                    T_Con_Casos[i, j ] <- SerieCarga[NumCol] # si mete varios match, da warning, error en la primera columna
                } 
            }else{}# VLP que no esta en la tabla
        }else if(  grepl("MAP continuo", Hist$NOMBRE[NumFilaHist] )  ){
            ISIN_Map_Cont <- substr(Hist$NOMBRE[NumFilaHist], gregexpr("MAP continuo", Hist$NOMBRE[NumFilaHist])[[1]]+13 , gregexpr("MAP continuo", Hist$NOMBRE[NumFilaHist])[[1]] +13+11)
            T_Con_Casos[i, 8:(length(T_Con_Casos))] <- T_Con_Casos[T_Con_Casos$ISIN %in% ISIN_Map_Cont,8:(length(T_Con_Casos))]
        }
    }else{ next }
}

list_of_datasets <- list("Original" = T_Maestra, "Divisas" = T_Pasada_A_Eur, "Casos" = T_Con_Casos)
openxlsx::write.xlsx(list_of_datasets,    file = "R:/RIESGOS/Procesos_Automaticos_Bankia/Outputs/Hco_ACTPOS4.xlsx", colNames = TRUE)

wb <- openxlsx::loadWorkbook( file = "R:/RIESGOS/Procesos_Automaticos_Bankia/Hco_ACTPOS.xlsx" )
openxlsx::writeData(wb, sheet="Original", Tab, startCol = 1, startRow = 1, xy = NULL,
                    colNames = TRUE, rowNames = FALSE, headerStyle = NULL)
openxlsx::writeData(wb, sheet="MAPEADOS", Tab, startCol = 1, startRow = 1, xy = NULL,
                    colNames = TRUE, rowNames = FALSE, headerStyle = NULL
                    ,borders =  "all",
                    borderColour = getOption("openxlsx.borderColour", "green"),
                    borderStyle = getOption("openxlsx.borderStyle", "dashDot"),
                    withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ", ")
openxlsx::writeData(wb, sheet="MAPEADOS", Tab, startCol = 1, startRow = 1, xy = NULL,
                    colNames = TRUE, rowNames = FALSE, headerStyle = NULL
                    ,borders =  "all",
                    borderColour = getOption("openxlsx.borderColour", "green"),
                    borderStyle = getOption("openxlsx.borderStyle", "dashDot"),
                    withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ", ")
openxlsx::saveWorkbook(wb,file = "R:/RIESGOS/Procesos_Automaticos_Bankia/Hco_ACTPOS.xlsx" , overwrite = T)

# [[Bloomberg]]	Lo mantiene NOESIS con sus fuentes, principalmente BLOOMBERG
# [[MAP fecha nombre]]	Mapeado desde la fecha indicada con los retornos del activo indicado en nombre
# [[MAP fecha auto]]	Mapeado desde la fecha indicada con sus propios retornos
# [[MAP continuo isin nombre]]	Mapeado sistem?ticamente con el activo indicado en isin y nombre
# [[Plano fecha]]	Cotiza plano desde la fecha indicada
# [[CAMBIO fecha]]	Se produjo alg?n cambio en la fecha indicada, remitirse al comentario
# [[COMENTARIO]]	Interesa que aparezca el comentario en el informe mensual a ALTAE
# [[Repeticion]]	Se repite el ?ltimo dato porque utiliza alguna fuente externa (VaR ALTAE para estructuras, por ejemplo)
# [[MAP mixto isin1 porc1 isin2 porc2]]	Se crea un ?ndice sint?tico usando porcentajes de dos ISIN presentes en la BBDD
# [[Transformacion codigo]]	Se multiplica el dato empleado por una referencia (t?picamente EURUSD para RF en d?lares)
# /High Yield/	Ser?n los activos con alta volatilidad




fil <- "R:/RIESGOS/Procesos_Automaticos_Bankia/Bkia Noesis MAPPING+VAR100.xlsx"
# Con esto No respeto formatos
Tabla <- openxlsx::read.xlsx(xlsxFile = fil , sheet = "MAPEADOS" , startRow = 1 , colNames = TRUE ,
                             rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                             skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
list_of_datasets <- list("Original" = T_Maestra, "Divisas" = T_Pasada_A_Eur, "Casos" = T_Con_Casos)
openxlsx::write.xlsx(list_of_datasets,    file = fil, colNames = TRUE)


# con esto SI respeto formatos
wb <- openxlsx::loadWorkbook( file = fil )
# Tab <- openxlsx::readWorkbook(xlsxFile = wb , sheet = "MAPEADOS" , startRow = 1 , colNames = TRUE ,
#                               rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                               skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
openxlsx::writeData(wb, sheet="MAPEADOS", Tab, startCol = 1, startRow = 1, xy = NULL,
                    colNames = TRUE, rowNames = FALSE, headerStyle = NULL
                    ,borders =  "all",
                    borderColour = getOption("openxlsx.borderColour", "green"),
                    borderStyle = getOption("openxlsx.borderStyle", "dashDot"),
                    withFilter = FALSE, keepNA = FALSE, name = NULL, sep = ", ")
openxlsx::saveWorkbook(wb,file = fil, overwrite = T)




# Gcion txt Hco Bankia ----
# Ej1 <- read.csv2(file = "R:/BANKIA/HISTORICO Original.txt", sep = ";", header = FALSE)
# ExcHco <-  openxlsx::read.xlsx(xlsxFile = "R:/BANKIA/HistoPrueba2.xlsx", sheet = "4388", startRow = 1, colNames = F,
#                                rowNames = FALSE, detectDates = F, skipEmptyRows = TRUE,
#                                skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
# HcoXaTxt <- ExcHco
# 
# HcoXaTxt[ 2:nrow(HcoXaTxt) , 3:ncol(HcoXaTxt) ] <- ArregDatos(Tabla = HcoXaTxt[ 2:nrow(HcoXaTxt) , 3:ncol(HcoXaTxt) ])
# HcoXaTxt[ 1 ,  ] <- ArregFila(Tabla = HcoXaTxt)
# HcoXaTxt[ 2:nrow(HcoXaTxt) , 1:2 ] <- ArregCol(Tabla = HcoXaTxt[ 2:nrow(HcoXaTxt) , 1:2 ])
# 
# write.table(  HcoXaTxt  , file= "C:\\AlxP\\HISTORICO_20171121.txt" , col.names = FALSE , row.names = FALSE , sep = ";" , quote = FALSE)








ArregDatos( HcoXaTxt[1:10,1:8])
ArregDatos( HcoXaTxt[3,3])
HcoXaTxt[ which(rownames(HcoXaTxt) %in% c(2678:2680)) ,] -> look
ArregDatos(look[,11:14])

sapply((look[,7:14]),class)
sapply( HcoXaTxt[3189:3192,8:14] ,class)

ArregDatos(paste0(as.character(as.numeric(look[1,7:14])/10000) ,"."))
ArregDatos(as.data.frame(as.character(as.numeric(look[1,7:14])/1),stringsAsFactors=FALSE))[1,1]








TablaCarga -> HcoXaTxt
setorder(HcoXaTxt,"Codigo Interno")
# LimpiaNAs(HcoXaTxt) -> HcoXaTxt
rownames( HcoXaTxt[rowSums(is.na(HcoXaTxt)) > 0,]) # filas con NAs
HcoXaTxt[rownames( HcoXaTxt[rowSums(is.na(HcoXaTxt)) > 0,]),] -> look
for(i in 1:nrow(HcoXaTxt)){# con scipen=100 ojo!!
    as.numeric(HcoXaTxt[i,3:ncol(HcoXaTxt)]) -> HcoXaTxt[i,3:ncol(HcoXaTxt)]
} 

for(i in 1:nrow(HcoXaTxt )){
    for(j in 3:ncol(HcoXaTxt )){
        if( (HcoXaTxt [i,j] == 10000000000) || (HcoXaTxt [i,j] == 10010000000) ){ as.character(as.numeric(HcoXaTxt [i,j])/1000) -> HcoXaTxt [i,j] }
    }}
ArregDatos( HcoXaTxt[,3:ncol(HcoXaTxt)] )-> HcoXaTxt[,3:ncol(HcoXaTxt)]

HcoXaTxt -> temp

HcoXaTxt[nrow(HcoXaTxt)+1,] <- NA
HcoXaTxt[nrow(HcoXaTxt)+1,] -> HcoXaTxt[1,] 
HcoXaTxt[1,] <- colnames(HcoXaTxt)
HcoXaTxt[1,] <- gsub("-","",HcoXaTxt[1,])
HcoXaTxt[ 1 ,  ] <- ArregFila(Tabla = HcoXaTxt)
"Codigo Interno----------------" -> HcoXaTxt[1,1]
"ISIN-----------" -> HcoXaTxt[1,2]
HcoXaTxt[4727,] <- HcoXaTxt[ which(rownames(HcoXaTxt) %in% c(2042)),]
HcoXaTxt[4727,1] <- "000000011565101"

HcoXaTxt[ 2:nrow(HcoXaTxt) , 1:2 ] <- ArregCol(Tabla = HcoXaTxt[ 2:nrow(HcoXaTxt) , 1:2 ])

write.table(  HcoXaTxt  , file= "C:\\AlxP\\HISTNOESIS_F20171201.TXT" , col.names = FALSE , row.names = FALSE , sep = ";" , quote = FALSE)

setorder(temp,"ISIN")








# Metodo 1
# wb <- openxlsx::loadWorkbook(file = "R:/RIESGOS/RENAULT/Informes mensuales/Octubre 2017/CARTERA FINANCIERA MENSUAL OCTUBRE.xlsm")
# openxlsx::getStyles(wb)[1:3]
# openxlsx::saveWorkbook(wb, "R:/RIESGOS/RENAULT/Informes mensuales/Octubre 2017/CarteraRecup2.xlsx" , overwrite = FALSE)

# Metodo 2
# NombreHojas <- openxlsx::getSheetNames("R:/RIESGOS/RENAULT/Informes mensuales/Octubre 2017/CARTERA FINANCIERA MENSUAL OCTUBRE.xlsm")
# list() -> Hojas
# for(i in 1:length(NombreHojas)){
#     Hojas[[i]] <- openxlsx::readWorkbook(xlsxFile = "R:/RIESGOS/RENAULT/Informes mensuales/Octubre 2017/CARTERA FINANCIERA MENSUAL OCTUBRE.xlsm" , sheet = NombreHojas[i]
#                                          ,startRow = 1 , colNames = TRUE ,
#                                          rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
#                                          skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE)
# }
# names(Hojas) <- NombreHojas
# openxlsx::write.xlsx(Hojas,    file = "R:/RIESGOS/RENAULT/Informes mensuales/Octubre 2017/CarteraRecup.xlsx", colNames = TRUE)



