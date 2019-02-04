
################################################################################
## Identify files to load
################################################################################

# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds454\\final\\DengAI"
# base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
# base.path <- "C:\\Users\\woehrle\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
base.path <- "D:\\ajc188\\Dropbox\\Education\\data science (2014-2019)\\coursework\\msds498\\_topics\\West Nile"
repo.path <- "D:\\ajc188\\github\\capstone_project"

filename.list <- list.files(path = paste(repo.path,"data\\raw\\vacant_properties",sep='\\'))           
filename.list



################################################################################
## Load vacancy data
################################################################################

vacancies.csv <- "Vacant_and_Abandoned_Buildings_-_Violations.csv"

vacancies <- read.csv(paste(repo.path,"data\\raw\\vacant_properties",vacancies.csv,sep="\\"),stringsAsFactors = FALSE)

# str(cdph.wnv)



################################################################################
## Data manipulation
################################################################################

vacancies$issued.date <- as.Date(vacancies$Issued.Date,format="%m/%d/%Y")

vacant.properties <- vacancies %>% group_by(Property.Address) %>%
  summarise(nrows=n(),min.issued.date=min(issued.date)
            ,max.issued.date = max(issued.date)
            # ,max.total.paid = max(is.finite(Total.Paid)*Total.Paid,na.rm = TRUE)
            )

vacant.properties$city <- 'Chicago'
vacant.properties$state <- 'IL'
vacant.properties$zip <- NA

vacant.properties$addr.nbr <- gsub("([0-9]+).*", "\\1", vacant.properties$Property.Address)
vacant.properties$addr.street.and.dir <- trimws(substring(vacant.properties$Property.Address
                                                   ,nchar(vacant.properties$addr.nbr) + 1
                                                   ,nchar(vacant.properties$Property.Address)))
vacant.properties$addr.dir <- ifelse(substring(vacant.properties$addr.street.and.dir,1,2) 
                                     %in% c('N ','S ','E ','W ')
                                     ,substring(vacant.properties$addr.street.and.dir,1,1)
                                     ,NA)
vacant.properties$addr.street <- ifelse(!is.na(vacant.properties$addr.dir)
                                        ,trimws(substring(vacant.properties$addr.street.and.dir
                                                          ,nchar(vacant.properties$addr.dir) + 1
                                                          ,nchar(vacant.properties$addr.street.and.dir)))
                                        ,vacant.properties$addr.street.and.dir
                                        )

chk.streets <- vacant.properties %>% group_by(addr.street) %>% 
  summarise(nrows=n()
            ,n_s = max(addr.dir %in% c("N","S"))
            ,e_w = max(addr.dir %in% c("E","W"))
            ,min.addr.nbr = min(as.numeric(addr.nbr))
            ,max.addr.nbr = max(as.numeric(addr.nbr))
            ,mean.addr.nbr = mean(as.numeric(addr.nbr))
  )

# vacant.properties[vacant.properties$addr.street2 == 'LAUNDALE',]$addr.street2 <- 'LAWNDALE'
vacant.properties$addr.nbr <- as.integer(vacant.properties$addr.nbr)
vacant.properties$addr.end <- word(vacant.properties$addr.street,-1) 
last.part <- vacant.properties %>% group_by(addr.end) %>% summarise(nrows=n())

vacant.properties$st.name.pot <- ifelse(
  vacant.properties$addr.end %in% c("ST","PL","BLVD","AVE","PKWY","PLACE","AV","BLV","BVLD","DR","DRIVE"
         ,"STREET","AVENUE")
  ,vacant.properties$addr.end
  ,NA
)
vacant.properties$addr.street <- ifelse(is.na(vacant.properties$st.name.pot)
                                        ,vacant.properties$addr.street
                                        ,trimws(substring(vacant.properties$addr.street
                                                   ,1
                                                   ,nchar(vacant.properties$addr.street) - 
                                                     nchar(vacant.properties$st.name.pot))
                                        ))

vacant.properties[vacant.properties$addr.street == 'LAUNDALE',]$addr.street <- 'LAWNDALE'
vacant.properties$addr.street <- vacant.properties$addr.street 
vacant.properties[vacant.properties$addr.street %in% c("LA CAROSSE"
                                                       ,"LA CROSSE"
                                                       ,"LACROSSE"),]$addr.street <- 'LA CROSSE'
vacant.properties[vacant.properties$addr.street %in% c("HONROE","HONRE"
                                                       ,"HONOE"
                                                       ,"HORORE"),]$addr.street <- 'HONORE'

vacant.properties[vacant.properties$addr.street %in% c("OAKELY","OALEY"
),]$addr.street <- 'OAKLEY'
vacant.properties[vacant.properties$addr.street %in% c("62MD"
),]$addr.street <- '62ND'
vacant.properties[vacant.properties$addr.street %in% c("60THJ"
),]$addr.street <- '60TH'
vacant.properties[vacant.properties$addr.street %in% c("ARTEASIAN"
                                                       ,"ARTEISAN"
                                                       ,"ARTSEIAN"
),]$addr.street <- 'ARTESIAN'
vacant.properties[vacant.properties$addr.street %in% c("ARTHIGNTON"
),]$addr.street <- 'ARTHINGTON'
vacant.properties[vacant.properties$addr.street %in% c("AULINA"
                                                       ,"PUALINA"
),]$addr.street <- 'PAULINA'
vacant.properties[vacant.properties$addr.street %in% c("CHAPPEL"
                                                       ,"CHAPPELL"
),]$addr.street <- 'CHAPPEL'
vacant.properties[vacant.properties$addr.street %in% c("ESCANBA"
),]$addr.street <- 'ESCANABA'
vacant.properties[vacant.properties$addr.street %in% c("EXCHNGE"
),]$addr.street <- 'EXCHANGE'
vacant.properties[vacant.properties$addr.street %in% c("FAIRFIELDDD"
),]$addr.street <- 'FAIRFIELD'

vacant.properties[vacant.properties$addr.street %in% c("FRANSICO"
                                                       ,"FRANCISO"
                                                       ,"FRANCISCON"
                                                       ,"FRNACISCO"
                                                       ,"SFRANCISCO"
),]$addr.street <- 'FRANCISCO'
vacant.properties[vacant.properties$addr.street %in% c("JSUTINE"
),]$addr.street <- 'JUSTINE'
vacant.properties[vacant.properties$addr.street %in% c("LEXINTON"
),]$addr.street <- 'LEXINGTON'
vacant.properties[vacant.properties$addr.street %in% c("LITHIUANIAN"
),]$addr.street <- 'LITHUANIAN'
vacant.properties[vacant.properties$addr.street %in% c("MAPLWOOD"
                                                       ,"MAPLE WOOD"
),]$addr.street <- 'MAPLEWOOD'
vacant.properties[vacant.properties$addr.street %in% c("MARQUTEE"
),]$addr.street <- 'MARQUETTE'
vacant.properties[vacant.properties$addr.street %in% c("MARSHIELD"
                                                       ,"MRSHFIELD"
),]$addr.street <- 'MARSHFIELD'
vacant.properties[vacant.properties$addr.street %in% c("MASSASOIT"
                                                       ,"MASSASIOT"
                                                       ,"MASSASSOIT"
),]$addr.street <- 'MASSASOIT'
vacant.properties[vacant.properties$addr.street %in% c("MIOGAN"
                                                       ,"MOGAN"
                                                       ,"MORAGN"
                                                       ,"MORGNA"
                                                       ,"M ORGAN"
),]$addr.street <- 'MORGAN'
vacant.properties[vacant.properties$addr.street %in% c("PRINCETEON"
),]$addr.street <- 'PRINCETON'
vacant.properties[vacant.properties$addr.street %in% c("RICHNMOND"
),]$addr.street <- 'RICHMOND'
vacant.properties[vacant.properties$addr.street %in% c("RIGEWAY"
                                                       ,"RIDEGWAY"
),]$addr.street <- 'RIDGEWAY'
vacant.properties[vacant.properties$addr.street %in% c("ROCKWEL"
),]$addr.street <- 'ROCKWELL'
vacant.properties[vacant.properties$addr.street %in% c("SACAMENTO"
                                                       ,"SACRAMENT"
),]$addr.street <- 'SACRAMENTO'
vacant.properties[vacant.properties$addr.street %in% c("SAGINAL"
),]$addr.street <- 'SAGINAW'
vacant.properties[vacant.properties$addr.street %in% c("SAWTER"
                                                       ,"SAWYEER"
),]$addr.street <- 'SAWYER'
vacant.properties[vacant.properties$addr.street %in% c("SHEILDS"
                                                       ,"SHIELD"
),]$addr.street <- 'SHIELDS'
vacant.properties[vacant.properties$addr.street %in% c("TALMEN"
),]$addr.street <- 'TALMAN'
vacant.properties[vacant.properties$addr.street %in% c("THROOOP"
),]$addr.street <- 'THROOP'
vacant.properties[vacant.properties$addr.street %in% c("WAHSTENAW"
                                                       ,"WASHENAW"
                                                       ,"WASHTEANW"
                                                       ,"WASTENAW"
                                                       ,"WASTHENAW"
),]$addr.street <- 'WASHTENAW'
vacant.properties[vacant.properties$addr.street %in% c("WALLCE"
),]$addr.street <- 'WALLACE'
vacant.properties[vacant.properties$addr.street %in% c("WASHIGNTON"
),]$addr.street <- 'WASHINGTON'
vacant.properties[vacant.properties$addr.street %in% c("WINCHESER"
                                                       ,"WINCHESETER"
                                                       ,"WNCHESTER"
),]$addr.street <- 'WINCHESTER'
vacant.properties[vacant.properties$addr.street %in% c("ABERDEN"
),]$addr.street <- 'ABERDEEN'
vacant.properties[vacant.properties$addr.street %in% c("BENSLEY"
                                                       ,"BENSLEYA"
),]$addr.street <- 'BENSLEY'
vacant.properties[vacant.properties$addr.street %in% c("BIHSOP"
),]$addr.street <- 'BISHOP'
vacant.properties[vacant.properties$addr.street %in% c("BRADNON"
),]$addr.street <- 'BRANDON'
vacant.properties[vacant.properties$addr.street %in% c("CAMLBELL"
),]$addr.street <- 'CAMPBELL'
vacant.properties[vacant.properties$addr.street %in% c("CARPENER"
),]$addr.street <- 'CARPENTER'
vacant.properties[vacant.properties$addr.street %in% c("CENTRAL PK"
                                                       ,"CENTRAL PARK"
),]$addr.street <- 'CENTRAL PARK'
vacant.properties[vacant.properties$addr.street %in% c("CHAMPLAINA"
                                                       ,"CHAMPLAIN"
),]$addr.street <- 'CHAMPLAIN'
vacant.properties[vacant.properties$addr.street %in% c("EMERAL"
                                                       ,"EMERALD"
),]$addr.street <- 'EMERALD'
vacant.properties[vacant.properties$addr.street %in% c("FIFTH"
),]$addr.street <- '5TH'
vacant.properties[vacant.properties$addr.street %in% c("HOMA"
),]$addr.street <- 'HOMAN'
vacant.properties[vacant.properties$addr.street %in% c("KARLOVE"
),]$addr.street <- 'KARLOV'
vacant.properties[vacant.properties$addr.street %in% c("KELLER"
),]$addr.street <- 'KEELER'
vacant.properties[vacant.properties$addr.street %in% c("KIGNSTON"
),]$addr.street <- 'KINGSTON'
vacant.properties[vacant.properties$addr.street %in% c("KOSTONER"
                                                       ,"KOSTNER"
                                                       ,"KOSTNERF"
),]$addr.street <- 'KOSTNER'
vacant.properties[vacant.properties$addr.street %in% c("LASALLE"
),]$addr.street <- 'LA SALLE'
vacant.properties[vacant.properties$addr.street %in% c("LE MOYNE"
                                                       ,"LE MOYNE"
),]$addr.street <- 'LE MOYNE'
vacant.properties[vacant.properties$addr.street %in% c("LE CLAIRE"
                                                       ,"LE CLAIRE"
),]$addr.street <- 'LECLAIRE'
vacant.properties[vacant.properties$addr.street %in% c("LOMIS"
),]$addr.street <- 'LOOMIS'
vacant.properties[vacant.properties$addr.street %in% c("MARYLAND UNIT"
),]$addr.street <- 'MARYLAND'
vacant.properties[vacant.properties$addr.street %in% c("MC LEAN"
),]$addr.street <- 'MCLEAN'
vacant.properties[vacant.properties$addr.street %in% c("MC VICKER"
),]$addr.street <- 'MCVICKER'
vacant.properties[vacant.properties$addr.street %in% c("MUSKEGAN"
),]$addr.street <- 'MUSKEGAN'
vacant.properties[vacant.properties$addr.street %in% c("PRAIRE"
                                                       ,"PRARIE"
                                                       ,"PRAIRIE"
),]$addr.street <- 'PRAIRIE'
vacant.properties[vacant.properties$addr.street %in% c("PRINCTON"
),]$addr.street <- 'PRINCETON'
vacant.properties[vacant.properties$addr.street %in% c("S LOUIS"
),]$addr.street <- 'SAINT LOUIS'
vacant.properties[vacant.properties$addr.street %in% c("SO SHORE"
),]$addr.street <- 'SOUTH SHORE'
vacant.properties[vacant.properties$addr.street %in% c("VAN BURENS"
),]$addr.street <- 'VAN BUREN'
vacant.properties[vacant.properties$addr.street %in% c("WASHINGTON BL"
),]$addr.street <- 'WASHINGTON'
vacant.properties[vacant.properties$addr.street %in% c("WELL"
),]$addr.street <- 'WELLS'
vacant.properties[vacant.properties$addr.street %in% c("100"
),]$addr.street <- '100TH'
vacant.properties[vacant.properties$addr.street %in% c("101"
),]$addr.street <- '101ST'
vacant.properties[vacant.properties$addr.street %in% c("109"
),]$addr.street <- '109TH'
vacant.properties[vacant.properties$addr.street %in% c("111"
),]$addr.street <- '111TH'
vacant.properties[vacant.properties$addr.street %in% c("116"
),]$addr.street <- '116TH'
vacant.properties[vacant.properties$addr.street %in% c("117"
),]$addr.street <- '117TH'
vacant.properties[vacant.properties$addr.street %in% c("15"
),]$addr.street <- '15TH'
vacant.properties[vacant.properties$addr.street %in% c("34"
),]$addr.street <- '34TH'
vacant.properties[vacant.properties$addr.street %in% c("38"
),]$addr.street <- '38TH'
vacant.properties[vacant.properties$addr.street %in% c("40"
),]$addr.street <- '40TH'
vacant.properties[vacant.properties$addr.street %in% c("42"
),]$addr.street <- '42ND'
vacant.properties[vacant.properties$addr.street %in% c("45"
),]$addr.street <- '45TH'
vacant.properties[vacant.properties$addr.street %in% c("47"
),]$addr.street <- '47TH'
vacant.properties[vacant.properties$addr.street %in% c("48"
),]$addr.street <- '48TH'
vacant.properties[vacant.properties$addr.street %in% c("49"
),]$addr.street <- '49TH'
vacant.properties[vacant.properties$addr.street %in% c("51","551ST","51TH"
),]$addr.street <- '51ST'
vacant.properties[vacant.properties$addr.street %in% c("52","52THY","52TH"
),]$addr.street <- '52ND'
vacant.properties[vacant.properties$addr.street %in% c("54"
),]$addr.street <- '54TH'
vacant.properties[vacant.properties$addr.street %in% c("55"
),]$addr.street <- '55TH'
vacant.properties[vacant.properties$addr.street %in% c("57"
),]$addr.street <- '57TH'
vacant.properties[vacant.properties$addr.street %in% c("59"
),]$addr.street <- '59TH'
vacant.properties[vacant.properties$addr.street %in% c("6"
),]$addr.street <- '6TH'
vacant.properties[vacant.properties$addr.street %in% c("61"
),]$addr.street <- '61ST'
vacant.properties[vacant.properties$addr.street %in% c("62"
),]$addr.street <- '62ND'
vacant.properties[vacant.properties$addr.street %in% c("64"
),]$addr.street <- '64TH'
vacant.properties[vacant.properties$addr.street %in% c("65"
),]$addr.street <- '65TH'
vacant.properties[vacant.properties$addr.street %in% c("68","68T"
),]$addr.street <- '68TH'
vacant.properties[vacant.properties$addr.street %in% c("69"
),]$addr.street <- '69TH'
vacant.properties[vacant.properties$addr.street %in% c("71"
),]$addr.street <- '71ST'
vacant.properties[vacant.properties$addr.street %in% c("78"
),]$addr.street <- '78TH'
vacant.properties[vacant.properties$addr.street %in% c("79"
),]$addr.street <- '79TH'
vacant.properties[vacant.properties$addr.street %in% c("81"
),]$addr.street <- '81ST'
vacant.properties[vacant.properties$addr.street %in% c("83"
),]$addr.street <- '83RD'
vacant.properties[vacant.properties$addr.street %in% c("85"
),]$addr.street <- '85TH'
vacant.properties[vacant.properties$addr.street %in% c("130"
),]$addr.street <- '130TH'
vacant.properties[vacant.properties$addr.street %in% c("133"
),]$addr.street <- '133RD'
vacant.properties[vacant.properties$addr.street %in% c("16"
),]$addr.street <- '16TH'
vacant.properties[vacant.properties$addr.street %in% c("21"
),]$addr.street <- '21ST'
vacant.properties[vacant.properties$addr.street %in% c("25"
),]$addr.street <- '25TH'
vacant.properties[vacant.properties$addr.street %in% c("28"
),]$addr.street <- '28TH'
vacant.properties[vacant.properties$addr.street %in% c("32"
),]$addr.street <- '32ND'
vacant.properties[vacant.properties$addr.street %in% c("35"
),]$addr.street <- '35TH'
vacant.properties[vacant.properties$addr.street %in% c("36"
),]$addr.street <- '36TH'
vacant.properties[vacant.properties$addr.street %in% c("37"
),]$addr.street <- '37TH'
vacant.properties[vacant.properties$addr.street %in% c("39"
),]$addr.street <- '39TH'
vacant.properties[vacant.properties$addr.street %in% c("44"
),]$addr.street <- '44TH'
vacant.properties[vacant.properties$addr.street %in% c("47","47TH T"
),]$addr.street <- '47TH'
vacant.properties[vacant.properties$addr.street %in% c("4S1T"
),]$addr.street <- '41ST'
vacant.properties[vacant.properties$addr.street %in% c("32TH"
),]$addr.street <- '32ND'
vacant.properties[vacant.properties$addr.street %in% c("42RD"
),]$addr.street <- '43RD'
vacant.properties[vacant.properties$addr.street %in% c("50","50PL"
),]$addr.street <- '50TH'
vacant.properties[vacant.properties$addr.street %in% c("51","51PL"
),]$addr.street <- '51ST'
vacant.properties[vacant.properties$addr.street %in% c("53"
),]$addr.street <- '53RD'
vacant.properties[vacant.properties$addr.street %in% c("92"
),]$addr.street <- '92ND'
vacant.properties[vacant.properties$addr.street %in% c("AVE B"
),]$addr.street <- 'AVENUE B'
vacant.properties[vacant.properties$addr.street %in% c("AVE C"
),]$addr.street <- 'AVENUE C'
vacant.properties[vacant.properties$addr.street %in% c("AVE D"
),]$addr.street <- 'AVENUE D'
vacant.properties[vacant.properties$addr.street %in% c("AVE E"
),]$addr.street <- 'AVENUE E'
vacant.properties[vacant.properties$addr.street %in% c("AVE F"
),]$addr.street <- 'AVENUE F'
vacant.properties[vacant.properties$addr.street %in% c("AVE G"
),]$addr.street <- 'AVENUE G'
vacant.properties[vacant.properties$addr.street %in% c("AVE H"
),]$addr.street <- 'AVENUE H'
vacant.properties[vacant.properties$addr.street %in% c("AVE I"
),]$addr.street <- 'AVENUE I'
vacant.properties[vacant.properties$addr.street %in% c("AVE J"
),]$addr.street <- 'AVENUE J'
vacant.properties[vacant.properties$addr.street %in% c("AVE K"
),]$addr.street <- 'AVENUE K'
vacant.properties[vacant.properties$addr.street %in% c("AVE L"
),]$addr.street <- 'AVENUE L'
vacant.properties[vacant.properties$addr.street %in% c("AVE M"
),]$addr.street <- 'AVENUE M'
vacant.properties[vacant.properties$addr.street %in% c("AVE N"
),]$addr.street <- 'AVENUE N'
vacant.properties[vacant.properties$addr.street %in% c("4T6H"
),]$addr.street <- '46TH'
vacant.properties[vacant.properties$addr.street %in% c("53TH"
),]$addr.street <- '53RD'
vacant.properties[vacant.properties$addr.street %in% c("54PL"
),]$addr.street <- '54TH'
vacant.properties[vacant.properties$addr.street %in% c("JUSTINA"
),]$addr.street <- 'JUSTINE'
vacant.properties[vacant.properties$addr.street %in% c("LAFIN"
),]$addr.street <- 'LAFLIN'
vacant.properties[vacant.properties$addr.street %in% c("BISHOPS"
),]$addr.street <- 'BISHOP'
vacant.properties[vacant.properties$addr.street %in% c("LA PORTE"
),]$addr.street <- 'LAPORTE'
vacant.properties[vacant.properties$addr.street %in% c(""
),]$addr.street <- 'LITHUANIAN'
vacant.properties[vacant.properties$addr.street %in% c("LEMOYNE"
),]$addr.street <- 'LE MOYNE'
vacant.properties[vacant.properties$addr.street %in% c("MUSKEGAN"
),]$addr.street <- 'MUSKEGON'
vacant.properties[vacant.properties$addr.street %in% c("RUMBULL"
),]$addr.street <- 'TRUMBULL'
vacant.properties[vacant.properties$addr.street %in% c("ST LAWRENCE"
),]$addr.street <- 'SAINT LAWRENCE'
vacant.properties[vacant.properties$addr.street %in% c("ST LOUIS"
),]$addr.street <- 'SAINT LOUIS'
vacant.properties[vacant.properties$addr.street %in% c("LITHUANIAN"
),]$addr.street <- 'LITHUANIAN PLAZA'
vacant.properties[vacant.properties$addr.street %in% c("6"
),]$addr.street <- '6TH'
vacant.properties[vacant.properties$addr.street %in% c("BELVINA"
),]$addr.street <- 'MELVINA'








chk.streets <- vacant.properties %>% group_by(addr.street) %>% 
  summarise(nrows=n()
            ,n_s = max(addr.dir %in% c("N","S"))
            ,e_w = max(addr.dir %in% c("E","W"))
            ,min.addr.nbr = min(as.numeric(addr.nbr))
            ,max.addr.nbr = max(as.numeric(addr.nbr))
            ,mean.addr.nbr = mean(as.numeric(addr.nbr))
  )





################################################################################
## Check to see if these locations exist in Cook County address points
################################################################################

address.point.filepath <- "D:\\ajc188\\Dropbox\\Forest Park data\\geo\\ccgisdata_-_Address_Point_Chicago"
address.point.chi <- read.dbf(paste(address.point.filepath,'addressPointChi.dbf',sep='\\'))

str(address.point.chi)
# head(address.point.chi)
# address.point.chi.head <- head(address.point.chi)

str(vacant.properties$Property.Address)

address.point.chi$STNAMEPRD2 <- substring(address.point.chi$STNAMEPRD,1,1)


# chk.streets2 <- address.point.chi %>% group_by(STNAME,STNAMEPRD2) %>% 
#   summarise(nrows=n(),min.addr.nbr = min(as.numeric(ADDRNO))
#             ,max.addr.nbr = max(as.numeric(ADDRNO))
#             ,mean.addr.nbr = mean(as.numeric(ADDRNO))
#   )
chk.streets2 <- address.point.chi %>% group_by(STNAME) %>% 
  summarise(nrows=n()
            ,n_s = max(STNAMEPRD2 %in% c("N","S"))
            ,e_w = max(STNAMEPRD2 %in% c("E","W"))
            ,min.addr.nbr = min(as.numeric(ADDRNO))
            ,max.addr.nbr = max(as.numeric(ADDRNO))
            ,mean.addr.nbr = mean(as.numeric(ADDRNO))
  )

vacant.properties.latlng <- inner_join(vacant.properties
                                      ,address.point.chi
                                      ,by=c("addr.nbr"="ADDRNO"
                                            ,"addr.dir"="STNAMEPRD2"
                                            ,"addr.street"="STNAME"))
vacant.properties.unmatched <- anti_join(vacant.properties,vacant.properties.latlng
                                         ,by=c("Property.Address")) %>%
  arrange(addr.street,addr.dir)

vacant.properties.st <- inner_join(vacant.properties
                                       ,chk.streets2
                                       ,by=c("addr.street"="STNAME"))
vacant.properties.st.unmatched <- anti_join(vacant.properties,vacant.properties.st
                                         ,by=c("Property.Address")) %>%
  arrange(addr.street,addr.dir)

str(vacancies)


################################################################################
## Geocode with google
################################################################################


str(vacant.properties)

vacant.properties$address.one.line <- 
  paste(
    as.character(vacant.properties$addr.nbr)
    ,'+'
    ,vacant.properties$addr.dir 
    ,'+'
    ,vacant.properties$addr.street
    ,ifelse(is.na(vacant.properties$st.name.pot),''
            ,paste('+',vacant.properties$st.name.pot,sep=''))
    ,'+Chicago,IL'
    ,sep=''
  )

geocode.api.url <- "https://maps.googleapis.com/maps/api/geocode/json?"
# geocode.api.key <- "xxx"

vacant.properties$google.call <- ifelse(is.na(vacant.properties$address.one.line) 
                                               | vacant.properties$address.one.line == "","",
                                               paste(geocode.api.url,"address="
                                                     ,vacant.properties$address.one.line
                                                     ,"&key=",geocode.api.key,sep=""))
vacant.properties$google.call <- gsub(' ','+',vacant.properties$google.call)


library("httr")
library("jsonlite")

# google.response[,1]


google.response <- sapply(vacant.properties$google.call,function(x) {
  if(!is.na(x) & x != "") { GET(x) }
})

## Process geocoding results

parse.address.components <- function(address_components) {
  addr.components.labels = as.list(rep(NA,length(address_components$types)))
  for(i in 1:length(address_components$types)) {
    addr.components.labels[i] = unlist(address_components$types[i])[1]
  }
  addr.components.df <- as.data.frame(address_components)
  
  addr.components.df2 <- transpose(addr.components.df[,c("long_name","short_name")])
  colnames(addr.components.df2) <- unlist(addr.components.labels)
  rownames(addr.components.df2) <- colnames(addr.components.df)[1:2]
  addr.components.df2$row.name <- rownames(addr.components.df2)
  
  return(addr.components.df2)
}

lparse.address.components <- function(component.list,type="short_name") {
  components <- bind_rows(lapply(component.list,FUN=function(x) {parse.address.components(x)}))
  return(components[components$row.name %in% c(type),])
}



process.subresult <- function(result1) {
  result$result.full <- as.character(result1)
  
  result$place.id <- result1$place_id
  result$formatted.address <- result1$formatted_address
  result$lat <- result1$geometry$location$lat
  result$lng <- result1$geometry$location$lng
  
  addr.components.labels = as.list(rep(NA,length(result1$address_components[[1]]$types)))
  for(i in 1:length(result1$address_components[[1]]$types)) {
    addr.components.labels[i] = unlist(result1$address_components[[1]]$types[i])[1]
  }
  
  addr.components.df <- as.data.frame(result1$address_components)
  
  addr.components.df2 <- transpose(addr.components.df[,c("long_name","short_name")])
  colnames(addr.components.df2) <- unlist(addr.components.labels)
  rownames(addr.components.df2) <- colnames(addr.components.df)[1:2]
  
  result$street.number = addr.components.df2["short_name",]$street_number
  result$route = addr.components.df2["short_name",]$route
  result$locality = addr.components.df2["short_name",]$locality
  result$admin_area_level_1 = addr.components.df2["short_name",]$administrative_area_level_1
  result$admin_area_level_2 = addr.components.df2["short_name",]$administrative_area_level_2
  result$admin_area_level_3 = addr.components.df2["short_name",]$administrative_area_level_3
  result$country = addr.components.df2["short_name",]$country
  result$postal_code = addr.components.df2["short_name",]$postal_code
  result$postal_code_suffix = addr.components.df2["short_name",]$postal_code_suffix
  
  return(result)  
}


process.geocoding.api.result <- function(x,limit.to.first=TRUE,type="short_name") {
  # For now, use the first result if there are multiple results.
  
  x <- fromJSON(rawToChar(unlist(x)))
  
  if(length(x$results) == 0 ) {
    results <- data.frame(seqn=0)
    results$json <- as.character(x[1])
    results$status <- NA
    results$result.cnt <- 0
    print('No results')
    return(results)
  }
  else {
    results <- data.frame(seqn=1:dim(x$results)[1])
    results$json <- as.character(x[1])
    results$status <- x$status 
    results$result.cnt <- dim(x$results)[1]
    results$place.id <- x$results$place_id
    results$formatted.address <- x$results$formatted_address
    results$lat <- x$results$geometry$location$lat
    results$lng <- x$results$geometry$location$lng
    
    address_components <- lparse.address.components(x$results$address_components
                                                    ,type=type)
    results <- cbind(results,address_components)
    
    if(limit.to.first) {
      if('administrative_area_level_1' %in% colnames(results)) {
        results <- results %>% arrange(-(administrative_area_level_1 == 'IL'))
      }
      return(results[1,])
    }
    else return(results)
  }
}


process.all.results <- function(response) {
  calls <- dim(response)[2]
  print(calls)
  
  for (i in 1:calls) {
    # print(i)
    my.result <- process.geocoding.api.result(response[6,i])
    if(dim(my.result)[1] != 1) {print(my.result)}
    if (i == 1) {
      my.results <- my.result
    }
    else {
      my.results <- bind_rows(my.results,my.result)
    }
  }
  return(my.results)
}


library(data.table)

# vacant.properties.address.results <- process.all.results(google.response[,1:22])
# google.response[,23]


# vacant.properties.address.results <- process.all.results(google.response[,102:102])
# Manually change this to 112th block since Talman doesn't exist at 11th block.
google.response[,102] <- GET(gsub('1127','11270',vacant.properties$google.call[102]))

# google.response[,237]    
process.all.results(google.response[,237])

vacant.properties.address.results <- process.all.results(google.response)

vacant.properties2 <- cbind(vacant.properties,vacant.properties.address.results)

summary(vacant.properties.address.results)
ftable(vacant.properties.address.results$status)
ftable(vacant.properties.address.results$result.cnt)

questionable <- vacant.properties2[
  gregexpr('[0-9]',substring(vacant.properties2$formatted.address,1,1)) < 0
  ,]

## Data quality is questionable but let's go with it.


################################################################################
## Save geocoded version, then run code in ArcGIS to do spatial joins.
################################################################################

write.csv(vacant.properties2,paste(repo.path,"data\\raw\\vacant_properties","vacant.properties2.csv",sep='\\'),row.names=FALSE)


################################################################################
## Read data from spatial joins back in
################################################################################

vacant.properties2.zcta <- read.dbf(paste(repo.path,'data\\raw\\vacant_properties\\GIS','vacant.properties2__zcta.dbf',sep="\\"))
vacant.properties2.blkgrp <- read.dbf(paste(repo.path,'data\\raw\\vacant_properties\\GIS','vacant.properties2__bg.dbf',sep="\\"))
vacant.properties2.community.area <- read.dbf(paste(repo.path,'data\\raw\\vacant_properties\\GIS','vacant.properties2__CommArea.dbf',sep="\\"))

# str(vacant.properties2)
# str(vacant.properties2.zcta)
# head(vacant.properties2$Property.Address)
# head(vacant.properties2.zcta$Property_A)
# vacant.properties2.zcta$Property.Address <- as.character(vacant.properties2.zcta$Property_A)
# head(vacant.properties2.zcta$Property.Address)
vacant.properties2$Property.Address <- trimws(vacant.properties2$Property.Address)

vacant.properties2.1 <- left_join(vacant.properties2,vacant.properties2.zcta[,c("Property_A","ZCTA5CE10")]
                        ,by=c("Property.Address" = "Property_A"))
vacant.properties2.2 <- left_join(vacant.properties2.1,vacant.properties2.blkgrp[,c("Property_A","STATEFP"
                                                      ,"COUNTYFP"
                                                      ,"TRACTCE"
                                                      ,"BLKGRPCE"
                                                      ,"GEOID")]
                        ,by=c("Property.Address" = "Property_A"))
vacant.properties2.3 <- left_join(vacant.properties2.2,vacant.properties2.community.area[,c("Property_A","community")]
                        ,by=c("Property.Address" = "Property_A"))

summary(vacant.properties2.3)
vacant.properties3 <- vacant.properties2.3[,colnames(vacant.properties2.3) %in% 
                                             c("Property.Address","address.one.line"
                                               ,"formatted.address"
                                               ,"ZCTA5CE10","STATEFP"
                                               ,"COUNTYFP","TRACTCE"
                                               ,"BLKGRPCE","GEOID"
                                               ,"community"
                                             )]
names(vacant.properties3)[names(vacant.properties3) == 'Property_A'] <- 'Property.Address'
vacant.properties3$address.one.line <- gsub('[+]',' ',vacant.properties3$address.one.line)
names(vacant.properties3)[names(vacant.properties3) == 'GEOID'] <- 'BlkGrp.geoid'


################################################################################
## Now combine back with the full vacancies data.frame
################################################################################

str(vacant.properties3)

# neighborhoods <- vacant.properties3 %>% group_by(community,neighborhood) %>%
#   summarise(n=n())

vacancies$Property.Address <- trimws(vacancies$Property.Address)

vacancies2 <- left_join(vacancies
                        ,vacant.properties3
                        ,by=c("Property.Address"))



################################################################################
## Save updated  data
################################################################################


# save(wnv.traps, file="wnv.traps.RData", compress = FALSE)
# write.csv(wnv.traps, paste(my.path,'wnv.traps.csv',sep='\\'),row.names = FALSE)

write.csv(vacant.properties2.3,paste(repo.path,"data\\raw\\vacant_properties","vacant.properties.rev.csv",sep='\\'),row.names=FALSE)
write.csv(vacancies2,paste(repo.path,"data\\raw\\vacant_properties","vacancies.rev.csv",sep='\\'),row.names=FALSE)



################################################################################
## Check high level summaries
################################################################################

vacancies2$issued.date <- as.Date(vacancies2$Issued.Date,format="%m/%d/%Y")

vacancies2 %>% group_by(community) %>% 
  summarise(n=n()
            ,min.date = min(issued.date)
            ,max.date = max(issued.date)
  ) %>% arrange(-n)



################################################################################
## Aggregate to time and place
################################################################################

# wnv.trap.date2 <- inner_join(wnv.traps
#                              ,wnv.trap.date[,colnames(wnv.trap.date) == 'trap.name' || 
#                                               !colnames(wnv.trap.date) %in% 
#                                               colnames(wnv.traps)]
#                              ,by=c("trap.name"))
wnv.trap.date2 <- wnv.trap.date


aggr.vac.for.date <- function(df,vac.df,window.d=180) {
  df$date.begin <- df$date - window.d
  out.df <-  fuzzyjoin::fuzzy_left_join(df
                                        ,vac.df
                                        ,by=c("date"="issued.date"
                                              ,"date.begin"="issued.date")
                                        ,match_fun = list(`>=`,`<=`)) %>%
    group_by(date) %>% summarise(vacancy.cnt = sum(vacancy.cnt))
  return(out.df)
}


aggr.vac.by.col <- function(cols,df=wnv.trap.date2,vac.df=vacancies2) {
  df.place <- df %>% group_by(!!! syms(cols)) %>% summarise(n=n())
  vac.place <- vac.df %>% group_by(!!! syms(cols)) %>% summarise(n=n())
  places <- inner_join(df.place,vac.place,by=cols)
  
  place.list <- as.list(rep(NA,nrow(places)))
  print(paste("Places: ",nrow(places)))
  for(i in 1:nrow(places)) {
    place.df.dates <- semi_join(df,places[i,],by=cols) %>%
      group_by(!!! syms(cols),date) %>% summarise(n=n())
    if(dim(place.df.dates)[1] == 0) place.list[i] <- NA
    else {
      # print("test")
      place.vac.df.dates <- semi_join(vac.df,places[i,],by=cols) %>%
        group_by(!!! syms(cols),issued.date) %>% summarise(vacancy.cnt = n())
      # print(place.vac.df.dates)
      if(dim(place.vac.df.dates)[1] == 0) place.list[i] <- NA
      else {
        if(exists("place.date.vacancies")) {
          place.date.vacancies <- bind_rows(
            place.date.vacancies
            ,cbind(places[i,length(cols)],aggr.vac.for.date(place.df.dates,place.vac.df.dates))
          )
        }
        else {
          place.date.vacancies <- cbind(places[i,length(cols)],aggr.vac.for.date(place.df.dates,place.vac.df.dates))
        }
      }
    }
  }
  return(place.date.vacancies)
}

# str(vacancies2)
# str(vacancies2$date)
# str(wnv.trap.date2$date)

CommArea.date.vacancies <- aggr.vac.by.col(c('community'))
summary(CommArea.date.vacancies)

vacancies2$BlkGrp.geoid <- as.character(vacancies2$BlkGrp.geoid)
wnv.trap.date2$BlkGrp.geoid <- as.character(wnv.trap.date2$BlkGrp.geoid)
BlkGrp.date.vacancies <- aggr.vac.by.col(c('BlkGrp.geoid'))
summary(BlkGrp.date.vacancies)

vacancies2$ZCTA5CE10 <- as.character(vacancies2$ZCTA5CE10)
wnv.trap.date2$ZCTA5CE10 <- as.character(wnv.trap.date2$ZCTA5CE10)
zcta.date.vacancies <- aggr.vac.by.col(c('ZCTA5CE10'))
summary(zcta.date.vacancies)

str(CommArea.date.vacancies)
str(BlkGrp.date.vacancies)
str(zcta.date.vacancies)


################################################################################
## Append these place- and time-specific data to WNV trap/date dataset
################################################################################

names(CommArea.date.vacancies)[names(CommArea.date.vacancies) == 'vacancy.cnt'] <- 'comm.180d.vacancies.cnt'
names(BlkGrp.date.vacancies)[names(BlkGrp.date.vacancies) == 'vacancy.cnt'] <- 'BlkGrp.180d.vacancies.cnt'
names(zcta.date.vacancies)[names(zcta.date.vacancies) == 'vacancy.cnt'] <- 'zcta.180d.vacancies.cnt'


wnv.trap.date3 <- left_join(wnv.trap.date2,CommArea.date.vacancies)
wnv.trap.date4 <- left_join(wnv.trap.date3,BlkGrp.date.vacancies)
wnv.trap.date5 <- left_join(wnv.trap.date4,zcta.date.vacancies)


summary(wnv.trap.date5$comm.180d.vacancies.cnt)
summary(wnv.trap.date5$BlkGrp.180d.vacancies.cnt)
summary(wnv.trap.date5$zcta.180d.vacancies.cnt)

wnv.trap.date5$comm.180d.vacancies.cnt <- coalesce(wnv.trap.date5$comm.180d.vacancies.cnt,as.integer(0))
wnv.trap.date5$BlkGrp.180d.vacancies.cnt <- coalesce(wnv.trap.date5$BlkGrp.180d.vacancies.cnt,as.integer(0))
wnv.trap.date5$zcta.180d.vacancies.cnt <- coalesce(wnv.trap.date5$zcta.180d.vacancies.cnt,as.integer(0))

summary(wnv.trap.date5$comm.180d.vacancies.cnt)
summary(wnv.trap.date5$BlkGrp.180d.vacancies.cnt)
summary(wnv.trap.date5$zcta.180d.vacancies.cnt)

summary(wnv.trap.date5$comm.180d.violation.cnt)
summary(wnv.trap.date5$BlkGrp.180d.violation.cnt)
summary(wnv.trap.date5$zcta.180d.violation.cnt)

summary(wnv.trap.date5$tot.NumMosquitos)



hist(wnv.trap.date5$comm.180d.vacancies.cnt)
hist(wnv.trap.date5$BlkGrp.180d.vacancies.cnt)
hist(wnv.trap.date5$zcta.180d.vacancies.cnt)

# chk <- wnv.trap.date5[,c("comm.180d.violation.cnt"
#                          ,"BlkGrp.180d.violation.cnt"
#                          ,"zcta.180d.violation.cnt"
#                          ,"comm.180d.vacancies.cnt"
#                          ,"BlkGrp.180d.vacancies.cnt"
#                          ,"zcta.180d.vacancies.cnt"
#                          # ,"LT_HS_pct__BlkGrp2017"
#                          # ,"LT_Pov_pct__BlkGrp2017"
#                          # ,"median_HHInc__BlkGrp2017"
#                          ,"lat"
#                          ,"lng"
#                          ,"tot.NumMosquitos")
#                       ]

# kdepairs(wnv.trap.date5[,c("comm.180d.vacancies.cnt"
#                            ,"BlkGrp.180d.vacancies.cnt"
#                            ,"zcta.180d.vacancies.cnt"
#                            # ,"LT_HS_pct__BlkGrp2017"
#                            # ,"LT_Pov_pct__BlkGrp2017"
#                            # ,"median_HHInc__BlkGrp2017"
#                            ,"lat"
#                            ,"lng"
#                            ,"tot.NumMosquitos")
#                         ])


################################################################################
## Save updated WNV trap date dataframe
################################################################################


wnv.trap.date <- wnv.trap.date5

getwd()
setwd(paste(repo.path,"data\\processed",sep='\\'))
getwd()


save(wnv.trap.date, file="wnv.trap.date.RData", compress = FALSE)
write.csv(wnv.trap.date,paste(repo.path,"data\\processed","wnv.trap.date.csv",sep='\\'),row.names=FALSE)








