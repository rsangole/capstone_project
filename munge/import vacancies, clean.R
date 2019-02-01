
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
address.point.chi.head <- head(address.point.chi)

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


################################################################################
## Save updated  data
################################################################################


# save(wnv.traps, file="wnv.traps.RData", compress = FALSE)
# write.csv(wnv.traps, paste(my.path,'wnv.traps.csv',sep='\\'),row.names = FALSE)

write.csv(vacant.properties,paste(repo.path,"data\\raw\\vacant_properties","vacant.properties.csv",sep='\\'),row.names=FALSE)


################################################################################
## Clean up more unnecessary objects
################################################################################








