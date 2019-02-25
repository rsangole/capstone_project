
library("httr")
library("jsonlite")
library("scrapeR")

# https://www.chicago.gov/city/en/depts/cdph/supp_info/infectious/west_nile_virus_surveillancereports2011.html


wnv.reports.url <- "https://www.chicago.gov/city/en/depts/cdph/supp_info/infectious/west_nile_virus_surveillancereports2011.html"


# x <- GET(wnv.reports.url)
# x2 <- rawToChar(unlist(x[6]))
# # x3 <- fileToXML(x2)
# 
# x.html <- htmlTreeParse(x2, asText=TRUE)
# 
# x.html2 <- xmlRoot(x.html)
# 
# xpathSApply(rawToChar(x[6]),"//href",xmlValue)
# content(x)
# 
# xpathSApply(content(x),"//href",xmlValue)
# 
# content(x) %>% htmlTreeParse(.,asText=TRUE) %>% 
#   xmlParse(x)
# 
# xmlParse(rawToChar(GET(wnv.reports.url)[6]))


'West Nile Virus 2018 Weekly Reports'

www <- scrapeR::scrape(wnv.reports.url)

# w2 <- xmlParse(www[[1]])
w2 <- www[[1]]

links <- xpathSApply(w2,'//a',xmlGetAttr,'href') 
pdfs <- links[regexpr('.pdf',links) > 0]
arbovirus.pdf.list <- pdfs[regexpr('arbo',tolower(pdfs)) > 0]

url.base <- "https://www.chicago.gov"

arbovirus.pdf.urls <- ifelse(
  regexpr('http',tolower(arbovirus.pdf.list)) > 0
  ,arbovirus.pdfs
  ,paste(url.base,arbovirus.pdf.list,sep='')
) %>% gsub('https://author.cityofchicago.org','https://www.chicago.gov',.)

# arbo.1 <- GET(arbovirus.pdf.urls[1])
# arbo.1b <- rawToChar(arbo.1[6])
# content(arbo.1)

# https://www.chicago.gov/content/dam/city/depts/cdph/food_env/general/West_Nile_Virus/Week26Arboreport_722018.pdf
# 
# https://author.cityofchicago.org/content/dam/city/depts/cdph/food_env/general/West_Nile_Virus/Week23Arboreport_6132018.pdf
#   
# https://www.chicago.gov/content/dam/city/depts/cdph/food_env/general/West_Nile_Virus/Week26Arboreport_722018.pdf

install.packages("pdftools")
# library(pdftools)
# download.file("https://github.com/Huitziii/crispy-pdf/raw/master/71_PV.62.pdf",
#               "./71_PV.62.pdf")
# text <- pdftools::pdf_text(content(arbo.1))



download.wnv.pdf <- function(url) {
  x <- GET(url)
  cntnt <- content(x)
  # txt <- pdftools::pdf_text(cntnt)
  # human.loc <- regexpr('Human', txt)
  # # print(substring(txt,human.loc,nchar(txt)))
  # human.loc.end <- regexpr('\r\n',substring(txt,human.loc,nchar(txt)))
  # # print(human.loc)
  # # print(human.loc.end)
  # human.txt <- substring(txt,human.loc,human.loc + human.loc.end - 2)
  # return(human.txt)
  return(cntnt)
}

# read.wnv.pdf(arbovirus.pdf.urls[1])
# read.wnv.pdf(arbovirus.pdf.urls[2])
# 
# rm(arbovirus.pdf.urls)

arbovirus.pdfs <- sapply(arbovirus.pdf.urls,download.wnv.pdf)

class(arbovirus.pdfs)
str(arbovirus.pdfs)




arbovirus.pdf.txt <- sapply(arbovirus.pdfs,pdftools::pdf_text)


extract.human.details <- function(x) {
  human.loc <- regexpr('Human', x)
  # print(substring(x,human.loc,nchar(x)))
  human.loc.end <- regexpr('\r\n',substring(x,human.loc,nchar(x)))
  # print(human.loc)
  # print(human.loc.end)
  human.txt <- substring(x,human.loc,human.loc + human.loc.end - 2)
  return(human.txt)
}

# arbovirus.pdf.txt[1]
# extract.human.details(arbovirus.pdf.txt[1])

human.details <- sapply(arbovirus.pdf.txt,extract.human.details)

# arbovirus.pdf.urls[122]
# 
# arbovirus.pdf.txt[122]


arbovirus.pdf.txt.df <- data.frame(
  url = names(human.details)
  ,full.txt = as.character(arbovirus.pdf.txt)
  ,human.txt = as.character(sapply(human.details,function (x) { x[[1]] }))
  ,stringsAsFactors = FALSE
)

# length(human.details)

# sapply(human.details,function (x) { x[[1]] })

# arbovirus.pdf.txt.df$human.txt

arbovirus.pdf.txt.df$human.txt2 <- 
  gsub('reported','',
       gsub('\\.','',
            gsub('human','',
                 gsub('Two','2',
                      gsub('Six','6',
                           gsub('case','',
                                gsub('cases','',
                                     gsub('No human cases','0 cases',
                                          gsub('Humans:','',
                                               gsub(
                                                 "No cases"
                                                 ,'0'
                                                 ,gsub('Human cases:'
                                                       ,'Humans:'
                                                       ,arbovirus.pdf.txt.df$human.txt)
                                               ))))))))))


write.csv(arbovirus.pdf.txt.df
          ,paste('D:\\ajc188\\github\\capstone_project\\data\\raw\\chi_dept_public_health'
                 ,'wnv_reports.csv'
                 ,sep='\\'))


# Week26Arboreport







