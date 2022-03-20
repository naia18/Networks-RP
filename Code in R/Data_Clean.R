# This file cleans the data by Banerjee et al. on "individual_characteristics.dta"
# Most of the subcastes have miswritten duplicates with typos
rm(list = ls())

# Import libraries
library("tidyverse")


data_ind <- read.dta("/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/Data/Duflo_Karnataka/Data/2. Demographics and Outcomes/individual_characteristics.dta")
# ------------------------------- Clean the data -------------------------------

# Remove whitespaces

# Adi Karnataka
data_ind[data_ind[,11]== "ADI  KARNATAKA" | data_ind[,11] == "ADI KARNATKA" 
         | data_ind[,11] == "ADI KARNAKATA" | data_ind[,11] == "ADI KATNATAKA" 
         | data_ind[,11] == "ADI KARNBATAKA" | data_ind[,11]=="ADIKARANATAKA"
         | data_ind[,11] == "ADIKARNAKATA" | data_ind[,11] == "ADIKARNATAKA"
         | data_ind[,11] == "ADI KARNATKA" | data_ind[,11] == "A K"
         | data_ind[,11] == "A.K" | data_ind[,11] == "AK"
         | data_ind[,11] == "ADI KARANATAKA"
         | data_ind[,11] == "ADI KARNATAKA",11] <- "ADIKARNATAKA"

# Adi Dravida
data_ind[data_ind[,11]== "A.D" | data_ind[,11] == "AD" 
         | data_ind[,11] == "ADI DHRAVIDA" 
         | data_ind[,11] == "ADI DRAVID" 
         | data_ind[,11] == "ADIDHRAVIDA"
         | data_ind[,11] == "ADIDRAVID"
         | data_ind[,11] == "ADI DRAVIDA",11] <- "ADIDRAVIDA"

data_ind[data_ind[,11]== "ACHAR" | data_ind[,11] == "ACHAR SHETTY" 
         | data_ind[,11] == "ACHARARU" 
         | data_ind[,11] == "ACHARY" 
         | data_ind[,11] == "ACHARYA",11] <- "ACHARI"

data_ind[data_ind[,11] == "AGARARU" | data_ind[,11] == "AGASA",11] <- "AGASARU"

data_ind[data_ind[,11] == "AKS" ,11] <- "AKKASALIGA"

data_ind[data_ind[,11] == "ALAMATHA GOWDA" 
         | data_ind[,11] == "ALMATA" | data_ind[,11] == "ALMATHA",11] <- "ALMATARU"

data_ind[data_ind[,11] == "BAIGH" ,11] <- "BAIG"

data_ind[data_ind[,11]== "BAJANJTHRI" | data_ind[,11] == "BAJANTHRE" 
         | data_ind[,11] == "BHAJANTHRI" 
         | data_ind[,11] == "BHAJANTRI"
         | data_ind[,11] == "BHAJENTHRI"
         | data_ind[,11] == "BHANDARI"
         | data_ind[,11] == "BAJANTRI"
         | data_ind[,11] == "BAJENTHRI",11] <- "BAJANTHRI"


data_ind[data_ind[,11]== "BALAGIGA" | data_ind[,11] == "BALAGIGARU" 
         | data_ind[,11] == "BALAJEGAS" | data_ind[,11] == "BALAJIG" 
         | data_ind[,11] == "BALAJIGARU" | data_ind[,11]=="BALAJIGAS"
         | data_ind[,11] == "BALIJA" | data_ind[,11] == "BALIJAGA"
         | data_ind[,11] == "BALIJIGA" | data_ind[,11] == "BALJIGARU"
         | data_ind[,11] == "BANAGIGA" | data_ind[,11] == "BANAJEGAS"
         | data_ind[,11] == "BANAJIGA" | data_ind[,11] == "BANAJIGARU"
         | data_ind[,11] == "BANAJIGAS"
         | data_ind[,11] == "BALJIGA",11] <- "BALAJIGA"

data_ind[data_ind[,11] == "BARBAR" ,11] <- "BARBER"

data_ind[data_ind[,11]== "BESTA" | data_ind[,11] == "BESTHA" 
         | data_ind[,11] == "BESTHARU"
         | data_ind[,11] == "BESTHRU"
         | data_ind[,11] == "BESTRU",11] <- "BESTARU"

data_ind[data_ind[,11]== "BHARAMINS" | data_ind[,11] == "BHARAMNI" 
         | data_ind[,11] == "BHRAMANA" | data_ind[,11] == "BHRAMIN" 
         | data_ind[,11] == "BHRAMINA" | data_ind[,11]=="BHRAMINS"
         | data_ind[,11] == "BRAHAMANA" | data_ind[,11] == "BRAHAMIN"
         | data_ind[,11] == "BRAHAMINS" | data_ind[,11] == "BRAHMANA"
         | data_ind[,11] == "BRAHMINS" | data_ind[,11] == "BRAMAN"
         | data_ind[,11] == "BRAMANA" | data_ind[,11] == "BRAMHINA"
         | data_ind[,11] == "BRAHMINS" | data_ind[,11] == "BRAMIN"
         | data_ind[,11] == "BRAMIN (SRI VYSHNAVA)" | data_ind[,11] == "BRHAMIN"
         | data_ind[,11] == "BRHAMINA"
         | data_ind[,11] == "BRAMHINS",11] <- "BRAHMIN"


data_ind[data_ind[,11]== "BHOVI (ADIKARNATAKA)" | data_ind[,11] == "BOVI" 
         | data_ind[,11] == "BOVI (ADI KARNATAKA)"
         | data_ind[,11] == "RAJ BHOVI",11] <- "BHOVI"

data_ind[data_ind[,11]== "BUDAG JANGAM" | data_ind[,11] == "BUDAGA JANGAM" 
         | data_ind[,11] == "BUDAGA JANGAMA" | data_ind[,11] == "BUDAGA JANGAMMA" 
         | data_ind[,11] == "BUDAGJANGAMA" | data_ind[,11]=="BUDAJANGAMA"
         | data_ind[,11] == "BUDDIJANGAMA" | data_ind[,11] == "BUDIGI JANGAMA"
         | data_ind[,11] == "BYADAR NAYAKA"
         | data_ind[,11] == "BUDUGA JANGAMA",11] <- "BUDUGAJANGAMA"

data_ind[data_ind[,11]== "CATHOLIC" | data_ind[,11] == "CRISTAIN" 
         | data_ind[,11] == "CRISTIAN"
         | data_ind[,11] == "ROMAN CATHOLIC"
         | data_ind[,11] == "PROTESTANT"
         | data_ind[,11] == "CONVERTED CHRISTIANITY",11] <- "CHRISTIAN"

data_ind[data_ind[,11] == "CHANNDASAR" ,11] <- "CHANNADASARU"

data_ind[data_ind[,11]== "DHARGI" | data_ind[,11] == "DHARJI" 
         | data_ind[,11] == "DASARARU",11] <- "DARJI"

data_ind[data_ind[,11]== "DHOBHI" | data_ind[,11] == "DOBHI" 
         | data_ind[,11] == "DOBI"
         | data_ind[,11] == "DOBI AGASA"
         | data_ind[,11] == "DOBI AGASARU",11] <- "DHOBI"

data_ind[data_ind[,11] == "DOMBURU" ,11] <- "DOMBORU"

data_ind[data_ind[,11]== "EDIGARU" | data_ind[,11] == "EDIGAS" 
         | data_ind[,11] == "ELEVAS"
         | data_ind[,11] == "IDIGARU",11] <- "EDIGA"


data_ind[data_ind[,11]== "GANEGASETTY" | data_ind[,11] == "GANGA SHETTY" 
         | data_ind[,11] == "GANGAMATHA" | data_ind[,11] == "GANGATAK GOWDA" 
         | data_ind[,11] == "GANIGA SHETTARU" | data_ind[,11]=="GANIGA SHETTY"
         | data_ind[,11] == "GANIGARU" | data_ind[,11] == "GANIGAS"
         | data_ind[,11] == "GANIGASETTY" | data_ind[,11] == "GANIGASHETTY"
         | data_ind[,11] == "GHANIGASHETTARU",11] <- "GANIGA"

data_ind[data_ind[,11] == "GOLLA" ,11] <- "GOLLARU"

# VOKKALIGA, GOWDA
data_ind[data_ind[,11]== "GOWDARU" 
         | data_ind[,11] == "GOWDAS"
         | data_ind[,11] == "REDDY"
         | data_ind[,11] == "REDDY VOKKALIGA",11] <- "GOWDA"

data_ind[data_ind[,11] == "HAKKIPIKKI" ,11] <- "HAKIPIKI"

data_ind[data_ind[,11] == "HALMATHA KURUBA" ,11] <- "HALAMATHA"

data_ind[data_ind[,11] == "HARIJAN" ,11] <- "HARIJANA"

data_ind[data_ind[,11]== "HOLEYA" | data_ind[,11] == "HOLIYA" 
         | data_ind[,11] == "HOLIYARU",11] <- "HELAVA"

data_ind[data_ind[,11]== "IYANGAR BHRAMIN" | data_ind[,11] == "IYANGAR BHRAMINS" 
         | data_ind[,11] == "IYENGER",11] <- "IYENGAR"


data_ind[data_ind[,11]== "JAGAMMA" 
         | data_ind[,11] == "JANGALI",11] <- "JANGAMA"

data_ind[data_ind[,11] == "JENUKURBAS" ,11] <- "JENUKURUBAS"

data_ind[data_ind[,11] == "JOGI JANGAMA" ,11] <- "JOGI"

data_ind[data_ind[,11]== "KADU KURUBAS" 
         | data_ind[,11] == "KADU KURUBARU"
         | data_ind[,11] == "KADU KURUBA",11] <- "KADUKURUBA"

data_ind[data_ind[,11] == "KARCHI" ,11] <- "KARACHA"

data_ind[data_ind[,11] == "KHANNADAS" ,11] <- "KHAN"

data_ind[data_ind[,11]== "KOMALIGARU SHETTY" 
         | data_ind[,11] == "KOMATURU",11] <- "KOMOTURU"

data_ind[data_ind[,11] == "KORACH" | data_ind[,11]=="KORASARU"
         |data_ind[,11] == "KORACHARU"
         | data_ind[,11] == "KARACHA",11] <- "KORACHA"

data_ind[data_ind[,11]== "KORAMA SHETTY" | data_ind[,11] == "KORAMASHETTARU" 
         | data_ind[,11] == "KORAMASHETTRU"
         | data_ind[,11] == "KORAMASHETTY"
         | data_ind[,11] == "KORMA"
         | data_ind[,11] == "KORVASHETTARU"
         | data_ind[,11] == "KORAMSHETTAR",11] <- "KORAMA"

data_ind[data_ind[,11] == "KSHTRIYA" | data_ind[,11]=="KSHOURIKA"
         |data_ind[,11] == "KSHITHRIYA",11] <- "KSHATRIYA"


data_ind[data_ind[,11]== "KUMBARASHETTY" | data_ind[,11] == "KUMBARASHETTRU" 
         | data_ind[,11] == "KUMBARA SHETTY" | data_ind[,11] == "KUMABARASHETTRU"
         | data_ind[,11] == "KUMBARA GOWDA"
         | data_ind[,11] == "KUMBARSHETTY"
         | data_ind[,11] == "KORMA"
         | data_ind[,11] == "KUMARASHETTY"
         | data_ind[,11] == "KUMBAR",11] <- "KUMBARA"

data_ind[data_ind[,11] == "KURUBHA"
         | data_ind[,11] == "KURUBAS"
         | data_ind[,11] == "KURUBARU"
         | data_ind[,11] == "KURUBAGOWDA"
         | data_ind[,11] == "KURUBA GOWDA",11] <- "KURUBA"

data_ind[data_ind[,11] == "LABBYE"
         |data_ind[,11] == "LABBE",11] <- "LABBAY"

data_ind[data_ind[,11] == "LAMBANI"
         |data_ind[,11] == "LAMBHANI",11] <- "LAVABIN"

data_ind[data_ind[,11] == "LINGAYAT"
         | data_ind[,11] == "LINGAYATHA"
         | data_ind[,11] == "LINGAITHRU"
         | data_ind[,11] == "LINGAYATHARU"
         | data_ind[,11] == "LINGAYATS"
         | data_ind[,11] == "LINGAYITHA"
         | data_ind[,11] == "LINGGAITARU",11] <- "LINGAYATH"

data_ind[data_ind[,11] == "MADIGARU" 
         |data_ind[,11] == "MAGGARU"
         | data_ind[,11] == "MANIGAR",11] <- "MADIGA"

data_ind[data_ind[,11] == "MADIVAL"
         | data_ind[,11] == "MADIVAL AGASAR"
         | data_ind[,11] == "MADIVAL DHOBI"
         | data_ind[,11] == "MADIVAL SHETTY"
         | data_ind[,11] == "MADIVALA SHETTARU"
         | data_ind[,11] == "MADIVALA SHETTRU"
         | data_ind[,11] == "MADIVALA SHETTY"
         | data_ind[,11] == "MADIVALARU"
         | data_ind[,11] == "MADIVALASHETTARU"
         | data_ind[,11] == "MADIVALASHETTY"
         | data_ind[,11] == "MADIWAL"
         | data_ind[,11] == "MADIWALA",11] <- "MADIVALA"

data_ind[data_ind[,11] == "MARATHI" 
         | data_ind[,11] == "MARATHIS" ,11] <- "MARATI"

data_ind[data_ind[,11] == "MARTHAMMA" 
         | data_ind[,11] == "SMARTHAS"
         | data_ind[,11] == "SMARTHARU",11] <- "MARATHA"

data_ind[data_ind[,11] == "MOHALLA" ,11] <- "MOGALL"

data_ind[data_ind[,11] == "MUSLIMS" ,11] <- "MUSLIM"

data_ind[data_ind[,11] == "NAGARTHA" 
         | data_ind[,11] == "NAGATHARU" ,11] <- "NAGATHRU"

data_ind[data_ind[,11] == "NAIKA" ,11] <- "NAIK"

data_ind[data_ind[,11] == "NAYDU"
         | data_ind[,11] == "NAYAN KSHATHRIYA"
         | data_ind[,11] == "NAYAKAS"
         | data_ind[,11] == "NAYAKARU VALMIKI"
         | data_ind[,11] == "NAYAKARU"
         | data_ind[,11] == "NAYAKA",11] <- "NAYAK"

data_ind[data_ind[,11] == "PATAN" 
         |data_ind[,11] == "PATHAN KHAN"
         | data_ind[,11] == "PATTARORU"
         | data_ind[,11] == "PATTER",11] <- "PATHAN"

data_ind[data_ind[,11] == "POLLIGARU" ,11] <- "POLIGARU"

data_ind[data_ind[,11] == "RAJA VAMSHA" ,11] <- "RAJAVAMSHA"

data_ind[data_ind[,11] == "SADARU" 
         | data_ind[,11] == "SADHRU" ,11] <- "SADHURU"

data_ind[data_ind[,11] == "SAYED"
         | data_ind[,11] == "SAYAD SAFEE"
         | data_ind[,11] == "SAIYAD"
         | data_ind[,11] == "SAIHADH"
         | data_ind[,11] == "SAHID"
         | data_ind[,11] == "SAYYAD",11] <- "SAYAD"

data_ind[data_ind[,11] == "SC" 
         |data_ind[,11] == "SCHEDULE TRIBE"
         | data_ind[,11] == "S.C"
         | data_ind[,11] == "-999"
         | data_ind[,11] == "7777777"
         | data_ind[,11] == "OBC"
         | data_ind[,11] == "ST",11] <- "TO BE DELETED"

data_ind[data_ind[,11] == "SHAIK" | data_ind[,11] == "SHAIKH"
         | data_ind[,11] == "SHAKH" | data_ind[,11] == "SAEKH"
         | data_ind[,11] == "SAEKH" | data_ind[,11] == "SHEAK"
         | data_ind[,11] == "SHIEK" | data_ind[,11] == "SHIEKH"
         | data_ind[,11] == "SHEIK KHAN" | data_ind[,11] == "SHEIKE"
         | data_ind[,11] == "SHEIKH" | data_ind[,11] == "SHEIKH SYED"
         | data_ind[,11] == "SHEK" | data_ind[,11] == "SHEKH"
         | data_ind[,11] == "SHEKHA" | data_ind[,11] == "SHIEK"
         | data_ind[,11] == "SHIEKH",11] <- "SHEIK"


data_ind[data_ind[,11] == "SETTY" 
         | data_ind[,11] == "SHETTYGANIGA" 
         | data_ind[,11] == "SHETTIGAR"
         | data_ind[,11] == "SHETTARU",11] <- "SHETTY"


data_ind[data_ind[,11] == "SHREE VAISHNAVA"
         | data_ind[,11] == "SHRI VAISHNAVA"
         | data_ind[,11] == "SHRIVISHNAVA"
         | data_ind[,11] == "SRI VAISHNAV"
         | data_ind[,11] == "SRI VISHNAV"
         | data_ind[,11] == "SRI VYSHNAVA"
         | data_ind[,11] == "SRI VAISHNAVA",11] <- "SRIVAISHNAVA"

data_ind[data_ind[,11] == "SHYAD" 
         | data_ind[,11] == "SYAD" ,11] <- "SYED"

data_ind[data_ind[,11] == "SUNIS" 
         | data_ind[,11] == "SUNNAKALLARU" ,11] <- "SUNNI"

data_ind[data_ind[,11] == "THIGAL GOWDAS"
         | data_ind[,11] == "THIGALA GOWDA"
         | data_ind[,11] == "THIGALA GOWDAS"
         | data_ind[,11] == "THIGALARU"
         | data_ind[,11] == "THIGALAS"
         | data_ind[,11] == "THIGULA JANANGA"
         | data_ind[,11] == "THIGULARU"
         | data_ind[,11] == "TIGALA"
         | data_ind[,11] == "TIGALARU"
         | data_ind[,11] == "TIGLA",11] <- "THIGALA"

data_ind[data_ind[,11] == "TOGATA" | data_ind[,11] == "THOGATA VEERA"
         | data_ind[,11] == "THOGATAVEERA" | data_ind[,11] == "THOGATAVEEVA"
         | data_ind[,11] == "THOGATRU" | data_ind[,11] == "THOTADAVARU"
         | data_ind[,11] == "THOTADORU" | data_ind[,11] == "THOTATHAVARU"
         | data_ind[,11] == "TOGATARU" 
         | data_ind[,11] == "TOGATAVEERA" ,11] <- "THOGATA"

data_ind[data_ind[,11] == "TOTIGA" 
         | data_ind[,11] == "TOTIGARU" ,11] <- "THOTIGAS"

data_ind[data_ind[,11] == "UPARU" 
         | data_ind[,11] == "UPPARA" ,11] <- "UPPAR"


data_ind[data_ind[,11] == "VAISHAS"
         | data_ind[,11] == "VAISHNAVAS"
         | data_ind[,11] == "VAISHYA"
         | data_ind[,11] == "VAISHYA ARADHYA",11] <- "VAISHNAVA"

data_ind[data_ind[,11] == "VAKKALIG"
         | data_ind[,11] == "VAKKALIGAS"
         | data_ind[,11] == "VAKKALIGARU",11] <- "VAKKALIGA"

data_ind[data_ind[,11] == "VIRASHAIVA" 
         | data_ind[,11] == "VEERASHIVA" ,11] <- "VEERASHAIVA"


data_ind[data_ind[,11] == "VALMIKI (NAIKA)"
         | data_ind[,11] == "VALMIKI NAYAK"
         | data_ind[,11] == "VALMIKI NAYAKA"
         | data_ind[,11] == "VALMIKI NAYAKARU"
         | data_ind[,11] == "VALMIKI NAYAKAS"
         | data_ind[,11] == "VALMUKI NAYAK",11] <- "VALMIKI"

data_ind[data_ind[,11] == "VANIKULA" 
         | data_ind[,11] == "VENNIKULA" ,11] <- "VANNIKULA"

data_ind[data_ind[,11] == "VISHAWAKARMA"
         | data_ind[,11] == "VISHNAVA"
         | data_ind[,11] == "VISHNAVA KARMA"
         | data_ind[,11] == "VISHYAS"
         | data_ind[,11] == "VISHVAKARMA"
         | data_ind[,11] == "VISHYA"
         | data_ind[,11] == "VISHWA KARMA"
         | data_ind[,11] == "VISHWAKARAMA"
         | data_ind[,11] == "VISHWAKARMA CHARI"
         | data_ind[,11] == "VISYA"
         | data_ind[,11] == "VYSHA"
         | data_ind[,11] == "VISWAKARMA",11] <- "VISHWAKARMA"

data_ind[data_ind[,11] == "VOKKALGAS"
         | data_ind[,11] == "VOKKALGIA"
         | data_ind[,11] == "VOKKALIGA GOWDA"
         | data_ind[,11] == "VOKKALIGARU"
         | data_ind[,11] == "VOKKALIGAS"
         | data_ind[,11] == "VOLKKALIGA"
         | data_ind[,11] == "YALAVARU VOKKALIGA"
         | data_ind[,11] == "REDDY VOKKALIGA",11] <- "VOKKALIGA"


data_ind[data_ind[,11] == "YADHAVAS" ,11] <- "YADAVA"

data_ind[data_ind[,11] == "YALAVAS"
         | data_ind[,11] == "YALAVARU"
         | data_ind[,11] == "YELAVAS",11] <- "HELAVA"

data_ind <- data_ind[!(data_ind[,11]=="TO BE DELETED"),]


# Delete whitespaces
data_ind[,11] <- gsub(" ", "", data_ind[,11], fixed = TRUE)

# library(tibble)
# tab <- as.data.frame(table(data_ind[,11]))
# 
# for (elem in tab[(tab[,2]<=3),][,1]) {
#   data_ind <- data_ind[!(data_ind[,11]==elem),]
# }

# Save cleaned data
save(data_ind, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 2/Networks/Term Paper/R code/individual_data.Rdata")

