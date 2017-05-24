### web scraping:

busCompanyList = read_csv("C:\\Users\\User\\Desktop\\shiny_tourBus\\busCompanyList.csv")

number = busCompanyList[,1] %>% unlist() %>% as.vector()

busUrl = "https://www.mvdis.gov.tw/m3-emv-mk3/tourBus/query?method=carEfficacy&seq="

busInfoList = matrix(as.character(rep("a",480000)), ncol=16)

counter = 0

tmp = tempfile()

bigN = nrow(number)

for(p in number){
  
  
  for(n in 1:9999){
    
    
    busFullUrl = paste0(busUrl, p, "&num=", n, sep="")
    
    getbusurl = GET(busFullUrl, write_disk(tmp, overwrite = TRUE) )
    
    buscont = content(getbusurl, "text")
    
    noValue = gregexpr('<td align=\\"center\\"></td>', buscont)[[1]] %>% as.numeric()
    
    if (noValue!=-1){
      
      break
      
    } else {
      
      counter = counter + 1
      
      startloc = gregexpr('<td align=\\"center\\">', buscont)[[1]] %>% as.numeric()
      
      endloc = gregexpr('</td>', buscont)[[1]] %>% as.numeric()
      
      for (i in 1:15){
        
        sloci = startloc[i] + 19
        
        eloci = endloc[i]
        
        busInfoList[counter ,i] = substr(buscont, sloci, eloci) %>% str_trim() 
        
        
      }
      
      busInfoList[counter, 16] = p
      
    }
    
  }
  
}

busInfoList1 = busInfoList[-c(16479:48000),] %>% as.data.frame(stringsAsFactors=F)
                                                               

write_excel_csv(busInfoList1, "C:\\Users\\User\\Desktop\\shiny_tourBus\\busInfoList1.csv")


### data cleaning:


names(busInfoList1) = c("TagNumber", "Make", "Year", "Expired", "Ticket", "Insurance", 
                       "Horsepower", "Torque", "Length", "Width",
                       "Weight", "EngineSize", "ABS", "BreakAssist", "GPS", "CompanyNumber")

busInfoList2 = cbind(busInfoList1[,1:4], busInfoList1$Ticket %>% str_split_fixed("/", n=2)
                    , busInfoList1[,6:16])


names(busInfoList2) = c("TagNumber", "Make", "Year", "Expired", "TicketProcessing",
                       "TicketAll", "Insurance", "Horsepower", "Torque", "Length", "Width",
                       "Weight", "EngineSize", "ABS", "BreakAssist", "GPS", "CompanyNumber")


busInfoList3 = lapply(busInfoList2, function(x) {
  gsub("<", "", x)
})

busInfoList4 = lapply(busInfoList3, function(x) {
  gsub("\t", "", x)
})

busInfoList5 = lapply(busInfoList4, function(x) {
  gsub("\r", "", x)
})

busInfoList6 = lapply(busInfoList5, function(x) {
  gsub("\n", "", x)
})
  
busInfoList7 = lapply(busInfoList6, function(x) {
  gsub("br/>", "", x)
}) %>%
  as.data.frame(stringsAsFactors=F)


busInfoList8 = busInfoList7


iupbound = nrow(busInfoList8)

for (i in 1: iupbound){
  if (nchar(busInfoList8$GPS[i])==3){
    busInfoList8$GPS[i] = substr(busInfoList8$GPS[i], 1, 3)
  } else{
    busInfoList8$GPS[i] = substr(busInfoList8$GPS[i], 1, 1)
  }
}


write_excel_csv(busInfoList8, "C:\\Users\\User\\Desktop\\shiny_tourBus\\busInfoList8.csv")


busInfoList9 = lapply(busInfoList8, function(x) {
  gsub("無資料", NA, x)
})

busInfoList10 = lapply(busInfoList9, function(x) {
  gsub("蒐集中", NA, x)
})

busInfoList11 = lapply(busInfoList10, function(x) {
  gsub("無", "N", x)
})

busInfoList12 = lapply(busInfoList11, function(x) {
  gsub("有", "Y", x)
}) 


busInfoList13 = busInfoList12

busInfoList13$Horsepower_kw = (busInfoList13$Horsepower %>% tolower() %>% str_split_fixed("kw", 2))[,1]

busInfoList13$Torque_kwm = (busInfoList13$Torque %>% tolower() %>% str_split_fixed("kg-m", 2))[,1]

busInfoList14 = busInfoList13 %>% as.data.frame() %>% select(-Horsepower, -Torque)

busInfoList14[, c(5,6,8,9,10,11,15,16,17)] = 
    busInfoList14[, c(5,6,8,9,10,11,15,16,17)] %>% lapply( function(x){as.numeric(as.character(x))})

busInfoList15 = busInfoList14

busInfoList15$Year = paste(busInfoList14$Year, "01", sep="") %>% as.Date(format="%Y%m%d")

write_excel_csv(busInfoList15, "C:\\Users\\User\\Desktop\\shiny_tourBus\\busInfoList15.csv")

