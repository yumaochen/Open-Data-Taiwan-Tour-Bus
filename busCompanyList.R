
companyPageList = rep("a",100) %>% as.character()

companyPageUrl = "https://www.mvdis.gov.tw/m3-emv-mk3/tourBus/query?d-1332667-p="

k=1 #k=1 to 94

for(k in 1:94){
  companyPageList[k] =
    paste0("https://www.mvdis.gov.tw/m3-emv-mk3/tourBus/query?d-1332667-p=", k, "")
}

kk = 1 #kk=1 to 94


#start
mylist = data.frame(matrix(rep("a",700), ncol=7), stringsAsFactors = F)

myentry = 'a' %>% as.character()

for(kk in 1:94){
  
  myurl = companyPageList[kk]
  
  getmyurl = GET(myurl)
  
  mycontent = content(getmyurl, "text")
  
  
  for(i in 1:10){
    
    startloc = gregexpr('goDetail\\(\'', mycontent)[[1]] %>% as.numeric()
    
    endloc = gregexpr('/></td></tr>', mycontent)[[1]] %>% as.numeric()
    
    myentry = substr(mycontent, startloc[i], endloc[i])
    
    myentry = str_replace_all(string=myentry, pattern=" ", repl="")
    
    quoteloc = gregexpr('\'', myentry)[[1]] %>% as.numeric()
    
    sqloc = min(quoteloc)
    
    eqloc = max(quoteloc)
    
    myentry = substr(myentry , sqloc, eqloc)
    
    myentry = str_replace_all(string=myentry, pattern="'", repl="")
    
    myentry = strsplit(myentry, ",") %>% unlist()
    
    mylist[kk,] = myentry
    
  }
  
}

mylist = mylist[-c(95:100),]

names(mylist) = c('number', 'name', 'city', 'district', 'address', 'phone', 'status')

write_excel_csv(mylist, "C:\\Users\\User\\Desktop\\shiny_tourBus\\busCompanyList.csv")

busCompanyList = read_csv("C:\\Users\\User\\Desktop\\shiny_tourBus\\busCompanyList.csv")


