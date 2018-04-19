##FGO Card Info

fgowiki_URL0<-"https://fgowiki.com/guide/petdetail/"
fgowiki_img_URL0<-"https://img.fgowiki.com/fgo/card/servant/"
N_Card<-205


datadetail.json<-NULL
datadetail.df<-data.frame()
setwd("E://practice//R//FGO//FGO//Kanons") #设定保存立绘的路径
start.Time<-Sys.time()
for(i in 1:N_Card){
  #i<-202
  fgowik_URL<-paste(fgowiki_URL0,i,sep="")
  web0<-read_Url(fgowik_URL)
  #data detail
  datadetail.json0<-web0 %>% html_text() %>% 
                str_extract(pattern="var datadetail = \\[.+\\]") %>% 
                str_extract(pattern="\\[.+\\]") %>%
                stri_unescape_unicode %>% str_replace_all(pattern="(^.)|(.$)","")
  datadetail.df0<- fromJSON(datadetail.json0) %>% unlist() %>% t %>% data.frame()
  datadetail.df<-bind_rows(datadetail.df,datadetail.df0)
  datadetail.json<-paste(datadetail.json,",",datadetail.json0,sep="")
  
  
  #保存图片
  if(T){
  ii<-ifelse(i<10,
             paste("00",i,sep=""),
             ifelse(i<100,
                    paste("0",i,sep=""),
                    trimws(as.character(i))))
  fgowiki_img0<-paste(fgowiki_img_URL0,ii,LETTERS[1:5],".png",sep="")
  
  
 
  picName<-paste(ii,LETTERS[1:5],".png",sep="")
  
  for(j in 1:5){
    curl_download_Url(fgowiki_img0[j],destfile = picName[j])
  }
  }
  
  #进度条
  now.Time<-Sys.time()
  duration0 <- difftime(now.Time,start.Time,units="sec") %>% as.numeric()
  ato.time<-(N_Card-i)/i*duration0
  stop.Time<-now.Time+ato.time
  progress0<- floor(i/N_Card*10000)/100
  progress<-paste(">>>> 已完成",progress0,"%,花费",floor(duration0*100)/100,
                  "sec,还剩",floor(ato.time*100)/100,"sec,预计到",stop.Time,"完成 >>>>",
                  sep="")
  print(progress)
}

datadetail.json<-str_replace(datadetail.json,pattern = "^,","")
datadetail.json<-paste("[",datadetail.json,"]",sep="")
datadetail<-fromJSON(datadetail.json)

setwd("E://practice//R//FGO//FGO") #设定保存数据的路径
write(datadetail.json,file="fgo_datadetail.json")
write.csv(datadetail.df,file="fgo_servant.csv")

