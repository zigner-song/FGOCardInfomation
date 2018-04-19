##FGO Card Info

fgowiki_URL0<-"https://fgowiki.com/guide/petdetail/"
fgowiki_img_URL0<-"https://img.fgowiki.com/fgo/card/servant/"
N_Card<-205


datadetail.json<-NULL
datadetail.df<-data.frame()
setwd("E://practice//R//FGO//FGO//Kanons") #�趨���������·��
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
  
  
  #����ͼƬ
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
  
  #������
  now.Time<-Sys.time()
  duration0 <- difftime(now.Time,start.Time,units="sec") %>% as.numeric()
  ato.time<-(N_Card-i)/i*duration0
  stop.Time<-now.Time+ato.time
  progress0<- floor(i/N_Card*10000)/100
  progress<-paste(">>>> �����",progress0,"%,����",floor(duration0*100)/100,
                  "sec,��ʣ",floor(ato.time*100)/100,"sec,Ԥ�Ƶ�",stop.Time,"��� >>>>",
                  sep="")
  print(progress)
}

datadetail.json<-str_replace(datadetail.json,pattern = "^,","")
datadetail.json<-paste("[",datadetail.json,"]",sep="")
datadetail<-fromJSON(datadetail.json)

setwd("E://practice//R//FGO//FGO") #�趨�������ݵ�·��
write(datadetail.json,file="fgo_datadetail.json")
write.csv(datadetail.df,file="fgo_servant.csv")
