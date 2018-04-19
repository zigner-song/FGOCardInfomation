#############
#  packages #
#############
library(XML)
library(rvest)
library(stringr)
library(dplyr)
library(stringi)
library(rjson)
library(curl)

###############
#  functions  #
###############

read_Url <- function(url) {
  #读取网页信息，避免由于暂时链接失效或者断网造成功亏一篑
  out <- tryCatch({
    url %>% as.character() %>% read_html() 
  },
  error=function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    return(NA)
  })    
  return(out)
}

curl_download_Url<-function(url,...) {
  #下载图片，避免由于暂时链接失效或者断网造成功亏一篑
  out <- tryCatch({
    url %>% as.character() %>% curl_download(...)
  },
  error=function(cond) {
    message(paste("URL does not seem to exist:", url))
    message("Here's the original error message:")
    message(cond)
    message("")
    return(NA)
  })    
  return(out)
}


###################
#  FGO Card Info  #
###################

fgowiki_URL0<-"https://fgowiki.com/guide/petdetail/"
fgowiki_img_URL0<-"https://img.fgowiki.com/fgo/card/servant/"

N_Card<-205 #现已更新至编号205的卡牌

datadetail.json<-NULL
datadetail.df<-data.frame()
setwd("E://practice//R//FGO//FGO//Kanons") #设定保存立绘的路径

start.Time<-Sys.time()
for(i in 1:N_Card){
  fgowik_URL<-paste(fgowiki_URL0,i,sep="")
  web0<-read_Url(fgowik_URL)
  #data detail 读取英灵信息
  datadetail.json0<-web0 %>% html_text() %>% 
                str_extract(pattern="var datadetail = \\[.+\\]") %>% 
                str_extract(pattern="\\[.+\\]") %>%
                stri_unescape_unicode %>% str_replace_all(pattern="(^.)|(.$)","")
  datadetail.json<-paste(datadetail.json,",",datadetail.json0,sep="") #以json格式合并英灵的信息
  
  #以data.frame的格式合并英灵的信息
  datadetail.df0<- fromJSON(datadetail.json0) %>% unlist() %>% t %>% data.frame()
  datadetail.df<-bind_rows(datadetail.df,datadetail.df0)
  
  #保存图片
  if(T){
  ii<-ifelse(i<10, paste("00",i,sep=""), ifelse(i<100,  paste("0",i,sep=""), trimws(as.character(i)))) #补全三位数
  fgowiki_img0<-paste(fgowiki_img_URL0,ii,LETTERS[1:5],".png",sep="")
  picName<-paste(ii,LETTERS[1:5],".png",sep="") #图片的命名，A~D分别为初始~满破，E为愚人节
  for(j in 1:5){curl_download_Url(fgowiki_img0[j],destfile = picName[j])}#下载英灵的5张图
   
  #进度条
  now.Time<-Sys.time()
  duration0 <- difftime(now.Time,start.Time,units="sec") %>% as.numeric() #已经花了多少时间
  ato.time<-(N_Card-i)/i*duration0 #还剩多少时间
  stop.Time<-now.Time+ato.time #预计到什么时候结束
  progress0<- floor(i/N_Card*10000)/100   #已完成的百分比
  progress<-paste(">>>> 已完成",progress0,"%,花费",floor(duration0*100)/100,
                  "sec,还剩",floor(ato.time*100)/100,"sec,预计到",stop.Time,"完成 >>>>",sep="")
  print(progress)
}
  
rm(list=na.omit(str_extract(ls(),pattern=".*0$")))  #删除临时变量

datadetail.json<-str_replace(datadetail.json,pattern = "^,","")
datadetail.json<-paste("[",datadetail.json,"]",sep="")
datadetail<-fromJSON(datadetail.json)

setwd("E://practice//R//FGO//FGO") #设定保存数据的路径
write(datadetail.json,file="fgo_datadetail.json")
write.csv(datadetail.df,file="fgo_servant.csv")

