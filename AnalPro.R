#######################loading#####
library(readODS)
library(rio)
library(readr)
library(dplyr)
library(rvest)
library(ggmap)
library(choroplethr)
wldISPop<-read_csv("https://goo.gl/9tsyQL",na = "NA")
wldISPop.df<-data.frame(
  wldISPop,stringsAsFactors = F
  )
remove(wldISPop)
wldISPop.df<-wldISPop.df[,c(1,2,3)]
OOOPop103_N<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
OOOPop104_N<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
OOOPop105_N<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
OOOPop106_N<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")

OOOPop103_C<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
OOOPop104_C<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
OOOPop105_C<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
OOOPop106_C<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")

TWtoWld <- read_csv("Student_RPT_07.csv", 
                    locale = locale(encoding = "BIG5"),
                    skip = 1)
TWtoWldNAME<-colnames(TWtoWld)
TWtoWldNAME01<-TWtoWld[1,]
colnames(TWtoWld)[c(3,4,5,6,11,12,13,14,15)]<-TWtoWldNAME01[c(3,4,5,6,11,12,13,14,15)]
TWtoWld <- TWtoWld[-1,]
remove(TWtoWldNAME01,TWtoWldNAME)
TWtoWld$小計<-as.numeric(TWtoWld$小計)
TWtoWld$男<-as.numeric(TWtoWld$男)
TWtoWld$女<-as.numeric(TWtoWld$女)
str(TWtoWld$小計)
View(TWtoWld)
##############################
#1.請問哪些國家來台灣唸書的學生最多呢？請取出前十名的國家與總人數，
#由大到小排序(5分)。又哪間大學的境外生最多呢？
#請取出前十名的大學與總人數，由大到小排序(5分)。加總103年以後的資料回答此題。
OOOPop103_N<-mutate(OOOPop103_N,
                    Pop103 = rowSums(OOOPop103_N[,c(-1,-2)]))
OOOPop104_N<-mutate(OOOPop104_N,
                    Pop104 = rowSums(OOOPop104_N[,c(-1,-2)]))
OOOPop105_N<-mutate(OOOPop105_N,
                    Pop105 = rowSums(OOOPop105_N[,c(-1,-2)]))
OOOPop106_N<-mutate(OOOPop106_N,
                    Pop106 = rowSums(OOOPop106_N[,c(-1,-2)]))


perYearPop<- inner_join(OOOPop103_N[,c(1,2,12)],OOOPop104_N[,c(1,2,12)],by = c("洲別", "國別"))

perYearPop<- inner_join(perYearPop,OOOPop105_N[,c(1,2,12)],by = c("洲別", "國別"))

perYearPop<- inner_join(perYearPop,OOOPop106_N[,c(1,2,12)],by = c("洲別", "國別"))



perYearPop<-mutate(perYearPop,
  totalPop = rowSums(perYearPop[,c(-1,-2)])) %>%
    arrange( desc(totalPop) , desc(Pop106) )
head(perYearPop, 10)

#學校
OOOPop103_C$`非學位生-大陸研修生`<-as.numeric(
  gsub("…",NA,OOOPop103_C$`非學位生-大陸研修生`))
OOOPop103_C<-mutate(OOOPop103_C,
                    Pop103 = rowSums(OOOPop103_C[,c(-1,-2,-3)], 
                                     na.rm = TRUE)  )
OOOPop104_C$`非學位生-大陸研修生`<-as.numeric(
  gsub("…",NA,OOOPop104_C$`非學位生-大陸研修生`))
OOOPop104_C<-mutate(OOOPop104_C,
                    Pop104 = rowSums(OOOPop104_C[,c(-1,-2,-3)], 
                                     na.rm = TRUE)  )
OOOPop105_C$`非學位生-大陸研修生`<-as.numeric(
  gsub("…",NA,OOOPop105_C$`非學位生-大陸研修生`))
OOOPop105_C<-mutate(OOOPop105_C,
                    Pop105 = rowSums(OOOPop105_C[,c(-1,-2,-3)], 
                                     na.rm = TRUE)  )
OOOPop106_C$`非學位生-大陸研修生`<-as.numeric(
  gsub("…",NA,OOOPop106_C$`非學位生-大陸研修生`))
OOOPop106_C<-mutate(OOOPop106_C,
                    Pop106 = rowSums(OOOPop106_C[,c(-1,-2,-3)], 
                                     na.rm = TRUE)  )
perColPop<- inner_join( OOOPop103_C[,c(3,13)] , 
                        OOOPop104_C[,c(3,13)],by = "學校名稱")
perColPop<- inner_join( perColPop , 
                        OOOPop105_C[,c(3,13)],by = "學校名稱")
perColPop<- inner_join( perColPop , 
                        OOOPop106_C[,c(3,13)],by = "學校名稱")
perColPop<- mutate(perColPop,
                   SumPop = rowSums( perColPop[,-1],
                                     na.rm = TRUE))%>%
  arrange(desc(SumPop))
head(perColPop[,c(1,6)],10)
##########################BAR####

ggplot(perYearPop)+
  geom_bar(aes(x=`國別`,y=totalPop),stat = "identity")+
  scale_y_sqrt()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#########################面量#####

library(readr)
EZ <- read_csv("EZ.csv", 
               locale = locale(encoding = "BIG5"))
colnames(EZ)<-c("國別",	"GEC代碼","二位字母代碼","iso_a3","ISO 3166-1三位數字代碼","STANAG_1059Stanag標準化國碼","網際網路"	,"註說")
countryCode<-left_join( perYearPop[,c(1,2,7)],EZ[,c(1,4)] ,by = "國別")

grep("[A-Z]{3}",countryCode$`iso_a3`,invert=T)
countryCode[grep("[A-Z]{3}",countryCode$iso_a3,invert=T),]
countryCode$iso_a3[grep("[A-Z]{3}",countryCode$iso_a3,invert=T)]<-
  c("CHN","KOR","ANZ","VCT","KNA","MHL","COD","SRB","UAR")

data(country.map)
country.map$iso_a3

final.plot<-merge(country.map,
                  countryCode,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
head(final.plot,10)

library(RColorBrewer) 
wldmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill =  totalPop), 
               color = "black", 
               size = 0.25,
               na.rm = T) +
  coord_cartesian(  xlim = c(-200, 200) ,ylim = c(-90, 90))+
  coord_fixed()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(7,"Reds"))+ 
  theme_void()+
  labs(fill="單位(人)",title = "各國來台留學生面量圖")
wldmap

###################胎灣人出去SHOW囉####
TWtoWld1 <- TWtoWld[grepl("進修|在職",TWtoWld$學制),]
datacheck<-TWtoWld1[grep("修|在",TWtoWld1$系所名稱),]
library(readr)
EZ <- read_csv("EZ.csv", 
               locale = locale(encoding = "BIG5"))
colnames(EZ)<-c("對方學校(機構)國別(地區)",	"GEC代碼","二位字母代碼","iso_a3","ISO 3166-1三位數字代碼","STANAG_1059Stanag標準化國碼","網際網路"	,"註說")

wldData1<-group_by(TWtoWld1,`對方學校(機構)國別(地區)`)%>% 
  summarise(cCount = sum(`小計`,na.rm = T))%>%
  arrange(desc(cCount))

countryCode1<-left_join( wldData1,EZ[,c(1,4)] ,by =  "對方學校(機構)國別(地區)")

countryCode1[grep("[A-Z]{3}",countryCode1$iso_a3,invert=T),]

countryCode1$iso_a3[grep("[A-Z]{3}",countryCode1$iso_a3,invert=T)]<-
  c("CHN","CHN","KOR","KOR","ANZ","VNM","THA","SGP","DEU","CZE",
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
countryCode1<-group_by(countryCode1,iso_a3)%>% 
  summarise(cCount = sum(cCount,na.rm = T))%>%
  arrange(desc(cCount))
wldIndex<-nrow(filter(countryCode1,cCount>20))

countryCode1<-group_by(countryCode1, `對方學校(機構)國別(地區)`)%>%
  tally(cCount, sort = TRUE) %>%
  group_by(`對方學校(機構)國別(地區)` =
             factor(c(`對方學校(機構)國別(地區)`[1:wldIndex],rep("Other", n() - wldIndex)),
                    levels = c(`對方學校(機構)國別(地區)`[1:wldIndex], "Other")) ) %>%
  tally(n)

knitr::kable(head(countryCode1,10))


clgData<-group_by(TWtoWld1,`學校名稱`)%>%
          summarise(cCount = sum(`小計`,na.rm = T))%>%
          arrange(desc(cCount))
clgIndex<-nrow(filter(clgData,cCount>10))
clgData<-group_by(clgData, `學校名稱`)%>%
          tally(cCount, sort = TRUE) %>%
          group_by(`學校名稱` = 
             factor(c(`學校名稱`[1:clgIndex],rep("Other", n() - clgIndex)),
                    levels = c(`學校名稱`[1:clgIndex], "Other")) ) %>%
  tally(n)
knitr::kable(head(clgData,10))
###################胎灣人出去SHOW囉BAR#####
ggplot(countryCode1)+
  geom_bar(aes(x =  iso_a3, y = cCount),
           stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###################胎灣人出去SHOW囉面量####
data(country.map)
#country.map$iso_a3

final.plot<-merge(country.map,
                  countryCode1,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
#head(final.plot,10)

library(RColorBrewer) 
wldmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill =  nn), 
               color = "black", 
               size = 0.25,
               na.rm = T) +
  coord_cartesian(  xlim = c(-200, 200) ,ylim = c(-90, 90))+
  coord_fixed()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(7,"Reds"))+ 
  theme_void()+
  labs(fill="單位(人)",title = "各國來台留學生面量圖")
wldmap
###################胎灣人出去國囉####
TWtoWld2 <- TWtoWld[!grepl("進修|在職",TWtoWld$學制),]
datacheck<-TWtoWld2[grep("修|在",TWtoWld2$系所名稱),]
wldData2<-group_by(TWtoWld2,`對方學校(機構)國別(地區)`)%>% 
  summarise(cCount = sum(`小計`,na.rm = T))%>%
  arrange(desc(cCount))
wldIndex<-nrow(filter(wldData2,cCount>20))

wldData2<-group_by(wldData2, `對方學校(機構)國別(地區)`)%>%
  tally(cCount, sort = TRUE) %>%
  group_by(`對方學校(機構)國別(地區)` = 
             factor(c(`對方學校(機構)國別(地區)`[1:wldIndex],rep("Other", n() - wldIndex)),
                    levels = c(`對方學校(機構)國別(地區)`[1:wldIndex], "Other")) ) %>%
  tally(n)
countryCode<-left_join( wldData2,EZ[,c(1,4)] ,by =  "對方學校(機構)國別(地區)")

grep("[A-Z]{3}",countryCode$`iso_a3`,invert=T)
countryCode[grep("[A-Z]{3}",countryCode$iso_a3,invert=T),]
countryCode$iso_a3[grep("[A-Z]{3}",countryCode$iso_a3,invert=T)]<-
  c("CHN","CHN","KOR","KOR","DEU","ANZ","THA","ESP","SGP","NLD",
    "CZE","AUT","RUS","SWE","PHL","BEL","IDN","FIN","ITA","POL",
    "TUR","VNM","IND","DNK","LTU","NOR","IRL","KHM","EGY","SVK","KHM")
countryCode<-group_by(countryCode,iso_a3)%>% 
  summarise(nn = sum(nn,na.rm = T))%>%
  arrange(desc(nn))
data(country.map)
final.plot<-merge(country.map,
                  countryCode,by="iso_a3",all.x=T)%>%
  group_by(group)%>%
  arrange(order)
knitr::kable(head(wldData2,10))

#############################backup########
TWtoWld2 <- TWtoWld[!grepl("進修|在職",TWtoWld$學制),]
datacheck<-TWtoWld2[grep("修|在",TWtoWld2$系所名稱),]
wldData<-group_by(TWtoWld,`對方學校(機構)國別(地區)`)%>% 
  summarise(cCount = sum(`小計`,na.rm = T))%>%
  arrange(desc(cCount))
wldIndex<-nrow(filter(wldData,cCount>20))

wldData<-group_by(wldData, `對方學校(機構)國別(地區)`)%>%
  tally(cCount, sort = TRUE) %>%
  group_by(`對方學校(機構)國別(地區)` = 
             factor(c(`對方學校(機構)國別(地區)`[1:wldIndex],rep("Other", n() - wldIndex)),
                    levels = c(`對方學校(機構)國別(地區)`[1:wldIndex], "Other")) ) %>%
  tally(n)
knitr::kable(head(wldData,10))
# library(readr)
# wldData1<-group_by(TWtoWld1,`對方學校(機構)國別(地區)`)%>% 
#   summarise(cCount = sum(`小計`,na.rm = T))%>%
#   arrange(desc(cCount))
# wldIndex<-nrow(filter(wldData1,cCount>20))
# wldData1<-group_by(wldData1, `對方學校(機構)國別(地區)`)%>%
#   tally(cCount, sort = TRUE) %>%
#   group_by(`對方學校(機構)國別(地區)` = 
#              factor(c(`對方學校(機構)國別(地區)`[1:wldIndex],rep("Other", n() - wldIndex)),
#                     levels = c(`對方學校(機構)國別(地區)`[1:wldIndex], "Other")) ) %>%
#   tally(n)
# countryCode<-left_join( wldData1,EZ[,c(1,4)] ,by =  "對方學校(機構)國別(地區)")
# countryCode$iso_a3[grep("[A-Z]{3}",countryCode$iso_a3,invert=T)]<-
#   c("CHN","CHN","KOR","KOR","DEU","ANZ","THA","SGP","ESP","NLD","CZE","AUT","RUS","SWE",NA)
# countryCode<-group_by(countryCode,iso_a3)%>% 
#   summarise(nn = sum(nn,na.rm = T))%>%
#   arrange(desc(nn))
###################胎灣人出去國囉面量#######
library(RColorBrewer) 
wldmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill =  nn), 
               color = "black", 
               size = 0.25,
               na.rm = T) +
  coord_cartesian(  xlim = c(-200, 200) ,ylim = c(-90, 90))+
  coord_fixed()+#維持地圖比例
  scale_fill_gradientn(colours = brewer.pal(7,"Reds"))+ 
  theme_void()+
  labs(fill="單位(人)",title = "各國來台留學生面量圖")
wldmap
################################
perYearPop<-NULL
perYearPop<- inner_join(OOOPop103_N[,c(1,2,12)],OOOPop104_N[,c(1,2,12)],by = c("洲別", "國別"))

perYearPop<- inner_join(perYearPop,OOOPop105_N[,c(1,2,12)],by = c("洲別", "國別"))

perYearPop<- inner_join(perYearPop,OOOPop106_N[,c(1,2,12)],by = c("洲別", "國別"))

perYearPop<-mutate(perYearPop,
                   totalPop = rowSums(perYearPop[,c(-1,-2)])) %>%
  arrange( desc(totalPop) , desc(Pop106) )
EZ1<-EZ[,c(1,4)]
colnames(EZ1)<-c("國別","iso_a3")
analData1<-left_join( perYearPop[,c(2,7)],EZ1 ,by =  "國別")
analData1$iso_a3[grep("[A-Z]{3}",analData1$iso_a3, invert=T)]<-
  c("CHN","KOR","ANZ","VCT","KNA","MHL","COD","SRB","UAR")

analData1Index<-nrow(filter(analData1,totalPop>20))
analData1<-group_by(analData1, iso_a3)%>%
  tally(totalPop, sort = TRUE) %>%
  group_by(iso_a3 = 
             factor(c(iso_a3[1:analData1Index],rep("Other", n() - analData1Index)),
                    levels = c(iso_a3[1:analData1Index], "Other")) ) %>%
  tally(n)

###############
TWtoWld2 <-rbind(TWtoWld%>%filter(`學年度`>102),TWtoWld%>%filter(`學年度`==102,`學期`==2))
TWtoWld2 <- TWtoWld2[!grepl("進修|在職",TWtoWld$`學制`),]
datacheck<-TWtoWld2[grep("修|在",TWtoWld2$`系所名稱`),]
wldData2<-group_by(TWtoWld2,`對方學校(機構)國別(地區)`)%>% 
  summarise(cCount = sum(`小計`,na.rm = T))%>%
  arrange(desc(cCount))
wldIndex<-nrow(filter(wldData2,cCount>20))

wldData2<-group_by(wldData2, `對方學校(機構)國別(地區)`)%>%
  tally(cCount, sort = TRUE) %>%
  group_by(`對方學校(機構)國別(地區)` = 
             factor(c(`對方學校(機構)國別(地區)`[1:wldIndex],rep("Other", n() - wldIndex)),
                    levels = c(`對方學校(機構)國別(地區)`[1:wldIndex], "Other")) ) %>%
  tally(n)
countryCode<-left_join( wldData2,EZ[,c(1,4)] ,by =  "對方學校(機構)國別(地區)")

grep("[A-Z]{3}",countryCode$`iso_a3`,invert=T)
countryCode[grep("[A-Z]{3}",countryCode$iso_a3,invert=T),]
countryCode$iso_a3[grep("[A-Z]{3}",countryCode$iso_a3,invert=T)]<-
  c("CHN","CHN","KOR","KOR","DEU","ANZ","THA","ESP","SGP","NLD",
    "CZE","AUT","RUS","SWE","PHL","BEL","IDN","FIN","ITA","POL",
    "TUR","VNM","IND","DNK","LTU","NOR","IRL","KHM","EGY","SVK","KHM")
countryCode<-group_by(countryCode,iso_a3)%>% 
  summarise(nn = sum(nn,na.rm = T))%>%
  arrange(desc(nn))
analData2<-countryCode
analData<-full_join(analData1,analData2,by = "iso_a3")


colnames(analData)<-c("iso","境內外國人","境外台灣人")
analData$TYPE<-ifelse(!is.na(analData[,2]-analData[,3]),"有交流","沒交流")

###################plot####
ggplot(analData,aes(TYPE,fill = TYPE))+
  geom_bar( width = 0.5)+
  coord_fixed(ratio = 1/30)+
  scale_fill_manual(values = c("red","royalblue"))
  
ggplot(analData)+
  geom_bar(aes(x=iso,y=`境內外國人`),
           stat = "identity",fill = "black")+
  geom_bar(aes(x=iso,y=`境外台灣人`),
           stat = "identity",fill = "red")+
  labs(fill="")+
  scale_y_sqrt()+
  theme(axis.text.x = 
          element_text(angle = 90, hjust = 1,size = 3))
