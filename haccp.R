getwd()
library(MASS)
library(dplyr)
library(ggplot2)
install.packages("ggplot2")

healthInf <- read.csv(".csv", header=T)

View(healthInf)
healthInf <- as.data.frame(healthInf)

names(healthInf)
healthInf_subset <- healthInf[,c(1:12)]
names(healthInf_subset) <- c("제품보관", "섭취량", "제품명", "제품년도", "제조업체", "라이선스번호", "주의사항", 
                             "유통기한", "형태", "주요기능", "레포트번호", "함유성분")

healthInf_set <- healthInf_subset[sample(nrow(healthInf_subset)),]
healthInf_set <- select(healthInf_subset, 제품보관, 섭취량, 제품명, 제조업체, 주의사항, 형태, 주요기능, 함유성분)
healthInf_sub <- subset(healthInf_subset, select=c(제품보관, 섭취량, 제품명, 제조업체, 주의사항, 형태, 주요기능, 함유성분))

View(healthInf_sub)
data.frame(healthInf_sub)

health_hac <- read.csv("건강_변수제거.csv", header=T)
names(health_hac)
health_hac <- subset(health_hac, select=c(제품보관, 섭취량, 제품명, 제조업체, 주의사항, 형태, 주요기능, 함유성분, X0))

names(health_hac) <- c("keeping", "intake", "prd_name", "com_name", "caution", "form", "pri_fuction", "ingrediant", "Haccp")

table(health_hac$Haccp)
bar <- ggplot(health_hac, aes(x=factor(1), fill=factor(Haccp)))+
  geom_bar(width=1)

pie <- bar + coord_polar(theta="y")
pie
#write.csv(health_hac, file="health_hac.csv")

health_hac <-read.csv("health_hac.csv", header=T)
health_hac <- as.data.frame(health_hac)
health_hac <- health_hac[,-1]
View(health_hac)

ingre <- as.data.frame(health_hac$ingrediant, stringAsFactors=FALSE)
install.packages("data.table", dependencies = TRUE)

library(data.table)
ingre2 <- as.data.frame(tstrsplit(ingre[,1], '[,]', type.convert=TRUE), stringAsFactors=FALSE)
names(ingre2)
colnames(ingre2) <- c(1:36)

ingre_list <- c("비타민C","아연","비타민B6","비타민B1","비타민B2","비타민E","비타민D","홍삼제품","나이아신","판토텐산","비타민A","엽산","셀레늄(또는셀렌)","칼슘","가르시니아캄보지아추출물","프로바이오틱스제품","망간","비타민B12","철","비오틴","오메가-3지방산함유유지제품","홍삼","마그네슘","프로바이오틱스(원료성)","프로바이오틱스","크롬","베타카로틴","구리","프로폴리스추출물제품","시발","홍삼(원료성)","단백질","글루코사민제품","인삼제품","루테인제품","차전자피제품","감마리놀렌산함유유지제품","옥타코사놀함유유지제품","요오드","밀크씨슬(카르두스마리아누스)추출물제품","비타민K","쏘팔메토열매추출물제품","난소화성말토덱스트린제품","N-아세틸글루코사민제품","EPA및DHA함유유지","코엔자임Q10제품","녹차추출물제품","밀크씨슬(카르두스마리아누스)추출물","공액리놀레산","알로에전잎제품","은행잎추출물제품","몰리브덴","루테인","홍경천추출물","알로에겔제품","식이섬유","은행잎추출물","키토산제품","감마리놀렌산함유유지","클로렐라제품","백수오등복합추출물","스피루리나제품","프로폴리스추출물","히알루론산(제2008-37호)","코엔자임Q10","난소화성말토덱스트린","차전자피식이섬유","레시틴제품","오메가-3지방산함유유지","대두이소플라본제품","회화나무열매추출물","옥타코사놀함유유지","HK나토배양물","루테인지아잔틴복합추출물20%","프락토올리고당제품","밀크씨슬추출물","녹차추출물","피브로인추출물BF-7","알로에전잎","이눌린제품","인삼(원료성)","스피루리나","필수지방산","폴리코사놀-사탕수수왁스알코올(2006-4)","테아닌제품","N-아세틸글루코사민","키토올리고당제품","히알루론산","키토산/키토올리고당","엽록소함유식물제품","바나바잎추출물제품","HK표고버섯균사체","폴리덱스트로스제품","크레아틴(2007-13)","나토균배양분말","헛개나무과병추출분말(제2008-55호)","돌외잎주정추출분말","쏘팔메토열매추출물","테아닌","미역등복합추출물(잔티젠)","프락토올리고당","인삼","프로폴리스추출물(원료성)","콜레우스포스콜리추출물","헤마토코쿠스추출물제품","키토올리고당(원료성)","귀리식이섬유제품","홍국제품","피크노제놀-프랑스해안송껍질추출물(2006-1)","MR-10민들레등복합추출물","대두이소플라본","L-카르니틴타르트레이트","와일드망고종자추출물(IGOB131)","폴리덱스트로스","석류농축액","바나바잎추출물","대웅코엔자임Q10(2007-192008-36)","구아바잎추출물등복합물(BENDU381)","자일로올리고당(xylooligosaccharide)분말","클로렐라","헛개나무과병추출분말","알로에겔","오비엑스(Ob-X)","스쿠알렌제품","BT-11원지추출분말","솔잎증류농축액","폴리감마글루탐산","라피노스(2006-15)","정어리펩타이드(2004-1)","LactobacillussakeiProbio65","초록입홍합추출오일복합물","글루코사민","알콕시글리세롤함유상어간유제품","강황추출물","크랜베리추출물(Cran-Max)","레시틴(원료성)","키토산","홍국","L-글루타민산유래GABA함유분말","과채유래유산균(L.plantarumCJLP133)","효모베타글루칸","황기추출물등복합물(HT042)","석류추출물","다래추출물","달맞이꽃종자추출물제품","와일드망고종자추출물","치커리추출물제품","마그나스표고균사체추출물분말","영지버섯자실체추출물제품","오메가-3지방산함유유지(원료성)","그린커피빈추출물","표고버섯균사체AHCC(2008-78호)","토마토추출물(제2008-51호)","미쯔비시코엔자임Q10","유단백가수분해물(락티움)","로즈힙분말","이눌린/치커리추출물","공액리놀렌산(ConjugatedLinoleicacid)","마늘","인삼가수분해농축액","매실추출물제품","녹차추출물(원료성)","전칠삼추출물등복합물","대두이소플라본(원료성)","유산균발효다시마추출물","청국장균배양정제물(폴리감마글루탐산칼륨)","마늘제품","EPA및DHA함유유지제품","엽록소함유식물","포스파티딜세린","곤약감자추출분말","알로에전잎(원료성)","헤마토코쿠스추출물","PME88메론추출물","화일코엔자임Q10(2008-26)","옥타코사놀함유유지(원료성)","저분자콜라겐펩타이드","포스파티딜세린제품","일신-바나바주정추출분말(제2007-19호)","참당귀추출분말(Nutragen)","아티초크추출물","귀리식이섬유","호박씨추출물등복합물","리프리놀-초록입홍합추출오일","씨폴리놀감태주정추출물","발효울금","피니톨(2005-15)","바나바주정추출물(2005-3)","바나바주정추출물","레시틴","풋사과추출폴리페놀(Applephenon)","가르시니아캄보지아껍질추출물60HCA")
ingre_matrix <- matrix(0, 20736, 192)
ingre_matrix[1,] <- ingre_list
colnames(ingre_matrix) <- ingre_list

for(i in 1:nrow(ingre2)){
  for(j in 1:ncol(ingre2)){
    ingmat_col = which(ingre_matrix[1,]==ingre2[i,j])
    ingre_matrix[i+1,ingmat_col] <- 1
  }
}

ingre_matrix2 <- as.data.frame(ingre_matrix[-1,], stringAsFactors=F)
ingre_col <- health_hac[,3]
ingre_matrix2 <- cbind(ingre_col, ingre_matrix2)

ingre_matrix2 <- read.csv("matrix_new.csv", header=T)
ingre_matrix2 <- as.data.frame(ingre_matrix2)

n <- 1000
ingre_matrix2[sample(length(ingre_matrix2), size=n)] <- rep(-1:1, length=n)



######################################################################################
####################유사제품 추출해내는 부분 python ########################################
######################################################################################



########User Information
user_info <- ingre_matrix2[20735,]

hamingDis <- matrix(0, 20735, 1)
hamingDis <- as.data.frame(hamingDis)

for(i in 1:20735){
  for(j in 2:193){
    if(ingre_matrix2[i,j] != user_info[1,j]){
      hamingDis[i,1] <- hamingDis[i,1]+1
    }
  }
}

hamingCol <- c(1:20735)
hamingCol <- cbind(hamingCol, hamingDis)

View(hamingCol)
table(hamingCol)
hist(hamingCol$V1)

a <- head(order(hamingCol$V1))
a <- as.data.frame(a)
a <- a[1:3, ]
a <- as.data.frame(a)
colnames(a) <- c("col")

b <- c(1:20735)
c <- cbind(b, ingre_matrix2)
recName = data.frame()
for(i in 1:20735){
  for(j in 1:3){
  if(c[i,1]==a[j,1]){
    recName[j,1] <- c[i,2]
  }
  }
}
head(recName)
summary(recName)
