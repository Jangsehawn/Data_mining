.libPaths()
.libPaths("E:/")
getwd()
setwd("E:/")

data= read.table("Students.dat",header = T)
head(data)

# 믿음여부 
data$alife <- ifelse(data$lif==1,1,2)
head(data)
believe=which(data$alife==1)
non_believe=which(data$alife==2)
believe
non_believe
M1 <- matrix(c(length(believe),c(length(non_believe))))
barplot(M1,beside = T,col = c(10,4),names.arg = c("믿음","믿지않음"))



# 성별

g1 <- which(data$gender==1 & data$alife==1)
g2 <- which(data$gender==0 & data$alife==2) 
g3 <- which(data$gender==1 & data$alife==2)
g4 <- which(data$gender==0 & data$alife==1)

M <- matrix(c(length(g1),length(g3),length(g4),length(g2)),2,2)
mosaic(M)
barplot(M,beside = T,col = c(10,4),names.arg = c("female","male"), main='성별 사후세계 믿음')


# age
d1<-density(data$age[which(data$alife==1)])
d0<-density(data$age[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(14,79),ylim=c(0,0.1),col=10,main='나이별 사후세계 믿음')
lines(d0,col=4)
legend(x=60,y=0.1,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))


#hsgpa :고등학교 학점(GPA) (4점 척도)
data$hsgpa
d1<-density(data$hsgpa[which(data$alife==1)])
d0<-density(data$hsgpa[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(0,5),ylim=c(0,2),col=10,main='고등학교 GPA별 사후세계 믿음')
lines(d0,col=4)
legend(x=4,y=1.9,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))


#cogpa : 대학 GPA

d1<-density(data$cogpa[which(data$alife==1)])
d0<-density(data$cogpa[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(0,5),ylim=c(0,2),col=10,main='대학교 GPA별 사후세계 믿음')
lines(d0,col=4)
legend(x=4,y=1.9,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))

#dhome : 본가에서 캠퍼스까지의 거리 (마일)
d1<-density(data$dhome[which(data$alife==1)])
d0<-density(data$dhome[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-1000,3000),ylim=c(0,0.001),col=10,main='본가에서 캠퍼스까지의 거리별 사후세계 믿음')
lines(d0,col=4)
legend(x=2500,y=0.001,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))
#dres :  현재 거주지에서 강의실까지의 거리 (마일)
d1<-density(data$dres[which(data$alife==1)])
d0<-density(data$dres[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,10),ylim=c(0,0.4),col=10,main='현재 거주지에서 강의실까지의 거리별 사후세계 믿음')
lines(d0,col=4)
legend(x=500,y=0.001,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))
#tv :TV를 시헝하는 평균시간 (주당)
d1<-density(data$tv[which(data$alife==1)])
d0<-density(data$tv[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,20),ylim=c(0,0.1),col=10,main='TV를 시청하는 평균시간별 사후세계 믿음')
lines(d0,col=4)
legend(x=-5,y=0.1,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))
#sport : 스포츠에 참여하거나 기타 신처 운동을 하는 평균시간 (주당)
d1<-density(data$sport[which(data$alife==1)])
d0<-density(data$sport[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,15),ylim=c(0,0.2),col=10,main='스포츠에 참여하거나 기타 신처 운동을 하는 평균시간 별 사후세계 믿음')
lines(d0,col=4)
legend(x=10,y=0.2,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))
#news : 1주일에 신문을 읽는 횟수
d1<-density(data$news[which(data$alife==1)])
d0<-density(data$news[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,15),ylim=c(0,0.2),col=10,main='1주일에 신문을 읽는 횟수별 사후세계 믿음')
lines(d0,col=4)
legend(x=10,y=0.2,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))
#aids : 아는 사람 중 에이즈로 사망한 사람 또는 HIV+인 사람 수
d1<-density(data$aids[which(data$alife==1)])
d0<-density(data$aids[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,15),ylim=c(0,0.5),col=10,main='아는 사람 중 에이즈로 사망한 사람 또는 HIV+인 사람 수별 사후세계 믿음')
lines(d0,col=4)
legend(x=10,y=0.5,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))
#veg : 채식주의자 여부 (1=예, 0=아니오)


g1 <- which(data$veg==1 & data$alife==1)
g2 <- which(data$veg==0 & data$alife==2) 
g3 <- which(data$veg==1 & data$alife==2)
g4 <- which(data$veg==0 & data$alife==1)

M <- matrix(c(length(g1),length(g3),length(g4),length(g2)),2,2)
mosaic(M)
barplot(M,beside = T,col = c(10,4),names.arg = c("vegan","none_vegan"), main='채식주의별 사후세계 믿음')



#affil : 가입 정당 (1=민주당, 2=공화당, 3=무소속)


g1 <- which(data$affil==1 & data$alife==1)
g2 <- which(data$affil==2 & data$alife==1) 
g3 <- which(data$affil==3 & data$alife==1)

g4 <- which(data$affil==1 & data$alife==2)
g5 <- which(data$affil==2 & data$alife==2)
g6 <- which(data$affil==3 & data$alife==2)

M <- matrix(c(length(g1),length(g4),length(g2),length(g5),length(g3),length(g6) ),2,3)
M
mosaic(M)
barplot(M,beside = T,col = c(10,4),names.arg = c("민주당","공화당","무소속"), main='가입정당별 사후세계 믿음')



#ideol : 정치성향 (1=매우 진보적, 2=진보적, 3=약간 진보적, 4=중간, 5=약간 보수적, 6=보수적, 7=매우 보수적)

d1<-density(data$ideol[which(data$alife==1)])
d0<-density(data$ideol[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,15),ylim=c(0,0.5),col=10,main='정치성향별 사후세계 믿음')
lines(d0,col=4)
legend(x=10,y=0.5,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))

#relig : 종교적인 행사에 참석하는 빈도 (0=절대 참석 안함, 1=가끔 참석함, 2=대부분 주일에 참석, 3=매주 참석)

d1<-density(data$relig[which(data$alife==1)])
d0<-density(data$relig[which(data$alife==2)])
d1
d0
plot(d1,xlim=c(-5,5),ylim=c(0,1),col=10,main='종교적인 행사에 참석하는 빈도별 사후세계 믿음')
lines(d0,col=4)
legend(x=3.8,y=1,c("믿음","안믿음"),col=c(10,4),pch=c(1,1))



#abor : 임신 3개월내 낙태의 합법화에 대한 의견 (1=찬성, 0=반대)
g1 <- which(data$abor==1 & data$alife==1)
g2 <- which(data$abor==0 & data$alife==2) 
g3 <- which(data$abor==1 & data$alife==2)
g4 <- which(data$abor==0 & data$alife==1)

M <- matrix(c(length(g1),length(g3),length(g4),length(g2)),2,2)
mosaic(M)
barplot(M,beside = T,col = c(10,4),names.arg = c("찬성","반대"), main='임신 3개월내 낙태의 합법화에 대한 의견별 사후세계 믿음')


#affirm : 차별금지 조례에 대한 의견 (1=찬성, 0=반대)

g1 <- which(data$affirm==1 & data$alife==1)
g2 <- which(data$affirm==0 & data$alife==2) 
g3 <- which(data$affirm==1 & data$alife==2)
g4 <- which(data$affirm==0 & data$alife==1)

M <- matrix(c(length(g1),length(g3),length(g4),length(g2)),2,2)
mosaic(M)
barplot(M,beside = T,col = c(10,4),names.arg = c("찬성","반대"), main='차별금지 조례에 대한 의견별 사후세계에 관한 믿음' )


