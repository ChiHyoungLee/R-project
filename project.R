install.packages("rJava")
install.packages("RJDBC")
install.packages("ggplot2")
install.packages("ggplotly")
install.packages("ggplot")

library(rJava)
library(RJDBC)
library(dplyr)
library(ggplot2)
library(plotly)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver",
                   classPath="C:/ojdbc6.jar")

conn <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@localhost:1521/xe", 
                  "scott", "tiger")

r <- dbGetQuery(conn, "SELECT * FROM scott.population") 

r$DATA1<-as.character(r$DATA1)

population<- r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                          ,"20190601","20181201","20180601","20180405")
            ,NAME1 %in% c("용산구","성북구","중구","구로구","동작구"))


population1<- r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                                       ,"20190601","20181201","20180601","20180405")
                          ,NAME1 %in% c("서울시"))

population2 <- r %>% filter(DATA1 %in% c("20201201","20200601","20191201"
                                        ,"20190601","20181201","20180601","20180405")
                           ,NAME1 %in% c("서울시"))



##구별로 날짜에 따른 인구수를 각각 점,직선그래프로 표현
ggplotly(ggplot(data=population,
       mapping = aes(x=DATA1,
                     y=SUM1, group=NAME1,color=NAME1))+
  xlab('날짜')+ylab('인구수')+ geom_line(size=1.3)+geom_point(size=3))



##날짜에 따른 전체인구수 변화  점,직선 그래프
ggplotly(ggplot(data=population1,
       mapping = aes(x=DATA1,
                     y=SUM1, group=NAME1,color=NAME1))+ geom_line(size=1.3)+
         geom_point(size=3))


##서울 전체 인구수변화의 추세선
ggplotly(ggplot(data=population1,
       mapping = aes(x=DATA1,
                     y=SUM1, group=NAME1,color=NAME1))+ geom_smooth())

## 내국인수와 서울외유입된 수 비교
ggplot(data=population2,
                mapping = aes(x=DATA1,
                              y=K1/7, group=NAME1,color=NAME1))+
           xlab('날짜')+ylab('인구수')+ geom_line(size=1.3)+geom_point(size=3)+
  geom_line(data=population2, aes(x=DATA1,
            y=K2,group=NAME1,color=NAME1))

##### 트리맵 #####
# 첨부된 ojdbc6.jar 파일을 다운로드 받아서 C:\ 아래에 둠
options("install.lock"=FALSE)
install.packages("rJava")
install.packages("RJDBC")  

library(rJava)
library(RJDBC)  

getwd()
dir()
#데이터베이스 연결 설정
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver"
                   , classPath="C://ojdbc6.jar")

conn <- dbConnect(jdbcDriver, 
                  "jdbc:oracle:thin:@localhost:1521/xe", "scott", "tiger")
#sql을 사용해서 r로 데이터 가져오기
r <- dbGetQuery(conn, "SELECT * FROM scott.population")
class(r)
str(r)
r
View(r)




# 파이프 라인을 사용하기 위해 dplyr 패키지를 인스톨한다. 
install.packages("dplyr")
library(dplyr)



# filter를 사용하여 찾고자하는 DATA1과 NAME1의 내용을 가져온다.
population<- r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                                       ,"20190601","20181201","20180601")
                          ,NAME1 %in% c("용산구","성북구","중구","구로구","동작구"))


# population 구조 확인 
str(population)


## 5개구 총 인구수 비교 
# 총 인구수가 많은 데이터를 10개 추출
population %>% select(DATA1,SUM1,NAME1) %>% arrange(desc(SUM1)) %>% head(10)

# 총 인구수가 적은 데이터를 10개 추출
population %>% select(DATA1,SUM1,NAME1) %>% arrange(SUM1) %>% head(10)


# 분기별 성북구 총 인구수 비교
population %>% filter(NAME1 == "성북구") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# 분기별 동작구 총 인구수 비교
population %>% filter(NAME1 == "동작구") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# 분기별 중구 총 인구수 비교
population %>% filter(NAME1 == "중구") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# 분기별 용산구 총 인구수 비교
population %>% filter(NAME1 == "용산구") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# 분기별 구로구 총 인구수 비교
population %>% filter(NAME1 == "구로구") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))



## 트리 맵을 사용하기 위해 패키지를 인스톯 함 
install.packages("treemap")
library(treemap)
# index : 계층을 선언하는 매개변수

# vSizw : 상대적 크기

# vColor : 지정한 색깔로 칠해짐

treemap(population,
        index=c("DATA1","NAME1"), # DATA1를 먼저 표현하고 그안에 NAME1을 넣음
        vSize="SUM1",
        vColor="SUM1",
        type="value",
        title="총 인구")


treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K1",
        vColor="K1",
        type="value",
        title="내국인 생활 인구수")

treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K2",
        vColor="K2",
        type="value",
        title="서울 외 유입 인구수")


treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K3",
        vColor="K3",
        type="value",
        title="장기 체류 외국인 인구수")


treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K4",
        vColor="K4",
        type="value",
        title="단기 체류 외국인 인구수")


library(dplyr)
###########################################################
population4<-r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                                      ,"20190601","20181201")
                         ,NAME1%in%c("용산구","성북구","중구","구로구","동작구"))

population5<-arrange(population4,DATA1)

e<-ggplot(data=population5,
          mapping = aes(x=DATA1,
                        y=K4, fill=NAME1))+ geom_col(position = "dodge")
e


