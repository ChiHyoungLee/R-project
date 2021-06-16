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
            ,NAME1 %in% c("��걸","���ϱ�","�߱�","���α�","���۱�"))


population1<- r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                                       ,"20190601","20181201","20180601","20180405")
                          ,NAME1 %in% c("�����"))

population2 <- r %>% filter(DATA1 %in% c("20201201","20200601","20191201"
                                        ,"20190601","20181201","20180601","20180405")
                           ,NAME1 %in% c("�����"))



##������ ��¥�� ���� �α����� ���� ��,�����׷����� ǥ��
ggplotly(ggplot(data=population,
       mapping = aes(x=DATA1,
                     y=SUM1, group=NAME1,color=NAME1))+
  xlab('��¥')+ylab('�α���')+ geom_line(size=1.3)+geom_point(size=3))



##��¥�� ���� ��ü�α��� ��ȭ  ��,���� �׷���
ggplotly(ggplot(data=population1,
       mapping = aes(x=DATA1,
                     y=SUM1, group=NAME1,color=NAME1))+ geom_line(size=1.3)+
         geom_point(size=3))


##���� ��ü �α�����ȭ�� �߼���
ggplotly(ggplot(data=population1,
       mapping = aes(x=DATA1,
                     y=SUM1, group=NAME1,color=NAME1))+ geom_smooth())

## �����μ��� ��������Ե� �� ��
ggplot(data=population2,
                mapping = aes(x=DATA1,
                              y=K1/7, group=NAME1,color=NAME1))+
           xlab('��¥')+ylab('�α���')+ geom_line(size=1.3)+geom_point(size=3)+
  geom_line(data=population2, aes(x=DATA1,
            y=K2,group=NAME1,color=NAME1))

##### Ʈ���� #####
# ÷�ε� ojdbc6.jar ������ �ٿ�ε� �޾Ƽ� C:\ �Ʒ��� ��
options("install.lock"=FALSE)
install.packages("rJava")
install.packages("RJDBC")  

library(rJava)
library(RJDBC)  

getwd()
dir()
#�����ͺ��̽� ���� ����
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver"
                   , classPath="C://ojdbc6.jar")

conn <- dbConnect(jdbcDriver, 
                  "jdbc:oracle:thin:@localhost:1521/xe", "scott", "tiger")
#sql�� ����ؼ� r�� ������ ��������
r <- dbGetQuery(conn, "SELECT * FROM scott.population")
class(r)
str(r)
r
View(r)




# ������ ������ ����ϱ� ���� dplyr ��Ű���� �ν����Ѵ�. 
install.packages("dplyr")
library(dplyr)



# filter�� ����Ͽ� ã�����ϴ� DATA1�� NAME1�� ������ �����´�.
population<- r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                                       ,"20190601","20181201","20180601")
                          ,NAME1 %in% c("��걸","���ϱ�","�߱�","���α�","���۱�"))


# population ���� Ȯ�� 
str(population)


## 5���� �� �α��� �� 
# �� �α����� ���� �����͸� 10�� ����
population %>% select(DATA1,SUM1,NAME1) %>% arrange(desc(SUM1)) %>% head(10)

# �� �α����� ���� �����͸� 10�� ����
population %>% select(DATA1,SUM1,NAME1) %>% arrange(SUM1) %>% head(10)


# �б⺰ ���ϱ� �� �α��� ��
population %>% filter(NAME1 == "���ϱ�") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# �б⺰ ���۱� �� �α��� ��
population %>% filter(NAME1 == "���۱�") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# �б⺰ �߱� �� �α��� ��
population %>% filter(NAME1 == "�߱�") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# �б⺰ ��걸 �� �α��� ��
population %>% filter(NAME1 == "��걸") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))

# �б⺰ ���α� �� �α��� ��
population %>% filter(NAME1 == "���α�") %>% select(DATA1,SUM1,NAME1) %>% 
  arrange(desc(SUM1))



## Ʈ�� ���� ����ϱ� ���� ��Ű���� �ν��� �� 
install.packages("treemap")
library(treemap)
# index : ������ �����ϴ� �Ű�����

# vSizw : ����� ũ��

# vColor : ������ ����� ĥ����

treemap(population,
        index=c("DATA1","NAME1"), # DATA1�� ���� ǥ���ϰ� �׾ȿ� NAME1�� ����
        vSize="SUM1",
        vColor="SUM1",
        type="value",
        title="�� �α�")


treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K1",
        vColor="K1",
        type="value",
        title="������ ��Ȱ �α���")

treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K2",
        vColor="K2",
        type="value",
        title="���� �� ���� �α���")


treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K3",
        vColor="K3",
        type="value",
        title="��� ü�� �ܱ��� �α���")


treemap(population,
        index=c("DATA1","NAME1"),
        vSize="K4",
        vColor="K4",
        type="value",
        title="�ܱ� ü�� �ܱ��� �α���")


library(dplyr)
###########################################################
population4<-r %>% filter(DATA1 %in% c("20210601","20201201","20200601","20191201"
                                      ,"20190601","20181201")
                         ,NAME1%in%c("��걸","���ϱ�","�߱�","���α�","���۱�"))

population5<-arrange(population4,DATA1)

e<-ggplot(data=population5,
          mapping = aes(x=DATA1,
                        y=K4, fill=NAME1))+ geom_col(position = "dodge")
e

