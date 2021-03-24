rm(list = ls())

#加载相关包
library(dplyr)

library(ggplot2)

#ggplot2介绍
library(help = "ggplot2")
?ggplot

#散点图
#使用plot绘制$y = 2\sin(x)+\epsilon$的散点图
gen <- function(n){        #编写成函数
  set.seed(12345)          #使得随机数发生变为固定，便于结果重现
  x <- runif(n,0,6*pi)     #x服从$[0, 6\pi]$上的均匀分布
  y <- 2*sin(x)+rnorm(n,0,0.25)      
  dataa <- as.data.frame(cbind(x,y))   #合成表格
  plot(x,y)                #绘图
  return()                 #这里实际上没有返回值，所以为空
}

gen(100)       #运行函数，生成100个符合要求的点对并画散点图

#使用ggplot绘制
#随机数生成
set.seed(12345)
x <- runif(100, 0, 6*pi)
y <- 2*sin(x)+rnorm(100, 0, 0.25)
dataa <- as.data.frame(cbind(x,y))

#绘制$y = 2\sin(x)+\epsilon$的散点图
ggplot(data = dataa,mapping = aes(x = x,y = y)) + 
  geom_point()+theme_bw()

#等价操作
ggplot(data = dataa, aes(x = x, y = y)) + 
  stat_summary(geom = "point", fun = "mean")+theme_bw()

#添加拟合曲线，这里选择使用$y ~ \sin(x)$
ggplot(data = dataa,mapping = aes(x = x,y = y)) + 
  geom_point()+theme_bw() + 
  geom_smooth(mapping = aes(x = x, y = y),formula = y~sin(x))


#柱状图
library(nCov2019)
ncov <- nCov2019::load_nCov2019(lang = "en") #注意需要联网

#世界范围新冠数据
ncov1 <- ncov$global 

#选出美国、印度和巴西的数据，并找出累计确诊人数
ncov2 <- rename(ncov1, time1 = time)%>%   #这里避开名称time，防止系统误判
  filter(country == "United States"|country == "India"|country == "Brazil")%>%
  group_by(country)%>%
  summarise(max(cum_dead))

#更换名称，使得变量容易表示
ncov2 <- rename(ncov2, dead = `max(cum_dead)`)
#换序，这里按照升序排列
ncov2 <- ncov2[order(ncov2$dead, decreasing = F),] 
#按照死亡人数升序排列绘制死亡人数柱状图
ggplot(ncov2, mapping = aes(x = reorder(country, dead), y = dead, fill = country, label = dead)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    geom_text(aes(label = dead, vjust = -1)) + 
    xlab("country") + 
    coord_cartesian(ylim = c(0,600000))


#仅仅筛选出美国、印度和巴西的数据
ncov4 <- filter(ncov1, country == "United States"|country == "India"|country == "Brazil")
#截取3月17日到3月19日数据
ncov5 <- ncov4[1208:1216,]
#并排柱状图
p1 <- ggplot(ncov5,aes(x = time, y = cum_dead, group = factor(country), fill = country)) + 
      geom_bar(stat = "identity", position = "dodge")
#堆叠柱状图
p2 <- ggplot(ncov5,aes(x = time, y = cum_dead, group = factor(country), fill = country)) + 
      geom_bar(stat = "identity", position = "stack", width = 0.5)
#使用ggpubr并排输出
library(ggpubr)
ggarrange(p1,p2,ncol = 2)   #2列，即并排

#折线图
data1 <- load_nCov2019(lang = "en")

data3 <- data1$global

head(data3)
summary(data3)
class(data3$time)#注意time的格式，无法直接用于逻辑筛选

#美国和印度的数据
data4 <- filter(data3,country == "United States"|country == "India")
#绘制折线图
ggplot(data4, aes(time, cum_confirm, colour = country)) + 
  geom_line()+theme_bw() + 
  labs(xlab = "time", ylab = "total")

#直方图
#导入数据，地址可以修改
library(readxl)
crawfish <- readxl::read_xlsx("C:/Users/lenovo/Desktop/Doc.1/Class 4/crawfish.xlsx")
#非对数变换的销量直方图
ggplot(crawfish,aes(sale)) + 
  geom_histogram(fill="blue",color = "red",bins = 100) + theme_bw()
#对数变换的销量直方图
ggplot(crawfish,aes(sale)) + 
  geom_histogram(fill="blue",color = "red",bins=100) + 
  scale_x_continuous(trans = "log") + 
  theme_bw() + xlab("log(sale)")


#箱线图
#非对数变换的箱线图
ggplot(data = crawfish,mapping = aes(x = reorder(city,-sale), y = sale, fill = city)) + 
    labs(x = "city" , y="sale") + 
    stat_boxplot(geom = "errorbar") + 
    geom_boxplot(varwidth = TRUE)+theme_update()
#对数变换的向线图
ggplot(data = crawfish,mapping = aes(x = reorder(city,-sale), y = sale, fill = city)) + 
    labs(x = "city" , y="log(sale)") + 
    stat_boxplot(geom = "errorbar") + 
    geom_boxplot(varwidth = TRUE) + 
    scale_y_continuous(trans = "log")+theme_update()


#地图绘制
world<-map_data("world2") #以太平洋为中心的世界地图
#黑白地图
ggplot(world, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
#彩色地图
ggplot(world, aes(long, lat, group = group)) +
  geom_polygon(fill = terrain.colors(99385), colour = "black") +
  coord_quickmap()+theme_minimal()
