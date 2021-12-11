#For any given London Borough, are the Blue Plaques within that borough distributed randomly or do they exhibit some kind of dispersed or clustered pattern?
#the Point Pattern Analysis functions <spatstat> package

#first library a few packages that we will use during the practical
#note you may need to install them first...
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(dplyr)

##set up data
##First, get the London Borough Boundaries
LondonBoroughs <- st_read(here::here("prac6_data", 
                                     "statistical-gis-boundaries-london", 
                                     "ESRI", 
                                     "London_Borough_Excluding_MHW.shp"))
#挑选在GSS_CODE这一列以E09开头的数据
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)
summary(BoroughMap)

##Now get the location of all Blue Plaques in the City
BluePlaques <- st_read(here::here("prac6_data",
                                  "open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(.,27700)
summary(BluePlaques)

#plot the blue plaques in the city
tmap_mode("plot")

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) + #alha透明度可以重叠显示
tm_shape(BluePlaques) +
  tm_dots(col = "blue") #refelct points nature of the data

#remove duplicates
library(tidyverse)

library(sf)
BluePlaques <- BluePlaques%>%
  distinct(geometry, .keep_all = T)

#选择伦敦市内的点
##spacial subsetting: select the points inside London &select from location
BluePlaquesSub <- BluePlaques[BoroughMap,]  #The default is intersects
#or
#BluePlaquesSub <- BluePlaques[BoroughMap, , op = st_within]
#to identify points completely within the borough outline

#check to see that they've been removed
tmap_mode("plot")
tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

#选择harrow的区域
#extract the borough
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)

#clip our Blue Plaques so that we have a subset of just those that fall within the borough or interest
#把blueplaques的数据剪辑成只有harrow的
#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]
#check that it's worked
tmap_mode("plot")

#图和数据：harrow地图和上面的蓝点
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")





####part2
#创建窗口，以便spatstat在其中进行分析
#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)


#点模式分析，创建点模式（ppp）对象
#create a sp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#create a ppp object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)

BluePlaquesSub@coords[,1]
BluePlaquesSub@coords[,2]

#look new ppp project
BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")


###点阵分析
##核密度分析
##总结点数据的一种方法是在称为“内核”（‘Kernel’）的窗口下绘制点的密度
#生成内核密度估计 (KDE) 地图（ Kernel Density Estimation (KDE) map）
#The sigma value sets the diameter of the Kernel 
#(in the units your map is in — in this case, as we are in British National Grid the units are in metres).
BluePlaquesSub.ppp %>%
  density(., sigma=500) %>%  #sigma 规定内核的直径  diameter of the Kernel 
  plot()



##二次方分析
#研究区域中的点分布是否不同于“完全空间随机性” —— CSR

plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
#网格覆盖在harrow地图ppp上
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")


#将我们观察到的点分布与基于泊松分布（Poisson distribution）的统计上可能的（完全空间随机）分布进行比较
#run the quadrat count 把6*6的网格放在地图上
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>% #把它变成一个data frame
#每个格子 格子里的个数Freq
  dplyr::count(Var1=Freq)%>% #每个网格里的数据（为0、1、2、3...的）
#var1 n（网格数量）
  dplyr::rename(Freqquadratcount=n) #重命名n

#检查第一列中的数据类型
Qcount %>% 
  summarise_all(class)

#计算我们的期望值
#期望公式

##expected probabilities预期的随便散落的情况
sums <- Qcount %>%
#calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>% #纵向相加
  dplyr::select(-Var1) #去掉 variable 1这一列

lambda<- Qcount%>% #希腊字母lambda 表示平均数
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>% #每列加起来求和
  mutate(lambda=total/Freqquadratcount) %>% #平均数的公式
  dplyr::select(lambda)%>%
  pull(lambda) #remeove column序号/只拽取出lambda这一列

##Calculate expected
QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, 
       col="Blue", 
       type="o", 
       lwd=3)

#我们可以观察到它们在低端都有更高的频率计数
#这让人想起泊松分布。这可能表明对于这组特定的象限，
#我们的模式接近完全空间随机性（即没有点的聚类或分散）

#为了确定，我们可以使用quadrat.test()内置于spatstat
#卡方检验来比较每个象限的观察频率和预期频率
#（而不是象限箱，正如我们刚刚计算的那样）
#原假设是没有pattern
#What we need to look for is a value for p > 0.05. 
#If our p-value is > 0.05 then this indicates that we have CSR and there is no pattern in our points.
#a Chi Squared test
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")

#p-value
plot(teststats, add=T, col = "red") <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")

#####text上的 运行这个
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")


#Ripley’s K - 比较for a whole range of different distance radii
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()


#用于发现空间(物理空间或可变空间)中的集群
##Density-based spatial clustering of applications with noise: DBSCAN
install.packages("fpc")
library(raster)
library(fpc)

#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)

##DBSCAN
#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)

#找到适合的eps
# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
install.packages("dbscan")
install.packages("factoextra")

library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

library(ggplot2)

db

db$cluster

#add this cluster membership info back into our dataframe
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)


#create some convex hull polygons to wrap around the points in our clusters
chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#As 0 isn’t actually a cluster (it’s all points that aren’t in a cluster) drop it from the dataframe
chulls <- chulls %>%
  filter(dbcluster >=1)


#现在ggplot2从我们的数据创建一个对象
dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()

###add a basemap
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()

#加底图
library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "stamen-toner")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")


#autoplot(basemap_bng) sometimes works
autoplot.OpenStreetMap(basemap_bng)+ 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  













#空间自相关度量的空间参考连续观测的模式。
#空间自相关是一种测量邻近数据间相似性的方法。

library(here)
library(janitor)
library(dplyr)
#read the ward data in
LondonWardsMerged <- st_read(here::here("prac6_data", 
                                        "statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)

##重点！！！！
WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                     na = c("NA", "n/a"),locale=locale(encoding="latin1")) %>%
  clean_names()

LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

#have a look to check that it's 
#in the right projection
st_crs(LondonWardsMerged)


tmap_mode("view")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

summary(BluePlaques)


BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")


#计算城市中每个ward内的所有蓝色斑块
library(sf)
points_sf_joined <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%
  add_count(ward_name)%>%
#accounting the ward name <n>
#for every point there is a new row of a ward
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

#我们想要一个dataset with the number of 蓝点 per ward
#first row of all the columns
points_sf_joined2<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined2) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

#定义Wij空间权重矩阵
library(spdep)
install.packages("spdep")

#选每个ward的质心，看每个polygon有多少个邻居
#First calculate the centroids of all Wards in London
coordsW <- points_sf_joined2%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)





#必须创建一个邻居列表——它是所有邻居的列表
#皇后
#create a neighbours list
install.packages("poly2nb")
LWard_nb <- points_sf_joined2 %>%
  poly2nb(., queen=T)

summary(LWard_nb)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined2$geometry, add=T)


#计算出来的nb对象，还仅仅是一个临近要素列表，
#还并非空间权重矩阵，所以还需要通过一个命令，把这个列表转换问空间权重矩阵
#必须制作一个空间权重矩阵
#####create a spatial weights matrix from these weights
##B可以忽略
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B")

sum(Lward.lw)


sum(Lward.lw[,1])


#M
#定义了我们的Wij矩阵
Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")


#Moran's I 测试告诉我们是否有聚集值（接近 1）或分散值（接近 -1），我们将计算密度而不是原始值
I_LWard_Global_Density <- points_sf_joined2 %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

#Geary的C也是..？这告诉我们相似的值还是不同的值是聚类
C_LWard_Global_Density <- 
  points_sf_joined2 %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

#盖蒂斯将军 G……？这告诉我们高值还是低值是聚类。如果 G > 预期 = 高值聚类；如果 G < 预期 = 低值聚类
G_LWard_Global_Density <- 
  points_sf_joined2 %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

#因此，全球统计数据表明我们对伦敦的蓝色斑块具有空间自相关性：
#Moran's I 统计量 = 0.67（记住 1 = 聚集，0 = 无模式，-1 = 分散）这表明我们有一些独特的聚类
#Geary 的 C 统计量 = 0.41（记住 Geary 的 C 介于 0 和 2 之间；1 表示没有空间自相关，<1 - 正空间自相关或相似值聚类，>1 - 负空间自相关或不同值聚类）这表明相似值是聚类
#一般 G 统计量 = G > 预期，因此高值趋于聚类。







#看本地的
#use the localmoran function to generate I for each ward in the city

I_LWard_Local_count <- points_sf_joined2 %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined2 %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

#We want to copy some of the columns (the I score (column 1) and the z-score standard deviation (column 4)) back into the LondonWards spatialPolygonsDataframe
points_sf_joined3 <- points_sf_joined2 %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))

tm_shape(points_sf_joined3) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")

#This map shows some areas in the centre of London that have relatively high scores, 
#indicating areas with lots of blue plaques neighbouring other areas with lots of blue plaques.


#热点和冷点的统计数
Gi_LWard_Local_Density <- points_sf_joined3 %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)


points_sf_joined4 <- points_sf_joined3 %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined4) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")





#use head to see what other variables are in the data file

slice_head(points_sf_joined3, n=2)

Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")

G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")