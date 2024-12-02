rm(list=ls())
dev.off()

dt <- iris[, -5]
dt_group <- iris[, 5]
write.table(iris, "clipboard", row.names = FALSE,   sep = ",")

#PCA 하기

pca_dt <- prcomp(dt,center = T,scale. = T) #데이터 표준화 포함

#PCA 결과 확인
pca_dt


#PCA 결과 시각화
# Proportion of variance 출력 

plot(pca_dt,type = "l")
screeplot(pca_dt, main = "", col = "green", type = "lines", pch = 1, npcs = length(pca_dt$sdev))

summary(pca_dt)

#PC1과 PC2만으로도 95.8%의 변동을 설명할 수 있다. 

require(graphics)
dev.off()
p <- biplot(pca_dt,
       col=c('blue', 'red'),
       cex=c(0.4, 1.0),
       xlim=c(-.2, .2),
       main='PCA Results',
       xlab='First Component',
       ylab='Second Component',
       expand=1.2)


yv <- predict(pca_dt)[, 1]
yv2 <- predict(pca_dt)[, 2]
par(mfrow = c(1,2))
dev.off()
plot(dt$Sepal.Width, yv, pch = 16, xlab = "Sepal_Width", ylab = "PC 1", col = "red")
plot(dt$Petal.Length, yv2, pch = 16, xlab = "Petal_Length", ylab = "PC 2", col = "blue")

#heptathlon 으로 1988년 서울 올림픽 육상 여성 7종 경기에 대한 결과이다. 

# hurdles(110m 허들)
# highjump(높이뛰기)
# shot(포환던지기)
# run200m(200m 달리기)
# longjump(멀리뛰기)
# javelin(창던지기),
# run800m(800m 달리기) 
# 전체 score

#install.packages("HSAUR")
library(HSAUR)

data(heptathlon)
head(heptathlon)
summary(heptathlon)   


hep.data.tmp <- heptathlon
hep.data.tmp$hurdles <- max(hep.data.tmp$hurdles)-hep.data.tmp$hurdles
hep.data.tmp$run200m <- max(hep.data.tmp$run200m)-hep.data.tmp$run200m
hep.data.tmp$run800m <- max(hep.data.tmp$run800m)-hep.data.tmp$run800m


hep.data <- hep.data.tmp[,-8]
hep.data.tmp[,-8]


m <- var(hep.data) 
M <- cor(hep.data)
m
M
library(corrplot)
corrplot(M,method="circle")
corrplot(M,method="square")
corrplot(M,method="ellipse")
corrplot(M,method="number")
corrplot(M,method="shade")
corrplot(M,method="color")
corrplot(M,method="pie")
corrplot(M,type="upper")
corrplot(M,type="lower")
corrplot.mixed(M)
corrplot.mixed(M,lower="ellipse",upper="circle")
corrplot.mixed(M,lower="square",upper="circle")

###
# AOE” ; Angle of eigenvectors(고유벡터의 각순서)로 정렬
# “FPC” ; first principal component order(첫번째 주요성분 순서)로 정렬
# “hclust” ; hierarchical clustering order(계층적 군집순서)로 정렬, 이때 응집 방법은 “hclust.method”로 정하는데 “hclust.method”는 ward, single, complete, average, mcquitty, median, 그리고 centroid 중의 하나이다.
# alphabet ; 알파벳 순서로 정렬

corrplot(M,order="AOE")
corrplot(M,order="hclust",addrect=3)

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
                           "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", 
                           "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue"))
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F", 
                           "cyan", "#007FFF", "blue", "#00007F"))
wb <- c("white", "black")
## using these color spectrums
corrplot(M, order = "hclust", addrect = 2, col = col1(100))
