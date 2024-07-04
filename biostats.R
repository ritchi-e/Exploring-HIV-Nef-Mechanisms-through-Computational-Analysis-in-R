2+4
6-2
6-3
6-4
2/4
0/0
cos(90)
cos(pi/2)
2=4
2+4
cos(1.57)
cos(22/7)
cos(22/14)
?cos
cospi(0.5)
pi
pi
Pi
PI
round(pi,2)

LETTERS[2]
LETTERS
LETTERS[200]
1:20->a
c(1:20)->b
month.name
#convert radians to degree
radi<- function(x){ x*180/pi}
radi(pi)
radi(pi/2)
#convert degree to radians
degree<- function(x){x*pi/180}
degree(360)
cos(6.283185)


clear
getwd()
setwd("/Users/ritchie/Downloads")
df2 <- read.csv("life_expectancy_years.csv", header = TRUE) #year name as header in alphanumeric
head(df2)
df2$country

#United Kingdom, Sweden, Australia, China, India, Bangladesh, Sri Lanka
UK <- df2[df2$country == "United Kingdom", ]
SW <- df2[df2$country == "Sweden", ]
AU <- df2[df2$country == "Australia", ]
CH <- df2[df2$country == "China", ]
IN <- df2[df2$country == "India", ]
BN <- df2[df2$country == "Bangladesh", ]
SL <- df2[df2$country == "Sri Lanka", ]

UK_L = na.omit(as.numeric(UK))
SW_L = na.omit(as.numeric(SW))
AU_L = na.omit(as.numeric(AU))
CH_L = na.omit(as.numeric(CH))
IN_L = na.omit(as.numeric(IN))
BN_L = na.omit(as.numeric(BN))
SL_L = na.omit(as.numeric(SL))

par(mfrow = c(1,1))
plot(c(1800:2100),UK_L,col="red",type="l",main="Life Expectancy",ylim=c(0,100),xlab="Years",ylab="Age")
lines(c(1800:2100),SW_L,col="blue")
lines(c(1800:2100),AU_L,col="brown")
lines(c(1800:2100),CH_L,col="green")
lines(c(1800:2100),IN_L,col="black")
lines(c(1800:2100),BN_L,col="magenta")
lines(c(1800:2100),SL_L,col="violet")
legend(x = "bottomright", legend = c("UK","Sweden","Australia","China","India","Bangladesh","Sri Lanka"), col = c("red","blue","brown","green","black","magenta","violet"), lwd=1, lty=c(1,1,1), cex = 0.75)


par(mfrow = c(1,1))

plot(
  x = rep(1800:2100, 7),
  y = c(UK_L, SW_L, AU_L, CH_L, IN_L, BN_L, SL_L),
  col = c(rep("red", length(UK_L)),
          rep("blue", length(SW_L)),
          rep("brown", length(AU_L)),
          rep("green", length(CH_L)),
          rep("black", length(IN_L)),
          rep("yellow", length(BN_L)),
          rep("violet", length(SL_L))),
  pch = 16,
  main = "Life Expectancy",
  xlab = "Years",
  ylab = "Age"
)

legend(
  x = "topright",
  legend = c("UK", "Sweden", "Australia", "China", "India", "Bangladesh", "Sri Lanka"),
  col = c("red", "blue", "brown", "green", "black", "yellow", "violet"),
  pch = 16,
  cex = 0.75
)


setwd("/Users/ritchie/Desktop/BIOSTATS")
infect_data <- read.delim(file = "infect.txt", header = FALSE)

infect_data <- infect_data[order(infect_data$V2),]
infect_ratio <- infect_data$V2
names(infect_ratio) <- infect_data$V3

color_given <- function(value){
  if (value < 5) return("orange")
  else if ( value < 25) return("violet")
  else return("turquoise")
}
par(mar = c(6, 8, 4, 4)) 

color_vector <- unlist(lapply(infect_data$V2, color_given))
infect_data$V4 <- color_vector

barplot(infect_ratio, cex.names=0.6, horiz = TRUE,las=1, xlab = "Nef+/Nef- infectivity ratio", col=color_vector,
        main = "Ratio of the infectivity vs different cell types", border=0)


# Set the working directory to the folder containing the HTSeq files
setwd("/Users/ritchie/Desktop")

