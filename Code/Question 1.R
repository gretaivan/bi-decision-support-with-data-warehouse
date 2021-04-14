#install.packages("RMySQL")
#library(RMySQL)
#install.packages("DBI")
#library(DBI)
#install.packages("odbc")
#library(odbc)
#con <- DBI::dbConnect(odbc::odbc(),
                   #   driver = "MySQL Driver",
                 #     database = "w1670486_0",
#UID    = rstudioapi::askForPassword("w1670486"),
                 #     PWD    = rstudioapi::askForPassword("AncMuaf1A94e"),
                 #     host = "localhost",
                  #    port = 2222/w1670486_0)

#mydb <- DBI::dbConnect(MySQL(), dbname='w1670486_0', user='w1670486', password='AncMuaf1A94e', host='localhost:2222/w1670486_0')
#dbListTables(mydb)
#jdbc:mysql://localhost:2222/w1670486_0

#con <- dbConnect(RMySQL::MySQL(),
 #                dbname ="w1670486_0",
 #                host = "localhost:2222/w1670486_0",
  #               port = 3306,
 #                user = "w1670486",
 #                password = "AncMuaf1A94e")

install.packages("RMySQL")
library(RMySQL)

mydb <- DBI::dbConnect(MySQL(), dbname='bi', user='root', password='P@ntera6', host='127.0.0.1')
dbListTables(mydb)



head(data, 10)

#Get four cylinder cars with miles per gallon greater than 30
cars = dbSendQuery(mydb, "select * from mtcars where cyl = 4 and mpg > 30")
carData = fetch(cars, n=-1)
carData

#get four cylinder cars
cars = dbSendQuery(mydb, "select * from mtcars where cyl = 4")
carData = fetch(cars, n=-1)
carData

#get names and horsepower where cyl = 8
cars = dbSendQuery(mydb, "select Brand, hp from mtcars where cyl = 8")
carData = fetch(cars, n=-1)
carData

#get names and hp of cars that starts with M and have cyl = 6 and 8
cars = dbSendQuery(mydb, "select Brand, hp from mtcars where (cyl = 8 or cyl = 6) AND (Brand LIKE 'M%')")
carData = fetch(cars, n=-1)
carData

#average hp and mpg by cyl groups
cars = dbSendQuery(mydb, "SELECT AVG(hp), AVG(mpg) FROM mtcars GROUP BY cyl")
carData = fetch(cars, n=-1)
carData


install.packages("dplyr")
library(dplyr)

#set the data table
table = dbSendQuery(mydb, "select * from mtcars")
data = fetch(table, n=-1)
test <- data.matrix(data)
?magrittr::`%>%`

glimpse(data)

#Get four cylinder cars with miles per gallon greater than 30
data %>% filter(cyl==4) %>% filter(mpg>30)

#get names and horsepower where cyl = 8
data %>% filter(cyl == 8) %>% select(Brand, hp)

#get names and hp of cars that starts with M and have cyl = 6 and 8
data %>% select(cyl, hp, mpg)%>% group_by(cyl) %>% summarise(average_hp = mean(hp, na.rm = TRUE), average_mpg = mean(mpg, na.rm = TRUE)) 

y <- data[,1]
x <- data[,2:12]



pie(table(data$carb))

#get the number of instances for each type
gears <- data.matrix(table(data$gear))
#change it to the vector
x <- gears[,0:1]

barplot(x, main = 'Number of gear types in mtcars',xlab = 'gear type', ylab = 'number of each type', horiz = FALSE)

#stacker bar graph
install.packages("ggplot2")
library(ggplot2)

#cylinders <- data %>% select(gear, cyl) %>% group_by(gear, cyl)
#cylinders_and_gears <- data.matrix(table(cylinders))
#cylinders_and_gears <- as.data.frame(cylinders)
#cylinders_and_gears$gear
#cylinders_and_gears$cyl
#cylinders_and_gears
#cylinders <- cylinders [,0:1]
#cylinders_and_gears
#cylinders_and_gears <- as.data.frame(cylinders_and_gears)
#cylinders_and_gears
#cylinders_and_gears <- cylinders_and_gears[, -1]
#cylinders_and_gears
#class(cylinders_and_gears)
#ggplot(cylinders_and_gears, aes(fill=cylinders_and_gears$gear, y=cylinders_and_gears$gear, x=cylinders_and_gears$cyl)) + 
 # geom_bar(position="stack", stat="identity")

?ggplot

cylinders <- data %>% select(gear, cyl) %>% group_by(gear, cyl)
(cylPerGear<- summarise(cylinders, value=n()))
cylinders
cylPerGear

#stackedBarsPlot <- ggplot(cylPerGear, aes(fill=cylPerGear$value, x=cylPerGear$gear, y=cylPerGear$cyl)+ 
#  geom_bar(position="stack", stat="identity") +
#  ggtitle("Number of of cylinders per each gear type"))


#stackedBarsPlot<-ggplot(cylPerGear,aes(fill=cylPerGear$value, x=cylPerGear$gear, y=cylPerGear$cyl)+ 
#                          geom_bar(position="stack", stat="identity") +
#                          geom_text(data=data, aes(x = "Gear type", y = "Number of Cylinders", label = paste0(cylPerGear$value)), size=4)+
#                          ggtitle("Number of of cylinders per each gear type"))
#stackedBarsPlot


#stackedBarsPlot<-ggplot(cylPerGear,aes(fill=cylPerGear$value, x=cylPerGear$gear, y=cylPerGear$cyl))+ 
#    geom_bar(position="stack", stat="identity") +
#    ggtitle("Number of of cylinders per each gear type")
# stackedBarsPlot
 

 
# stackedBarsPlot<-ggplot(cylPerGear,aes(fill=cyl, x=gear, y=value))+ 
#   geom_bar(position="stack", stat="identity") +
#   geom_text(data=cylPerGear, aes(x = gear, y = cyl, label = paste0(value)), size=5) +
#   ggtitle("Number of cylinders per each gear type") +
#   labs(x="Gear type", y="Total no of cylinders for each gear type") 

 #cylPerGear <- ddply(cylPerGear, .(cyl),


cyl_factor <- as.factor(cylPerGear$cyl)
cyl_factor
gear_factor <- as.factor(cylPerGear$gear)
gear_factor
dividedBycyl<- as.factor(cylPerGear$value)
cylPerGear
#data_factorised <- data.frame(gear_factor,cyl_factor, dividedBycyl)
data_factorised <- data.frame(gear_factor,cyl_factor, cylPerGear$value)
data_factorised

#create the bar plot
stackedBarsPlot<-ggplot(data_factorised,aes(fill=cyl_factor, x= gear_factor, y=cylPerGear$value))+ 
  geom_bar(data=data_factorised,  stat="identity") +
  #geom_text(data=data_factorised, aes(x =gear_factor , y =data_factorised, label = paste0(cylPerGear$value, "car(s)")), size=5) +
  ggtitle("Number of cylinders per each gear type") +
  labs(x="Gear type", y="Total no of cylinders for each gear type") +
  labs(fill = "Number of cylinders")
stackedBarsPlot

#breakdown the count of cylinders to smaller sequences
stackedBarsPlot<-stackedBarsPlot + scale_y_continuous(breaks=seq(0,15,1))
stackedBarsPlot


#PLOTING THE SCATTER PLOT
data
plot(data$wt, data$mpg)

#boxplot for distribution of mpg values per no of gears
#boxplot(data$mpg, data$gear,)
?boxplot

boxplot(data$mpg~data$gear,
        data=data,
        main="MPG values per number of gears",
        xlab="Number of gears",
        ylab="MPG",
        col="orange",
        border="brown"
)
