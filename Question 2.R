install.packages("RMySQL")
library(RMySQL)
mydb <- DBI::dbConnect(MySQL(), dbname='bi', user='root', password='P@ntera6', host='127.0.0.1')
dbListTables(mydb)

#create branches
branch_table <- 
  data.frame(key=c("UK", "USA", "AUS", "CN", "ZA"),
             city=c("London", "New York", "Sydney", "Shanghai", "Johannesburg"),
             country=c("UK", "USA", "Australia", "China", "South Africa"))

dbWriteTable(mydb, name='branch_table', value=branch_table)

#create produced products
product_table <- 
  data.frame(key=1:4,
             product=c("kettle","fryer", "toaster","iron"),
             price=c(20,90,60,80))

dbWriteTable(mydb, name='product_table', value=product_table)


month_table <-
  data.frame(key=1:12,
             month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             year=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
dbWriteTable(mydb, name='month_table', value=month_table)

#generate the Sales table
gen_sales <- function(no_of_recs) {
  branch <- sample(branch_table$key, no_of_recs, 
                replace=T, prob=c(2,2,1,2,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2010, 2011, 2012, 2013, 2014, 2015), no_of_recs, replace=T)
  product <- sample(product_table$key, no_of_recs, replace=T, prob=c(1, 4, 2, 3))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3))
  amount <- unit*product_table[product,]$price
  
  sales <- data.frame(month=time_month,
                      year=time_year,
                      branch=branch,
                      product=product,
                      unit=unit,
                      amount=amount)
  
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}
#end loop

#generate sales table
sales_table <- gen_sales(500)
sales_table
head(sales_table)

#multi-dimentional cube
revenue_cube <- 
  tapply(sales_table$amount, 
         sales_table[,c("product", "month", "year", "branch")], 
         FUN=function(x){return(sum(x))})

revenue_cube
dimnames(revenue_cube)

#OLAP OPERATIONS

#Roll-up
apply(revenue_cube, c("year", "product"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("branch", "product"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


#Slice
revenue_cube[,"1","2011",]
revenue_cube["3", ,"2012",]

#Dice
revenue_cube[c("1","2","3"), 
             c("1","12"), 
             c("2010", "2015"),
             ]

revenue_cube[c("1","2","3"), 
             c("1","12"), 
             c("2010"),
             c("ZA")
             ]

#drill-down
apply(revenue_cube, c("month", "year", "product"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


#pivot
apply(revenue_cube, c("product", "branch"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("year", "branch"), 
             FUN=function(x) {return(sum(x, na.rm=TRUE))})


  