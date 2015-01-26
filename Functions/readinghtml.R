require("XML")

doc <- readHTMLTable("http://daac.ornl.gov/cgi-bin/MODIS/GR_col5_1/corners.1.pl?site=fn_estoledo&res=250m")
table <- doc[1]
a <- as.data.frame(table)

b <- as.character(a[,2][5:8])
listt <- c()
for(i in 1:length(b)){
  splitt = strsplit(b[i]," , ")
  for(j in 1:2){
    print(splitt[[1]][j])
    listt = c(listt, splitt[[1]][j])
  }
}
print(listt)
c <- split(b," ")
splitt = strsplit(b[1], " , ")
str(splitt)
