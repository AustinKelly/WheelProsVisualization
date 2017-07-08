
# Mining a dataset for Tipton. First 6 months of 2017
# Use the importer for the csv or it will come in crazy
# wheels <- read_csv("H:/Desktop/Data Requests/Wheel Team/Karl Tipton/email_SalesDiscountTireKT_6-30-2017_11.02.07_PM.csv")
class(wheels) # "tbl_df" "tbl" "data.frame"

# Check out the columns
summary(wheels)

# Get rid of the unneccessary cols
wheels$`YTD Through` <- NULL
wheels$`Site#` <- NULL
wheels$Cost <- NULL
wheels$`MTD$` <- NULL
wheels$`YTD$` <- NULL
wheels$SuggestedRetail_Map <- NULL
wheels$`MFG#` <- NULL
summary(wheels)

# Single out the diameter column. We want to look at that first
diams <- wheels$Diameter
summary(diams)

# Get rid of the extreme values in diameter column
wheels <- wheels[wheels$Diameter<30,]
diams <- wheels$Diameter
summary(diams)

# Drop the null values
wheels <- wheels[!is.na(wheels$Diameter),]
diams <- wheels$Diameter
summary(diams)
# Data is now good to go. Let's continue exploring

# Generate the histogram with labels and no axes
diamsh <- hist(diams, 16, xlab = "Diameters", ylab = NULL, xaxt = "n", yaxt = "n", 
               labels = TRUE, main = "Distribution of Diameters", col = "gray", border = "red")
# add the appropriate tick labels
axis(side = 1, at = diamsh$mids, labels = seq(11,26))

# Let's look at wheel widths 
width <- wheels$Width
summary(width)

# Plot the widths distribution with a histogram as well.
widthh <- hist(width, xlab = "Wheel Widths", ylab = NULL, xaxt = "n", yaxt = "n", labels = TRUE, 
               main = "Distribution of Wheel Widths", col = "gray", border = "red")
axis(side =1, at = widthh$mids, labels = seq(4,15))


# Let's take a look at the unique bolt patterns
unique(wheels$BoltPattern1)
pcd <- sort(unique(wheels$BoltPattern1))

# Get rid of uppercase x's
wheels$BoltPattern1 <- tolower(wheels$BoltPattern1)

pcd <- sort(unique(wheels$BoltPattern1))
pcd

# harmonize the duplicates and convert to metric
# 4-lug
wbp <- wheels$BoltPattern1

wbp[wbp == "4x100.00"] <- "4x100"
wbp[wbp == "4x108.00"] <- "4x108"
wbp[wbp == "4x110.00"] <- "4x110"
wbp[wbp == "4x115.00"] <- "4x115"
wbp[wbp == "4x137.00"] <- "4x137"
wbp[wbp == "4x156.00"] <- "4x156"

# 5-lug
wbp[wbp == "5x100.00"] <- "5x100"
wbp[wbp == "5x105.00"] <- "5x105"
wbp[wbp == "5x108.00"] <- "5x108"
wbp[wbp == "5x110.00"] <- "5x110"
wbp[wbp == "5x112.00"] <- "5x112"
wbp[wbp == "5x115.00"] <- "5x115"
wbp[wbp == "5x120.00"] <- "5x120"
wbp[wbp == "5x127.00"] <- "5x127"
wbp[wbp == "5x130.00"] <- "5x130"
wbp[wbp == "5x135.00"] <- "5x135"
wbp[wbp == "5x150.00"] <- "5x150"
wbp[wbp == "5x160.00"] <- "5x160"
wbp[wbp == "5x4.25"] <- "5x108"
wbp[wbp == "5x4.5"] <- "5x114.30"
wbp[wbp == "5x4.75"] <- "5x120.65"
wbp[wbp == "5x5.0"] <- "5x127"
wbp[wbp == "5x5.5"] <- "5x139.70"

# 6-lug
wbp[wbp == "6x120.00"] <- "6x120"
wbp[wbp == "6x127.00"] <- "6x127"
wbp[wbp == "6x130.00"] <- "6x130"
wbp[wbp == "6x132.00"] <- "6x132"
wbp[wbp == "6x135.00"] <- "6x135"
wbp[wbp == "6x4.5"] <- "6x114.30"
wbp[wbp == "6x5"] <- "6x127"
wbp[wbp == "6x5.0"] <- "6x127"
wbp[wbp == "6x5.5"] <- "6x139.70"

#8-lug
wbp[wbp == "8x170.00"] <- "8x170"
wbp[wbp == "8x180.00"] <- "8x180"
wbp[wbp == "8x200.00"] <- "8x200"
wbp[wbp == "8x210.00"] <- "8x210"

# Check the results
pcd <- sort(unique(wbp))
pcd


# Results look good. Let's see what the table looks like
table(wheels$Diameter)
table(wheels$Width)
table(wheels$BoltPattern1)

barplot(prop.table(table(wheels$BoltPattern1)))


# Let's explore offsets
summary(wheels$Offset)
wheels$Offset <- as.numeric(wheels$Offset)
offs <- hist(wheels$Offset, xlab = "Offset from Zero", ylab = NULL, xaxt = "n", xlim = c(-50,50), labels = TRUE, main = "Offsets Distribution", col = "green", border = "blue")
axis(side = 1, at = seq(-50,50,5))
boxplot(wheels$Offset, horizontal = TRUE, col = "orange", main = "Boxplot of Offsets")

# How many different dual-drilled wheels are we working with? 
notdual <- sum(is.na(wheels$BoltPattern2))
dual <- sum(!is.na(wheels$BoltPattern2))
blanks <- length(which(wheels$BoltPattern1=="blank"))

drilled <- c(notdual, dual, blanks)

barplot(drilled, col = c("black", "orange", "red"), ylim = c(0,22000))
text(.7, 20500, "Single Drilled")
text(1.9, max(dual)+500, "Dual Drilled")
text(3.1, max(blanks)+500, "Blanks")

# Okay. It is time to start looking into proportions of quantities sold. 
# Start with wheel diameters. Sum the Quantity column based on Diameters
quants <-wheels$Quantity
quantd <- wheels$Diameter
quantw <- wheels$Width

dia <- aggregate(quants~quantd, wheels, length)
dia # This is only returning frequencies. We want to sum the quantity by diameter...

# This is what we want for multiple values
sum(wheels[wheels$Diameter == 20,]$Quantity)

# Diameters
ds <- data.frame(aggregate(quants, by=list(Diameter = quantd), FUN = sum))
ds

plot(ds$Diameter, ds$x, type = "h", xlab = "Diameter", ylab = "Volume")


# Widths
ws <- data.frame(aggregate(quants, by=list(Width = quantw), FUN = sum))
ws
plot(ws$Width, ws$x, type = "h", xlab = "Widths", ylab = "Volume")

# The histograms of volume look very proportionate to the histograms of the overall count of records. 

#
# Rim Diameter/Width combined
#
wheels$wheelsize <- paste(wheels$Diameter,'x', wheels$Width)
wheelsize <- wheels$wheelsize
head(wheelsize)
wd <- data.frame(aggregate(quants, by=list(Size = wheelsize), FUN = sum))
wd
plot(wd$x, type = "h", xaxt = "n", xlab = "Sizes", ylab = "Volume")
axis(1, at=1:82, labels = wd$Size)

#Let's look at volumes above 2000
wdmax <- wd[wd$x > 2000,]
wdmax
par(las=3)
plot(wdmax$x, type = "h", xaxt = "n", xlab = "Sizes", ylab = "Volume", main = "Volume by Size")
axis(1, at=1:10, labels = wdmax$Size)


#
# - Rim Diameter/Bolt Pattern (combined)
#
wheels$diamBP <- paste(wheels$Diameter, wheels$BoltPattern1)
diamBP <- wheels$diamBP
head(diamBP)
dbp <- data.frame(aggregate(quants, by=list('Diam&PCD' = diamBP), FUN = sum))
dbp
summary(dbp$x)
# Min.     1st Qu.  Median   Mean    3rd Qu.    Max. 
# -12.0     6.0      45.0    343.8   219.5     4581.0 

dbmax <- dbp[dbp$x > 1000,] # Currently an arbitrary value
dbmax
par(las = 3, mar=c(6,2,2,1)) #mar = c(bottom, left, top, right)
plot(dbmax$x, type = "h", xaxt = "n", xlab = "", ylab = "Volume", main = "Most Popular Diameter/Bolt Pattern")
axis(1, at = 1:25, labels = dbmax$Diam.PCD)

#
# - Rim Diameter/Bolt Pattern/Offset (combined)
# Maybe we should be looking at this as a sub-group of Diameter/Bolt Pattern. 
#
wheels$diamBPOffs <- paste(diamBP, wheels$Offset)
diamBPOffs <- wheels$diamBPOffs
head(diamBPOffs)
dbo <- data.frame(aggregate(quants, by=list(Motley=diamBPOffs), FUN=sum))
dbo # Just under 900 different combos. Let's see if any stick out. We may need to start binning the offsets...
summary(dbo$x)
# Min.    1st Qu.  Median    Mean  3rd Qu.  Max. 
# -4.00    5.00   20.00     78.39   67.00   1835.00 
#Let's see what 100 and above look like
dbomax <- dbo[dbo$x > 800,]
par(mar = c(7, 2, 1, 1))
plot(dbomax$x, type = "h", xaxt = 'n', xlab = "", ylab = "Volume", main = "Diameter, PCD and ET")
axis(1, at = 1:12, labels = dbomax$Motley)


#
summary(wheels)

# Time to bin the offsets. Let's see what the distribution of the offsets look like.
hist(wheels$Offset)
offs <- hist(wheels$Offset, xlab = "Offset from Zero", ylab = NULL, xaxt = "n", xlim = c(-50,60), labels = TRUE, main = "Offsets Distribution", col = "green", border = "blue")
axis(side = 1, at = seq(-50,60,5))

# Bin offsets to properly display with Diameter, Bolt Pattern and Offset.
# Grouping by 10 seems to be adequate
# <-30, (-29,-20), (-19,-10),(-9,0), (1,10), (11,20), (21,30), (31,40), >40

# using seq(-30,40,10) which will give us 8 break points. Thus, we need 7 labels. 
summary(wheels$Offset)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -98.000 -12.000   0.000   7.595  25.000 111.000    1077 

# Anything less than -50 and anything greater than 60 is just ridiculous. Let's get  rid of these records
wheels <- wheels[wheels$Offset >= -50,]
wheels <- wheels[wheels$Offset <= 60,]
summary(wheels$Offset)

breaks <- seq(-50,60,10)
breaks
length(breaks) 

# There are 12 breaks, therefore we need 11 labels
offlabs <- c(
  "(-50,-40)", "(-39,-30)","(-29,-20)",
  "(-19,-10)", "(-9,0)",   "(1,10)",
  "(11,19)",   "(20,29)",  "(30,39)",
  "(40,49)", ">50"
)

wheels$offbin <- cut(wheels$Offset, 
                     breaks =  seq(-50,60,10), 
                     labels = offlabs)

# Everything has been binned. 
# Time to get rid of the rows with NA values
wheels <- wheels[!is.na(wheels$StoreCode),]
summary(diams)

write.csv(wheels, "CleanData.csv")

# Number of Records with DTC A#'s vs. Miscellaneous
arts <- sum(!is.na(wheels$`DTArticle#`))
arts

narts <- sum(is.na(wheels$`DTArticle#`)) # We don't have article numbers for these. 
narts

# Total number of records
total <- (arts + narts)
total #24632




