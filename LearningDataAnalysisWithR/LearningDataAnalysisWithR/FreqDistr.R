data(airquality)
#help(airquality)
#str(airquality)
#head(airquality)
#str(airquality)

print("Temperature table after binning data")
table(cut(airquality$Temp, 9))

histogram(cut(airquality$Temp, 9), xlab = "Temperatures ranges", ylab = "Percentage")
histogram(cut(airquality$Temp, 9), xlab = "Temperatures ranges", ylab = "Count", type="count")