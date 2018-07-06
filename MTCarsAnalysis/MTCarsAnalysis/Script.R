data(mtcars)
str(mtcars)
head(mtcars, 6)

## type conversion function
to.factors = function(df, variables) {
    for (variable in variables) {
        df[[variable]] <- as.factor(df[[variable]])
    }
    return (df)
}
## performing data type conversion
categorical.vars = c("cyl", "vs", "am", "gear", "carb")
mtcars = to.factors(mtcars, categorical.vars)
## verify conversion
str(mtcars)
## pairs visualization
png(file = "cars_mpg_cyl.png")
pairs(mtcars, panel = panel.smooth,
main = "Pairs plot for mtcars dataset")
plot(x = mtcars[, c("mpg", "cyl")]$cyl, y = mtcars[, c("mpg", "cyl")]$mpg,
     xlab = "Cylinders", ylab = "Miles per gallon", main = "MPGs and Cylinders")
dev.off()
##dotchart
dotchart(mtcars$mpg, labels = row.names(mtcars),
         cex = 0.7, pch = 16, main = "Miles per Gallon of Cars", xlab = "Miles per Gallon")
## least mpg
head(mtcars[order(mtcars$mpg),], 2)
## max mpg
tail(mtcars[order(mtcars$mpg),], 2)
## cylinder counts
barplot(table(mtcars$cyl), col = "lightblue",
        main = "Car Cylinder Counts Distribution",
        xlab = "Number of Cylinders", ylab = "Total Cars",
        cex.main = 0.8, cex.axis = 0.6,
        cex.names=0.6, cex.lab=0.8)
## gear counts
barplot(table(mtcars$gear), col = "lightblue",
        main = "Car Gears Counts Distribution",
        xlab = "Number of Gears", ylab = "Total Cars",
        cex.main = 0.8, cex.axis = 0.6,
        cex.names = 0.6, cex.lab = 0.8)
## transmission counts
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
barplot(table(mtcars$am), col = "lightblue",
        main = "Car Transmission Type",
        xlab = "Type of Transmission", ylab = "Total Cars",
        cex.main = 0.8, cex.axis = 0.6,
        cex.names = 0.6, cex.lab = 0.8)
## distribution by cylinders and transmission
counts <- table(mtcars$am, mtcars$cyl)
barplot(counts, main = "Car Distribution by Cylinders and Transmission",
        xlab = "Number of Cylinders", ylab = "Total Cars",
        col = c("steelblue", "lightblue"),
        legend = rownames(counts), beside = TRUE,
        args.legend = list(x = "top", title = "Transmission Type",
                           cex = 0.8),
        cex.main = 0.8, cex.axis = 0.6,
        cex.names = 0.6, cex.lab = 0.8)
## distribution by cylindes and gears
counts <- table(mtcars$gear, mtcars$cyl)
barplot(counts, main = "Car Distribution by Cylinders and Gears",
        xlab = "Number of Cylinders", ylab = "Total Cars",
        col = c("darkblue","steelblue", "lightblue"),
        legend = rownames(counts), beside = TRUE,
        args.legend = list(x = "top", title = "Gears",
                           cex = 0.8),
        cex.main = 0.8, cex.axis = 0.6,
        cex.names = 0.6, cex.lab = 0.8)