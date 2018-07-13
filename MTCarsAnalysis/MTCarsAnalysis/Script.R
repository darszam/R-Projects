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
## grouped dot plot miles per gallon for various cars, grouped by cylinders
mtcars <- within(mtcars, { color <- ifelse(cyl == 4, "coral", ifelse(cyl == 6, "cadetblue", "darkolivegreen")) })
dotchart(mtcars$mpg, labels = row.names(mtcars), groups = mtcars$cyl,
         color = mtcars$color, cex = 0.7, pch = 16,
         main = "Miles per Gallon of cars\nby Cylinders",
         xlab = "Miles per Gallon")
#removing color column within data frame after plot
mtcars <- within(mtcars, rm("color"))

## using ggplot2
library(ggplot2)
theme <- theme_set(theme_minimal())
ggplot(mtcars, mapping = aes_string(y = "mpg", x = "cyl")) + xlab("Number of Cylinders") + ylab("Miles per Gallon") +
    ggtitle("Distribution of Miles per Gallon \nby number of cylinders") +
    geom_boxplot(outlier.colour = NULL,
        aes_string(colour = "cyl", fill = "cyl"),
        alpha = 0.8) +
        stat_summary(geom = "crossbar",
            width = 0.7,
            fatten = 0.5,
            color = "white",
            fun.data = function(x) {
                    return(c(y = median(x), ymin = median(x), ymax = median(x)))
                    }
) +
    stat_summary(fun.data = function(x) {
        return(c(y=median(x)*1.03,label=round(median(x),2)))
    },
    geom = "text",
    fun.y = mean,
    colour="white")
## print avg/median mpg by cylinders
aggregate(list(mpg = mtcars$mpg),
    list(cylinders = mtcars$cyl),
    FUN = function(mpg) {
        c(avg = mean(mpg),
          median=median(mpg))
    }
    )
## Car mpgs by transmission type visualization
ggplot(mtcars,
    mapping = aes_string(y = "mpg", x = "am")) +
       xlab("Transmission Type") +
       ylab("Miles per Gallon") +
       ggtitle("Distribution of Miles per Gallon \nby transmission type") +
       geom_boxplot(outlier.colour = NULL,
                    aes_string(colours = "am",
                    fill = "am"), alpha = 0.8) +
                               stat_summary(geom = "crossbar",
                                            width = 0.7,
                                            fatten = 0.5,
                                            color = "white",
                                            fun.data = function(x) {
                                                return(c(y = median(x),
                                                       ymin = median(x),
                                                       ymax=median(x)))
                                            }) +
                                            stat_summary(fun.data = function(x) {
                                                return(c(y = median(x) * 1.03,
                                                label = round(median(x),2)))
                                            },
                                            geom = "text",
                                            fun.y = mean,
                                            colour = "white")
## print avg/median
aggregate(list(mpg = mtcars$mpg),
    list(transmission = mtcars$am),
    FUN = function(mpg) {
        c(avg = mean(mpg),
          median = median(mpg))
    }
    )

## Statistical inference
## Hypothesis 0 -> difference in mpg means for automatic and manual transmission cars is zero
## Using t-test
# Viewing data distribution
ggplot(mtcars, aes(x = mpg)) +
    geom_density(colour = "steelblue",
    fill = "lightblue", alpha = 0.8) +
                 expand_limits(x = 0, y = 0)
# Now using t-test
t.test(mpg ~ am, data = mtcars)
## Visualizing t-test results
aggr <- aggregate(list(mpg = mtcars$mpg),
                  list(transmission = mtcars$am),
                  FUN = function(mpg) { c(avg = mean(mpg)) })
ggplot(mtcars, aes(x = mpg)) +
    geom_density(aes(group = am, colour = am, fill = am),
    alpha = 0.6) +
                 geom_vline(data = aggr, aes(xintercept = mpg, color = transmission),
                            linetype = "dashed", size = 1)
## Statistical modeling with regression
