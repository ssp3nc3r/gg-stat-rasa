# gg-stat-rasa
Generic statistic for transforming data with arbitrary functions in ggplot.

# Example uses

```{r}
Detrend <- function(data, method = "lm", span = 0.2) {
   if (method == "lm") {
      data$y <- resid(lm(y ~ x, data = data))
   } else {
      data$y <- resid(loess(y ~ x, span = span, data = data))
   }
   as.data.frame(data)
}
```

```{r}
library(ggplot2)
set.seed(42)
x <- seq(-1, 3, length.out = 30)
y <- x^2 + rnorm(30)*0.5
df <- data.frame(x = x, y = y)
ggplot(df, aes(x, y)) +
   geom_line() +
   stat_rasa(geom = "line", fun = Detrend, method = "smooth",
             color = "steelblue")
```

```{r}
stat_detrend <- function(...) {
   stat_rasa(fun = Detrend, ...)
}

ggplot(df, aes(x, y)) +
   geom_line() +
   stat_detrend(method = "lm", color = "blue", geom = "line")
```

```{r}
IrregularContour <- function(data, breaks = scales::fullseq, 
                             binwidth = NULL,
                             bins = 10) {
   if (is.function(breaks)) {
      # If no parameters set, use pretty bins to calculate binwidth
      if (is.null(binwidth)) {
         binwidth <- diff(range(data$z)) / bins
      }
      
      breaks <- breaks(range(data$z), binwidth)
   }
   
   cl <- contoureR::getContourLines(x = data$x, y = data$y, z = data$z, 
                                    levels = breaks)
   
   if (length(cl) == 0) {
      warning("Not possible to generate contour data", call. = FALSE)
      return(data.frame())
   }
   cl <- cl[, 3:7]
   colnames(cl) <- c("piece", "group", "x", "y", "level")
   return(cl)
}

stat_contour_irregular <- function(...) {
   stat_rasa(fun = IrregularContour, geom = "path", ...)
}
```

```{r}
set.seed(42)
df <- data.frame(x = rnorm(500),
                 y = rnorm(500))
df$z <- with(df, -x*y*exp(-x^2 - y^2))

ggplot(df, aes(x, y)) +
   geom_point(aes(color = z)) +
   stat_contour_irregular(aes(z = z, color = ..level..), bins = 15) +
   scale_color_viridis_c()
```
