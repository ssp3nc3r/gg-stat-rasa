# ggproto object
StatRasa <- ggplot2::ggproto("StatRasa", ggplot2::Stat,
  compute_group = function(data, scales, fun, fun.args) {
     # Change default arguments of the function to the 
     # values in fun.args
     args <- formals(fun)
     for (i in seq_along(fun.args)) {
        if (names(fun.args[i]) %in% names(fun.args)) {
           args[[names(fun.args[i])]] <- fun.args[[i]]
        } 
     }
     formals(fun) <- args
     
     # Apply function to data
     fun(data)
})

# stat function used in ggplot
stat_rasa <- function(mapping = NULL, data = NULL,
                      geom = "point", 
                      position = "identity",
                      fun = NULL,
                      ...,
                      show.legend = NA,
                      inherit.aes = TRUE) {
   # Check arguments 
   if (!is.function(fun)) stop("fun must be a function")
   
   # Pass dotted arguments to a list
   fun.args <- match.call(expand.dots = FALSE)$`...`
   
   ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatRasa,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      check.aes = FALSE,
      check.param = FALSE,
      params = list(
         fun = fun, 
         fun.args = fun.args,
         na.rm = FALSE,
         ...
      )
   )
}
