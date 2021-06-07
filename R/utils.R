side_by_side <- function(...){
  mc <- list(...)
  lapply(mc, function(x){
    tags$div(style=paste("display: inline-block;",
                         "vertical-align: top;"),
             x)
  })
}
