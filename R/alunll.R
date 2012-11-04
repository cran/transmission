#'  Compute Survival portion of transmission model log likelihood.
#'
#'  @param alun.data is Alun formated data. See Details
#'  @param transmission The transmission parameter
#'         
#'  @section ComputeSurvivalLLViaAlunMethod Details:   
#'  alun.data is data formatted in Alun's format of an event list.
#'  It is a data.frame with three columns of time, patien id, and event type.
#'  
#'  @export
#'  @import plyr
#'  @rdname alunll
#'  @return single numeric for survival portion of log likelihhod.
#'  @seealso ConvertToAlun, ConvertFromAlun
ComputeSurvivalLLViaAlunMethod <- function(alun.data, transmission){
  ts.event.data <- arrange(alun.data, time, event, pid)
  ts.event.data <- mutate(ts.event.data,
    cpresent.change  = .ConvertCPresent(event),
    cinfected.change = .ConvertCInfected(event),
    cpresent         = cumsum(cpresent.change),
    cinfected        = cumsum(cinfected.change),
    diff.count       = cpresent - cinfected,
    time.to.next     = c(diff(time), 0))
  with(ts.event.data,
    -transmission * sum(cinfected * diff.count * time.to.next))
}

#' .ConvertCPresent converts to change in present count
#' @param x event either integer, character or factor
#'
#' @section .ConvertCPresent Details:   
#' Converts event type to change in number present.
#' @rdname alunll
.ConvertCPresent <- function(x){
  switch(class(x)[[1]],
    numeric    = c(+1,0,0,-1,0)[x+1],
    integer    = c(+1,0,0,-1,0)[x+1],
    factor     = aaply(levels(x)[x], 1, switch, admit = +1, discharge = -1, 0),
    character  = aaply(x           , 1, switch, admit = +1, discharge = -1, 0))
}

#' convert to change in infected count
#' @inheritParams .ConvertCPresent
#' @section .ConvertCInfected Details:
#' Converts event type to change in number infected
#' @rdname alunll
.ConvertCInfected <- function(x){
  switch(class(x)[[1]],
    numeric    = c(0,0,0,-1,+1)[x+1],
    integer    = c(0,0,0,-1,+1)[x+1],
    factor     = aaply(levels(x)[x], 1, switch, infection = +1, discharge = -1, 0),
    character  = aaply(x           , 1, switch, infection = +1, discharge = -1, 0))
}

