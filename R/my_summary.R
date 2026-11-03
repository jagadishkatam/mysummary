#' to generate the summary stats for the variables
#'
#' @param dataset here you can pass adam dataset name
#' @param ... here you can pass subset condition
#' @param gcols pass group variable
#' @param acols pass analyis variables
#' @param param pass a variable name which you want to derive with that name
#'
#' @return list
#' @import dplyr
#' @import rlang
#' @importFrom rlang as_string
#'
#' @examples
#' df <- data.frame(USUBJID='01-701-1015', SAFFL= 'Y',   SEX='F', ARM='Placebo',AGE=63, DMDY=-7)
#' my_summmary(df,SAFFL=='Y', SEX=='F',gcols='ARM', acols=c('AGE','DMDY'), param=name)
#'
#' @export

my_summmary <- function(dataset,..., gcols, acols, param){

  objectvec <- ls(envir = .GlobalEnv)
  objectvec2 <- is_empty(objectvec[objectvec==as_string(substitute(dataset))])
  # browser()

  # subset <- rlang::enexprs(subset)
  gcols <- rlang::syms(gcols)

  param <- rlang::enquo(param)

  if (objectvec2) {
    stop('no data exists')
  } else {
    # browser()
   data <- map(acols, function(x) {
      dataset %>% filter(...) %>% group_by(!!!gcols) %>% summarise(n=n(),
                                                                mean=mean(.data[[x]]),
                                                                !!param := x)
    }
    )

  }
  return(data)
}

