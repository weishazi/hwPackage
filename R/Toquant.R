#' Title
#'
#' @param df
#' @param quant
#'
#' @return
#' @export
#'
#' @examples
#'
#'
Toquant <- function(df = df, quant = quant) {

  library(dplyr)

  if (quant == 2) {

    for (i in 1:ncol(df)) {

      quant_con = quantile(df[, i], probs = 0.5, na.rm = T)

      df = df %>% mutate(new_vars = ifelse(df[, i] <= quant_con[1], "Q1", "Q2"))

      colnames(df)[colnames(df) == "new_vars"] = paste0(colnames(df)[i], "_2Q")

    }

    return(df)

  }

  if (quant == 3) {

    for (i in 1:ncol(df)) {

      quant_con = quantile(df[, i], probs = c(0.333, 0.666), na.rm = T)

      df = df %>% mutate(new_vars = ifelse(df[, i] <= quant_con[1], "Q1",
                                           ifelse(df[, i] <= quant_con[2], "Q2", "Q3")))

      colnames(df)[colnames(df) == "new_vars"] = paste0(colnames(df)[i], "_3Q")

    }

    return(df)

  }

  if (quant == 4) {

    for (i in 1:ncol(df)) {

      quant_con = quantile(df[, i], probs = c(0.25, 0.5, 0.75), na.rm = T)

      df = df %>% mutate(new_vars = ifelse(df[, i] <= quant_con[1], "Q1",
                                           ifelse(df[, i] <= quant_con[2], "Q2",
                                                  ifelse(df[, i] <= quant_con[3], "Q3", "Q4"))))

      colnames(df)[colnames(df) == "new_vars"] = paste0(colnames(df)[i], "_4Q")

    }

    return(df)

  }

}
