eDII_calculate <- function(df = df) {

  library(dplyr)

  # load(file = "DII_eDII_ref_databases.RData")

  DII_names = eDII$Food[eDII$Food %in% c(colnames(df))]

  # calculate Energy Normalization food content

  df_efood = df[, DII_names]

  df_efood2 = apply(df_efood, 2, function(x) {
    temp = x / df$Energy * 1000
    return(temp)
  })

  df_efood2 = as.data.frame(df_efood2)

  # calculate eDII

  list <- list()

  for (i in 1:length(DII_names)) {
    list[[i]] <- (pnorm((df_efood2[, DII_names[i]] - eDII[eDII$Food == DII_names[i], "Global_daily_mean_intake"]) / eDII[eDII$Food == DII_names[i], "SD"]) * 2 - 1) * eDII[eDII$Food == DII_names[i], "Overall_inflammatory_effect_score"]
  }

  names(list) <- DII_names

  df_edii <- do.call(cbind.data.frame, list)

  # df_edii$edii <- apply(df_edii, 1, sum)
  df_edii$edii <- rowSums(df_edii, na.rm = TRUE)

  # df$DII <- df_dii$dii

  # summary(df$DII)
  return(df_edii)
}
