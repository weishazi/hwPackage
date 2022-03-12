DII_calculate <- function(df = df) {

  library(dplyr)

  # load(file = "DII_eDII_ref_databases.RData")

  DII_names = DII$Food[DII$Food %in% c(colnames(df))]

  list <- list()

  for (i in 1:length(DII_names)) {
    list[[i]] <- (pnorm((df[, DII_names[i]] - DII[DII$Food == DII_names[i], "Global_daily_mean_intake"]) / DII[DII$Food == DII_names[i], "SD"]) * 2 - 1) * DII[DII$Food == DII_names[i], "Overall_inflammatory_effect_score"]
  }

  names(list) <- DII_names

  df_dii <- do.call(cbind.data.frame, list)

  # df_dii$dii <- apply(df_dii, 1, sum)
  df_dii$dii <- rowSums(df_dii, na.rm = TRUE)

  # df$DII <- df_dii$dii
  #
  # summary(df$DII)

  return(df_dii)

}

