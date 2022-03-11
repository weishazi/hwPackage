Tocat <- function(vector = vector,
                  cutpoint = cutpoint,
                  label = label,
                  equal = TRUE) {

  cutpoint <- sort(cutpoint)

  n <- length(cutpoint)

  k <- n + 1

  labels <- paste0(label, 1:k)

  if (equal == TRUE) {

    if (n == 1) {

      cat <- ifelse(vector <= cutpoint[1], labels[1], labels[2])

    }

    if (n == 2) {

      cat <- ifelse(vector <= cutpoint[1], labels[1],
                    ifelse(vector <= cutpoint[2], labels[2], labels[3]))

    }

    if (n == 3) {

      cat <- ifelse(vector <= cutpoint[1], labels[1],
                    ifelse(vector <= cutpoint[2], labels[2],
                          ifelse(vector <= cutpoint[3], labels[3], labels[4])))

    }

    if (n == 4) {

      cat <- ifelse(vector <= cutpoint[1], labels[1],
                    ifelse(vector <= cutpoint[2], labels[2],
                           ifelse(vector <= cutpoint[3], labels[3],
                                  ifelse(vector <= cutpoint[4], labels[4], labels[5]))))

    }

    if (n == 5) {

      cat <- ifelse(vector <= cutpoint[1], labels[1],
                    ifelse(vector <= cutpoint[2], labels[2],
                           ifelse(vector <= cutpoint[3], labels[3],
                                  ifelse(vector <= cutpoint[4], labels[4],
                                        ifelse(vector <= cutpoint[5], labels[5], labels[6])))))

    }

  }
      else {

        if (n == 1) {

          cat <- ifelse(vector < cutpoint[1], labels[1], labels[2])

        }

        if (n == 2) {

          cat <- ifelse(vector < cutpoint[1], labels[1],
                        ifelse(vector < cutpoint[2], labels[2], labels[3]))

        }

        if (n == 3) {

          cat <- ifelse(vector < cutpoint[1], labels[1],
                        ifelse(vector < cutpoint[2], labels[2],
                               ifelse(vector < cutpoint[3], labels[3], labels[4])))

        }

        if (n == 4) {

          cat <- ifelse(vector < cutpoint[1], labels[1],
                        ifelse(vector < cutpoint[2], labels[2],
                               ifelse(vector < cutpoint[3], labels[3],
                                      ifelse(vector < cutpoint[4], labels[4], labels[5]))))

        }

        if (n == 5) {

          cat <- ifelse(vector < cutpoint[1], labels[1],
                        ifelse(vector < cutpoint[2], labels[2],
                               ifelse(vector < cutpoint[3], labels[3],
                                      ifelse(vector < cutpoint[4], labels[4],
                                             ifelse(vector < cutpoint[5], labels[5], labels[6])))))

        }

      }

  cat = as.factor(cat)

  return(cat)

}
