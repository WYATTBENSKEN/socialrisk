#' Social Risk
#'
#' \code{socialrisk} returns a summary dataset containing indicators of social risk,
#'     which vary based on the taxonomy command, for each patient.
#'
#' This function uses data which has been properly prepared to identify and flag
#'     social risks.
#'
#' @param dat dataset which has been properly prepared in long format
#' @param id variable of the unique patient identifier
#' @param dx the column with the diagnoses (defaults to 'dx')
#' @param taxonomy the taxonomy one wishes to use for social risk, with options of
#'     "cms" (default), "mha", and "siren"
#'
#' @return dataframe with one row per patient, a column for their patient id,
#'     a column with whether they have any social risk, a column with the number
#'     of social risk domains, and columns with indicator variables for each social risk
#'
#' @examples
#' socialrisk(dat = prepared_data, id = patient_id, dx = dx, taxonomy = "cms")
#'
#'
#' @export

#' @importFrom rlang .data
socialrisk <- function(dat = NULL,
                       id = NULL,
                       dx = "dx",
                       taxonomy = "cms"){

  id2 <- rlang::quo_name(rlang::enquo(id))

  if (taxonomy = "cms"){

    dat1 <- dat %>%
      dplyr::mutate(z55 = dplyr::if_else(stringr::str_starts(dx, "Z55"), 1 , 0),
                    z56 = dplyr::if_else(stringr::str_starts(dx, "Z56"), 1 , 0),
                    z57 = dplyr::if_else(stringr::str_starts(dx, "Z57"), 1 , 0),
                    z59 = dplyr::if_else(stringr::str_starts(dx, "Z59"), 1 , 0),
                    z60 = dplyr::if_else(stringr::str_starts(dx, "Z60"), 1 , 0),
                    z62 = dplyr::if_else(stringr::str_starts(dx, "Z62"), 1 , 0),
                    z63 = dplyr::if_else(stringr::str_starts(dx, "Z63"), 1 , 0),
                    z64 = dplyr::if_else(stringr::str_starts(dx, "Z64"), 1 , 0),
                    z65 = dplyr::if_else(stringr::str_starts(dx, "Z65"), 1 , 0))

    dat2 <- dat1 %>%
      dplyr::group_by({{id}}) %>%
      dplyr::summarize(
        z55_education = max(.data$z55),
        z56_employment = max(.data$z56),
        z57_occupational = max(.data$z57),
        z59_housing = max(.data$z59),
        z60_social = max(.data$z60),
        z62_upbringing = max(.data$z62),
        z63_family = max(.data$z63),
        z64_psychosocial = max(.data$z64),
        z65_psychosocial_other = max(.data$z65)) %>%
      dplyr::ungroup()

    dat3 <- dat2 %>%
      dplyr::mutate(number_domains = z55_education + z56_employment + z57_occupation +
                      z59_housing + z60_social + z62_upbringing + z63_family +
                      z64_psychosocial + z65_psychosocial_other,
                    any_social_risk = dplyr::if_else(number_domains >= 1, 1, 0))

    dat4 <- dat3 %>%
      dplyr::select(id, any_social_risk, number_domains, z55_education, z56_employment,
                    z57_occupation, z59_housing, z60_social, z62_upbringing,
                    z63_family, z64_psychosocial, z65_psychosocial_other)

    return(dat4)

  }

  else if (taxonomy == "mha"){




  }


  else if (taxonomy == "siren") {




  }

  else {stop("Please specify an allowed taxonomy, either: cms, aha, or siren.")

  }

}


