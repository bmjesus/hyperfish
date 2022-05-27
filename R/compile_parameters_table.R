#' @title Script to apply the reflectance_color_indices function to a folder
#' @description Script to compile all the excel files produced by the script process_lab_lch_folder. It uses the name of the files to extract information about the replicate number and the sampling day. Only applicable to Konstantina's workflow.
#' @param my_folder path to folder excel files
#' @return a list with lightness, a*, b*, chroma and hue table (tibble). It also produces an excell file with all the values LAB_LCH_parameters_full_table.xlsx.
#' @keywords external
#' @export

compile_parameters_table <- function(my_folder){

  #list all files with the LAB_parameters.xlsx
  parameter_files_long <-  list.files(path = my_folder, pattern = "LAB_parameters.xlsx", all.files = FALSE,full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

  parameter_files_short <-  list.files(path = my_folder, pattern = "LAB_parameters.xlsx", all.files = FALSE,full.names = FALSE, recursive = TRUE, include.dirs = FALSE)

  #list all files with the LCH_parameters.xlsx
  parameter_files_long_lch <-  list.files(path = my_folder, pattern = "LCH_parameters.xlsx", all.files = FALSE,full.names = TRUE, recursive = TRUE, include.dirs = FALSE)

  #extracting the replicate
  length_name <- nchar(parameter_files_short)

  end_string_rep <- length_name - 28
  start_string_rep <- length_name - 33

  replicate <- substr(parameter_files_short, start_string_rep, end_string_rep)
  replicate  <-gsub("_","", replicate )
  replicate  <-gsub("m","", replicate )
  replicate  <-gsub("s","", replicate )

  #redseabream_Rep3_d1_21ms_LAB_parameters.xlsx

  #print(length_name)

  print(parameter_files_short)

  #print(replicate)

  end_string_day <- length_name - 24
  start_string_day <- length_name - 27

  day <- substr(parameter_files_short, start_string_day, end_string_day)
  day  <-gsub("_$","", day )
  day  <-gsub("_.$","", day )
  day  <-gsub("_","", day )

  #print(day)

  #adding 0s after the ds so that the plots are ordered
  day  <-gsub("d1$","d01",day  )
  day  <-gsub("d2$","d02",day  )
  day  <-gsub("d3$","d03",day  )
  day  <-gsub("d4$","d04",day  )
  day  <-gsub("d5$","d05",day  )
  day  <-gsub("d6$","d06",day  )
  day  <-gsub("d7$","d07",day  )
  day  <-gsub("d8$","d08",day  )
  day  <-gsub("d9$","d09",day  )


  #Lightness table
  xls <- tibble::tibble()
  for (i in 1:length(parameter_files_long))
  {
    temp_xls <- readxl::read_xlsx(path = parameter_files_long[i],sheet = "Lightness")
    temp_xls$rep <- rep(replicate[i],length(temp_xls[,1]))
    temp_xls$day <- rep(day[i],length(temp_xls[,1]))
    xls <- rbind(xls,temp_xls)
  }
  xls_lightness <- xls

  #a* table
  xls <- tibble::tibble()
  for (i in 1:length(parameter_files_long))
  {
    temp_xls <- readxl::read_xlsx(path = parameter_files_long[i],sheet = "a")
    temp_xls$rep <- rep(replicate[i],length(temp_xls[,1]))
    temp_xls$day <- rep(day[i],length(temp_xls[,1]))
    xls <- rbind(xls,temp_xls)
  }
  xls_a <- xls

  #b* table
  xls <- tibble::tibble()
  for (i in 1:length(parameter_files_long))
  {
    temp_xls <- readxl::read_xlsx(path = parameter_files_long[i],sheet = "b")
    temp_xls$rep <- rep(replicate[i],length(temp_xls[,1]))
    temp_xls$day <- rep(day[i],length(temp_xls[,1]))
    xls <- rbind(xls,temp_xls)
  }
  xls_b <- xls

  #Chroma table
  xls <- tibble::tibble()
  for (i in 1:length(parameter_files_long))
  {
    temp_xls <- readxl::read_xlsx(path = parameter_files_long_lch[i],sheet = "Chroma")
    temp_xls$rep <- rep(replicate[i],length(temp_xls[,1]))
    temp_xls$day <- rep(day[i],length(temp_xls[,1]))
    xls <- rbind(xls,temp_xls)
  }
  xls_chroma <- xls

  #Hue table
  xls <- tibble::tibble()
  for (i in 1:length(parameter_files_long))
  {
    temp_xls <- readxl::read_xlsx(path = parameter_files_long_lch[i],sheet = "Hue")
    temp_xls$rep <- rep(replicate[i],length(temp_xls[,1]))
    temp_xls$day <- rep(day[i],length(temp_xls[,1]))
    xls <- rbind(xls,temp_xls)
  }
  xls_hue <- xls





  #output section
  output <- list(xls_lightness, xls_a, xls_b, xls_chroma,xls_hue)

  names(output) <- c("lightness", "a", "b","chroma","hue")

  #exporting the full tables in xls format

  writexl::write_xlsx(output,
                      path = "LAB_LCH_parameters_full_table.xlsx",
                      col_names = TRUE,
                      format_headers = TRUE)
  return(output)

}

