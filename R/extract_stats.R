#' @title Calculate several statistics from selected ROI
#' @description This function calculates statistics from six ROI stored in a shapefile. It was designed for Konstantina's workflow and it is unlikely to work in another context. The ROI need to be called D1, D2, V1, V2, eye, gills.
#' @param tif_file path to tif file with the parameters to extract
#' @param roi_file path shapefile with the ROI file
#' @param type_file parameter to setup if the file is LAB or LCH.
#' @return The function returns a list with the statistics and the pixel values for each ROI.
#' @keywords external
#' @export

extract_stats <- function(tif_file,roi_file, type_file ){

  fish <- raster::stack(tif_file)
  fish_roi <- rgdal::readOGR(roi_file)


  if (type_file == "LAB"){
    names(fish) <- c("L","a","b")

  }

  if (type_file == "LCH"){
    names(fish) <- c("L","C","H")
  }

  pixels_roi <- fish[fish_roi]
  names(pixels_roi) <- as.character(fish_roi$CLASS_NAME)


  if (type_file == "LAB"){
    ########ROI D1
    D1_mean_L <- mean(pixels_roi$`ROI D1`[,"L"])
    D1_mean_a <- mean(pixels_roi$`ROI D1`[,"a"])
    D1_mean_b <- mean(pixels_roi$`ROI D1`[,"b"])

    D1_median_L <- median(pixels_roi$`ROI D1`[,"L"])
    D1_median_a <- median(pixels_roi$`ROI D1`[,"a"])
    D1_median_b <- median(pixels_roi$`ROI D1`[,"b"])

    D1_min_L <- min(pixels_roi$`ROI D1`[,"L"])
    D1_min_a <- min(pixels_roi$`ROI D1`[,"a"])
    D1_min_b <- min(pixels_roi$`ROI D1`[,"b"])

    D1_max_L <- max(pixels_roi$`ROI D1`[,"L"])
    D1_max_a <- max(pixels_roi$`ROI D1`[,"a"])
    D1_max_b <- max(pixels_roi$`ROI D1`[,"b"])

    D1_sd_L <- sd(pixels_roi$`ROI D1`[,"L"])
    D1_sd_a <- sd(pixels_roi$`ROI D1`[,"a"])
    D1_sd_b <- sd(pixels_roi$`ROI D1`[,"b"])

    D1_cv_L <- D1_sd_L/D1_mean_L * 100
    D1_cv_a <- D1_sd_a/D1_mean_a * 100
    D1_cv_b <- D1_sd_b/D1_mean_b * 100

    D1_num_pixels <- length(pixels_roi$`ROI D1`[,"L"])

    D1_se_L <- D1_sd_L/sqrt(D1_num_pixels)
    D1_se_a <- D1_sd_a/sqrt(D1_num_pixels)
    D1_se_b <- D1_sd_b/sqrt(D1_num_pixels)


    ########ROI D2
    D2_mean_L <- mean(pixels_roi$`ROI D2`[,"L"])
    D2_mean_a <- mean(pixels_roi$`ROI D2`[,"a"])
    D2_mean_b <- mean(pixels_roi$`ROI D2`[,"b"])

    D2_median_L <- median(pixels_roi$`ROI D2`[,"L"])
    D2_median_a <- median(pixels_roi$`ROI D2`[,"a"])
    D2_median_b <- median(pixels_roi$`ROI D2`[,"b"])

    D2_min_L <- min(pixels_roi$`ROI D2`[,"L"])
    D2_min_a <- min(pixels_roi$`ROI D2`[,"a"])
    D2_min_b <- min(pixels_roi$`ROI D2`[,"b"])

    D2_max_L <- max(pixels_roi$`ROI D2`[,"L"])
    D2_max_a <- max(pixels_roi$`ROI D2`[,"a"])
    D2_max_b <- max(pixels_roi$`ROI D2`[,"b"])

    D2_sd_L <- sd(pixels_roi$`ROI D2`[,"L"])
    D2_sd_a <- sd(pixels_roi$`ROI D2`[,"a"])
    D2_sd_b <- sd(pixels_roi$`ROI D2`[,"b"])

    D2_cv_L <- D2_sd_L/D2_mean_L * 100
    D2_cv_a <- D2_sd_a/D2_mean_a * 100
    D2_cv_b <- D2_sd_b/D2_mean_b * 100

    D2_num_pixels <- length(pixels_roi$`ROI D2`[,"L"])

    D2_se_L <- D2_sd_L/sqrt(D2_num_pixels)
    D2_se_a <- D2_sd_a/sqrt(D2_num_pixels)
    D2_se_b <- D2_sd_b/sqrt(D2_num_pixels)


    #######V1
    V1_mean_L <- mean(pixels_roi$`ROI V1`[,"L"])
    V1_mean_a <- mean(pixels_roi$`ROI V1`[,"a"])
    V1_mean_b <- mean(pixels_roi$`ROI V1`[,"b"])

    V1_median_L <- median(pixels_roi$`ROI V1`[,"L"])
    V1_median_a <- median(pixels_roi$`ROI V1`[,"a"])
    V1_median_b <- median(pixels_roi$`ROI V1`[,"b"])

    V1_min_L <- min(pixels_roi$`ROI V1`[,"L"])
    V1_min_a <- min(pixels_roi$`ROI V1`[,"a"])
    V1_min_b <- min(pixels_roi$`ROI V1`[,"b"])

    V1_max_L <- max(pixels_roi$`ROI V1`[,"L"])
    V1_max_a <- max(pixels_roi$`ROI V1`[,"a"])
    V1_max_b <- max(pixels_roi$`ROI V1`[,"b"])

    V1_sd_L <- sd(pixels_roi$`ROI V1`[,"L"])
    V1_sd_a <- sd(pixels_roi$`ROI V1`[,"a"])
    V1_sd_b <- sd(pixels_roi$`ROI V1`[,"b"])

    V1_cv_L <- V1_sd_L/V1_mean_L * 100
    V1_cv_a <- V1_sd_a/V1_mean_a * 100
    V1_cv_b <- V1_sd_b/V1_mean_b * 100

    V1_num_pixels <- length(pixels_roi$`ROI V1`[,"L"])

    V1_se_L <- V1_sd_L/sqrt(V1_num_pixels)
    V1_se_a <- V1_sd_a/sqrt(V1_num_pixels)
    V1_se_b <- V1_sd_b/sqrt(V1_num_pixels)


    ######V2
    V2_mean_L <- mean(pixels_roi$`ROI V2`[,"L"])
    V2_mean_a <- mean(pixels_roi$`ROI V2`[,"a"])
    V2_mean_b <- mean(pixels_roi$`ROI V2`[,"b"])

    V2_median_L <- median(pixels_roi$`ROI V2`[,"L"])
    V2_median_a <- median(pixels_roi$`ROI V2`[,"a"])
    V2_median_b <- median(pixels_roi$`ROI V2`[,"b"])

    V2_min_L <- min(pixels_roi$`ROI V2`[,"L"])
    V2_min_a <- min(pixels_roi$`ROI V2`[,"a"])
    V2_min_b <- min(pixels_roi$`ROI V2`[,"b"])

    V2_max_L <- max(pixels_roi$`ROI V2`[,"L"])
    V2_max_a <- max(pixels_roi$`ROI V2`[,"a"])
    V2_max_b <- max(pixels_roi$`ROI V2`[,"b"])

    V2_sd_L <- sd(pixels_roi$`ROI V2`[,"L"])
    V2_sd_a <- sd(pixels_roi$`ROI V2`[,"a"])
    V2_sd_b <- sd(pixels_roi$`ROI V2`[,"b"])

    V2_cv_L <- V2_sd_L/V2_mean_L * 100
    V2_cv_a <- V2_sd_a/V2_mean_a * 100
    V2_cv_b <- V2_sd_b/V2_mean_b * 100

    V2_num_pixels <- length(pixels_roi$`ROI V2`[,"L"])

    V2_se_L <- V2_sd_L/sqrt(V2_num_pixels)
    V2_se_a <- V2_sd_a/sqrt(V2_num_pixels)
    V2_se_b <- V2_sd_b/sqrt(V2_num_pixels)


    #####eye
    eye_mean_L <- mean(pixels_roi$`ROI eye`[,"L"])
    eye_mean_a <- mean(pixels_roi$`ROI eye`[,"a"])
    eye_mean_b <- mean(pixels_roi$`ROI eye`[,"b"])

    eye_median_L <- median(pixels_roi$`ROI eye`[,"L"])
    eye_median_a <- median(pixels_roi$`ROI eye`[,"a"])
    eye_median_b <- median(pixels_roi$`ROI eye`[,"b"])

    eye_min_L <- min(pixels_roi$`ROI eye`[,"L"])
    eye_min_a <- min(pixels_roi$`ROI eye`[,"a"])
    eye_min_b <- min(pixels_roi$`ROI eye`[,"b"])

    eye_max_L <- max(pixels_roi$`ROI eye`[,"L"])
    eye_max_a <- max(pixels_roi$`ROI eye`[,"a"])
    eye_max_b <- max(pixels_roi$`ROI eye`[,"b"])

    eye_sd_L <- sd(pixels_roi$`ROI eye`[,"L"])
    eye_sd_a <- sd(pixels_roi$`ROI eye`[,"a"])
    eye_sd_b <- sd(pixels_roi$`ROI eye`[,"b"])

    eye_cv_L <- eye_sd_L/eye_mean_L * 100
    eye_cv_a <- eye_sd_a/eye_mean_a * 100
    eye_cv_b <- eye_sd_b/eye_mean_b * 100

    eye_num_pixels <- length(pixels_roi$`ROI eye`[,"L"])

    eye_se_L <- eye_sd_L/sqrt(eye_num_pixels)
    eye_se_a <- eye_sd_a/sqrt(eye_num_pixels)
    eye_se_b <- eye_sd_b/sqrt(eye_num_pixels)


    ###gill
    gill_mean_L <- mean(pixels_roi$`ROI gill`[,"L"])
    gill_mean_a <- mean(pixels_roi$`ROI gill`[,"a"])
    gill_mean_b <- mean(pixels_roi$`ROI gill`[,"b"])

    gill_median_L <- median(pixels_roi$`ROI gill`[,"L"])
    gill_median_a <- median(pixels_roi$`ROI gill`[,"a"])
    gill_median_b <- median(pixels_roi$`ROI gill`[,"b"])

    gill_min_L <- min(pixels_roi$`ROI gill`[,"L"])
    gill_min_a <- min(pixels_roi$`ROI gill`[,"a"])
    gill_min_b <- min(pixels_roi$`ROI gill`[,"b"])

    gill_max_L <- max(pixels_roi$`ROI gill`[,"L"])
    gill_max_a <- max(pixels_roi$`ROI gill`[,"a"])
    gill_max_b <- max(pixels_roi$`ROI gill`[,"b"])

    gill_sd_L <- sd(pixels_roi$`ROI gill`[,"L"])
    gill_sd_a <- sd(pixels_roi$`ROI gill`[,"a"])
    gill_sd_b <- sd(pixels_roi$`ROI gill`[,"b"])

    gill_cv_L <- gill_sd_L/gill_mean_L * 100
    gill_cv_a <- gill_sd_a/gill_mean_a * 100
    gill_cv_b <- gill_sd_b/gill_mean_b * 100

    gill_num_pixels <- length(pixels_roi$`ROI gill`[,"L"])

    gill_se_L <- gill_sd_L/sqrt(gill_num_pixels)
    gill_se_a <- gill_sd_a/sqrt(gill_num_pixels)
    gill_se_b <- gill_sd_b/sqrt(gill_num_pixels)


  }

  if (type_file == "LCH"){
    ########ROI D1
    D1_mean_L <- mean(pixels_roi$`ROI D1`[,"L"])
    D1_mean_C <- mean(pixels_roi$`ROI D1`[,"C"])
    D1_mean_H <- mean(pixels_roi$`ROI D1`[,"H"])

    D1_median_L <- median(pixels_roi$`ROI D1`[,"L"])
    D1_median_C <- median(pixels_roi$`ROI D1`[,"C"])
    D1_median_H <- median(pixels_roi$`ROI D1`[,"H"])

    D1_min_L <- min(pixels_roi$`ROI D1`[,"L"])
    D1_min_C <- min(pixels_roi$`ROI D1`[,"C"])
    D1_min_H <- min(pixels_roi$`ROI D1`[,"H"])

    D1_max_L <- max(pixels_roi$`ROI D1`[,"L"])
    D1_max_C <- max(pixels_roi$`ROI D1`[,"C"])
    D1_max_H <- max(pixels_roi$`ROI D1`[,"H"])

    D1_sd_L <- sd(pixels_roi$`ROI D1`[,"L"])
    D1_sd_C <- sd(pixels_roi$`ROI D1`[,"C"])
    D1_sd_H <- sd(pixels_roi$`ROI D1`[,"H"])

    D1_cv_L <- D1_sd_L/D1_mean_L * 100
    D1_cv_C <- D1_sd_C/D1_mean_C * 100
    D1_cv_H <- D1_sd_H/D1_mean_H * 100

    D1_num_pixels <- length(pixels_roi$`ROI D1`[,"L"])

    D1_se_L <- D1_sd_L/sqrt(D1_num_pixels)
    D1_se_C <- D1_sd_C/sqrt(D1_num_pixels)
    D1_se_H <- D1_sd_H/sqrt(D1_num_pixels)


    ########ROI D2
    D2_mean_L <- mean(pixels_roi$`ROI D2`[,"L"])
    D2_mean_C <- mean(pixels_roi$`ROI D2`[,"C"])
    D2_mean_H <- mean(pixels_roi$`ROI D2`[,"H"])

    D2_median_L <- median(pixels_roi$`ROI D2`[,"L"])
    D2_median_C <- median(pixels_roi$`ROI D2`[,"C"])
    D2_median_H <- median(pixels_roi$`ROI D2`[,"H"])

    D2_min_L <- min(pixels_roi$`ROI D2`[,"L"])
    D2_min_C <- min(pixels_roi$`ROI D2`[,"C"])
    D2_min_H <- min(pixels_roi$`ROI D2`[,"H"])

    D2_max_L <- max(pixels_roi$`ROI D2`[,"L"])
    D2_max_C <- max(pixels_roi$`ROI D2`[,"C"])
    D2_max_H <- max(pixels_roi$`ROI D2`[,"H"])

    D2_sd_L <- sd(pixels_roi$`ROI D2`[,"L"])
    D2_sd_C <- sd(pixels_roi$`ROI D2`[,"C"])
    D2_sd_H <- sd(pixels_roi$`ROI D2`[,"H"])

    D2_cv_L <- D2_sd_L/D2_mean_L * 100
    D2_cv_C <- D2_sd_C/D2_mean_C * 100
    D2_cv_H <- D2_sd_H/D2_mean_H * 100

    D2_num_pixels <- length(pixels_roi$`ROI D2`[,"L"])

    D2_se_L <- D2_sd_L/sqrt(D2_num_pixels)
    D2_se_C <- D2_sd_C/sqrt(D2_num_pixels)
    D2_se_H <- D2_sd_H/sqrt(D2_num_pixels)


    ########ROI V1
    V1_mean_L <- mean(pixels_roi$`ROI V1`[,"L"])
    V1_mean_C <- mean(pixels_roi$`ROI V1`[,"C"])
    V1_mean_H <- mean(pixels_roi$`ROI V1`[,"H"])

    V1_median_L <- median(pixels_roi$`ROI V1`[,"L"])
    V1_median_C <- median(pixels_roi$`ROI V1`[,"C"])
    V1_median_H <- median(pixels_roi$`ROI V1`[,"H"])

    V1_min_L <- min(pixels_roi$`ROI V1`[,"L"])
    V1_min_C <- min(pixels_roi$`ROI V1`[,"C"])
    V1_min_H <- min(pixels_roi$`ROI V1`[,"H"])

    V1_max_L <- max(pixels_roi$`ROI V1`[,"L"])
    V1_max_C <- max(pixels_roi$`ROI V1`[,"C"])
    V1_max_H <- max(pixels_roi$`ROI V1`[,"H"])

    V1_sd_L <- sd(pixels_roi$`ROI V1`[,"L"])
    V1_sd_C <- sd(pixels_roi$`ROI V1`[,"C"])
    V1_sd_H <- sd(pixels_roi$`ROI V1`[,"H"])

    V1_cv_L <- V1_sd_L/V1_mean_L * 100
    V1_cv_C <- V1_sd_C/V1_mean_C * 100
    V1_cv_H <- V1_sd_H/V1_mean_H * 100

    V1_num_pixels <- length(pixels_roi$`ROI V1`[,"L"])

    V1_se_L <- V1_sd_L/sqrt(V1_num_pixels)
    V1_se_C <- V1_sd_C/sqrt(V1_num_pixels)
    V1_se_H <- V1_sd_H/sqrt(V1_num_pixels)



    ########ROI V2
    V2_mean_L <- mean(pixels_roi$`ROI V2`[,"L"])
    V2_mean_C <- mean(pixels_roi$`ROI V2`[,"C"])
    V2_mean_H <- mean(pixels_roi$`ROI V2`[,"H"])

    V2_median_L <- median(pixels_roi$`ROI V2`[,"L"])
    V2_median_C <- median(pixels_roi$`ROI V2`[,"C"])
    V2_median_H <- median(pixels_roi$`ROI V2`[,"H"])

    V2_min_L <- min(pixels_roi$`ROI V2`[,"L"])
    V2_min_C <- min(pixels_roi$`ROI V2`[,"C"])
    V2_min_H <- min(pixels_roi$`ROI V2`[,"H"])

    V2_max_L <- max(pixels_roi$`ROI V2`[,"L"])
    V2_max_C <- max(pixels_roi$`ROI V2`[,"C"])
    V2_max_H <- max(pixels_roi$`ROI V2`[,"H"])

    V2_sd_L <- sd(pixels_roi$`ROI V2`[,"L"])
    V2_sd_C <- sd(pixels_roi$`ROI V2`[,"C"])
    V2_sd_H <- sd(pixels_roi$`ROI V2`[,"H"])

    V2_cv_L <- V2_sd_L/V2_mean_L * 100
    V2_cv_C <- V2_sd_C/V2_mean_C * 100
    V2_cv_H <- V2_sd_H/V2_mean_H * 100

    V2_num_pixels <- length(pixels_roi$`ROI V2`[,"L"])

    V2_se_L <- V2_sd_L/sqrt(V2_num_pixels)
    V2_se_C <- V2_sd_C/sqrt(V2_num_pixels)
    V2_se_H <- V2_sd_H/sqrt(V2_num_pixels)

    ########ROI eye
    eye_mean_L <- mean(pixels_roi$`ROI eye`[,"L"])
    eye_mean_C <- mean(pixels_roi$`ROI eye`[,"C"])
    eye_mean_H <- mean(pixels_roi$`ROI eye`[,"H"])

    eye_median_L <- median(pixels_roi$`ROI eye`[,"L"])
    eye_median_C <- median(pixels_roi$`ROI eye`[,"C"])
    eye_median_H <- median(pixels_roi$`ROI eye`[,"H"])

    eye_min_L <- min(pixels_roi$`ROI eye`[,"L"])
    eye_min_C <- min(pixels_roi$`ROI eye`[,"C"])
    eye_min_H <- min(pixels_roi$`ROI eye`[,"H"])

    eye_max_L <- max(pixels_roi$`ROI eye`[,"L"])
    eye_max_C <- max(pixels_roi$`ROI eye`[,"C"])
    eye_max_H <- max(pixels_roi$`ROI eye`[,"H"])

    eye_sd_L <- sd(pixels_roi$`ROI eye`[,"L"])
    eye_sd_C <- sd(pixels_roi$`ROI eye`[,"C"])
    eye_sd_H <- sd(pixels_roi$`ROI eye`[,"H"])

    eye_cv_L <- eye_sd_L/eye_mean_L * 100
    eye_cv_C <- eye_sd_C/eye_mean_C * 100
    eye_cv_H <- eye_sd_H/eye_mean_H * 100

    eye_num_pixels <- length(pixels_roi$`ROI eye`[,"L"])

    eye_se_L <- eye_sd_L/sqrt(eye_num_pixels)
    eye_se_C <- eye_sd_C/sqrt(eye_num_pixels)
    eye_se_H <- eye_sd_H/sqrt(eye_num_pixels)


    ########ROI gill
    gill_mean_L <- mean(pixels_roi$`ROI gill`[,"L"])
    gill_mean_C <- mean(pixels_roi$`ROI gill`[,"C"])
    gill_mean_H <- mean(pixels_roi$`ROI gill`[,"H"])

    gill_median_L <- median(pixels_roi$`ROI gill`[,"L"])
    gill_median_C <- median(pixels_roi$`ROI gill`[,"C"])
    gill_median_H <- median(pixels_roi$`ROI gill`[,"H"])

    gill_min_L <- min(pixels_roi$`ROI gill`[,"L"])
    gill_min_C <- min(pixels_roi$`ROI gill`[,"C"])
    gill_min_H <- min(pixels_roi$`ROI gill`[,"H"])

    gill_max_L <- max(pixels_roi$`ROI gill`[,"L"])
    gill_max_C <- max(pixels_roi$`ROI gill`[,"C"])
    gill_max_H <- max(pixels_roi$`ROI gill`[,"H"])

    gill_sd_L <- sd(pixels_roi$`ROI gill`[,"L"])
    gill_sd_C <- sd(pixels_roi$`ROI gill`[,"C"])
    gill_sd_H <- sd(pixels_roi$`ROI gill`[,"H"])

    gill_cv_L <- gill_sd_L/gill_mean_L * 100
    gill_cv_C <- gill_sd_C/gill_mean_C * 100
    gill_cv_H <- gill_sd_H/gill_mean_H * 100

    gill_num_pixels <- length(pixels_roi$`ROI gill`[,"L"])

    gill_se_L <- gill_sd_L/sqrt(gill_num_pixels)
    gill_se_C <- gill_sd_C/sqrt(gill_num_pixels)
    gill_se_H <- gill_sd_H/sqrt(gill_num_pixels)


  }


  #constructing the outputs

  if (type_file == "LAB"){

    #L dataframe

    L <- data.frame(matrix(nrow=6,ncol=9))
    names(L) <- c("roi","mean","median","min","max","sd","cv","num_pixels","se")
    L$roi <- c("D1","D2","V1","V2","eye","gill")
    L$mean <- c(D1_mean_L, D2_mean_L, V1_mean_L,V2_mean_L,eye_mean_L,gill_mean_L)
    L$median <- c(D1_median_L, D2_median_L, V1_median_L,V2_median_L,eye_median_L,gill_median_L)
    L$min <- c(D1_min_L, D2_min_L, V1_min_L,V2_min_L,eye_min_L,gill_min_L)
    L$max <- c(D1_max_L, D2_max_L, V1_max_L,V2_max_L,eye_max_L,gill_max_L)
    L$sd <- c(D1_sd_L, D2_sd_L, V1_sd_L,V2_sd_L,eye_sd_L,gill_sd_L)
    L$cv <- c(D1_cv_L, D2_cv_L, V1_cv_L,V2_cv_L,eye_cv_L,gill_cv_L)
    L$num_pixels <- c(D1_num_pixels, D2_num_pixels, V1_num_pixels,V2_num_pixels,eye_num_pixels,gill_num_pixels)
    L$se <- c(D1_se_L, D2_se_L, V1_se_L,V2_se_L,eye_se_L,gill_se_L)


    #a* dataframe
    a <- data.frame(matrix(nrow=6,ncol=7))
    names(a) <- c("roi","mean","median","min","max","sd","cv")
    a$roi <- c("D1","D2","V1","V2","eye","gill")
    a$mean <- c(D1_mean_a, D2_mean_a, V1_mean_a,V2_mean_a,eye_mean_a,gill_mean_a)
    a$median <- c(D1_median_a, D2_median_a, V1_median_a,V2_median_a,eye_median_a,gill_median_a)
    a$min <- c(D1_min_a, D2_min_a, V1_min_a,V2_min_a,eye_min_a,gill_min_a)
    a$max <- c(D1_max_a, D2_max_a, V1_max_a,V2_max_a,eye_max_a,gill_max_a)
    a$sd <- c(D1_sd_a, D2_sd_a, V1_sd_a,V2_sd_a,eye_sd_a,gill_sd_a)
    a$cv <- c(D1_cv_a, D2_cv_a, V1_cv_a,V2_cv_a,eye_cv_a,gill_cv_a)
    a$num_pixels <- c(D1_num_pixels, D2_num_pixels, V1_num_pixels,V2_num_pixels,eye_num_pixels,gill_num_pixels)
    a$se <- c(D1_se_a, D2_se_a, V1_se_a,V2_se_a,eye_se_a,gill_se_a)


    #b* dataframe
    b <- data.frame(matrix(nrow=6,ncol=7))
    names(b) <- c("roi","mean","median","min","max","sd","cv")
    b$roi <- c("D1","D2","V1","V2","eye","gill")
    b$mean <- c(D1_mean_b, D2_mean_b, V1_mean_b,V2_mean_b,eye_mean_b,gill_mean_b)
    b$median <- c(D1_median_b, D2_median_b, V1_median_b,V2_median_b,eye_median_b,gill_median_b)
    b$min <- c(D1_min_b, D2_min_b, V1_min_b,V2_min_b,eye_min_b,gill_min_b)
    b$max <- c(D1_max_b, D2_max_b, V1_max_b,V2_max_b,eye_max_b,gill_max_b)
    b$sd <- c(D1_sd_b, D2_sd_b, V1_sd_b,V2_sd_b,eye_sd_b,gill_sd_b)
    b$cv <- c(D1_cv_b, D2_cv_b, V1_cv_b,V2_cv_b,eye_cv_b,gill_cv_b)
    b$num_pixels <- c(D1_num_pixels, D2_num_pixels, V1_num_pixels,V2_num_pixels,eye_num_pixels,gill_num_pixels)
    b$se <- c(D1_se_b, D2_se_b, V1_se_b,V2_se_b,eye_se_b,gill_se_b)


    output <- list(L,a,b)
    names(output) <- c("Lightness","a","b")


  }

  if (type_file == "LCH"){

    #Lightness dataframe
    L <- data.frame(matrix(nrow=6,ncol=9))
    names(L) <- c("roi","mean","median","min","max","sd","cv","num_pixels","se")
    L$roi <- c("D1","D2","V1","V2","eye","gill")
    L$mean <- c(D1_mean_L, D2_mean_L, V1_mean_L,V2_mean_L,eye_mean_L,gill_mean_L)
    L$median <- c(D1_median_L, D2_median_L, V1_median_L,V2_median_L,eye_median_L,gill_median_L)
    L$min <- c(D1_min_L, D2_min_L, V1_min_L,V2_min_L,eye_min_L,gill_min_L)
    L$max <- c(D1_max_L, D2_max_L, V1_max_L,V2_max_L,eye_max_L,gill_max_L)
    L$sd <- c(D1_sd_L, D2_sd_L, V1_sd_L,V2_sd_L,eye_sd_L,gill_sd_L)
    L$cv <- c(D1_cv_L, D2_cv_L, V1_cv_L,V2_cv_L,eye_cv_L,gill_cv_L)
    L$num_pixels <- c(D1_num_pixels, D2_num_pixels, V1_num_pixels,V2_num_pixels,eye_num_pixels,gill_num_pixels)
    L$se <- c(D1_se_L, D2_se_L, V1_se_L,V2_se_L,eye_se_L,gill_se_L)

    C <- data.frame(matrix(nrow=6,ncol=7))
    names(C) <- c("roi","mean","median","min","max","sd","cv")
    C$roi <- c("D1","D2","V1","V2","eye","gill")
    C$mean <- c(D1_mean_C, D2_mean_C, V1_mean_C,V2_mean_C,eye_mean_C,gill_mean_C)
    C$median <- c(D1_median_C, D2_median_C, V1_median_C,V2_median_C,eye_median_C,gill_median_C)
    C$min <- c(D1_min_C, D2_min_C, V1_min_C,V2_min_C,eye_min_C,gill_min_C)
    C$max <- c(D1_max_C, D2_max_C, V1_max_C,V2_max_C,eye_max_C,gill_max_C)
    C$sd <- c(D1_sd_C, D2_sd_C, V1_sd_C,V2_sd_C,eye_sd_C,gill_sd_C)
    C$cv <- c(D1_cv_C, D2_cv_C, V1_cv_C,V2_cv_C,eye_cv_C,gill_cv_C)
    C$num_pixels <- c(D1_num_pixels, D2_num_pixels, V1_num_pixels,V2_num_pixels,eye_num_pixels,gill_num_pixels)
    C$se <- c(D1_se_C, D2_se_C, V1_se_C,V2_se_C,eye_se_C,gill_se_C)


    H <- data.frame(matrix(nrow=6,ncol=7))
    names(H) <- c("roi","mean","median","min","max","sd","cv")
    H$roi <- c("D1","D2","V1","V2","eye","gill")
    H$mean <- c(D1_mean_H, D2_mean_H, V1_mean_H,V2_mean_H,eye_mean_H,gill_mean_H)
    H$median <- c(D1_median_H, D2_median_H, V1_median_H,V2_median_H,eye_median_H,gill_median_H)
    H$min <- c(D1_min_H, D2_min_H, V1_min_H,V2_min_H,eye_min_H,gill_min_H)
    H$max <- c(D1_max_H, D2_max_H, V1_max_H,V2_max_H,eye_max_H,gill_max_H)
    H$sd <- c(D1_sd_H, D2_sd_H, V1_sd_H,V2_sd_H,eye_sd_H,gill_sd_H)
    H$cv <- c(D1_cv_H, D2_cv_H, V1_cv_H,V2_cv_H,eye_cv_H,gill_cv_H)
    H$num_pixels <- c(D1_num_pixels, D2_num_pixels, V1_num_pixels,V2_num_pixels,eye_num_pixels,gill_num_pixels)
    H$se <- c(D1_se_H, D2_se_H, V1_se_H,V2_se_H,eye_se_H,gill_se_H)

    output <- list(L,C,H)
    names(output) <- c("Lightness","Chroma","Hue")


  }

  #export the pixel values for each ROI and each parameter. It could be useful to use in histogram analysis

  if (type_file == "LAB"){
    pixels_L_D1 <- pixels_roi$`ROI D1`[,"L"]
    pixels_a_D1 <- pixels_roi$`ROI D1`[,"a"]
    pixels_b_D1 <- pixels_roi$`ROI D1`[,"b"]

    pixels_L_D2 <- pixels_roi$`ROI D2`[,"L"]
    pixels_a_D2 <- pixels_roi$`ROI D2`[,"a"]
    pixels_b_D2 <- pixels_roi$`ROI D2`[,"b"]

    pixels_L_V1 <- pixels_roi$`ROI V1`[,"L"]
    pixels_a_V1 <- pixels_roi$`ROI V1`[,"a"]
    pixels_b_V1 <- pixels_roi$`ROI V1`[,"b"]

    pixels_L_V2 <- pixels_roi$`ROI V2`[,"L"]
    pixels_a_V2 <- pixels_roi$`ROI V2`[,"a"]
    pixels_b_V2 <- pixels_roi$`ROI V2`[,"b"]

    pixels_L_eye <- pixels_roi$`ROI eye`[,"L"]
    pixels_a_eye <- pixels_roi$`ROI eye`[,"a"]
    pixels_b_eye <- pixels_roi$`ROI eye`[,"b"]

    pixels_L_gill <- pixels_roi$`ROI gill`[,"L"]
    pixels_a_gill <- pixels_roi$`ROI gill`[,"a"]
    pixels_b_gill <- pixels_roi$`ROI gill`[,"b"]

    D1 <- data.frame(matrix(nrow= D1_num_pixels,ncol=3))
    names(D1) <- c("L","a","b")
    D1$L <- pixels_L_D1
    D1$a <- pixels_a_D1
    D1$b <- pixels_b_D1

    D2 <- data.frame(matrix(nrow= D2_num_pixels,ncol=3))
    names(D2) <- c("L","a","b")
    D2$L <- pixels_L_D2
    D2$a <- pixels_a_D2
    D2$b <- pixels_b_D2

    V1 <- data.frame(matrix(nrow= V1_num_pixels,ncol=3))
    names(V1) <- c("L","a","b")
    V1$L <- pixels_L_V1
    V1$a <- pixels_a_V1
    V1$b <- pixels_b_V1

    V2 <- data.frame(matrix(nrow= V2_num_pixels,ncol=3))
    names(V2) <- c("L","a","b")
    V2$L <- pixels_L_V2
    V2$a <- pixels_a_V2
    V2$b <- pixels_b_V2

    eye <- data.frame(matrix(nrow= eye_num_pixels,ncol=3))
    names(eye) <- c("L","a","b")
    eye$L <- pixels_L_eye
    eye$a <- pixels_a_eye
    eye$b <- pixels_b_eye

    gill <- data.frame(matrix(nrow= gill_num_pixels,ncol=3))
    names(gill) <- c("L","a","b")
    gill$L <- pixels_L_gill
    gill$a <- pixels_a_gill
    gill$b <- pixels_b_gill

    output_pixels <- list(D1,D2,V1,V2,eye,gill)
    names(output_pixels) <- c("D1","D2","V1","V2","eye","gill")
  }

  if (type_file == "LCH"){

    pixels_L_D1 <- pixels_roi$`ROI D1`[,"L"]
    pixels_C_D1 <- pixels_roi$`ROI D1`[,"C"]
    pixels_H_D1 <- pixels_roi$`ROI D1`[,"H"]

    pixels_L_D2 <- pixels_roi$`ROI D2`[,"L"]
    pixels_C_D2 <- pixels_roi$`ROI D2`[,"C"]
    pixels_H_D2 <- pixels_roi$`ROI D2`[,"H"]

    pixels_L_V1 <- pixels_roi$`ROI V1`[,"L"]
    pixels_C_V1 <- pixels_roi$`ROI V1`[,"C"]
    pixels_H_V1 <- pixels_roi$`ROI V1`[,"H"]

    pixels_L_V2 <- pixels_roi$`ROI V2`[,"L"]
    pixels_C_V2 <- pixels_roi$`ROI V2`[,"C"]
    pixels_H_V2 <- pixels_roi$`ROI V2`[,"H"]

    pixels_L_eye <- pixels_roi$`ROI eye`[,"L"]
    pixels_C_eye <- pixels_roi$`ROI eye`[,"C"]
    pixels_H_eye <- pixels_roi$`ROI eye`[,"H"]

    pixels_L_gill <- pixels_roi$`ROI gill`[,"L"]
    pixels_C_gill <- pixels_roi$`ROI gill`[,"C"]
    pixels_H_gill <- pixels_roi$`ROI gill`[,"H"]


    D1 <- data.frame(matrix(nrow= D1_num_pixels,ncol=3))
    names(D1) <- c("L","C","H")
    D1$L <- pixels_L_D1
    D1$C <- pixels_C_D1
    D1$H <- pixels_H_D1

    D2 <- data.frame(matrix(nrow= D2_num_pixels,ncol=3))
    names(D2) <- c("L","C","H")
    D2$L <- pixels_L_D2
    D2$C <- pixels_C_D2
    D2$H <- pixels_H_D2

    V1 <- data.frame(matrix(nrow= V1_num_pixels,ncol=3))
    names(V1) <- c("L","C","H")
    V1$L <- pixels_L_V1
    V1$C <- pixels_C_V1
    V1$H <- pixels_H_V1

    V2 <- data.frame(matrix(nrow= V2_num_pixels,ncol=3))
    names(V2) <- c("L","C","H")
    V2$L <- pixels_L_V2
    V2$C <- pixels_C_V2
    V2$H <- pixels_H_V2

    eye <- data.frame(matrix(nrow= eye_num_pixels,ncol=3))
    names(eye) <- c("L","C","H")
    eye$L <- pixels_L_eye
    eye$C <- pixels_C_eye
    eye$H <- pixels_H_eye

    gill <- data.frame(matrix(nrow= gill_num_pixels,ncol=3))
    names(gill) <- c("L","C","H")
    gill$L <- pixels_L_gill
    gill$C <- pixels_C_gill
    gill$H <- pixels_H_gill

    output_pixels <- list(D1,D2,V1,V2,eye,gill)
    names(output_pixels) <- c("D1","D2","V1","V2","eye","gill")
  }

  output<-list(output,output_pixels)
  names(output)<- c("parameters","pixels")
  return(output)

}
