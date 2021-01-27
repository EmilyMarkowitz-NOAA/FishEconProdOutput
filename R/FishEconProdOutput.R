
#' Tornqvist Price Index Base Year Function
#'
#' Tornqvist Price Index Base Year Function
#' @param dat The dataset you would like to use.
#' @param Year Name of the column holding year data.
#' @param pvar Name of the column holding price data.
#' @param vvar Name of the column holding value data.
#' @param prodID Name of the column holding prodID data.
#' @param baseyr The year dollar values need to be in.
#' @export
#' @examples
#' tornb(dat = data.frame("Year" = c(2001:2020, 2001:2020, 2001:2020, 2001:2020),
#'                        "p" = rnorm(n = 80, mean = 1, sd = .1),
#'                        "v" = rnorm(n = 80, mean = 500, sd = 300),
#'                        "prod" = c(rep_len("A", 20), rep_len("B", 20),
#'                                   rep_len("C", 20), rep_len("D", 20))),
#'      Year = "Year",
#'      pvar = "p",
#'      vvar = "v",
#'      prodID = "prod",
#'      baseyr = 2015)
tornb <- function(dat,
                  Year = "Year",
                  pvar = "p",
                  vvar = "v",
                  prodID = "prod",
                  baseyr) {

  p <- v <- NULL #satisfy CRAN

  data1<-dat
  data1<-data1[order(data1$Year), ]

  names(data1)[names(data1) %in% Year] <- "Year"
  names(data1)[names(data1) %in% pvar] <- "p"
  names(data1)[names(data1) %in% vvar] <- "v"
  names(data1)[names(data1) %in% prodID] <- "prod"
  data1 <- data1[, c("Year", "p", "v", "prod")]
  # tornc<-function(data1,baseyr){
  #
  years<-unique(data1$Year)
  N=length(years)

  data1 <- data1[order(data1$Year, decreasing = T), ]

  FPI<-as.data.frame(matrix(0,nrow=N,2)) #Set up Data frame to hold results
  colnames(FPI)[1]<-"Year"                #Name columns in Data Frame PI
  colnames(FPI)[2]<-"BPI"

  FPI[,1]=years                       #Put Years into first column of dataframe

  t=1

  for(i in min(years):(max(years)-1)){
    base <- subset(data1, (Year == baseyr &
                             p > 0 & v > 0))    #only keep observations with positive p,v
    year2 <- subset(data1, (Year == i &
                              p > 0 & v > 0))        #only keep observations with positive p,v

    year1_2 <- merge(base, year2, by = "prod", all.x = TRUE, all.y = TRUE, no.dups = TRUE) #merge two data frames
    year1_2 <- stats::na.omit(year1_2)  #Any rows with "NA" values are deleted.
    year1_2 <- year1_2[!(is.infinite(year1_2$p.y) | is.infinite(year1_2$p.x)),]

    yr1tval = sum(year1_2$v.x, na.rm = T)   #Calculate total value for year 1
    yr2tval = sum(year1_2$v.y, na.rm = T)   #Calculate total value for year 2

    year1_2$yr1shr = (year1_2$v.x / yr1tval) #calculate share values for year 1 products
    year1_2$yr2shr = (year1_2$v.y / yr2tval) #calculate share values for year 2 products

    year1_2$avgr = (year1_2$yr1shr + year1_2$yr2shr) / 2     #calculate average share values for years 1 and 2

    year1_2$tp = (year1_2$p.y / year1_2$p.x) ^ (year1_2$avgr) #calculate tornqvist value for each product

    BTI = prod(year1_2$tp, na.rm = T)   #calculates total tornqvist value

    FPI[t, 2] = BTI #creates base Tornqvist Value
    t = t + 1
  }

  return(FPI)
}


#' Tornqvist Price Index Base Year chain Function
#'
#' Tornqvist Price Index Base Year chain Function
#' @param dat The dataset you would like to use.
#' @param Year Name of the column holding year data.
#' @param pvar Name of the column holding price data.
#' @param vvar Name of the column holding value data.
#' @param prodID Name of the column holding prodID data.
#' @param baseyr The year dollar values need to be in.
#' @export
#' @examples
#' tornc(dat = data.frame("Year" = c(2001:2020, 2001:2020, 2001:2020, 2001:2020),
#'                        "p" = rnorm(n = 80, mean = 1, sd = .1),
#'                        "v" = rnorm(n = 80, mean = 500, sd = 300),
#'                        "prod" = c(rep_len("A", 20), rep_len("B", 20),
#'                                   rep_len("C", 20), rep_len("D", 20))),
#'      Year = "Year",
#'      pvar = "p",
#'      vvar = "v",
#'      prodID = "prod",
#'      baseyr = 2015)
tornc <- function(dat,
                  Year = "Year",
                  pvar = "p",
                  vvar = "v",
                  prodID = "prod",
                  baseyr) {

  p <- v <- NULL #satisfy CRAN

  data1<-dat
  data1<-data1[order(data1$Year), ]

  names(data1)[names(data1) %in% Year] <- "Year"
  names(data1)[names(data1) %in% pvar] <- "p"
  names(data1)[names(data1) %in% vvar] <- "v"
  names(data1)[names(data1) %in% prodID] <- "prod"
  data1 <- data1[, c("Year", "p", "v", "prod")]

  years <- unique(data1$Year)
  N = length(years)

  data1 <- data1[order(data1$Year, decreasing = T), ]

  CPI <- as.data.frame(matrix(0, nrow = N, 3)) #Set up Data frame to hold results
  colnames(CPI)[1] <- "Year"                #Name columns in Data Frame PI
  colnames(CPI)[2] <- "CPI"
  colnames(CPI)[3] <- "BPI"

  CPI[, 1] = years                      #Put Years into first column of dataframe
  CPI[1, 2] = 1

  t=1

  for(i in min(years):(max(years)-1)){

    year1<-subset(data1, (Year==i & p>0 & v>0))    #only keep observations with positive p,v
    year2<-subset(data1, (Year==(i+1) & p>0 & v>0))#only keep observations with positive p,v

    year1_2<-merge(year1, year2, by="prod", all.x=TRUE, all.y=TRUE, no.dups=TRUE) #merge two data frames
    year1_2<-stats::na.omit(year1_2)  #Any rows with "NA" values are deleted.
    year1_2 <- year1_2[!(is.infinite(year1_2$p.y) | is.infinite(year1_2$p.x)),]

    yr1tval=sum(year1_2$v.x, na.rm = T)   #Calculate total value for year 1
    yr2tval=sum(year1_2$v.y, na.rm = T)   #Calculate total value for year 2

    year1_2$yr1shr <- (year1_2$v.x/yr1tval) #calculate share values for year 1 products
    year1_2$yr2shr <- (year1_2$v.y/yr2tval) #calculate share values for year 2 products

    year1_2$avgr <- (year1_2$yr1shr+year1_2$yr2shr)/2     #calculate average share values for years 1 and 2

    year1_2$tp <- (year1_2$p.y/year1_2$p.x)^(year1_2$avgr) #calculate tornqvist value for each product

    CPTI=prod(year1_2$tp, na.rm = T)   #calculates total tornqvist value


    CPI[(t+1),2]=CPI[t,2]*CPTI #creates chained Tornqvist Value

    t=t+1
  }

  baseval=CPI[CPI$Year == baseyr,2]
  CPI$BPI=CPI$CPI/baseval


  return(CPI)
}

#' Price Methods - Category Level
#'
#' This function systematically runs the Price Method Productivity Output analysis for all species of a cateorgy.
#' @param dat00 Default dataset.
#' @param ii Category number.
#' @param category A character string. A unique string from the 'category0' column of the group being evaluated.
#' @param category0 A character string. The column where the category is defined.
#' @param baseyr Numeric year (YYYY). The base year you are assessing the anaylsis with. Typically this is the earliest year in the data set, but it can be any year you choose.
#' @param maxyr The maxium year to assess in the dataset.
#' @param minyr The minium year to assess in the dataset.
#' @param warnings_list A list where warnings are stored. If using this function in the PriceMethodOutput it will be inherited. If using outside of that function, put ls().
#' @export
PriceMethodOutput_Category <- function(dat00,
                                       ii,
                                       category,
                                       category0,
                                       baseyr,
                                       maxyr,
                                       minyr,
                                       warnings_list = ls()) {

  temp_cat <- dat00[dat00[, category0] %in% category,]

  temp_cat<-dplyr::rename(temp_cat,
                          "q" = "Pounds",
                          "v" = "Dollars")

  temp_cat <- subset(temp_cat, "p" > 0 &  "v" > 0) #only keep observations with positive p,v,q

  temp_cat <-
    stats::aggregate.data.frame(
      x = temp_cat[, c("q", "v")],
      by = list("Year" = temp_cat$Year,
                "prod" = temp_cat$Tsn,
                "cat" = temp_cat[, category0]),
      FUN = sum,
      na.rm = T
    )

  # temp_cat<-temp_cat[order(temp_cat$Year),]

  temp_cat$p <- temp_cat$v / temp_cat$q

  if (sum(minyr:maxyr %in% unique(temp_cat$Year)) != length(minyr:maxyr)) {
    temp0 <- data.frame(matrix(
      data = NA,
      nrow = length(setdiff(
        x = minyr:maxyr, y = unique(temp_cat$Year)
      )),
      ncol = ncol(temp_cat)
    ))
    names(temp0) <- names(temp_cat)
    temp0$Year <- setdiff(x = minyr:maxyr, y = unique(temp_cat$Year))
    temp0$cat <- category
    # temp0$time <- setdiff(x = ((minyr:maxyr) - (minyr - 1)), y = unique(temp_cat$time))
    temp0$prod <- 0

    temp_cat <- rbind.data.frame(temp_cat, temp0)
    temp_cat <- temp_cat[order(temp_cat$Year), ]

    # #imputed
    # temp_cat <-
    #   ReplaceFirst(colnames = c("p", "q", "v"), dat00 = temp_cat)
    # temp_cat <-
    #   ReplaceMid(colnames = c("p", "q", "v"), dat00 = temp_cat)

    warnings_list <-
      c(warnings_list, list(
        paste0(
          'Warning: ',
          category,
          ': Error in priceIndex(temp_cat, pvar = "p", qvar = "q", pervar = "time",  : The time period variable is not continuous. '
        )
      ))
  }

  temp_cat <- temp_cat[order(temp_cat$Year), ]
  temp_cat$time<-((temp_cat$Year-minyr)+1)

  temp_ind <- data.frame("Year" = minyr:maxyr)

  if ((sum(minyr:maxyr %in% unique(temp_cat$Year)) != length(minyr:maxyr))) {
    warnings_list <- c(warnings_list,
                       list(paste0(category, ' with chained: ', warnings())))
  }

  #Chain
  a <- tornc(
    dat = temp_cat,
    Year = "Year",
    pvar = "p",
    vvar = "v",
    prodID = "prod",
    baseyr = baseyr)


  names(a)[names(a) %in% "CPI"] <- "PI_C"
  names(a)[names(a) %in% "BPI"] <- "PI_CB"

  temp_ind <- merge.data.frame(x = a, y = temp_ind, by = "Year")

  #Base
  a <- tornb(
    dat = temp_cat,
    Year = "Year",
    pvar = "p",
    vvar = "v",
    prodID = "prod",
    baseyr = baseyr)


  names(a)[names(a) %in% "BPI"] <- "PI_B"

  temp_ind <- merge.data.frame(x = temp_ind, y = a, by = "Year")

  temp_ind$tyear<-((temp_ind$Year-minyr)+1)

  temp_cat0 <- stats::aggregate.data.frame(
    x = temp_cat[, c("v", "q")],
    by = list("Year" = temp_cat$Year),
    FUN = sum,
    na.rm = T)

  temp_cat0$p <- temp_cat0$v / temp_cat0$q

  temp_ind<-dplyr::inner_join(temp_cat0,
                              temp_ind,
                              by="Year")

  #Implicit Q
  temp_ind$Q_CB <- temp_ind$v / temp_ind$PI_CB
  temp_ind$Q_B <- temp_ind$v / temp_ind$PI_B
  temp_ind$Q_C <- temp_ind$v / temp_ind$PI_C

  #Quantity Index
  temp_ind$QI_CB <- temp_ind$Q_CB / temp_ind$Q_CB[temp_ind$Year %in% baseyr]
  temp_ind$QI_B <- temp_ind$Q_B / temp_ind$Q_B[temp_ind$Year %in% baseyr]
  temp_ind$QI_C <- temp_ind$Q_C / temp_ind$Q_C[temp_ind$Year %in% baseyr]

  temp_ind <- cbind.data.frame(cat = category,
                               cat0 = ii,
                               temp_ind)

  return(list(
    "Index" = temp_ind,
    "Species Level" = temp_cat,
    "warnings_list" = warnings_list
  ))
}

#' Price Method
#'
#' This function calculates the Implicit Quanity Output at Fishery Level by systematically runing the Price Method Productivity Output analysis for all species of each cateorgy.
#' @param dat00 Dataset.
#' @param baseyr Numeric year (YYYY). The base year you are assessing the anaylsis with. Typically this is the earliest year in the data set, but it can be any year you choose.
#' @param title0 Title of analysis
#' @param place Area you are assessing the analysis for. This can also be used as a title.
#' @param category0 A character string. The column where the category is defined. A character string.
#' @export
PriceMethodOutput <- function(dat00,
                              baseyr,
                              title0 = "",
                              place = "",
                              category0) {

 val <- NULL #satisfy CRAN

  dat00<-data.frame(stats::na.omit(object = dat00))
  maxyr <- max(dat00$Year)
  minyr <- min(dat00$Year)

  warnings_list <- figures_list <- list()
  spp.level <- index.data <- data.frame()

  category_name <- sort(unique(dat00[, category0]))

  for (ii in 1:length(category_name)) {

    category <- category_name[ii]

    temp00 <- PriceMethodOutput_Category(
      dat00 = dat00,
      ii = ii,
      category = category,
      category0 = category0,
      baseyr = baseyr,
      maxyr = maxyr,
      minyr = minyr,
      warnings_list = warnings_list)

    index.data <- rbind.data.frame(index.data,
                                   temp00$Index)

    spp.level <- rbind.data.frame(spp.level,
                                  temp00$`Species Level`)

    warnings_list <- c(warnings_list, unique(temp00$warnings_list))

  }

  #Whole fishery

  temp_ind0 <- data.frame(matrix(
    data = NA,
    nrow = length(minyr:maxyr),
    ncol = ncol(index.data)
  ))

  names(temp_ind0) <- names(index.data)
  temp_ind0$Year <- minyr:maxyr
  temp_ind0$time <- ((minyr:maxyr) - (minyr - 1))
  temp_ind0$cat <- "Total"
  temp_ind0$cat0 <- 0

  temp_ind00 <- stats::aggregate.data.frame(
    x = index.data[, c("v", "q")],
    by = list("Year" = index.data$Year),
    FUN = sum,
    na.rm = T
  )

  temp_ind00$p <- temp_ind00$v / temp_ind00$q

  temp_ind0$v <- temp_ind00$v
  temp_ind0$q <- temp_ind00$q
  temp_ind0$p0 <- temp_ind00$p

  temp_ind0 <- temp_ind0[order(temp_ind0$Year, decreasing = F), ]
  index.data <- index.data[order(index.data$Year, decreasing = F), ]

  # temp_ind0$PI_B_cran<-priceIndex(index.data,
  #                                     pvar='PI_B_cran',
  #                                     qvar='q', # this might just need to be "Q_B_cran"
  #                                     pervar='time',
  #                                     prodID = 'cat',
  #                                     sample='matched',
  #                                     output='fixedBase',
  #                                     indexMethod='Tornqvist')  #This is a fixed base index
  #
  #
  # temp_ind0$PI_C_cran<-priceIndex(index.data,
  #                                        pvar='PI_C_cran',
  #                                        qvar='q', # this might just need to be "Q_C"
  #                                        pervar='time',
  #                                        prodID = 'cat',
  #                                        sample='matched',
  #                                        indexMethod='Tornqvist',
  #                                        output='chained')      #This is a chain Index


  names(index.data)[names(index.data) %in% "p"] <- "p0"
  a <- tornc(
    dat = index.data,
    Year = "Year",
    pvar = "PI_C",
    vvar = "v",
    prodID = "cat",
    baseyr = baseyr
  )

  temp_ind0$PI_C <- a$CPI
  temp_ind0$PI_CB <- a$BPI

  a <- tornb(
    dat = index.data,
    Year = "Year",
    pvar = "PI_B",
    vvar = "v",
    prodID = "cat",
    baseyr = baseyr
  )

  temp_ind0$PI_B <- a$BPI
  temp_ind0$p <- NULL

  #Implicit Q
  temp_ind0$Q_CB <- temp_ind0$v / temp_ind0$PI_CB
  temp_ind0$Q_B <- temp_ind0$v / temp_ind0$PI_B
  temp_ind0$Q_C <- temp_ind0$v / temp_ind0$PI_C
  # temp_ind0$Q_B_cran<-temp_ind0$v*temp_ind0$PI_B_cran
  # temp_ind0$Q_C_cran<-temp_ind0$v*temp_ind0$PI_C_cran

  #Quantity Index
  temp_ind0$QI_CB <- temp_ind0$Q_CB / temp_ind0$Q_CB[temp_ind0$Year %in% baseyr]
  temp_ind0$QI_B <- temp_ind0$Q_B   / temp_ind0$Q_B[temp_ind0$Year %in% baseyr]
  temp_ind0$QI_C <- temp_ind0$Q_C   / temp_ind0$Q_C[temp_ind0$Year %in% baseyr]
  # temp_ind0$QI_B_cran<-temp_ind0$Q_B_cran/
  #   temp_ind0$Q_B_cran[temp_ind0$Year %in% baseyr]
  # temp_ind0$QI_C_cran<-temp_ind0$Q_C_cran/
  #   temp_ind0$Q_C_cran[temp_ind0$Year %in% baseyr]

  #Combine
  temp_ind0 <- temp_ind0[, match(table = names(temp_ind0),
                                 x = names(index.data))]
  index.data <- rbind.data.frame(index.data, temp_ind0)

  ##########Make plots

  #############Compare Price Indexes for Total and Each Category
  for (i in 1:length(unique(index.data$cat))) {
    title00 <- paste0("_PI_", unique(index.data$cat)[i])

    a0 <- index.data[index.data$cat %in% unique(index.data$cat)[i],
                     c("Year",
                       names(index.data)[grep(pattern = "PI_", x = names(index.data))])]

    a <- tidyr::gather(a0, cat, val,
                       names(index.data)[grep(pattern = "PI_", x = names(index.data))],
                       factor_key = TRUE)

    g <- plotnlines(dat = a, title00, place)

    figures_list[[length(figures_list) + 1]] <- g
    names(figures_list)[length(figures_list)] <- paste0(place,"_",title00)

  }

  #############Compare each type of Price Indexes across Each Category
  for (i in (names(index.data)[grep(pattern = "PI_", x = names(index.data))])) {
    title00 <- paste0("_", i)

    a0 <- index.data[, c("Year", "cat", i)]
    names(a0)[3] <- "val"

    a <- a0

    g <- plotnlines(dat = a, title00, place)

    figures_list[[length(figures_list) + 1]] <- g
    names(figures_list)[length(figures_list)] <- paste0(place,"_",title00, "_PI")

  }

  #############Compare each type of Price Indexes across Each Category
  for (i in c("q", (names(index.data)[grep(pattern = "Q_", x = names(index.data))]))) {
    title00 <- paste0("_", i)

    a0 <- index.data[,
                     c("Year", "cat", i)]
    names(a0)[3] <- "val"

    a <- a0

    g <- plotnlines(dat = a, title00, place)

    figures_list[[length(figures_list) + 1]] <- g
    names(figures_list)[length(figures_list)] <- paste0(place,"_",title00, "_Q")

  }

  #############Plot Category and Total Q
  for (i in c("q", (names(index.data)[grep(pattern = "Q_", x = names(index.data))]))) {
    title00 <- paste0("_", i, "_CatTot")

    a <- index.data[, c("Year", "cat", "q")]
    names(a)[3] <- "val"

    g <- plotnlines(dat = a, title00, place)

    figures_list[[length(figures_list) + 1]] <- g
    names(figures_list)[length(figures_list)] <- paste0(place,"_",title00, "_QCatTot")
  }

  #############Plot Category and Total V
  for (i in c("v", (names(index.data)[grep(pattern = "V_", x = names(index.data))]))) {
    title00 <- paste0("_", i, "_CatTot")

    a <- index.data[, c("Year", "cat", "v")]
    names(a)[3] <- "val"

    g <- plotnlines(dat = a, title00, place)

    figures_list[[length(figures_list) + 1]] <- g
    names(figures_list)[length(figures_list)] <-paste0(place,"_",title00, "_VCatTot")
  }

  #############Save Wrok
  return(
    list(
      "Index" = index.data,
      "Species Level" = spp.level,
      "warnings_list" = warnings_list,
      "figures_list" = figures_list
    )
  )

}


#' How Many Speices are in a Dataset numeric identifier
#'
#' This funciton standardizes the length of the category or species numbers e.g.,(numbers of 33, 440, and 1 are converted to 033, 440, and 001)
#' @param x x is a string of all the numbers you are interested in 'standardizing'.
#' @export
#' @examples
#' numbers0(x = c(1,14,302))
numbers0 <- function(x) {
  xx <- rep_len(x = NA, length.out = length(x))
  for (i in 1:length(x)) {
    xx[i] <- paste0(paste(rep_len(
      x = 0,
      length.out = nchar(max(x)) - nchar(x[i])
    ),
    collapse = ""),
    as.character(x[i]))
  }
  return(xx)
}


#' Counter
#'
#' This funciton advances a value of 'counter0' +1 each time it is used.
#' @param counter0 value to be advanced by 1.
#' @return counter
#' @export
#' @examples counter00X(c(1, 2))
counter00X <- function(counter0) {
  counter0 <- as.numeric(counter0) + 1
  counter <- formatC(x = counter0, width = 3)
  counter <- gsub(counter, pattern = " ", replacement = "0")
  return(counter)
}


#' Standardize Units
#'
#' This funciton standardizes units of a value. For example, 1,000,000 would become "1 Million."
#' @param val Value to be evaluated.
#' @param combine TRUE/FALSE (Default = TRUE). Asks if you want two strings (FALSE) or 1 concatenated string (TRUE).
#' @export
#' @examples xunits(1234567890)
xunits <- function(val, combine = T) {
  val <- sum(as.numeric(val))
  sigfig <- format(val, digits = 3, scientific = TRUE)
  sigfig0 <-
    as.numeric(substr(
      x = sigfig,
      start = (nchar(sigfig) - 1),
      stop = nchar(sigfig)
    ))

  if (sigfig0 <= 5) {
    # if (sigfig0<4) {
    unit <- ""
    x <- format(
      x = val,
      big.mark = ",",
      digits = 0,
      scientific = F
    )
    # } else if (sigfig0>=4 & sigfig0<6) {
    #   unit<-" thousand"
    # x<-round(val/1e3, digits = 1)
    # } else if (sigfig0==5) {
    #   unit<-" thousand"
    #   x<-round(val/1e3, digits = 0)
  } else if (sigfig0 >= 6 & sigfig0 < 9) {
    unit <- " million"
    x <- round(val / 1e6, digits = 1)
  } else if (sigfig0 >= 9 & sigfig0 < 12) {
    unit <- " billion"
    x <- round(val / 1e9, digits = 1)
  } else if (sigfig0 >= 12) {
    unit <- " trillion"
    x <- round(val / 1e12, digits = 1)
  }

  out <- ifelse(combine == T, paste0(x, unit), list(x, unit))

  return(out)
}

#' Plot n lines in ggplot
#'
#' This funciton plots n lines in a ggplot.
#' @param dat Default data.
#' @param titleyaxis y-axis title.
#' @param title0 Title of plot.
#' @export
#' @examples
#' dat<-data.frame(Year = c(2016:2020, 2016:2020),
#'                 val = rnorm(n = 10, mean = 500, sd = 100),
#'                 cat = c(rep_len("A", 5), rep_len("B", 5)))
#' plotnlines(dat = dat,
#'            titleyaxis = "Normal Distribution of 10 Numbers",
#'            title0 = "Anywhere")
plotnlines <- function(dat,
                       titleyaxis = "",
                       title0 = "") {

  Year <- val <- NULL #satisfy CRAN

  xnames <- as.numeric(paste0(dat$Year))
  xnames[!(xnames %in% seq(
    from = min((as.numeric(xnames))),
    to = max(as.numeric(xnames)),
    by = 10
  ))] <- ""

  dat$val <- as.numeric(as.character(dat$val))
  dat$val[(is.infinite(dat$val))] <- NA
  divideby <-
    paste0("(", strsplit(x = xunits(mean(dat$val, na.rm = T)), split = " ")[[1]][2], "s)")
  if (divideby %in% "(trillions)") {
    divideby0 <- 1e12
  } else if (divideby %in% "(billions)") {
    divideby0 <- 1e9
  } else if (divideby %in% "(millions)") {
    divideby0 <- 1e6
  } else if (divideby %in% "(thousands)") {
    divideby0 <- 1e3
  } else if (divideby %in% "(NAs)") {
    divideby0 <- 1
    divideby <- ""
  }

  dat$val <- dat$val / divideby0
  # ynames<-as.numeric(paste0(val))

  g <-
    ggplot2::ggplot(data = dat, ggplot2::aes(
      x = factor(Year),
      y = val,
      color = cat
    )) +
    ggplot2::geom_line(ggplot2::aes(group = cat), size = 3) +
    ggplot2::geom_point() +
    ggplot2::theme(
      # legend.position = c(0.9, 0.2),
      # panel.grid.major.y = ggplot2::element_line(color = NOAALightBlue, size = .1),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      # axis.line = ggplot2::element_line(color = NOAALightBlue, size = .1),
      axis.ticks = ggplot2::element_blank(),
      # remove ticks
      panel.background = ggplot2::element_blank()
    )  +
    ggplot2::ylab(paste0(
      gsub(
        pattern = "_",
        replacement = "",
        x = strsplit(x = titleyaxis, split = "-")[[1]][1]
      ),
      " ",
      divideby
    )) +
    ggplot2::xlab("Year") +
    ggplot2::scale_x_discrete(labels = xnames) +
    # scale_y_discrete(labels= ynames) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::ggtitle(paste0(title0))

  return(g)
}


#' Run Analysis for the US and several regions.
#'
#' @param landings_data Landings data with the following columns: "Year", "Pounds", "Dollars", category0, "Tsn", "State"
#' @param category0 A character string. The column where the category is defined.
#' @param baseyr Numeric year (YYYY). The base year you are assessing the anaylsis with. Typically this is the earliest year in the data set, but it can be any year you choose.
#' @param titleadd A string to add to the file with the outputs to remind you why this particular analysis was interesting.
#' @param dir_analyses A directory that your analyses will be saved to (e.g., "./output/").
#' @param reg_order The US and each region that you would like to assess. Default = c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico").
#' @param reg_order_abbrv Acronym of the US and each region listed in reg_order. Default = c("US", "NP", "Pac", "WP", "NE", "MA", "NorE", "SA", "GOM").
#' @param skipplots TRUE (create and save plots) or don't FALSE.
#' @param save_outputs_to_file TRUE (save outputs from analysis within function) or don't FALSE.
#' @return warnings_list, editeddata_list, index_list, spp_list, figures_list, gridfigures_list
#' @export
#' @examples
#' browseVignettes("FishEconProdOutput")
OutputAnalysis<-function(landings_data,
                         category0,
                         baseyr,
                         titleadd,
                         dir_analyses,
                         reg_order = c("National", "North Pacific", "Pacific",
                                       "Western Pacific (Hawai`i)", "New England",
                                       "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico"),
                         reg_order_abbrv = c("US", "NP", "Pac", "WP", "NE", "MA", "NorE", "SA", "GOM"),
                         skipplots = FALSE,
                         save_outputs_to_file = TRUE) {

  if (save_outputs_to_file) {
    dir_analyses1<-paste0(dir_analyses, "/",titleadd, "_", #analysisby, "_",
                          gsub(pattern = "\\.", replacement = "", x = category0), "/")
    dir.create(dir_analyses1)
    # dir_reports<-paste0(dir_analyses1, "/reports/")
    dir.create(paste0(dir_analyses1, "/reports/"))
    dir_figures<-paste0(dir_analyses1, "/figures/")
    dir.create(paste0(dir_analyses1, "/figures/"))
    dir_outputtables<-paste0(dir_analyses1, "/outputtables/")
    dir.create(paste0(dir_analyses1, "/outputtables/"))
  }

  #Save Stuff
  editeddata_list <- index_list <- spp_list <- finaltable_list <- warnings_list <- figures_list<-list()
  counter<-0
  for (r in 1:length(reg_order)) {

    if (reg_order[r] == "Northeast") {
      landings_data$Region[landings_data$Region %in% c("Mid-Atlantic", "New England")]<-"Northeast"
      landings_data$abbvreg[landings_data$Region %in% c("Mid-Atlantic", "New England")]<-"NorE"
    }

    place<-reg_order[r]
    print(place)
    counter<-counter00X(counter)

    title000<-paste0("_","byr",baseyr)
    title0<-paste0(counter, "_", gsub(pattern = "\\(", replacement = "", x =
                                        gsub(pattern = ")", replacement = "", x =
                                               gsub(pattern = "`", replacement = "", x =
                                                      gsub(reg_order_abbrv[r], pattern = " ",
                                                           replacement = "")))),
                   title000, "_", titleadd)


    idx<-c(1:nrow(landings_data))
    if (reg_order[r] != "National") {
      idx<-which(landings_data$State %in% landings_data$State[landings_data$Region %in% place])
    }

    temp_orig<-landings_data[idx,
                             c(category0, "Year", "Pounds", "Dollars", "Tsn")]

    temp00<-PriceMethodOutput(dat00 = temp_orig,
                              baseyr = baseyr,
                              title0 = title0,
                              place = place,
                              category0 = category0)

    warnings_list<-c(warnings_list, temp00$warnings_list)
    figures_list<-c(figures_list, temp00$figures_list)

    # Obtain the implicit quantity estimates

    # EditedData
    editeddata_list[[r]]<-temp_orig
    names(editeddata_list)[r]<-place
    if (save_outputs_to_file) {
      utils::write.csv(x = editeddata_list[[r]],
                       file = paste0(dir_outputtables, title0,"_EditedData.csv"))
    }
    #Raw
    if (save_outputs_to_file) {
      utils::write.csv(x = temp00$Index,
                       file = paste0(dir_outputtables, title0,"_AllData.csv"))
    }
    index_list[[r]]<-temp00$Index
    names(index_list)[r]<-place

    #Raw
    if (save_outputs_to_file) {
      utils::write.csv(x = temp00$`Species Level`,
                       file = paste0(dir_outputtables, title0,"_AllDataSpp.csv"))
    }
    spp_list[[r]]<-temp00$`Species Level`
    names(spp_list)[r]<-place

  }

  ########SPREADSHEETS
  print("Create spreadsheets")

  if (save_outputs_to_file) {

    # utils::write.csv(x = spptable, file = paste0(dir_outputtables, "000_All", title000,"_Species.csv"))

    for (r in 1:length(reg_order)){

      # #Print
      # xlsx::write.xlsx2(x = editeddata_list[[r]],
      #             file = paste0(dir_outputtables, "000_All", title000, "_", titleadd, "_EditedData.xlsx"),
      #             sheetName = reg_order[r],
      #             col.names = T, row.names = T, append = T)

      #Review
      xlsx::write.xlsx2(x = index_list[[r]],
                        file = paste0(dir_outputtables, "000_All", title000, "_", titleadd, "_AllData.xlsx"),
                        sheetName = reg_order[r],
                        col.names = T, row.names = T, append = T)

      # #All Data
      # xlsx::write.xlsx2(x = spp_list[[r]],
      #             file = paste0(dir_outputtables, "000_All", title000, "_", titleadd, "_AllDataSpp.xlsx"),
      #             sheetName = reg_order[r],
      #             col.names = T, row.names = T, append = T)

    }
  }
  ######PLOTS

  print("Create plots")

  #Side by Side graphs
  figs<-unique(paste0(lapply(X = strsplit(x = names(figures_list),
                                          split = "__"),
                             function(x) x[2])))
  gridfigures_list<-list()

  for (i in 1:length(figs)){

    a<-strsplit(x = names(figures_list)[i],
                split = "_")[[1]][length(strsplit(x = names(figures_list)[i], split = "_")[[1]])]

    fig<-figs[i]
    list0<-figures_list[grep(pattern = fig, x = names(figures_list))]

    g<-ggpubr::ggarrange(plotlist = list0,
                         nrow=3, ncol = 3)
    if (save_outputs_to_file) {
      dir.create(paste0(dir_figures, "/", a, "/"))
      ggplot2::ggsave(filename = paste0(dir_figures, "/", a, "/", "000_All_byr",baseyr,
                                        "_",gsub(pattern = "\\.", replacement = "", x = category0), fig, ".png"),
                      plot = g,
                      width = 11, height = 8.5)
    }
    gridfigures_list<-c(gridfigures_list, list(g))
    names(gridfigures_list)[length(gridfigures_list)]<-paste0("000_All_byr",baseyr,
                                                              "_",gsub(pattern = "\\.", replacement = "", x = category0), fig)
  }

  if (save_outputs_to_file) {

    # save(gridfigures_list,
    #      file = paste0(dir_figures, "AllFiguresGrid.rdata"))

    #make single plots
    for (i in 1:length(figures_list)) {

      a<-strsplit(x = names(figures_list)[i], split = "_")[[1]][length(strsplit(x = names(figures_list)[i], split = "_")[[1]])]
      dir.create(paste0(dir_figures, "/", a, "/"))

      ggplot2::ggsave(filename = paste0(dir_figures, "/", a, "/", names(figures_list)[i], ".png"),
                      plot = figures_list[[i]],
                      width = 11, height = 8.5)
    }
  }

  out<-list("warnings_list" = warnings_list,
            "editeddata_list" = editeddata_list,
            "index_list" = index_list,
            "spp_list" = spp_list,
            "figures_list" = figures_list,
            "gridfigures_list" = gridfigures_list)

  if (save_outputs_to_file) {
    save(warnings_list,
         file = paste0(dir_analyses, "/OutputWarnings.rdata"))

    save(editeddata_list, index_list, spp_list,
         file = paste0(dir_analyses, "/OutputTables.rdata"))

    save(figures_list, gridfigures_list,
         file = paste0(dir_analyses, "/OutputFigures.rdata"))

  }

  return(out)

}



#' Reclassify ITIS species based off a list of higher taxonomic groupings
#'
#' @param tsn A vector of Taxonomic Serial Numbers to be evaluated.
#' @param categories A list of the categories and associated TSN values. within a list of a category, a minus (-) in front of a number is short hand to remove organisms within that tsn's taxonomy from being listed in a category. See the example for an instance where that makes sense.
#' @param uncategorized_name A string of what to call the missing value.
#' @return df_out, tsn_indata
#' @export
#' @examples
#' itis_reclassify(tsn = c(83677, # subphylum Crustacea; shellfish
#'                         172746, # Scophthalmus aquosus; finfish
#'                         173747, # class Reptilia; uncategorized as part of tetrapoda
#'                         98678), # Cancer borealis; shellfish
#'                 categories = list('Finfish' = c(914179, #  Infraphylum	Gnathostomata
#'                                                -914181), # Tetrapoda; - = do NOT include
#'                                   "Shellfish" = c(82696, # Phylum	Arthropoda
#'                                                   69458)), # Phylum	Mollusca
#'                 uncategorized_name = "uncategorized")
itis_reclassify<-function(tsn,
                          categories,
                          uncategorized_name = "Uncategorized"){

  tsn0 <- as.numeric(tsn)
  tsn0<-tsn0[!(is.na(tsn0))]
  tsn0<-tsn0[!(tsn0 %in% 0)]
  tsn_indata <- taxize::classification(sci_id = tsn0, db = "itis")
  tsn_indata <- tsn_indata[!(names(tsn_indata) %in% 0)]
  tsn_indata <- tsn_indata[!(is.na(names(tsn_indata)))]
  valid0 <- sciname <- category0 <- bottomrank <- sppname <- TSN <- c()
  for (i in 1:length(categories)) {
    keep <- c()
    remove <- c()
    for (iii in 1:length(categories[i][[1]])) {
      if (categories[i][[1]][iii] > 0) {
        keep <- c(keep, rlist::list.search(lapply(X = tsn_indata,
                                                  "[", 3), categories[i][[1]][iii] %in%
                                             .))
      }
      else {
        remove <- c(remove, rlist::list.search(lapply(X = tsn_indata,
                                                      "[", 3), abs(categories[i][[1]][iii]) %in%
                                                 .))
      }
    }
    a <- keep[setdiff(names(keep), names(remove))]
    if (length(a) != 0) {
      sppcode <- names(a)
      sppcode <- gsub(pattern = "[a-zA-Z]+", replacement = "",
                      x = sppcode)
      sppcode <- gsub(pattern = "\\.", replacement = "",
                      x = sppcode)
      for (ii in 1:length(sppcode)) {
        TSN <- c(TSN, as.numeric(sppcode[ii]))
        bottomrank <- c(bottomrank, tsn_indata[names(tsn_indata) %in%
                                                 sppcode[ii]][[1]]$rank[nrow(tsn_indata[names(tsn_indata) %in%
                                                                                          sppcode[ii]][[1]])])
        category0 <- c(category0, names(categories[i]))
        sciname <- c(sciname, tsn_indata[names(tsn_indata) %in%
                                           sppcode[ii]][[1]]$name[nrow(tsn_indata[names(tsn_indata) %in%
                                                                                    sppcode[ii]][[1]])])
        valid0 <- c(valid0, ifelse(nrow(tsn_indata[names(tsn_indata) %in%
                                                     sppcode[ii]][[1]]) > 1, "valid", "invalid"))
      }
    }
  }
  sppcode <- setdiff(tsn0, TSN)
  if (!(length(sppcode) %in% 0)) { # if there are things that would be uncategorized... do the below
    tsn_indata <- taxize::classification(sci_id = sppcode, db = "itis")
    TSN <- c(TSN, sppcode)
    for (ii in 1:length(sppcode)) {
      bottomrank <- c(bottomrank, tsn_indata[names(tsn_indata) %in%
                                               sppcode[ii]][[1]]$rank[nrow(tsn_indata[names(tsn_indata) %in%
                                                                                        sppcode[ii]][[1]])])
      category0 <- c(category0, uncategorized_name)
      sciname <- c(sciname, tsn_indata[names(tsn_indata) %in%
                                         sppcode[ii]][[1]]$name[nrow(tsn_indata[names(tsn_indata) %in%
                                                                                  sppcode[ii]][[1]])])
      valid0 <- c(valid0, ifelse(nrow(tsn_indata[names(tsn_indata) %in%
                                                   sppcode[ii]][[1]]) > 1, "valid", "invalid"))
    }
  }
  df_out <- data.frame(TSN = TSN, category = category0, valid = valid0,
                       rank = bottomrank, sciname = sciname)
  return(list(df_out = df_out,
              tsn_indata = tsn_indata))
}


#' Modified Landings Data
#'
#' Modified and cleaned data from NOAA Fisheries Office of Science and Technology’s Fisheries Statistics Division’s Commercial Landings Query, Available at: https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200:::::: for all coastal states combined with state and regional data.
#'
#' @docType data
#'
#' @usage data(land)
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{Year}{four-digit year}
#'   \item{Pounds}{weight of fish caught, in pounds}
#'   \item{Dollars}{value of fish caught, in USD}
#'   \item{category}{category of organism. For our analysis, we aggregated landings and revenue data into two different fisheries: finfish (defined by all organisms in the infraphylum Gnathostomata) and shellfish (defined by all organisms in the phyla Arthropoda and Mollusca)}
#'   \item{Tsn}{Taxonomic Serial Number (TSN) as defined by the Integrated Taxonomic Information System  Distinguishing species fishery categories was done easily with the R package ‘taxize'}
#'   \item{State}{The state the fish was caught in, in full name}
#'   \item{Region}{The region the fish was caught in, in full name}
#'   \item{abbvreg}{The region the fish was caught in, abbrevated}
#' }
#'
#' @keywords datasets
#'
#' @source \href{https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::::}{NOAA Fisheries FOSS}
#'
#' @examples
#' data(land)
"land"



## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
