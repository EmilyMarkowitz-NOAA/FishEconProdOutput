



#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @keywords cats
#' @export
#' @examples
#' funct_list()
funct_list <- function(x) {
  x <- x[which(x != "")]
  # x<-x[which(!is.null(x))]
  x <- x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x) == 2) {
    str1 <- paste(x, collapse = " and ")
  } else if (length(x) > 2) {
    str1 <- paste(x[1:(length(x) - 1)], collapse = ", ")
    str1 <- paste0(str1, ", and ", x[length(x)])
  } else {
    str1 <- x
  }
  return(str1)
}


#'Replace the first value, if missing, with the next nearest value.
#'
#' If the first value of the timeseries of this column (c) is 0/NaN/NA. Change the first value (and subsequent 0/NaN/NA values) to the first available non-0/NaN/NA value. Then, used in before with 'ReplaceMid'.
#' @param colnames Names of columns to apply this action to.
#' @param temp Name of dataset to apply this action to.
#' @keywords Replace, First
#' @export
#' @examples
#' ReplaceFirst()
ReplaceFirst <- function(colnames, temp) {
  for (c0 in 1:length(colnames)) {
    if (temp[1, colnames[c0]] %in% c(0, NA, NaN, NULL)) {
      findfirstvalue <-
        temp[which(!(temp[, colnames[c0]]  %in% c(0, NA, NaN, NULL))),
             colnames[c0]][1]
      temp[1, colnames[c0]] <- findfirstvalue
    }
  }
  return(temp)
}


#'Replace the first value, if missing, with the next nearest value.
#'
#' If a middle value of the timeseries of this column (c) is 0/NaN/NA. Change the currently 0/NaN/NA value to the previous available non-0/NaN/NA value. Then, used after with 'ReplaceFirst'.
#' @param colnames Names of columns to apply this action to.
#' @param temp Name of dataset to apply this action to.
#' @keywords Replace, Mid, Middle
#' @export
#' @examples
#' ReplaceMid()
ReplaceMid <- function(colnames, temp) {
  for (c0 in 1:length(colnames)) {
    #If a middle value of the timeseries of this column (c) is 0/NaN/NA
    #Change the currently 0/NaN/NA value to the previous available non-0/NaN/NA value
    if (sum(temp[, colnames[c0]] %in% c(0, NA, NaN, NULL)) > 0) {
      troublenumber <- which(temp[, colnames[c0]] %in% c(0, NA, NaN, NULL))
      for (r in 1:length(troublenumber)) {
        findlastvalue <- temp[troublenumber[r] - 1, colnames[c0]][1]
        temp[troublenumber[r], colnames[c0]] <- findlastvalue
      }
    }
  }
  return(temp)
}


#' Tornqvist Price Index Base Year Function
#'
#' Tornqvist Price Index Base Year Function
#' @param data1 The dataset you would like to use.
#' @param Year Name of the column holding year data.
#' @param pvar Name of the column holding price data.
#' @param vvar Name of the column holding value data.
#' @param prodID Name of the column holding prodID data.
#' @param base.year The year dollar values need to be in.
#' @export
#' @examples
#' tornb()
tornb <- function(dat,
                  Year,
                  pvar,
                  vvar,
                  prodID,
                  base.year) {

  data1<-dat
  data1<-data1[order(data1$Year), ]

  names(data1)[names(data1) %in% Year] <- "Year"
  names(data1)[names(data1) %in% pvar] <- "p"
  names(data1)[names(data1) %in% vvar] <- "v"
  names(data1)[names(data1) %in% prodID] <- "prod"
  data1 <- data1[, c("Year", "p", "v", "prod")]
  # tornc<-function(data1,base.year){
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
    year1_2 <- na.omit(year1_2)  #Any rows with "NA" values are deleted.
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
#' @param data1 The dataset you would like to use.
#' @param Year Name of the column holding year data.
#' @param pvar Name of the column holding price data.
#' @param vvar Name of the column holding value data.
#' @param prodID Name of the column holding prodID data.
#' @param base.year The year dollar values need to be in.
#' @export
#' @examples
#' tornc()
tornc <- function(dat,
                  Year,
                  pvar,
                  vvar,
                  prodID,
                  base.year) {

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
    year1_2<-na.omit(year1_2)  #Any rows with "NA" values are deleted.
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

  baseval=CPI[CPI$Year == base.year,2]
  CPI$BPI=CPI$CPI/baseval


  return(CPI)
}

#' Price Methods - Category Level
#'
#' This function systematically runs the Price Method Productivity Output analysis for all species of a cateorgy.
#' @param temp Default dataset.
#' @param ii Category number.
#' @param category A character string. A unique string from the 'category0' column of the group being evaluated.
#' @param category0 A character string. The column where the category is defined.
#' @param baseyr Numeric year (YYYY). The base year you are assessing the anaylsis with. Typically this is the earliest year in the data set, but it can be any year you choose.
#' @param maxyr The maxium year to assess in the dataset.
#' @param minyr The minium year to assess in the dataset.
#' @param warnings_list A list where warnings are stored. If using this function in the PriceMethodOutput it will be inherited. If using outside of that function, put ls().
#' @export
#' @examples
#' PriceMethodOutput_Category()
PriceMethodOutput_Category <- function(temp,
                                       ii,
                                       category,
                                       category0,
                                       baseyr,
                                       maxyr,
                                       minyr,
                                       warnings_list = ls()) {

  temp.cat <- temp[temp[, category0] %in% category,]

  temp.cat<-dplyr::rename(temp.cat,
                          q = Pounds,
                          v = Dollars)

  temp.cat <- subset(temp.cat, p > 0 &  v > 0) # & q > 0)  #only keep observations with positive p,v,q

  temp.cat <-
    aggregate.data.frame(
      x = temp.cat[, c("q", "v")],
      by = list("Year" = temp.cat$Year,
                "prod" = temp.cat$Tsn,
                "cat" = temp.cat[, category0]),
      FUN = sum,
      na.rm = T
    )

  # temp.cat<-temp.cat[order(temp.cat$Year),]

  temp.cat$p <- temp.cat$v / temp.cat$q

  if (sum(minyr:maxyr %in% unique(temp.cat$Year)) != length(minyr:maxyr)) {
    temp0 <- data.frame(matrix(
      data = NA,
      nrow = length(setdiff(
        x = minyr:maxyr, y = unique(temp.cat$Year)
      )),
      ncol = ncol(temp.cat)
    ))
    names(temp0) <- names(temp.cat)
    temp0$Year <- setdiff(x = minyr:maxyr, y = unique(temp.cat$Year))
    temp0$cat <- category
    # temp0$time <- setdiff(x = ((minyr:maxyr) - (minyr - 1)), y = unique(temp.cat$time))
    temp0$prod <- 0

    temp.cat <- rbind.data.frame(temp.cat, temp0)
    temp.cat <- temp.cat[order(temp.cat$Year), ]

    # #imputed
    # temp.cat <-
    #   ReplaceFirst(colnames = c("p", "q", "v"), temp = temp.cat)
    # temp.cat <-
    #   ReplaceMid(colnames = c("p", "q", "v"), temp = temp.cat)

    warnings_list <-
      c(warnings_list, list(
        paste0(
          'Warning: ',
          category,
          ': Error in priceIndex(temp.cat, pvar = "p", qvar = "q", pervar = "time",  : The time period variable is not continuous. '
        )
      ))
  }

  temp.cat <- temp.cat[order(temp.cat$Year), ]
  temp.cat$time<-((temp.cat$Year-minyr)+1)

  temp.ind <- data.frame("Year" = minyr:maxyr)

  if ((sum(minyr:maxyr %in% unique(temp.cat$Year)) != length(minyr:maxyr))) {
    warnings_list <- c(warnings_list,
                       list(paste0(category, ' with chained: ', warnings())))
  }

  #Chain
  a <- tornc(
    dat = temp.cat,
    Year = "Year",
    pvar = "p",
    vvar = "v",
    prodID = "prod",
    base.year = baseyr)


  names(a)[names(a) %in% "CPI"] <- "PI_C"
  names(a)[names(a) %in% "BPI"] <- "PI_CB"

  temp.ind <- merge.data.frame(x = a, y = temp.ind, by = "Year")

  #Base
  a <- tornb(
    dat = temp.cat,
    Year = "Year",
    pvar = "p",
    vvar = "v",
    prodID = "prod",
    base.year = baseyr)


  names(a)[names(a) %in% "BPI"] <- "PI_B"

  temp.ind <- merge.data.frame(x = temp.ind, y = a, by = "Year")

  temp.ind$tyear<-((temp.ind$Year-minyr)+1)

  temp.cat0 <- aggregate.data.frame(
    x = temp.cat[, c("v", "q")],
    by = list("Year" = temp.cat$Year),
    FUN = sum,
    na.rm = T)

  temp.cat0$p <- temp.cat0$v / temp.cat0$q

  temp.ind<-join(temp.cat0,
                 temp.ind,by="Year",
                 type="inner")

  #Implicit Q
  temp.ind$Q_CB <- temp.ind$v / temp.ind$PI_CB
  temp.ind$Q_B <- temp.ind$v / temp.ind$PI_B
  temp.ind$Q_C <- temp.ind$v / temp.ind$PI_C

  #Quantity Index
  temp.ind$QI_CB <- temp.ind$Q_CB / temp.ind$Q_CB[temp.ind$Year %in% baseyr]
  temp.ind$QI_B <- temp.ind$Q_B / temp.ind$Q_B[temp.ind$Year %in% baseyr]
  temp.ind$QI_C <- temp.ind$Q_C / temp.ind$Q_C[temp.ind$Year %in% baseyr]

  temp.ind <- cbind.data.frame(cat = category,
                               cat0 = ii,
                               temp.ind)

  return(list(
    "Index" = temp.ind,
    "Species Level" = temp.cat,
    "warnings_list" = warnings_list
  ))
}

#' Price Method
#'
#' This function calculates the Implicit Quanity Output at Fishery Level by systematically runing the Price Method Productivity Output analysis for all species of each cateorgy.
#' @param temp Default dataset.
#' @param baseyr Numeric year (YYYY). The base year you are assessing the anaylsis with. Typically this is the earliest year in the data set, but it can be any year you choose.
#' @param title0 Title of analysis
#' @param place Area you are assessing the analysis for. This can also be used as a title.
#' @param category0 A character string. The column where the category is defined. A character string.
#' @export
#' @examples
#' PriceMethodOutput()
PriceMethodOutput <-function(temp,
                             baseyr,
                             title0 = "",
                             place = "",
                             category0) {

  temp<-data.frame(na.omit(temp))
  maxyr <- max(temp$Year)
  minyr <- min(temp$Year)

  warnings_list <- figures_list <- list()
  spp.level <- index.data <- data.frame()

  category_name <- sort(unique(temp[, category0]))

  for (ii in 1:length(category_name)) {

    category <- category_name[ii]

    temp00 <- PriceMethodOutput_Category(
      temp = temp,
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

  temp.ind0 <- data.frame(matrix(
    data = NA,
    nrow = length(minyr:maxyr),
    ncol = ncol(index.data)
  ))

  names(temp.ind0) <- names(index.data)
  temp.ind0$Year <- minyr:maxyr
  temp.ind0$time <- ((minyr:maxyr) - (minyr - 1))
  temp.ind0$cat <- "Total"
  temp.ind0$cat0 <- 0

  temp.ind00 <- aggregate.data.frame(
    x = index.data[, c("v", "q")],
    by = list("Year" = index.data$Year),
    FUN = sum,
    na.rm = T
  )

  temp.ind00$p <- temp.ind00$v / temp.ind00$q

  temp.ind0$v <- temp.ind00$v
  temp.ind0$q <- temp.ind00$q
  temp.ind0$p0 <- temp.ind00$p

  temp.ind0 <- temp.ind0[order(temp.ind0$Year, decreasing = F), ]
  index.data <- index.data[order(index.data$Year, decreasing = F), ]

  # temp.ind0$PI_B_cran<-priceIndex(index.data,
  #                                     pvar='PI_B_cran',
  #                                     qvar='q', # this might just need to be "Q_B_cran"
  #                                     pervar='time',
  #                                     prodID = 'cat',
  #                                     sample='matched',
  #                                     output='fixedBase',
  #                                     indexMethod='Tornqvist')  #This is a fixed base index
  #
  #
  # temp.ind0$PI_C_cran<-priceIndex(index.data,
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
    base.year = baseyr
  )

  temp.ind0$PI_C <- a$CPI
  temp.ind0$PI_CB <- a$BPI

  a <- tornb(
    dat = index.data,
    Year = "Year",
    pvar = "PI_B",
    vvar = "v",
    prodID = "cat",
    base.year = baseyr
  )

  temp.ind0$PI_B <- a$BPI
  temp.ind0$p <- NULL

  #Implicit Q
  temp.ind0$Q_CB <- temp.ind0$v / temp.ind0$PI_CB
  temp.ind0$Q_B <- temp.ind0$v / temp.ind0$PI_B
  temp.ind0$Q_C <- temp.ind0$v / temp.ind0$PI_C
  # temp.ind0$Q_B_cran<-temp.ind0$v*temp.ind0$PI_B_cran
  # temp.ind0$Q_C_cran<-temp.ind0$v*temp.ind0$PI_C_cran

  #Quantity Index
  temp.ind0$QI_CB <- temp.ind0$Q_CB / temp.ind0$Q_CB[temp.ind0$Year %in% baseyr]
  temp.ind0$QI_B <- temp.ind0$Q_B   / temp.ind0$Q_B[temp.ind0$Year %in% baseyr]
  temp.ind0$QI_C <- temp.ind0$Q_C   / temp.ind0$Q_C[temp.ind0$Year %in% baseyr]
  # temp.ind0$QI_B_cran<-temp.ind0$Q_B_cran/
  #   temp.ind0$Q_B_cran[temp.ind0$Year %in% baseyr]
  # temp.ind0$QI_C_cran<-temp.ind0$Q_C_cran/
  #   temp.ind0$Q_C_cran[temp.ind0$Year %in% baseyr]

  #Combine
  temp.ind0 <- temp.ind0[, match(table = names(temp.ind0),
                                 x = names(index.data))]
  index.data <- rbind.data.frame(index.data, temp.ind0)

  ##########Make plots
  NOAALightBlue <- "#C9E1E6"
  NOAADarkBlue <- "#0098A6"
  NOAADarkGrey <- "#56575A" #text
  NOAABlueScale <- colorRampPalette(colors = c(NOAALightBlue, NOAADarkBlue))

  #############Compare Price Indexes for Total and Each Category
  for (i in 1:length(unique(index.data$cat))) {
    title00 <- paste0("_PI_", unique(index.data$cat)[i])

    a0 <- index.data[index.data$cat %in% unique(index.data$cat)[i],
                     c("Year",
                       names(index.data)[grep(pattern = "PI_", x = names(index.data))])]

    a <- gather(a0, cat, val,
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
#' numbers0()
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
#' @export
#' @examples counter<-0; counter<-funct_counter(counter); counter; [1] "001"
#' funct_counter()
funct_counter <- function(counter0) {
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
#' @examples
#' xunits(1234567890)
#' "1.2 billion"
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

#' Counter
#'
#' This funciton advances a value of 'counter0' +1 each time it is used.
#' @param counter0 value to be advanced by 1.
#' @export
#' @examples counter<-0; counter<-funct_counter(counter); counter; [1] "001"
#' funct_counter()
funct_counter<-function(counter0) {
  counter0<-as.numeric(counter0)+1
  counter<-formatC(x = counter0, width = 3)
  counter<-gsub(counter, pattern = " ", replacement = "0")
  return(counter)
}

#' Plot n lines in ggplot
#'
#' This funciton plots n lines in a ggplot.
#' @param dat Default data.
#' @param titleyaxis y-axis title.
#' @param title0 Title of plot.
#' @export
#' @examples xunits(1234567890); "1.2 billion"
#' plotnlines()
plotnlines <- function(dat, titleyaxis, title0) {
  NOAALightBlue <- "#C9E1E6"
  NOAADarkBlue <- "#0098A6"
  NOAADarkGrey <- "#56575A" #text
  NOAABlueScale <-
    colorRampPalette(colors = c(NOAALightBlue, NOAADarkBlue))

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
    ggplot(data = dat, aes(
      x = factor(Year),
      y = val,
      color = cat
    )) +
    geom_line(aes(group = cat), size = 3) +
    geom_point() +
    theme(
      # legend.position = c(0.9, 0.2),
      panel.grid.major.y = element_line(color = NOAALightBlue, size = .1),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.line = element_line(color = NOAALightBlue, size = .1),
      axis.ticks = element_blank(),
      # remove ticks
      panel.background = element_blank()
    )  +
    ylab(paste0(
      gsub(
        pattern = "_",
        replacement = "",
        x = strsplit(x = titleyaxis, split = "-")[[1]][1]
      ),
      " ",
      divideby
    )) +
    xlab("Year") +
    scale_x_discrete(labels = xnames) +
    # scale_y_discrete(labels= ynames) +
    guides(fill = FALSE) +
    ggtitle(paste0(title0))

  return(g)
}


#' Run Analysis for the US and several regions.
#'
#' @param landings_data Landings data with the following columns: "Year", "Pounds", "Dollars", category0, "Tsn", "State"
#' @param category0 A character string. The column where the category is defined.
#' @param baseyr Numeric year (YYYY). The base year you are assessing the anaylsis with. Typically this is the earliest year in the data set, but it can be any year you choose.
#' @param titleadd A string to add to the file with the outputs to remind you why this particular analysis was interesting.
#' @param dir-analyses A directory that your analyses will be saved to (e.g., "./output/").
#' @param reg_order The US and each region that you would like to assess. Default = c("National", "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "Northeast", "South Atlantic", "Gulf of Mexico").
#' @param reg_order_abbrv Acronym of the US and each region listed in reg_order. Default = c("US", "NP", "Pac", "WP", "NE", "MA", "NorE", "SA", "GOM").
#' @param skipplots TRUE (create and save plots) or don't FALSE.
#'
#' @return
#' @export
#'
#' @examples
#' vignette(FEUS-tables)
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
    counter<-funct_counter(counter)

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

    temp00<-PriceMethodOutput(temp = temp_orig,
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
      write.csv(x = editeddata_list[[r]],
                file = paste0(dir_outputtables, title0,"_EditedData.csv"))
    }
    #Raw
    if (save_outputs_to_file) {
      write.csv(x = temp00$Index,
                file = paste0(dir_outputtables, title0,"_AllData.csv"))
    }
    index_list[[r]]<-temp00$Index
    names(index_list)[r]<-place

    #Raw
    if (save_outputs_to_file) {
      write.csv(x = temp00$`Species Level`,
              file = paste0(dir_outputtables, title0,"_AllDataSpp.csv"))
    }
    spp_list[[r]]<-temp00$`Species Level`
    names(spp_list)[r]<-place

  }

  ########SPREADSHEETS########
  print("Create spreadsheets")

  if (save_outputs_to_file) {

  # write.csv(x = spptable, file = paste0(dir_outputtables, "000_All", title000,"_Species.csv"))

  for (r in 1:length(reg_order)){

    # #Print
    # write.xlsx2(x = editeddata_list[[r]],
    #             file = paste0(dir_outputtables, "000_All", title000, "_", titleadd, "_EditedData.xlsx"),
    #             sheetName = reg_order[r],
    #             col.names = T, row.names = T, append = T)

    #Review
    write.xlsx2(x = index_list[[r]],
                file = paste0(dir_outputtables, "000_All", title000, "_", titleadd, "_AllData.xlsx"),
                sheetName = reg_order[r],
                col.names = T, row.names = T, append = T)

    # #All Data
    # write.xlsx2(x = spp_list[[r]],
    #             file = paste0(dir_outputtables, "000_All", title000, "_", titleadd, "_AllDataSpp.xlsx"),
    #             sheetName = reg_order[r],
    #             col.names = T, row.names = T, append = T)

  }
}
  ######PLOTS##########

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

    g<-ggarrange(plotlist = list0,
                 nrow=3, ncol = 3)
    if (save_outputs_to_file) {
      dir.create(paste0(dir_figures, "/", a, "/"))
      ggsave(filename = paste0(dir_figures, "/", a, "/", "000_All_byr",baseyr,
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

      ggsave(filename = paste0(dir_figures, "/", a, "/", names(figures_list)[i], ".png"),
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



#' Title
#'
#' @param tsn A vector of Taxonomic Serial Numbers to be evaluated.
#' @param categories A list of the categories and associated TSN values.
#' @param missing_name A string of what to call the missing value.
#'
#' @return
#' @export
#'
#' @examples
#'
itis_reclassify<-function(tsn, categories, missing_name){

  # Find which codes are in which categories
  tsn0<-as.numeric(tsn)[!(is.na(tsn))]
  tsn_indata<-classification(sci_id = tsn0, db = 'itis')
  tsn_indata<-tsn_indata[!(names(tsn_indata) %in% 0)]
  valid0<- sciname<-category0<-bottomrank<-sppname<- TSN<-c()

  TSN<-c()
  bottomrank<-c()
  category0<-c()
  sciname<-c()
  valid0<-c()


  for (i in 1:length(categories)) {

    a<-list.search(lapply(X = tsn_indata, '[', 3), categories[i][[1]] %in% . )

    # for (ii in 1:length(categories[i][[1]])) {
    # a<-c(a, list.search(lapply(X = tsn_indata, '[', 3), categories[i][[1]][[ii]] %in% . ))
    # }

    if (length(a)!=0) {

      sppcode<-names(a)
      sppcode<-gsub(pattern = "[a-zA-Z]+", replacement = "", x = sppcode)
      sppcode<-gsub(pattern = "\\.", replacement = "", x = sppcode)

      for (ii in 1:length(sppcode)) {
        TSN<-c(TSN, sppcode[ii])

        bottomrank<-c(bottomrank, tsn_indata[names(tsn_indata) %in% sppcode[ii]][[1]]$rank[
          nrow(tsn_indata[names(tsn_indata) %in% sppcode[ii]][[1]])])

        category0<-c(category0, names(categories[i]))

        sciname<-c(sciname, tsn_indata[names(tsn_indata) %in% sppcode[ii]][[1]]$name[
          nrow(tsn_indata[names(tsn_indata) %in% sppcode[ii]][[1]])])

        valid0<-c(valid0,
                  ifelse(nrow(tsn_indata[names(tsn_indata) %in% sppcode[ii]][[1]])>1,
                         "valid", "invalid"))
      }
    }
  }

  df_out<-data.frame(TSN = TSN,
                     category = category0,
                     valid = valid0,
                     rank = bottomrank,
                     sciname = sciname )

  return(list("df_out" = df_out,
              "tsn_indata" = tsn_indata))
}


