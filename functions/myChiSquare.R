################################################################################
# File:         myChiSquare
#
# Description:  function reads in vector of one or column numbers referring to
#               response variables of interest (_colnums_), a column number for
#               a grouping/treatment of interest (group_col), and a data frame
#               (df).  Produces a dataframe of results to be printed. Also has
#               option to run standard chi-square vs. fisher's exact test
#               (_fisher_), which defaults to FALSE.  If running fisher's
#               exact test, workspace size (_wrksp_) can be changed.
#
# Created by:	  Alex Jakubow
# requires:     index numbers for response and grouping variables; data frame
# provides:     dataframe to be printed as a table
################################################################################


myChiSquare <- function(df, colnums, group_col, fisher = FALSE, wrksp = 2e5) {
  for (i in colnums) {
    #execute crosstab
    print(i)
    xt <- crosstab(df[, i], df[, group_col], plot = FALSE,
                   prop.c = TRUE, chisq = TRUE)
    if (fisher == TRUE) {
      fish <- fisher.test(df[,i], df[, group_col], workspace = wrksp)
    }
    #grab dimensions
    r <- dim(xt$tab)[1]
    c <- dim(xt$tab)[2]
    
    #create inner cells with freq and column proportions
    cells <- paste0(xt$tab, " (", format(round(xt$prop.col*100, 1), nsmall = 1,
                                         trim = TRUE), ")")
    #grab group/treatment names (x) and response labels (y)
    xvar_labs <- names(table(df[,i]))
    yvar_labs <- names(table(df[, group_col]))
    
    #split inner cells into c vectors of length c and create table
    splits <- seq(1, length(cells), r)
    inner_table <- cbind(xvar_labs, cells[splits[1]:splits[2]-1])
    for (j in 2:length(splits)) {
      if (j != length(splits)) {
        inner_table <- cbind(inner_table, cells[splits[j]:(splits[j+1]-1)])
      }
      else {
        inner_table <- cbind(inner_table, cells[splits[j]:length(cells)])
      }
    }
    
    #add totals
    rsums <- rowSums(xt$tab)
    csums <- colSums(xt$tab)
    rtotal <- sapply(rsums, function(x) {
      paste0(x, " (", format(round((x/sum(rsums))*100, 1), nsmall = 1, 
                             trim = TRUE), ")")
    }) 
    ctotal <- sapply(csums, function(x) {
      paste0(x, " (", format(round((x/sum(csums))*100, 1), nsmall = 1, 
                             trim = TRUE), ")")
    }) 
    ctotal <- c("Total", ctotal, sum(csums))
    table_tots <- cbind(inner_table, rtotal)
    table_tots <- rbind(table_tots, ctotal)
    table_tots <- rbind(rep("", ncol(table_tots)), table_tots)
    #add item/question row
    item <- c(questions[i], rep("", nrow(table_tots)-1))
    table_out <- cbind(item, table_tots)
    
    #add significance
    if (fisher == FALSE) {
      table_out[2,1] = paste0("(p-value = ", addStars(xt$CST$p.value), ")")
    }
    if (fisher == TRUE) {
      table_out[2,1] = paste0("(p-value = ", addStars(fish$p.value), ")")
    }
    
    #append
    if (i == 5) {
      master_table <- table_out
    }
    else {
      master_table <- rbind(master_table, table_out)
    }
  }
  #final table
  colnames(master_table) <- c("Item", "Response", yvar_labs, "Total")
  return(as.data.frame(master_table))
}
