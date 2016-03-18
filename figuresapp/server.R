library(shiny)

shinyServer(function(input, output, session) {
  gen.all <- function(x, y, z) {
    if (is.null(z)) {
      setwd(paste('C:', 'Users', Sys.getenv('USERNAME'), 'Documents', sep = '/'))
    } else{
      pathway <- as.character(z)
      setwd(pathway)
    }
    
    
    info <- as.character(x)
    data <- read.csv((info))
    data[data == 0] <- NA
    data[data == 'x'] <- NA
    data[data == 'X'] <- NA
    
    #y-scaling function
    ymax <- function(x) {
      mround <- function(x, base) {
        base * ceiling(x / base)
      }
      if (max(x) > 40) {
        response <- mround(max(x), 10)
      } else{
        if (max(x) > 20) {
          response <- mround(max(x), 5)
        } else{
          if (max(x) > 14) {
            response <- 20
          } else{
            if (max(x) > 10) {
              response <- 14
            } else{
              if (max(x) > 5) {
                response <- 10
              } else{
                response <- 5
              }
            }
          }
        }
      }
      return(response)
    }
    
    n <- function(x) {
      return(length(na.exclude(x)) - 1)
    }
    
    createtitle <- function(x, y, z) {
      w <- as.character(z)
      subsample <- function(x, y) {
        if ((n(x) / y) == 1) {
          response <- 'N'
        }
        else{
          response <- 'n'
        }
        return(response)
      }
      if (length(grep('not applicable', x, ignore.case = TRUE)) > 0) {
        response <-
          paste(w,
                ' (',
                subsample(x, y),
                '=',
                n(x),
                ') (N/As:',
                length(grep('not applicable', x, ignore.case = TRUE)),
                ')',
                sep = '')
      } else{
        response <-
          paste(w, ' (', subsample(x, y), '=', n(x), ')', sep = '')
      }
      return(response)
    }
    
    #function to set up matrices
    creatematrix <- function(x) {
      if (((length(grep(
        'poor', x, ignore.case = TRUE
      ))) + (length(grep(
        'good', x, ignore.case = TRUE
      )))) >= 1) {
        vp <- length(which(x == 'Very Poor'))
        p <- length(which(x == 'Poor'))
        ok1 <- length(which(x == 'Okay'))
        g <- length(which(x == 'Good'))
        vg <- length(which(x == 'Very Good'))
        response <-
          (matrix(c(vp, p, ok1, g, vg), byrow = TRUE, ncol = 5))
      } else{
        if (((length(
          grep('difficult', x, ignore.case = TRUE)
        )) + (length(
          grep('easy', x, ignore.case = TRUE)
        ))) >= 1) {
          vd <- length(which(x == 'Very Difficult'))
          d <- length(which(x == 'Difficult'))
          ok2 <- length(which(x == 'Okay'))
          e <- length(which(x == 'Easy'))
          ve <- length(which(x == 'Very Easy'))
          response <- (matrix(
            c(vd, d, ok2, e, ve),
            byrow = TRUE,
            ncol = 5
          ))
        } else{
          if (((length(
            grep('yes', x, ignore.case = TRUE)
          )) + (length(which(x == 'No')))) >= 1) {
            yes <- length(grep('yes', x, ignore.case = TRUE))
            no <- length(which(x == 'No'))
            response <- (matrix(c(yes, no), byrow = TRUE, ncol = 2))
          } else{
            if (((length(which(x == 1))) + (length(which(x == 2))) + (length(which(x ==
                                                                                   3))) + (length(which(x == 4))) + (length(grep('5 (',x,fixed=TRUE))) + (length(which(x ==
                                                                                                                                                            6))) + (length(which(x == 7))) + (length(which(x == 8))) + (length(which(x ==
                                                                                                                                                                                                                                     9))) + (length(grep('10 (',x,fixed=TRUE))) + (length(
                                                                                                                                                                                                                                       grep('0 (', x, fixed = TRUE)
                                                                                                                                                                                                                                     ))
            ) >= 1) {
              n0 <- length(intersect(grep('0 (',x,fixed=TRUE),grep('10',x,invert=TRUE)))
              n1 <- length(which(x == 1))
              n2 <- length(which(x == 2))
              n3 <- length(which(x == 3))
              n4 <- length(which(x == 4))
              n5 <- length(grep('5 (',x,fixed=TRUE))
              n6 <- length(which(x == 6))
              n7 <- length(which(x == 7))
              n8 <- length(which(x == 8))
              n9 <- length(which(x == 9))
              n10 <- length(grep('10 (',x,fixed=TRUE))
              response <-
                (matrix(
                  c(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9, n10),
                  byrow = TRUE,
                  ncol = 11
                ))
            } else{
              if (length(grep('acceptable', x, ignore.case = TRUE)) >= 1) {
                acc <- length(which(x == 'Acceptable'))
                unacc <- length(which(x == 'Unacceptable'))
                response <- (matrix(
                  c(acc, unacc),
                  byrow = TRUE,
                  ncol = 2
                ))
              } else{
                response <- 'ERROR'
              }
            }
          }
        }
      }
      return(response)
    }
    
    createlabel <- function(x) {
      if (((length(grep(
        'poor', x, ignore.case = TRUE
      ))) + (length(grep(
        'good', x, ignore.case = TRUE
      )))) >= 1) {
        response <-
          c('Very Poor\n(1)',
            'Poor\n(2)',
            'Okay\n(3)',
            'Good\n(4)',
            'Very Good\n(5)')
      } else{
        if (((length(grep(
          'yes', x, ignore.case = TRUE
        ))) + (length(which(x == 'No')))) >= 1) {
          response <- c('Yes', 'No')
        } else{
          if (((length(which(x == 1))) + (length(which(x == 2))) + (length(which(x ==
                                                                                 3))) + (length(which(x == 4))) + (length(grep('5 (',x,fixed=TRUE))) + (length(which(x ==
                                                                                                                                                                     6))) + (length(which(x == 7))) + (length(which(x == 8))) + (length(which(x ==
                                                                                                                                                                                                                                              9))) + (length(grep('10 (',x,fixed=TRUE))) + (length(
                                                                                                                                                                                                                                                grep('0 (', x, fixed = TRUE)
                                                                                                                                                                                                                                              ))>= 1)) {
            response <-
              c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10')
          } else{
            if (((length(
              grep('difficult', x, ignore.case = TRUE)
            )) + (length(
              grep('easy', x, ignore.case = TRUE)
            ))) >= 1) {
              response <-
                c(
                  'Very\nDifficult (1)',
                  'Difficult\n(2)',
                  'Okay\n(3)',
                  'Easy\n(4)',
                  'Very\nEasy (5)'
                )
            } else{
              if (length(grep('acceptable', x, ignore.case = TRUE)) >= 1) {
                response <- c('Acceptable', 'Unacceptable')
              } else{
                response <-
                  'Error: Check data source for errors.  Note: You may receive this error if all values are blank, Not Applicable, or Okay'
              }
            }
          }
        }
      }
      return(response)
    }
    
    createaverage <- function(x) {
      if (((length(grep(
        'poor', x, ignore.case = TRUE
      ))) + (length(grep(
        'good', x, ignore.case = TRUE
      )))) >= 1) {
        response <-
          (((length(
            which(x == 'Very Poor')
          )) + (2 * (
            length(which(x == 'Poor'))
          )) + (3 * (
            length(which(x == 'Okay'))
          )) + (4 * (
            length(which(x == 'Good'))
          )) + (5 * (
            length(which(x == 'Very Good'))
          ))) / ((length(
            which(na.exclude(x != 'Not Applicable'))
          )) - 1))
      } else{
        if (((length(
          grep('difficult', x, ignore.case = TRUE)
        )) + (length(
          grep('easy', x, ignore.case = TRUE)
        ))) >= 1) {
          response <-
            (((length(
              which(x == 'Very Difficult')
            )) + (2 * (
              length(which(x == 'Difficult'))
            )) + (3 * (
              length(which(x == 'Okay'))
            )) + (4 * (
              length(which(x == 'Easy'))
            )) + (5 * (
              length(which(x == 'Very Easy'))
            ))) / ((length(
              which(na.exclude(x != 'Not Applicable'))
            )) - 1))
        } else{
          if (((length(which(x == 1))) + (length(which(x == 2))) + (length(which(x ==
                                                                                 3))) + (length(which(x == 4))) + (length(grep('5 (',x,fixed=TRUE))) + (length(which(x ==
                                                                                                                                                                     6))) + (length(which(x == 7))) + (length(which(x == 8))) + (length(which(x ==
                                                                                                                                                                                                                                              9))) + (length(grep('10 (',x,fixed=TRUE))) + (length(
                                                                                                                                                                                                                                                intersect(grep('0 (',x,fixed=TRUE),grep('10',x,invert=TRUE))
                                                                                                                                                                                                                                              ))                                                                                                                                                                                                                        
           >= 1)) {
            response <-
              (((length(intersect(grep('0 (',x,fixed=TRUE),grep('10',x,invert=TRUE))
              ))) + (length(which(x == 1))) + (2 * (length(
                which(x == 2)
              ))) + (3 * (length(
                which(x == 3)
              ))) + (4 * (length(
                which(x == 4)
              ))) + (5 * (length(
                grep('5 (',x,fixed=TRUE)
              ))) + (6 * (length(
                which(x == 6)
              ))) + (7 * (length(
                which(x == 7)
              ))) + (8 * (length(
                which(x == 8)
              ))) + (9 * (length(
                which(x == 9)
              ))) + (10 * (length(
                grep('10 (',x,fixed=TRUE)
              )))
              ) / ((length(
                which(na.exclude(x != 'Not Applicable'))
              )) - 1)
          } else{
            response <-
              'Error: Check data source for errors.  Note: You may receive this error if all values are blank, Not Applicable, or Okay'
          }
        }
      }
      return(round(response, digits = 2))
    }
    
    createlines <- function(x) {
      if (max(x) > 30) {
        abline(h = seq(0, ymax(x), by = 5),
               lty = 'dotted',
               col = 'gray50')
      } else{
        abline(h = seq(0, ymax(x), by = 1),
               lty = 'dotted',
               col = 'gray50')
      }
    }
    
    createchart <-
      function(x = 'variable',
               y = 'item w/ all participants',
               z = 'title',
               a = 'cex.main') {
        if ((length(which(x == 'Yes')) + length(which(x == 'No')) + length(grep('acceptable', x, ignore.case =
                                                                                TRUE))) >= 1) {
          barplot(
            creatematrix(x),
            col = 'cornflowerblue',
            ylab = "Number of particpants",
            ylim = c(0, (ymax(
              creatematrix(x)
            ))),
            main = createtitle(x, y, z),
            names.arg = createlabel(x),
            cex.main = a,
            cex.lab = 1.5,
            cex.sub = 1.35
          )
          mtext(
            'Subjective Rating',
            side = 1,
            line = 2.5,
            cex = 1.5
          )
          createlines(creatematrix(x))
        } else{
          barplot(
            creatematrix(x),
            col = 'cornflowerblue',
            ylab = "Number of particpants",
            ylim = c(0, (ymax(
              creatematrix(x)
            ))),
            main = createtitle(x, y, z),
            sub = paste("Average: ", createaverage(x)),
            names.arg = createlabel(x),
            cex.main = a,
            cex.lab = 1.5,
            cex.sub = 1.35
          )
          mtext(
            'Subjective Rating',
            side = 1,
            line = 2.5,
            cex = 1.5
          )
          createlines(creatematrix(x))
        }
      }
    
    if (input$type == 1) {
      genpng <- function(x, n, t, s) {
        for (i in 1:1) {
          title <- gsub('\\n', '\n', as.character(t), fixed = TRUE)
          if ((
            length(grep('good', x, ignore.case = TRUE)) + length(which(x == 'Yes')) +
            length(which(x == 'No')) + length(grep('acceptable', x, ignore.case = TRUE)) +
            length(grep('poor', x, ignore.case = TRUE)) + length(grep('easy', x, ignore.case =
                                                                      TRUE)) + length(grep('difficult', x, ignore.case = TRUE)) +
            length(which(x == 1)) + (length(which(x == 2))) + (length(which(x ==
                                                                            3))) + (length(which(x == 4))) + (length(grep('5 (',x,fixed=TRUE))) + (length(which(x ==
                                                                                                                                                     6))) + (length(which(x == 7))) + (length(which(x == 8))) + (length(which(x ==
                                                                                                                                                                                                                              9))) + (length(grep('10 (',x,fixed=TRUE))) + (length(
                                                                                                                                                                                                                                intersect(grep('0 (',x,fixed=TRUE),grep('10',x,invert=TRUE))
                                                                                                                                                                                                                              ))
          ) != 0) {
            win.metafile(
              filename = paste(
                Sys.Date(),
                gsub('\n', ' ', gsub(
                  '?', ' ', gsub('/', ' ', gsub(':', ' ', title, fixed = TRUE), fixed = TRUE), fixed = TRUE
                ), fixed = TRUE),
                '.wmf',
                sep = ''
              ),
              width = 10,
              height = 8
            )
            createchart(x, n, title, s)
            dev.off()
          } else{
            next
          }
        }
      }
      
      for (i in 1:(length(data))) {
        genpng(data[, i], y, data[1, i], 1.5)
        
      }
    } else{
      if (input$type == 2) {
        genpng <- function(x, n, t, s) {
          for (i in 1:1) {
            title <- gsub('\\n', '\n', as.character(t), fixed = TRUE)
            if ((
              length(grep('good', x, ignore.case = TRUE)) + length(which(x == 'Yes')) +
              length(which(x == 'No')) + length(grep(
                'acceptable', x, ignore.case = TRUE
              )) +
              length(grep('poor', x, ignore.case = TRUE)) + length(grep('easy', x, ignore.case =
                                                                        TRUE)) + length(grep('difficult', x, ignore.case = TRUE)) +
              length(which(x == 1)) + (length(which(x == 2))) + (length(which(x ==
                                                                              3))) + (length(which(x == 4))) + (length(grep('5 (',x,fixed=TRUE))) + (length(which(x ==
                                                                                                                                                                  6))) + (length(which(x == 7))) + (length(which(x == 8))) + (length(which(x ==
                                                                                                                                                                                                                                           9))) + (length(grep('10 (',x,fixed=TRUE))) + (length(
                                                                                                                                                                                                                                             intersect(grep('0 (',x,fixed=TRUE),grep('10',x,invert=TRUE))
                                                                                                                                                                                                                                           ))) != 0) {
              png(filename = paste(
                Sys.Date(),
                gsub('\n', ' ', gsub(
                  '?', ' ', gsub('/', ' ', gsub(':', ' ', title, fixed = TRUE), fixed = TRUE), fixed = TRUE
                ), fixed = TRUE),
                '.png',
                sep = ''
              ))
              createchart(x, n, title, s)
              dev.off()
            } else{
              next
            }
          }
        }
        
        for (i in 1:(length(data))) {
          genpng(data[, i], y, data[1, i], 1.5)
          
        }
      } else{
        genpng <- function(x, n, t, s) {
          for (i in 1:1) {
            title <- gsub('\\n', '\n', as.character(t), fixed = TRUE)
            if ((
              length(grep('good', x, ignore.case = TRUE)) + length(which(x == 'Yes')) +
              length(which(x == 'No')) + length(grep(
                'acceptable', x, ignore.case = TRUE
              )) +
              length(grep('poor', x, ignore.case = TRUE)) + length(grep('easy', x, ignore.case =
                                                                        TRUE)) + length(grep('difficult', x, ignore.case = TRUE)) +
              length(which(x == 1)) + (length(which(x == 2))) + (length(which(x ==
                                                                              3))) + (length(which(x == 4))) + (length(grep('5 (',x,fixed=TRUE))) + (length(which(x ==
                                                                                                                                                                  6))) + (length(which(x == 7))) + (length(which(x == 8))) + (length(which(x ==
                                                                                                                                                                                                                                           9))) + (length(grep('10 (',x,fixed=TRUE))) + (length(
                                                                                                                                                                                                                                             intersect(grep('0 (',x,fixed=TRUE),grep('10',x,invert=TRUE))
                                                                                                                                                                                                                                           ))                                                                                                                                                                                                                   
             != 0)) {
              jpeg(filename = paste(
                Sys.Date(),
                gsub('\n', ' ', gsub(
                  '?', ' ', gsub('/', ' ', gsub(':', ' ', title, fixed = TRUE), fixed = TRUE), fixed = TRUE
                ), fixed = TRUE),
                '.jpg',
                sep = ''
              ))
              createchart(x, n, title, s)
              dev.off()
            } else{
              next
            }
          }
        }
        
        for (i in 1:(length(data))) {
          genpng(data[, i], y, data[1, i], 1.5)
          
        }
      }
    }
  }
  
  observeEvent(input$run, {
    gen.all(input$file, input$pop, input$path)
    output$done <-
      renderText('Done!--Your figures are in the folder listed under \'file path\'')
  })
  
  observeEvent(input$clear, {
    updateTextInput(session, 'file', value = 'yourdata.csv')
    updateTextInput(session, 'path', value = (paste(
      'C:', 'Users', Sys.getenv("USERNAME"), 'Documents', sep = '/'
    )))
    updateNumericInput(session, 'pop', value = 25)
    updateTextInput(session, 'done', value = 'Ready...')
    updateSelectInput(session, 'type', choices = c(
      '.wmf' = 1,
      '.png' = 2,
      '.jpg' = 3
    ))
    output$done <- renderText(' ')
  })
  
})
            
            