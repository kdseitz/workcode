library(shiny)

shinyUI(
  fluidPage(
    fluidRow(column(1,
                    img(src = 'logo_starkey2.png')),
             column(
               4,
               offset = 3,
               h2('Figure generator', style = 'color:darkblue', align = 'justify')
             )),
    
    fluidRow(column(12,
                    helpText('      '))),
    
    fluidRow(
      column(
        style = 'border-right:1px solid silver',
        4,
        h4(
          tags$b('IMPORTANT: READ THIS FIRST'),
          align = 'center',
          style = 'color:red'
        ),
        tags$ul(
          tags$li('This app was designed to work ONLY with .csv files.  Use .csv (comma delimited) when saving in Excel.'),
          tags$li(
            'This app will only generate figures for certain response types.  See instructions at the bottom of the screen.  The app will skip questions with unrecognized response types.'
          ),
          tags$li(
            'Columns should have an identifier and figure title, followed by participant responses (i.e., NO COUNTS, STATS, ETC).'
          ),
          tags$li(
            'This app defaults to working from your Documents folder, however, you may set whichever file path you like.'
          ),
          tags$li(
            'The path is the location of the .csv file, and it is also where your figures will appear.  This app can only work from folders on your hard drive.'
          ),
          tags$li(
            tags$b(
              'Correct formatting of the spreadsheet is highly important.',
              style = 'color:red'
            ),
            'See notes below on how to set up your data file correctly.'
          ),
          align = 'justify',
          style = 'color:darkblue'
        )
      ),
      column(
        8,
        fluidRow(style='border-bottom:1px solid silver',
          column(style='border-right:1px dotted silver',
            6,
            textInput('file', 'File name', value = 'yourfile.csv'),
            helpText(
              'Ensure the data file is saved as a .csv file.  Enter the full file name including .csv',
              align = 'justify'
            )
          ),
          column(
            6,
            numericInput('pop', 'Total number of subjects in study', value =
                           25),
            helpText('Enter the total number of subjects who were included in the study.')
          )
        ),
        
        fluidRow(column(12,
                        h1('   '))),
        
        fluidRow(
          style='border-bottom:1px dotted silver',
          column(6,
                 style='border-right:1px dotted silver',
                        textInput(
                          'path', 'File path', value = (paste(
                            'C:', 'Users', Sys.getenv("USERNAME"), 'Documents', sep = '/'
                          ))
                        )),
                 column(
                   6,
                   selectInput(
                     'type',
                     label = 'Choose a file type for your figures',
                     choices = c(
                       '.wmf (no background)' = 1,
                       '.png (white background)' = 2,
                       '.jpg (white background)' = 3
                     )
                   )
                 )),
        
        fluidRow(style='border-bottom:1px solid silver',
          column(
          12,
          helpText(
            'If you saved your file in your Documents folder, file path will default to the correct value. If your data is saved in another folder, type the correct path here (excluding the filename and extension).  The folder entered here will be where your figures appear.',
            align = 'justify'
          )
        )),
        
        fluidRow(column(12,
                        h1('   '))),

        fluidRow(
          column(
            2,offset=4,
            actionButton('run', 'Generate', style = 'color:green')
            ),
            column(
              2,
            actionButton('clear', 'Clear all fields', style = 'color:red')
            )
          ),
        fluidRow(
          column(
           12,
            h3(tags$b(textOutput('done')),style='color:darkgreen')
          )
        )
        )
        ),
    
    fluidRow(column(12,
                    helpText('     '))),
    
    fluidRow(column(12,
                    helpText('     '))),
    
    fluidRow(column(12,
                    helpText('     '))),
    
    fluidRow(style = 'border-top:1px solid silver;border-bottom:1px solid silver',
             column(
               12,
               h4(
                 tags$b('Guide to correct data set-up'),
                 align = 'center',
                 style = 'color:darkblue'
               )
             )),
    
    fluidRow(fluidRow(
      column(
        4,
        tags$ul(
          tags$b('General', style = 'color:darkblue;border-bottom:1px solid darkblue'),
          tags$li(
            'You can either delete left-most columns from Lime Survey or leave them.'
          ),
          tags$li(
            'When entering data, use NA, 0, or x for missing data points.',
            tags$b(
              '\'Not Applicable\' should be reserved for responses marked as such by the participant',
              style = 'color:red'
            )
          ),
          tags$li(
            'Make sure row 1 is occupied with a unique identifier (such as Q1, Q2, Q3...)'
          ),
          align = 'justify'
        )
      ),
      column(
        4,
        tags$ul(
          tags$b('Figure titles', style = 'color:darkblue;border-bottom:1px solid darkblue'),
          tags$li(
            'Row 2 should contain your desired title (Row 1: identifier; Row 2: Title; Row 3+: Responses)'
          ),
          tags$li(
            'To create a line break in the title, enter backslash+n into the title text where you would like to start a new line.'
          ),
          tags$li(
            tags$b(
              'Only these punctuation marks are permitted in titles: ?, :, /',
              style = 'color:red'
            )
          ),
          align = 'justify'
        )
      ),
      column(
        4,
        tags$ul(
          tags$b(
            'Applicable responses should be entered as below',
            tags$b('VERBATIM',
                   style = 'color:red;border-bottom:1px solid red'),
            style = 'color:darkblue;border-bottom:1px solid darkblue'
          ),
          tags$li("Very Poor, Poor, Okay, Good, Very Good", style =
                    'color:black'),
          tags$li('Very Difficult, Difficult, Okay, Easy, Very Easy', style =
                    'color:black'),
          tags$li('0 (...), 1, 2, 3, 4, 5 (...), 6, 7, 8, 9, 10 (...)', style =
                    'color:black'),
          tags$li('Yes, No', style = 'color:black'),
          tags$li('Acceptable, Unacceptable', style = 'color:black')
        )
      )
    )),
    
    fluidRow(style = 'border-top:1px solid silver',
             column(1,
                    h4(
                      tags$b('Example:'), style = 'color:darkblue'
                    )),
             column(11,
                    img(src = 'example.png'), align = 'center'))
  )
)
