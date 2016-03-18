library(shiny)

shinyUI(fluidPage(
  
  fluidRow(
    column(1,
           img(src='logo_starkey2.png')
           ),
    column(11,
    h2('Past program comparisons',style='color:darkblue',align='center')
    )
    ),
  
  fluidRow(
    column(7,
           h2(verbatimTextOutput('result'))
    ),
    column(3,
           style='border-right:1px dotted silver; border-left:1px dotted silver',
           radioButtons('x',label=h5('Select data type below'),c('Likert or Very Difficult...Very Easy'=1,
                                                                 '10 point scale'=2))
           ),
    column(1,
           br(),
           actionButton('reset','Reset all to zero',style='color:red')
    )
  ),
  
  fluidRow(
    
    column(6,
           helpText(h3('Five point scales',style='color:darkblue',align='center'))
           ),
    column(6,
           helpText(h3('10 point scales',style='color:darkblue',align='center'))
           )
  ),
  
  fluidRow(
    column(3, 
           style='border-right:1px dotted silver',
           helpText(h4('Current study',style='color:darkblue',align='center')),
           br(),
           numericInput("a",label = h5("Very Good/Very Easy"),value = 0),
           numericInput("b",label = h5('Good/Easy'),value = 0),
           numericInput("c",label = h5("Okay"),value = 0),
           numericInput("d",label = h5("Poor/Difficult"),value = 0),
           numericInput("e",label = h5("Very Poor/Very Difficult"),value = 0)
    ),
    column(3,
           helpText(h4('Past study',style='color:darkblue',align='center')),
           br(),
           numericInput("f",label = h5("Very Good/Very Easy"),value = 0),
           numericInput("g",label = h5('Good/Easy'),value = 0),
           numericInput("h",label = h5("Okay"),value = 0),
           numericInput("i",label = h5("Poor/Difficult"),value = 0),
           numericInput("j",label = h5("Very Poor/Very Difficult"),value = 0)
    ),
    
    column(3,
           style='border-left:1px solid silver',
           helpText(h4('Current study',style='color:darkblue',align='center')),
           br(),
           numericInput("z",label = h5("Zero"),value = 0),
           numericInput("gg",label = h5('One'),value = 0),
           numericInput("hh",label = h5("Two"),value = 0),
           numericInput("ii",label = h5("Three"),value = 0),
           numericInput("jj",label = h5("Four"),value = 0),
           numericInput("kk",label = h5("Five"),value = 0),
           numericInput("ll",label = h5('Six'),value = 0),
           numericInput("mm",label = h5("Seven"),value = 0),
           numericInput("nn",label = h5("Eight"),value = 0),
           numericInput("oo",label = h5("Nine"),value = 0),
           numericInput("pp",label = h5("Ten"),value = 0)
    ),
    column(3,
           style='border-left:1px dotted silver',
           helpText(h4('Past Study',style='color:darkblue',align='center')),
           br(),
           numericInput("z1",label = h5("Zero"),value = 0),
           numericInput("qq",label = h5('One'),value = 0),
           numericInput("rr",label = h5("Two"),value = 0),
           numericInput("ss",label = h5("Three"),value = 0),
           numericInput("tt",label = h5("Four"),value = 0),
           numericInput("uu",label = h5("Five"),value = 0),
           numericInput("vv",label = h5('Six'),value = 0),
           numericInput("ww",label = h5("Seven"),value = 0),
           numericInput("xx",label = h5("Eight"),value = 0),
           numericInput("yy",label = h5("Nine"),value = 0),
           numericInput("zz",label = h5("Ten"),value = 0)
    )
  )
)
)
