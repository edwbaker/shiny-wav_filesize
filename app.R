library(shiny)
library(sonicscrewdriver)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WAV file size calculator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          numericInput(
            "sampleRate",
            "Sample Rate (Hz)",
            48000,
            min = 1,
            max = NA,
            step = 10000,
            width = NULL
          ),
          numericInput(
            "bitDepth",
            "Bit Depth",
            16,
            min = 1,
            max = NA,
            step = 8,
            width = NULL
          ),
          numericInput(
            "channels",
            "Channels",
            2,
            min = 1,
            max = NA,
            step = 1,
            width = NULL
          ),
          numericInput(
            "duration",
            "Duration",
            1,
            min = 1,
            max = NA,
            step = 1,
            width = NULL
          ),
          radioButtons(
            "durationUnit",
            "Duration Unit",
            choices = c("seconds", "minutes", "hours", "days"),
            selected = "minutes",
            inline = FALSE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
          ),
          numericInput(
            "repeats",
            "Repeats",
            1,
            min = 1,
            max = NA,
            step = 1,
            width = NULL
          ),
        ),
        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput("human"),
          textOutput("bytes"),
          textOutput("bits")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  bits<- reactive(
    audio_filesize(
      samp.rate = input$sampleRate,
      bit.depth = input$bitDepth,
      channels = input$channels,
      duration = input$duration,
      duration.unit = input$durationUnit
      )
    * input$repeats)
  output$bits <- renderText(paste({bits()}, "bits"))
  output$bytes <- renderText(paste({convert2bytes(bits())}, "bytes"))
  output$human <- renderText({humanBytes(bits())})
}

# Run the application
shinyApp(ui = ui, server = server)