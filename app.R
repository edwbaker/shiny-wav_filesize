library(shiny)
library(sonicscrewdriver)
library(schite)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WAV file size calculator"),

    tabsetPanel(
      tabPanel("Calculate file size",
        fluid=TRUE,
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
              textOutput("bits"),
              htmlOutput("cite")
            )
        )
      ),
      tabPanel(
        "Calculate duration",
        fluid=TRUE,
        sidebarLayout(
          sidebarPanel(
            numericInput(
              "sampleRate2",
              "Sample Rate (Hz)",
              48000,
              min = 1,
              max = NA,
              step = 10000,
              width = NULL
            ),
            numericInput(
              "bitDepth2",
              "Bit Depth",
              16,
              min = 1,
              max = NA,
              step = 8,
              width = NULL
            ),
            numericInput(
              "channels2",
              "Channels",
              2,
              min = 1,
              max = NA,
              step = 1,
              width = NULL
            ),
            numericInput(
              "filesize",
              "Filesize",
              1,
              min = 1,
              max = NA,
              step = 1,
              width = NULL
            ),
            radioButtons(
              "filesizeUnit",
              "Filesize Unit",
              choices = c("bytes", "kB", "MB", "GB"),
              selected = "GB",
              inline = FALSE,
              width = NULL,
              choiceNames = NULL,
              choiceValues = NULL
            )
          ),
          # Show a plot of the generated distribution
          mainPanel(
            verbatimTextOutput("humanTime"),
            textOutput("seconds"),
            textOutput("bitrate"),
            htmlOutput("cite2")
          )
        )
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
  bytes <- reactive(convert2bytes(bits()))
  output$bits <- renderText(paste({bits()}, "bits"))
  output$bytes <- renderText(paste({bytes()}, "bytes"))
  output$human <- renderText({humanBytes(bytes())})

  filesize <- reactive(convert2bytes(input$filesize, input$filesizeUnit))
  bitrate <- reactive(
    audio_filesize(
      samp.rate = input$sampleRate2,
      bit.depth = input$bitDepth2,
      channels = input$channels2,
      duration = 1,
      duration.unit ="seconds"
    )
  )

  output$bitrate <- renderText(paste({bitrate()}, "bits/second"))
  seconds <- reactive(filesize()*8/bitrate())
  output$seconds <- renderText(paste({seconds()}, "seconds"))
  output$humanTime <- reactive(humanTime({seconds()}))

  cite <- list(
    cite_r_package("sonicscrewdriver")
  )
  output$cite <- citationUI(cite, title="Calculated using SonicScrewdriveR")
  output$cite2 <- citationUI(cite, title="Calculated using SonicScrewdriveR")
}

# Run the application
shinyApp(ui = ui, server = server)
