library(shiny)

ui <- fluidPage(
    titlePanel("GPA Planning Tool"),

    sidebarLayout(
        sidebarPanel(
            numericInput("cumulative_attempted", "Cumulative Hours Attempted:", value = 115, min = 0),
            numericInput("cumulative_earned", "Cumulative Hours Earned:", value = 84.5, min = 0),
            numericInput("gpa_hours", "Cumulative GPA Hours:", value = 100, min = 0),
            numericInput("current_gpa", "Cumulative GPA:", value = 1.8, min = 0, max = 4, step = 0.001),
            numericInput("program_gpa", "Primary Program of Study GPA:", value = 2.0, min = 0, max = 4, step = 0.001),
            textInput("class_standing", "Class Standing at Start of Period:", value = "Junior"),
            textInput("load_status", "Load Status:", value = "Full-time"),
            numericInput("future_credits", "Planned GPA Credits Next Term:", value = 15, min = 0),
            numericInput("target_gpa", "Target GPA:", value = 1.9, min = 0, max = 4, step = 0.01)
        ),

        mainPanel(
            h3("Entered Student Profile"),
            tableOutput("studentProfile"),

            h3("GPA Projections by Grade"),
            tableOutput("gpaTable"),

            h3("What You Need to Reach Target GPA"),
            verbatimTextOutput("neededGrade")
        )
    )
)

server <- function(input, output) {
    gpa_scale <- c("A" = 4.0, "A-" = 3.67,"B+" = 3.33,"B" = 3.0, "B-" = 2.67,"C+" = 2.33, "C" = 2.0,  "C-" = 1.67,"D+" = 1.33, "D" = 1.0, "D-" = 0.67, "F" = 0.0)
# https://bulletin.wfu.edu/undergraduate/procedures/exams-grading/
    output$studentProfile <- renderTable({
        data.frame(
            Metric = c("Cumulative Hours Attempted",
                       "Cumulative Hours Earned",
                       "Cumulative GPA Hours",
                       "Cumulative GPA",
                       "Primary Program GPA",
                       "Class Standing",
                       "Load Status"),
            Value = c(input$cumulative_attempted,
                      input$cumulative_earned,
                      input$gpa_hours,
                      input$current_gpa,
                      input$program_gpa,
                      input$class_standing,
                      input$load_status),
            stringsAsFactors = FALSE
        )
    })

    output$gpaTable <- renderTable({
        current_qp <- input$gpa_hours * input$current_gpa
        total_hours <- input$gpa_hours + input$future_credits

        projections <- sapply(gpa_scale, function(g) {
            future_qp <- g * input$future_credits
            total_qp <- current_qp + future_qp
            round(total_qp / total_hours, 4)
        })

        data.frame(
            Grade = names(gpa_scale),
            `Projected Cumulative GPA` = projections,
            check.names = FALSE
        )
    })

    output$neededGrade <- renderPrint({
        current_qp <- input$gpa_hours * input$current_gpa
        total_hours <- input$gpa_hours + input$future_credits
        required_total_qp <- input$target_gpa * total_hours
        required_future_qp <- required_total_qp - current_qp
        required_avg_gpa <- required_future_qp / input$future_credits
        closest_letter <- names(gpa_scale)[which.min(abs(gpa_scale - required_avg_gpa))]
        cat(sprintf("To raise your GPA to %.3f:\n", input$target_gpa))
        cat(sprintf("- You must earn %.2f quality points over your next %d GPA credits.\n",
                    required_future_qp, input$future_credits))
        cat(sprintf("- This means an average GPA of %.3f in the upcoming term.\n", required_avg_gpa))
      #  cat(sprintf("- Closest matching letter grade: %s\n", closest_letter))

        cat("- This is equivalent to earning:\n")

# what to suggest if grade needed is between two letter grades
        if( gpa_scale[closest_letter] < required_avg_gpa &&
            gpa_scale[closest_letter] < 4) {
            next_letter <- names(gpa_scale)[which.min(abs(gpa_scale - required_avg_gpa))-1]
           cat(sprintf("  - %s or higher in all your classes, and at least one %s", closest_letter, next_letter))

        } else {
            cat(sprintf("  - %s or higher in all your classes.\n", closest_letter))
        }



        if (required_avg_gpa > 4) {
            cat("- This average GPA is not achievable with standard letter grades.\n")
}



    })
}

shinyApp(ui, server)
