library(shiny)

ui <- fluidPage(
    titlePanel("GPA Improvement Simulator"),
    div(
        style = "background-color:#f2f2f2; padding: 10px; margin-bottom: 10px; border-radius: 5px;",
        HTML("<strong>You're here because you care—and that’s a powerful first step.</strong> This tool is meant to support your success, not to judge it.")
    ),
    sidebarLayout(
        sidebarPanel(
            p("📌 Enter your current academic profile from Workday, and future plans to see how different grades can impact your GPA."),
            numericInput("cumulative_attempted", "Cumulative Hours Attempted:", value = 115, min = 0),
            numericInput("cumulative_earned", "Cumulative Hours Earned:", value = 84.5, min = 0),
            numericInput("gpa_hours", "Cumulative GPA Hours:", value = 100, min = 0),
            numericInput("current_gpa", "Cumulative GPA:", value = 1.8, min = 0, max = 4, step = 0.001),
            numericInput("program_gpa", "Primary Program of Study GPA:", value = 2.0, min = 0, max = 4, step = 0.001),
            textInput("class_standing", "Class Standing at Start of Period:", value = "Junior"),
            textInput("load_status", "Load Status:", value = "Full-time"),
            numericInput("future_credits", "Planned GPA Credits Next Term:", value = 15, min = 0),
            numericInput("target_gpa", "Target GPA:", value = 1.9, min = 0, max = 4, step = 0.01),
            p("💡 This tool is designed to help you explore your options and plan for success. Remember, you’re not alone in this journey!"),

        ),

        mainPanel(
                tabsetPanel(
                    tabPanel("Your Current Profile",
                             h3("Your Current Profile"),
                             tableOutput("studentProfile"),
                             uiOutput("supportMessage")
                    ),
                    tabPanel("GPA Projections",
                             h3("GPA Projections by Grade"),
                             p("📊 This table shows how your GPA could change based on different grades you might earn in the upcoming term."),
                             p("💡 Use this to explore how different grades can help you reach your target GPA."),
                             tableOutput("gpaTable")
                    ),
                    tabPanel("Target GPA Plan",
                             h3("What You Need to Reach Your Target GPA"),
                             p("📣 Remember: GPA is just one part of your academic story.\n",
                               "This tool is here to help you plan ahead—you’re not expected to do it alone.\n",
                               "If you need support, reach out to your academic advisor or a trusted faculty member."),
                             uiOutput("neededGrade"),
                             p(em("These estimates are based on GPA credits only. Reach out to an advisor for help tailoring your plan."))
                    ),
                    tabPanel("Encouragement & Support",
                             h3("You're Not Alone"),
                             p("This tool helps you make an academic plan—it doesn't define your future."),
                             p(strong("You're not alone:"), "Many capable students experience academic probation. What matters most is what you do next."),
                             p("Need help? Reach out to your advisor, a mentor, or visit the ",
                               a("Student Success Center", href = 'https://class.wfu.edu/', target = "_blank"), "."),
                             p(em("Planning is a powerful first step. You've got this.")),
                             p("This tool was built by Dr. Garrison and inspired by recent research on student success from Dr. Brady. Read more about it at: ",
                               a("More Colleges Are No Longer Putting Students on Academic Probation",
                                  href = "https://www.moorparkcollege.edu/sites/moorparkcollege/files/media/pdf_document/2024/More%20Colleges%20Are%20No%20Longer%20Putting%20Students%20on%20Academic%20Probation..pdf",
                                  target = "_blank"),
                               "and the College Transition Collaborative's research brief: ",
                               a("Brady, S. T., Kroeper, K. M., Ozier, E. M., Henderson, A. G., Walton, G. M. & the College Transition Collaborative",
                                  href = "http://collegetransitioncollaborative.org/content/sass_toolkit_researchbrief_final.pdf",
                                  target = "_blank")
                    )
                )
            )

)))

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
    output$neededGrade <- renderUI({
        current_qp <- input$gpa_hours * input$current_gpa
        total_hours <- input$gpa_hours + input$future_credits
        required_total_qp <- input$target_gpa * total_hours
        required_future_qp <- required_total_qp - current_qp
        required_avg_gpa <- required_future_qp / input$future_credits

        closest_letter <- names(gpa_scale)[which.min(abs(gpa_scale - required_avg_gpa))]

        if (required_avg_gpa > 4&&FALSE) {
            tagList(
                p(strong(sprintf("To reach a cumulative GPA of %.3f:", input$target_gpa))),
                tags$ul(
                    tags$li(sprintf("You would need to earn %.2f quality points across %d GPA credits.",
                                    required_future_qp, input$future_credits)),
                    tags$li(sprintf("This requires an average GPA of %.3f, which is not achievable with standard grades (maximum GPA = 4.0).",
                                    required_avg_gpa))
                )
            )
        } else {
            idx <- match(closest_letter, names(gpa_scale))
            if (gpa_scale[closest_letter] < required_avg_gpa && gpa_scale[closest_letter] < 4) {
                next_letter <- names(gpa_scale)[idx - 1]
                grade_message <- sprintf("This is approximately equivalent to: %s or higher in most classes, with at least one %s.",
                                         closest_letter, next_letter)
            } else {
                grade_message <- sprintf("This is approximately equivalent to: %s or higher in all classes.", closest_letter)
            }

            tagList(
                p(strong(sprintf("To reach a cumulative GPA of %.3f:", input$target_gpa))),
                tags$ul(
                    tags$li(sprintf("You need to earn %.2f quality points across %d GPA credits.",
                                    required_future_qp, input$future_credits)),
                    tags$li(sprintf("This means an average GPA of %.3f in the upcoming term.",
                                    required_avg_gpa)),
                    tags$li(grade_message)
                )
            )
        }
    })

    output$supportMessage <- renderUI({
        HTML(paste(
            "<p><strong>You are not alone.</strong> Many students go through academic challenges—this doesn’t define your intelligence, potential, or future.</p>",
            "<p>Academic probation is not a punishment. It’s a structured opportunity to get support and make a comeback.</p>",
            "<p><strong>You can do this.</strong> Students in your situation have successfully returned to good standing, often within a single term.</p>",
            "<p>We recommend reaching out to an academic advisor, tutor, or mentor to make a plan that works for you.</p>",
            "<p><a href='https://class.wfu.edu/'>Visit the Center for Learning, Access, and Student Success</a> to learn more about resources available to you.</p>"
        ))
    })

}

shinyApp(ui, server)
