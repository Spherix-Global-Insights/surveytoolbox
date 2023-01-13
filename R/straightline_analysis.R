
straightline_analysis <- function(dat, id_var) {

  straightline_sheet <- data.frame(dat[id_var])

  sl_q_num <- 0
  sl_case <- data.frame(array(0, nrow(dat)))

  mem <- array()

  for(i in 1:length(sl_questions)) {

    if(sl_questions[i] == "/n") { #marks the end of a question

      sl_case <- sl_case[1] + check_straightline(dat, mem[-1]) #add to number of sl cases for each respondent

      sl_q_num <- sl_q_num + 1 #count number of checked questions
      mem <- array()

    } else {

      mem <- c(mem, sl_questions[i])
    }
  }

  straightline_sheet <- cbind(straightline_sheet, sl_case/sl_q_num*100) #append the percentage of straightlined questions to the sheet

  colnames(straightline_sheet) <- c(id_var, paste("Percent Straight-Lined (", sl_q_num, " questions tested)", sep = ""))

  return(straightline_sheet)
}

