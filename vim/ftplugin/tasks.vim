" Vim plugin for todo lists
" Last Change: June 22, 2009
" Maintainer: Emily Price <price.emily@gmail.com>
" Licence: Public domain

if exists("b:loaded_tasks")
  finish
endif
let b:loaded_tasks = 1

if !exists("no_plugin_maps") && !exists("no_tasks_maps")
  if !exists(":TasksMarkComplete")
    command TasksMarkComplete :call <SID>TasksMarkComplete()
  endif

  if !hasmapto("<Plug>TasksMarkComplete")
    map <buffer> <unique> <LocalLeader>t :TasksMarkComplete<CR>
  endif
  noremap <buffer> <unique> <Plug>TasksMarkComplete <SID>TasksMarkComplete()<CR>
endif

if !exists("*s:TasksMarkComplete")
  function! s:TasksMarkComplete()
    let currLine = getline(".")
    let matchPos = match(currLine, "\- ")
    let timeString = strftime("%F %R")
    if matchPos != -1 " if it's a task or note line
      if match(currLine, "\- TODO") != -1 " handle todo tasks
        let replaceLine = substitute(currLine, "\- TODO", "\- DONE", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "TODO->DONE " . timeString
      endif

      if match(currLine, "\- LATER") != -1 " handle later tasks
        let replaceLine = substitute(currLine, "\- LATER", "\- DONE", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "LATER->DONE " . timeString
      endif

      if match(currLine, "\- WAITING") != -1 " handle waiting tasks
        let replaceLine = substitute(currLine, "\- WAITING", "\- DONE", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "WAITING->DONE " . timeString
      endif

      if match(currLine, "\- QUESTION") != -1 " handle question tasks
        let replaceLine = substitute(currLine, "\- QUESTION", "\- ANSWER", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "QUESTION->ANSWER " . timeString
        if exists("g:tasks_prompt_answer") || exists("b:tasks_prompt_answer")
          let theAnswer = input("Answer: ")
          if theAnswer !~ "^\s*$"
            let theAnswer = repeat(" ", matchPos+2) . theAnswer
            call append(".", theAnswer)
          endif
        endif
      endif

      if match(currLine, "\- BUG") != -1 " handle bug tasks
        let replaceLine = substitute(currLine, "\- BUG", "\- FIXED", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "BUG->FIXED " . timeString
      endif

      if match(currLine, "\- SMELL") != -1 " handle smell tasks
        let replaceLine = substitute(currLine, "\- SMELL", "\- FIXED", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "SMELL->FIXED " . timeString
      endif

      if match(currLine, "\- REPORT") != -1 " handle report tasks
        let replaceLine = substitute(currLine, "\- REPORT", "\- FIXED", "")
        call setline(".", replaceLine)
        let didReplace = 1
        let statusChangeString = repeat(" ", matchPos+2) . "REPORT->FIXED " . timeString
      endif

      if exists("didReplace") && !exists("g:tasks_no_record_state_change") && !exists("b:tasks_no_record_state_change")
        call append(".", statusChangeString)
      end
    endif
  endf
endif
