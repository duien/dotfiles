syntax clear
syntax case match
syntax sync minlines=20

syntax cluster taskKeyword contains=taskTodo,taskLater,taskWaiting,taskCancel,taskDone,taskQuestion,taskAnswer,taskBug,taskReport,taskSmell,taskFixed
syntax cluster taskText contains=taskTodoText,taskLaterText,taskWaitingText,taskCancelText,taskDoneText,taskQuestionText,taskAnswerText,taskBugText,taskSmellText,taskFixed

syntax match taskLine /^\s*- .*\(\n\(\s*\)\@>[^\-].*\)*/ contains=@taskKeyword

syntax keyword taskTodo TODO nextgroup=taskTodoText contained
syntax match taskTodoText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskLater LATER nextgroup=taskLaterText contained
syntax match taskLaterText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskWaiting WAITING nextgroup=taskWaitingText contained
syntax match taskWaitingText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskCancel CANCEL CANCELED nextgroup=taskCancelText contained
syntax match taskCancelText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskDone DONE nextgroup=taskDoneText contained
syntax match taskDoneText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskQuestion QUESTION nextgroup=taskQuestionText contained
syntax match taskQuestionText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskAnswer ANSWER ANSWERED nextgroup=taskAnswerText contained
syntax match taskAnswerText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskBug BUG nextgroup=taskBugText contained
syntax match taskBugText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskSmell SMELL nextgroup=taskSmellText contained
syntax match taskSmellText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskReport REPORT nextgroup=taskReportText contained
syntax match taskReportText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax keyword taskFixed FIXED nextgroup=taskFixedText contained
syntax match taskFixedText / .*\(\n\(\s*\)\@>[^\-].*\)*/ contained

syntax match taskProject /^\w.*:$/


" let's do lots of linking!
hi link taskTodoText taskActiveText
hi link taskLaterText taskActiveText
hi link taskWaitingText taskInactiveText
hi link taskCancelText taskInactiveText
hi link taskDoneText taskInactiveText
hi link taskQuestionText taskActiveText
hi link taskAnswerText taskInactiveText
hi link taskBugText taskActiveText
hi link taskReportText taskActiveText
hi link taskSmellText taskActiveText
hi link taskFixedText taskInactiveText
" highlight link taskTodo <GlobalType>

" this stuff doesn't belong in syntax
" it's helpful for testing, though
" and it'll probably end up staying

" hi taskLine guibg=#000000
hi taskTodo guibg=#DA4939 ctermfg=167 guifg=#111111 gui=bold cterm=bold,reverse
hi taskLater guibg=#FFC66D ctermfg=221 guifg=#111111 gui=bold cterm=bold,reverse
hi taskWaiting guibg=grey40 ctermfg=240 guifg=#111111 gui=bold cterm=bold,reverse
hi taskCancel guifg=grey40 ctermfg=240 gui=bold cterm=bold
hi taskDone guifg=#DA4939 ctermfg=167 gui=bold cterm=bold
hi taskQuestion guibg=#6D9CBE ctermfg=73 guifg=#111111 gui=bold cterm=bold,reverse
hi taskAnswer guifg=#6D9CBE ctermfg=73 gui=bold cterm=bold
hi taskReport guibg=#A5C261 ctermfg=107 guifg=#111111 gui=bold cterm=bold,reverse
hi taskBug guibg=#519F50 ctermfg=29 guifg=#111111 gui=bold cterm=bold,reverse
hi taskSmell guibg=#519F50 ctermfg=29 guifg=#111111 gui=bold cterm=bold,reverse
hi taskFixed guifg=#519F50 ctermfg=29 gui=bold cterm=bold

hi taskInactiveText guifg=grey40 ctermfg=240
hi taskCancelText guifg=grey20 ctermfg=235
hi taskDoneText guifg=grey20 ctermfg=235
hi taskFixedText guifg=grey20 ctermfg=235

hi taskProject gui=bold,reverse cterm=bold,reverse
" dull brown: 6A4939
