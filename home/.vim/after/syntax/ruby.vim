syn keyword rubyKeyword new
if exists("ruby_operators")
  syn match rubyOperator "=>\@!"
endif

if expand('%') =~# '_spec\.rb$'
  syn keyword rubyRspec describe context it specify it_should_behave_like before after setup subject let
endif

hi def link rubyRspec Define
