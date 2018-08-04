# Your init script
#
# Atom will evaluate this file each time a new window is opened. It is run
# after packages are loaded/activated and after the previous editor state
# has been restored.
#
# An example hack to make opened Markdown files always be soft wrapped:
#
# path = require 'path'
#
# atom.workspaceView.eachEditorView (editorView) ->
#   editor = editorView.getEditor()
#   if path.extname(editor.getPath()) is '.md'
#     editor.setSoftWrap(true)

# document.body.classList.add('earthsung-modify-ui')

# atom.workspace.observeTextEditors (editor) ->
#   original = editor.getGrammar()
#   if original? and original is atom.grammars.grammarForScopeName('text.plain.null-grammar')
#     editor.setGrammar(atom.grammars.grammarForScopeName('text.md'))


# atom.grammars.onDidAddGrammar (grammar) ->
#   console.log("added grammar", grammar)
atom.workspace.observeTextEditors (editor) ->
  default_scope = 'text.md'
  original = editor.getGrammar()

  # If the editor has "null" grammar (aka unset)
  if original? and original is atom.grammars.grammarForScopeName('text.plain.null-grammar')
    default_grammar = atom.grammars.grammarForScopeName(default_scope)
    if default_grammar? # check if markdown grammar is already loaded
      editor.setGrammar(default_grammar)
    else
      # grammar was not loaded yet, so add a callback as grammars load
      callback = atom.grammars.onDidAddGrammar (grammar) ->
        if grammar.id is default_scope
          # once we've loaded the grammar, set it and dispose of the callback
          editor.setGrammar(grammar)
          callback.dispose()
