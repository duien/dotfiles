path = require 'path'
{$, $$$, ScrollView} = require 'atom'
# _ = require 'underscore-plus'

module.exports =
class GraphvizPreviewView extends ScrollView
  atom.deserializers.add(this)

  @deserialize: (state) ->
    new GraphvizPreviewView(state)

  @content: ->
    @div class: 'graphviz-preview native-key-bindings', tabindex: -1

  constructor: ({@editorId, filePath}) ->
    super

    if @editorId?
      @resolveEditor(@editorId)
    else
      if atom.workspace?
        @subscribeToFilePath(filePath)
      else
        @subscribe atom.packages.once 'activated', =>
          @subscribeToFilePath(filePath)

  serialize: ->
    deserializer: 'GraphvizPreviewView'
    filePath: @getPath()
    editorId: @editorId

  destroy: ->
    @unsubscribe()

  subscribeToFilePath: (filePath) ->
    @trigger 'title-changed'
    @handleEvents()
    @renderHTML()

  resolveEditor: (editorId) ->
    resolve = =>
      @editor = @editorForId(editorId)

      if @editor?
        @trigger 'title-changed' if @editor?
        @handleEvents()
      else
        # The editor this preview was created for has been closed so close
        # this preview since a preview cannot be rendered without an editor
        @parents('.pane').view()?.destroyItem(this)

    if atom.workspace?
      resolve()
    else
      @subscribe atom.packages.once 'activated', =>
        resolve()
        @renderHTML()

  editorForId: (editorId) ->
    for editor in atom.workspace.getEditors()
      return editor if editor.id?.toString() is editorId.toString()
    null

  handleEvents: ->

    changeHandler = =>
      @renderHTML()
      pane = atom.workspace.paneForUri(@getUri())
      if pane? and pane isnt atom.workspace.getActivePane()
        pane.activateItem(this)

    if @editor?
      @subscribe(@editor.getBuffer(), 'contents-modified', changeHandler)
      @subscribe @editor, 'path-changed', => @trigger 'title-changed'

  renderHTML: ->
    @showLoading()
    if @editor?
      @renderHTMLCode(@editor)

  renderHTMLCode: (editor) ->
    text = @editor.getText();
    text = """
    <!doctype html>
    <html>
      <head>
        <meta charset="utf-8">
        <title>Dot Preview</title>
        <style>
          body {
            font-family: "Helvetica Neue", Helvetica, sans-serif;
            font-size: 14px;
            /* line-height: 1.6; */
            background-color: #fff;
            overflow: scroll;
            box-sizing: border-box;
          }
          .dot-syntax-error {
            color: red;
            font-weight: bold;
            font-size: larger;
          }
        </style>
        <!-- Viz.js by mdaines from https://github.com/mdaines/viz.js -->
        <script src="atom://graphviz-preview/assets/viz.js"></script>
        <script>
          function src(id) {
            return document.getElementById(id).innerHTML;
          }
          function plotGraphviz(dot) {
            if (dot.trim() != "") { // Empty buffer
              oldConsoleLog = console.log;
              lastConsoleMessage = "";
              window['console']['log'] = function (msg) {
                  if (msg && msg.indexOf("line") > -1) {
                    lastConsoleMessage = msg.replace(/(\\d+)/g, function(a,n){ return "<b>" + (n-1) + "</b>"; });
                  }
              }
              try {
                document.getElementById('rendered-graph').innerHTML = Viz(dot, 'svg');
              } catch (err) {
                document.getElementById('rendered-graph').innerHTML = "<div class='dot-syntax-error'>Dot syntax error</div>"
                  + lastConsoleMessage
                  + "<div style='text-align: center; width: 100%; position:absolute; bottom:0;'>"
                   + "<b>Need GraphViz Help?</b> Try the docs: "
                   + "<a href='http://www.graphviz.org/Documentation.php'>GraphViz Documentation</a></div>";
                   + " -- <a href='http://www.graphviz.org/pdf/dotguide.pdf' target='_blank'>DOT Guide</a>"
                   + " -- <a href='http://www.graphviz.org/pdf/neatoguide.pdf'>Neato Guide</a>"
                if (lastConsoleMessage && lastConsoleMessage.indexOf("line") > -1) {
                  document.getElementById('error-line').innerText = lastConsoleMessage.match(/(\\d+)/)[0];
                }
              }
              window['console']['log'] = oldConsoleLog;
            } else {
              sampleGraph1 = 'digraph g {\\n Hello->World\\n Hello->Atom\\n}';
              sampleGraph2 = 'digraph g {\\n rankdir=LR; graph[label=\\"Example Title\\",labelloc=t, labeljust=l, size="3.5"]\\n node [fontsize=10, shape=record]\\n H[label=\\"Hello!\\", shape=circle, color=green]\\n H->World\\n Atom->Rocks\\n H->Atom\\n The->World->Cup->Rocks[style=dashed]\\n {rank=same; H Atom World }\\n}';

              document.getElementById('rendered-graph').innerHTML = "<h1>Empty Editor</h1>"
               + "<p>This preview panel will show the output of <a href='http://en.wikipedia.org/wiki/DOT_(graph_description_language)'>DOT language</a> "
               + " editor buffers the same way <a href='http://www.graphviz.org/'>GraphViz</a> would. </p>"
               + "<p>It's possible to make some pretty amazing graphs using DOT+GraphViz - check out the <a href='http://www.graphviz.org/Gallery.php'>Graphiz Gallery</a> for some examples.</p>"
               + "<p>GraphViz lays each node out automatically.  You can also feed it parameters to change how it renders.</p>"
               + "<p>Here's a very simple Hello World example (Copy the code into the buffer to make this preview update):</p>"
               + "<div><textarea cols='30' rows='10'>"+sampleGraph1+"</textarea>"
               + Viz(sampleGraph1, 'svg')
               + "</div>"
               + "<p>Here is a slightly more complicated version of the same graph with some addtional rendering parameters:</p>"
               + "<div><textarea cols='30' rows='14'>"+sampleGraph2+"</textarea>"
               + Viz(sampleGraph2, 'svg')
               + "</div>";
            }
          }
        </script>
        </head>
      <body onload="plotGraphviz(src('preview-render'))">
        <script type="text/vnd.graphviz" id="preview-render">
        #{text}
        </script>
        <div id="rendered-graph"></div>
        <div id="error-line" style="visibility: hidden;"></div>
      </body>
    </html>
    """
    iframe = document.createElement("iframe")
    iframe.src = "data:text/html;charset=utf-8,#{encodeURI(text)}"
    @html $ iframe
    # TODO: jump to error line in the editor
    # somehow get rendered HTML from iframe body and extract out error line number
    # if (iframe.innerText)
    # row = extact error line
    # @editor.setCursorBufferPosition([row,Infinity])

    @trigger('graphviz-preview:html-changed')

  getTitle: ->
    if @editor?
      "#{@editor.getTitle()} Preview"
    else
      "HTML Preview"

  getUri: ->
    "html-preview://editor/#{@editorId}"

  getPath: ->
    if @editor?
      @editor.getPath()

  showError: (result) ->
    failureMessage = result?.message

    @html $$$ ->
      @h2 'Previewing DOT Failed'
      @h3 failureMessage if failureMessage?

  showLoading: ->
    @html $$$ ->
      @div class: 'graphviz-spinner', 'Loading DOT Preview\u2026'
