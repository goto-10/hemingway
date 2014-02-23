#!/usr/bin/python
#- Copyright 2014 GOTO 10.
#- Licensed under the Apache License, Version 2.0 (see LICENSE).


## # Hemingway
##
## A tool for turning markdown-based literate programs into HTML. Inspired by
## [Knuth](http://www.literateprogramming.com/knuthweb.pdf) and
## [pycco](http://fitzgen.github.io/pycco/) (and its ilk). This isn't the full
## literate programming model as envisioned by Knuth but an attempt to get some
## of the benefits without requiring extensions to the source language.
##
## ## Basic usage
##
## Hemingway is intended to be run on a collection of files located under the
## same root directory. You specify the argument like so,
##
##     hemingway --root path/to/src "**/*.c" "**/*.h"
##
## What this means is: starting from `path/to/src`, find all files that match
## the globs `**/*.c` and `**/*.h` relative to that root and convert them. The
## output will be stored in a subdirectory, `doc/` by default, also using paths
## relative to the root. So `path/to/src/foo/bar/baz.c` will be converted to
## `doc/foo/bar/baz.c.html`. See
##
##     hemingway --help
##
## for an overview of all the options supported by the tool.
##
## ## Watchdog
##
## Besides doing batch conversion Hemingway also supports live editing. If you
## pass the `--watchdog` flag the script won't terminate, instead it will watch
## the file system and re-convert any files that change. To end the program just
## Ctrl-C it.
##
## ## Implementation
##
## The script has a few different components that each have a separate
## responsibility in the process. To keep things simple and not have to worry
## about `$PYTHONPATH` everything is in this one file.
##
##   - The {{#SourceIndex}}(index) keeps track of the files to convert. During
##     initialization it locates all the files that match the arguments. After
##     that it answers queries from the other components but the index stays the
##     same.
##   - The {{#Scheduler}}(scheduler) schedules file conversions. It's has an
##     async queue which other parts of the program can add conversions to and
##     then the scheduler takes care of getting the conversions done.
##   - The {{#Conversion}}(conversion) code does the actual work of splitting
##     the source of a file into code and comment parts and then converting
##     each part appropriately.
##   - The file handling (open, closing, watching) is done by the {{#Main}}(main)
##     class.
##
## The {{#UnitTests}}(please_run_the_tests) function runs the unit
## tests.


import argparse
import glob
import logging
import os.path
import Queue
import re
import sys
import threading


## Import the nonstandard libraries defensively since they are more likely than
## the others to fail and theres a reasonable action the user can take to fix
## the issue.


# Try importing markdown to see whether it's available.
try:
  import markdown
except ImportError, e:
  print "Hemingway requires the markdown package. Try 'pip install markdown'."

# Try importing pygments to see whether it's available.
try:
  import pygments
except ImportError, e:
  print "Hemingway requires the pygments package. Try 'pip install pygments'."

import pkg_resources

# Import the subpackages we're actually going to use.
import markdown.extensions
import markdown.inlinepatterns
import markdown.preprocessors
import markdown.treeprocessors
import markdown.util
import pygments.formatters
import pygments.lexers


_HTML_TEMPLATE = """\
<html>
  <head>
    %(header)s
    <link rel="stylesheet" href="%(assets)s/markdown.css" type="text/css" />
    <link rel="stylesheet" href="%(assets)s/code.css" type="text/css" />
  </head>
  <body>
    <div class="container">
      <div class="page">
        %(contents)s
      </div>
    </div>
  </body>
</html>
"""


## ## Markdown extensions
##
## To support the integration of markdown with source code it's convenient to
## add a few extensions to the markdown processor. Luckily the python markdown
## framework has an api intended to allow exactly the kinds of extensions that
## we want. Good work guys. The extensions are,
##
##   - {{#CrossReferences}}(Cross references). The ability to create references
##     to other source files and program elements and have the processor resolve
##     them to HTML links. The syntax is `{{ref}}(title)` where the title is
##     optional.
##   - {{#AutomaticAnchorNames}}(Automatic anchor names). This is a post-processor
##     that adds anchor names to generated markdown headers such that the cross
##     references have something to link to besides just toplevel files.

## ### Cross References
##
## The cross reference syntax fits pretty nicely into the markdown library's
## inline pattern API hook. The framework matches based on the regexp passed to
## the constructor and then we perform a secondary match on the contents of the
## cross reference.

class CrossRefPattern(markdown.inlinepatterns.Pattern):

  MARKUP_PATTERN = r"\{\{([^\{\}]*)\}\}(?:\(([^)]+)\))?"
  TARGET_PATTERN = re.compile(r"([^#]*)(#.*)?")

  def __init__(self, context):
    super(CrossRefPattern, self).__init__(CrossRefPattern.MARKUP_PATTERN)
    self.context = context

  def handleMatch(self, match):
    name = match.group(2)
    title = match.group(3)
    if title is None:
      title = name
    ref_match = CrossRefPattern.TARGET_PATTERN.match(name)
    filename = ref_match.group(1)
    anchor = ref_match.group(2)
    if filename.strip() == "":
      destination = ""
    else:
      destination = self.context.resolve_cross_reference(filename)
    if destination is None:
      element = markdown.util.etree.Element('em')
      element.text = title
      return element
    else:
      if not anchor is None:
        destination += anchor
      element = markdown.util.etree.Element('a')
      element.text = title
      element.set("href", destination)
      return element


## ### Automatic Anchor Names
##
## This tree processor takes an element tree that is about to be serialized and
## adds named anchors to all the headers such that they can be linked to from
## cross references.
##
## Again the markdown library has a convenient API that lets us traverse the
## DOM before it gets serialized and wrap all headers in appropriately named
## anchors.

class AutoAnchorProcessor(markdown.treeprocessors.Treeprocessor):

  TAGS_TO_ANCHOR = ['h1', 'h2', 'h3']

  # Recursively find all headers and interject anchor nodes around them.
  def extend_headers(self, node):
    if markdown.util.etree.iselement(node):
      child_index = 0
      while child_index < len(node):
        child = node[child_index]
        self.extend_headers(child)
        new_child = self.get_extended_header(child)
        if not new_child is child:
          node[child_index] = new_child
        child_index += 1

  # Given a node, if it is a header returns a replacement with an anchor
  # otherwise returns the node itself.
  def get_extended_header(self, node):
    if not node.tag in AutoAnchorProcessor.TAGS_TO_ANCHOR:
      return node
    text = AutoAnchorProcessor.get_node_text(node)
    name = AutoAnchorProcessor.header_to_anchor_name(text)
    anchor = markdown.util.etree.Element('a')
    anchor.set("name", name)
    anchor.append(node)
    return anchor

  # Given the raw text contents of a header returns the anchor name to use.
  @staticmethod
  def header_to_anchor_name(text):
    # Strip out any non alpha numeric characters. Convert them to spaces so they
    # contribute to the titlecase behavior below.
    alnum = re.sub(r"\W", " ", text)
    # Split the string by spaces.
    raw_parts = re.split(r"\s+", alnum)
    # Capitalize the first character in each part.
    cap_parts = [part.title() for part in raw_parts]
    # Join the parts back together.
    return "".join(cap_parts)

  # Returns just the raw text contained in a given node.
  @staticmethod
  def get_node_text(node):
    result = []
    if not node.text is None:
      result.append(node.text)
    if markdown.util.etree.iselement(node):
      for child in node:
        result.append(AutoAnchorProcessor.get_node_text(child))
    if not node.tail is None:
      result.append(node.tail)
    return "".join(result)

  # The method called by the framework.  
  def run(self, root):
    self.extend_headers(root)
    return root


## ### Extension hook
##
## The hook used to install the hemingway extension into the markdown processor.
## This is somewhat cargo-cult, I'm not sure what the scope is of an extension
## (I assume it's just the current markdown object) and I'm not clear on exactly
## what `not_strong` is.

class HemingwayExtension(markdown.extensions.Extension):

  def __init__(self, context):
    self.context = context

  def extendMarkdown(self, md, globals):
    md.inlinePatterns.add('crossref', CrossRefPattern(self.context), '>not_strong')
    md.treeprocessors['autoanchor'] = AutoAnchorProcessor()


## ## Source index
##
## This class is responsible for looking through the file system and sorting out
## which files to convert. This all happens at startup in {{#IndexLoad}}(load).
## After loading its main responsibility is resolving cross-references between
## the source files. That happens in {{#ResolvingCrossReferences}}(resolve_cross_reference).
## For each source file there's a {{#SourceFile}}(source file) object that
## keeps track of the meta-information we need about the file.

class SourceIndex(object):

  def __init__(self, root, patterns):
    self.root = root
    self.patterns = patterns
    self.sources = None

  # Generates all the source files in sorted order.
  def list_sources(self):
    for abspath in sorted(self.sources.keys()):
      source = self.sources[abspath]
      yield source

  # Returns the source file that corresponds to the given absolute path. If no
  # file exists None is returned.
  def get_absolute_source(self, path):
    return self.sources.get(path, None)

  ## ### Index load
  ##
  ## Loading the index is pretty straightforward, `glob` does all the work. We
  ## need to know both the absolute and relative paths for each file which would
  ## be easy if `glob` worked relative to a given path (in this case the root).
  ## Alas it doesn't so we have to join the root and paths together and then
  ## remove the root again from the files. If the root has no trailing path
  ## separator that is particularly sticky because one will be added by glob, so
  ## we ensure at the top that it does.
  def load(self):
    sources = {}
    if not self.root.endswith(os.sep):
      self.root += os.sep
    for pattern in self.patterns:
      path = os.path.join(self.root, pattern)
      for rootpath in glob.glob(path):
        if not rootpath.startswith(self.root):
          raise AssertionError("Found %s under %s?" % (rootpath, self.root))
        relpath = rootpath[len(self.root):]
        abspath = os.path.abspath(rootpath)
        sources[abspath] = SourceFile(relpath, abspath)
    self.sources = sources

  ## ### Resolving cross references
  ##
  ## Cross references can be specified using short paths, so if there is a file
  ## called `foo/bar/baz.c` you can refer to it just as `baz.c`. The current
  ## implementation is kind of a hack but that's because I'd want to see what
  ## use cases are for more complex references before attempting to add support.
  ##
  ## Once a target has been found we need to strip the common prefix between the
  ## place where the reference will appear and the target, since otherwise it
  ## messes up the relative path.
  def resolve_cross_reference(self, name, whos_asking):
    for source in self.list_sources():
      if source.relpath.endswith(name):
        target_out = source.get_relative_output_path()
        asking_out = whos_asking.get_relative_output_path()
        prefix = os.path.commonprefix([target_out, asking_out])
        return target_out[len(prefix):]
    return None


## ### Source File
##
## A source file encapsulates the nitty-gritty of where the file is and what the
## output path is.

class SourceFile(object):

  def __init__(self, relpath, abspath):
    self.relpath = relpath
    self.abspath = abspath
    self.language = LanguageInfo.for_file(abspath)

  # Reads and returns the contents of this source file.
  def read(self):
    port = open(self.abspath, "rt")
    try:
      contents = port.read()
    finally:
      port.close()
    return contents

  # Returns the language info describing this file.
  def get_language(self):
    return self.language

  # Returns the output path to use for this file given that the output
  # directory root is the given path.
  def get_absolute_output_path(self, outdir):
    relpath = self.get_relative_output_path()
    return os.path.join(outdir, relpath)

  # Returns a relative path to the output corresponding to this file.
  def get_relative_output_path(self):
    return "%s.html" % self.relpath

  def __str__(self):
    return "source %s" % self.relpath


## ## Scheduler
##
## The scheduler is responsible for getting files converted. It runs
## asynchronously which may seem like an odd choice but it's really convenient
## to be able to reserve the main thread for the watchdog functionality and then
## have the conversion take place in the background on a different thread.
##
## The only part of the scheduler that's slightly complicated is the fact that
## when you ask it to stop (using `finish_up`) it doesn't stop immediately, it
## completes any pending conversion requests first. That way the non-watchdog
## version can get it started and ask it to finish immediately and then rely on
## it finishing up nicely.

class Scheduler(object):

  def __init__(self, context):
    self.context = context
    self.queue = Queue.Queue()
    self.keep_going = True

  # Spawns the scheduler thread then returns immediately.
  def start(self):
    threading.Thread(target=self.process).start()

  # Loop around processing conversions until asked to stop and the queue is
  # empty.
  def process(self):
    while self.keep_going or (not self.queue.empty()):
      next = self.queue.get(True)
      if next is None:
        return
      self.context.convert_file(next)

  def finish_up(self):
    self.keep_going = False
    self.queue.put(None)

  def schedule(self, source):
    self.queue.put(source)


## ## Conversion
##
## The source conversion process happens in two steps:
##
##   1. First the source is {{#Chopping}}(chopped into) contiguous comment and
##      code blocks according to the syntax of the {{#LanguageProcessing}}(language).
##   2. Then each block is asked to {{#BlockConversion}}(convert) its lines
##      which it does either using the custom
##      {{#MarkdownExtensions}}(markdown processor) or custom 
##      {{#PygmentsExtensions}}(pygments formatter).

class Converter(object):

  def __init__(self, context):
    self.context = context
    extension = HemingwayExtension(context)
    self.markdown = markdown.Markdown(extensions=['footnotes', extension])

  # Converts the text of a source file, returning the result as a string.
  def convert_source(self, source):
    contents = source.read()
    language = source.get_language()
    lines = contents.splitlines()
    blocks = self.chop_into_blocks(language, lines)
    results = [block.to_html(self) for block in blocks]
    body = "".join(results)
    return _HTML_TEMPLATE % {
      "header": self.context.get_headers(),
      "contents": body,
      "assets": self.get_assets_path(source)
    }

  def get_assets_path(self, origin):
    depth = -1
    last_parent = origin.relpath
    parent = os.path.dirname(last_parent)
    while parent != last_parent:
      last_parent = parent
      parent = os.path.dirname(parent)
      depth += 1
    if depth == 0:
      relative = "."
    else:
      relative = "/".join([".."] * depth)
    return "%s/assets" % relative

  ## ### Chopping
  ##
  ## The lines of the source is chopped into blocks really straightforwardly:
  ## for each line, ask the language if it's a comment. If the type of the line
  ## is the same as the block we're building, keep building. Otherwise flush the
  ## current block and start a new one. Lines can be ignored in which case we
  ## just skip past them (which lines to ignore is user configurable) and whole
  ## blocks can be ignored, specifically empty code blocks which don't really
  ## makes sense. Empty comment blocks are keps because the render as vertical
  ## space and it turns out to look wrong if they're stripped.
  ##
  ## Of special consideration are hashbangs which are always ignored at the top
  ## of a file.

  # Given a list of lines returns a list of blocks where each block contains a
  # contiguous list of lines of the same type, code or comment.
  def chop_into_blocks(self, language, lines):
    blocks = []
    CurrentType = CodeBlock
    current_lines = []
    def add_block(block):
      if not block.is_empty():
        blocks.append(block)
    is_beginning = True
    for line in lines:
      if is_beginning:
        try:
          if line.startswith("#!"):
            continue
        finally:
          is_beginning = False
      if language.is_ignored(line):
        continue
      if language.is_comment(line):
        NextType = CommentBlock
      else:
        NextType = CodeBlock
      if NextType != CurrentType:
        if len(current_lines) > 0:
          add_block(CurrentType(current_lines, language))
        CurrentType = NextType
        current_lines = []
      current_lines.append(line)
    if len(current_lines) > 0:
      add_block(CurrentType(current_lines, language))
    return blocks

  # Convertes the given text using the shared markdown converter.
  def convert_markdown(self, text):
    return self.markdown.convert(text)


## ### Block conversion
##
## The two types of blocks, {{#CommentBlocks}}(comments) and
## {{#CodeBlocks}}(code), each have their own way to do conversion. An
## alternative approach would be to convert everything as markdown with a
## preprocessor step that converts the code blocks and wraps them in `<div>`s.
## I actually tried that and it was painfully slow, presumably because the
## pygments output is large which slows down markdown processing.
##
## ### Comment blocks
##
## Comment blocks produce their output by stripping the literate comment marker
## (plus one space if present, which is usually is) and then just passing the
## result as a block to the {{#Converter}}(converter) to be markdowned.
## Initially I stripped all the whitespace after the marker but since whitespace
## is significant in markdown that turned out the be a bad choice.

# A contiguous block of end-of-line comments to be markdowned.
class CommentBlock(object):

  def __init__(self, lines, language):
    self.lines = lines
    self.language = language

  def is_empty(self):
    return False

  def to_html(self, context):
    marker = self.language.get_marker()
    result = []
    for line in self.lines:
      result.append(CommentBlock.strip_marker(marker, line))
    result.append("")
    return context.convert_markdown("\n".join(result))

  @staticmethod
  def strip_marker(marker, line):
    if marker is None:
      return line
    index = line.find(marker)
    if index == -1:
      return line
    else:
      removed = line[index+len(marker):]
      if removed.startswith(" "):
        removed = removed[1:]
      return removed


## ### Pygments extensions
##
## It's convenient to have anchors added to type and function names in the
## pygments output. For this we're using a custom formatter that scans each
## line of output for those and wraps them in anchors. It's not the ideal
## approach but it works; how well also depends on how the language lexer
## chooses to annotate the source code. For instance, it does well with
## python but not nearly as well with C. In python the word `Foo` in
## `class Foo` and `f = Foo()` are annotated differently, the first is a class
## name and the second is just a name, whereas in C `typedef Foo` and `Foo f` 
## are annotated the same which means that we can't distinguish which is the
## declaration.

class CustomHtmlFormatter(pygments.formatters.HtmlFormatter):

  PATTERN = re.compile(r"(<span class=\"(?:nc|nf)\">([^>]*)</span>)")
  TEMPLATE = "<a name=\"%s\">%s</a>"

  def __init__(self):
    super(CustomHtmlFormatter, self).__init__(cssclass='hll')

  # Overide the default wrapper implementation.
  def wrap(self, source, outfile):
    transformed = self.transform_lines(source)
    return super(CustomHtmlFormatter, self).wrap(transformed, outfile)

  # The transformed source must be a generator so this method returns one that
  # generates the transformed output.
  def transform_lines(self, source):
    for (depth, line) in source:
      new_line = CustomHtmlFormatter.PATTERN.sub(
        CustomHtmlFormatter.inject_anchor, line)
      yield (depth, new_line)

  # Given a match for the span pattern above, returns a string that wraps the
  # match appropriately in an anchor.
  @staticmethod
  def inject_anchor(match):
    everything = match.group(1)
    text = match.group(2)
    return CustomHtmlFormatter.TEMPLATE % (text, everything)


## ### Code blocks
##
## Code blocks are converted using the lexer that was used to construct the
## {{#DeterminingTheLanguage}}(language). Pretty straightforward.

# A block of source code to be formatted using pygments.
class CodeBlock(object):

  FORMATTER = CustomHtmlFormatter()

  def __init__(self, lines, language):
    self.language = language
    self.lines = lines

  def is_empty(self):
    return len("".join(self.lines).strip()) == 0

  def to_html(self, context):
    highlighted = pygments.highlight("\n".join(self.lines),
      self.language.get_lexer(), CodeBlock.FORMATTER)
    return "<div class=\"codehilite\">%s</div>" % highlighted


## ## Language Processing
##
## The language specific processing is all done by the `LanguageInfo` class.
## The information is siphoned out of pygments -- it knows the syntax of the
## languages and it's feasible to extract it in a lot of cases. Besides the
## defaults there are some hardcoded overrides, and longer term it should be
## possible to specify custom overrides probably via YAML.

_LANGUAGE_OVERRIDES = {
  "C": {
    "comments": [r"//.*$"]
  }
}

# Collection of information needed to process a file in a particular language.
class LanguageInfo(object):

  def __init__(self, lexer, marker, ignore):
    self.lexer = lexer
    self.marker = marker
    if ignore is None:
      self.ignore = None
    else:
      self.ignore = re.compile(ignore)

  # Returns the pygments lexer to use for this language.
  def get_lexer(self):
    return self.lexer

  # Returns the marker used to identify the lines that contain documentation.
  def get_marker(self):
    return self.marker

  # Is the given line an end-of-line comment within this language?
  def is_comment(self, line):
    stripped = line.strip()
    return stripped.startswith(self.marker)

  def is_ignored(self, line):
    if self.ignore is None:
      return False
    return self.ignore.match(line.strip())

  ## ### Determining the language
  ##
  ## The language for a file is determined from the filename using pygments.
  ## As it happens, this is incredibly slow for some reason. The lexer is then
  ## analyzed: we need to know what the end-of-line
  ## {{#DeterminingTheInitial}}(comment syntax) is and which
  ## {{#DeterminingTheMarker}}(marker) to use for literate comments. 
  ## Alternatively, if any of these values have been specified explicitly we
  ## don't try to infer them but just use the overrides.

  # Tries to produce a language info for the source file with the given name
  # by querying pygments.
  @staticmethod
  def for_file(filename):
    lexer = pygments.lexers.get_lexer_for_filename(filename)
    return LanguageInfo.for_lexer(lexer)

  cache = {}

  # Returns a language descriptor for the given pygments lexer. If no language
  # can be constructed None is returned.
  @staticmethod
  def for_lexer(lexer):
    marker = None
    comments = None
    overrides = None
    ignore = None
    # Grab the name if possible.
    if hasattr(lexer, 'name'):
      name = lexer.name
      # Now that we have the name check if we've created this language info
      # before.
      if name in LanguageInfo.cache:
        return LanguageInfo.cache[name]
      else:
        LanguageInfo.cache[name] = None
    else:
      name = None
    # Prime the variables with the overrides -- if the values are set we don't
    # do any work below to try to infer them.
    overrides = _LANGUAGE_OVERRIDES.get(name, None)
    if not overrides is None:
      comments = overrides.get("comments", None)
      marker = overrides.get("marker", None)
      ignore = overrides.get("ignore", None)
    if comments is None:
      # If comments haven't been set explicitly and the lexer doesn't have
      # tokens (that we can see) we have to give up.
      if not hasattr(lexer, 'tokens'):
        return None
      comments = []
      for matcher in LanguageInfo.flatten_list_dicts(lexer.tokens):
        if not isinstance(matcher, tuple):
          continue
        action = matcher[1]
        if action in pygments.token.Comment:
          comments.append(matcher[0])
    if (marker is None) or (ignore is None):
      # If the initial hasn't been set explicitly try to infer it from the
      # comments.
      for comment in comments:
        initial = LanguageInfo.initial_from_regexp(comment)
        if not initial is None:
          if marker is None:
            marker = LanguageInfo.marker_from_initial(initial)
          if ignore is None:
            ignore = LanguageInfo.ignore_from_initial(initial)
    result = LanguageInfo(lexer, marker, ignore)
    if not name is None:
      LanguageInfo.cache[name] = result
    return result

  # Given a dict whose values are lists returns a list that is the concatenation
  # of the list values. If some of the values are themselves list-dicts they are
  # traversed and flattened recursively.
  @staticmethod
  def flatten_list_dicts(root):
    result = []
    def add_values(value):
      if isinstance(value, dict):
        for (k, v) in value.items():
          add_values(v)
      else:
        for elm in value:
          result.append(elm)
    add_values(root)
    return result


  ## ### Determining the initial
  ##
  ## This is the cheesiest part, extracting the string that starts and
  ## end-of-line comment from a regexp that matches it. Generally they look
  ## the same (something like `#.*$` or `^\s*%.*\n` etc.) which can be matched
  ## by a regexp (yes, a meta-regexp). If we can't then it's fine to fall back
  ## on explicit defaults.

  # Given a regexp that is presumed to match end-of-line comments attempts to
  # extract the characters that start the comments. If this is not possible
  # None is returned.
  @staticmethod
  def initial_from_regexp(regexp):
    match = re.match(_INITIAL_REGEXP, regexp)
    if match:
      return match.group(1)
    return None

  ## ### Determining the marker
  ##
  ## This is also cheesy: it just doubles the last initial character. So if
  ## end-of-line comments start at `#` then the literate comment marker will be
  ## `##`. Again these are only defaults, longer term they can be overridden.

  # Given an end-of-line comment initial returns a documentation marker by just
  # doubling the last character.
  @staticmethod
  def marker_from_initial(initial):
    return initial + initial[-1]

  # Given an end-of-line comment initial returns an ignore marker. Just adds a
  # minus at the end.
  @staticmethod
  def ignore_from_initial(initial):
    return initial + "-"


# This is just the regexp used above split into separate parts to make it
# easier to read.
_COMMENT_PREFIX = r"^\^?(?:\\s\*)?"
_INITIAL_CHARS = r"((?:#|\\|/|\"|;|@|!|%)+)"
_COMMENT_SUFFIX = r"\.\*\??(?:\$|\\n)?$"
_INITIAL_REGEXP = re.compile(_COMMENT_PREFIX + _INITIAL_CHARS + _COMMENT_SUFFIX)


## ## Main
##
## The main class ties everything together (man). It handles
## {{#build_option_parser}}(flag parsing), runs the
## {{#RunningTheWatchdog}}(watchdog), and handles
## {{#WritingTheOutput}}(writing the output).

class Hemingway(object):

  LOG_FORMAT = "%(levelname)s: %(message)s"

  def __init__(self, args):
    parser = self.build_option_parser()
    self.options = parser.parse_args(args)
    self.initialize_logging()
    self.validate_options()
    self.index = SourceIndex(self.options.root, self.options.pattern)
    self.current_source = None
    self.scheduler = Scheduler(self)
    self.converter = Converter(self)

  # Configure logging appropriately.
  def initialize_logging(self):
    loglevel = self.options.log
    level_value = getattr(logging, loglevel.upper())
    logging.basicConfig(format=Hemingway.LOG_FORMAT, level=level_value)

  # Main entry-point for actually performing the conversion.
  def run(self):
    if self.options.profile:
      self.run_with_profile()
    else:
      self.run_no_profile()

  def run_with_profile(self):
    import cProfile
    import pstats
    profile = cProfile.Profile()
    profile.enable()
    try:
      self.run_no_profile()
    finally:
      profile.disable()
      stats = pstats.Stats(profile)
      stats.sort_stats('cumtime')
      stats.print_stats(32)

  # Does what .run says it does but where run also takes care of profiling this
  # one just does the conversion.
  def run_no_profile(self):
    self.index.load()
    for source in self.index.list_sources():
      self.scheduler.schedule(source)
    self.copy_assets()
    try:
      self.scheduler.start()
      if self.options.watchdog:
        logging.info("Watching for file changes under %s", self.options.root)
        self.run_watchdog()
    finally:
      self.scheduler.finish_up()

  ## ### Running the watchdog
  ##
  ## The watchdog is actually totally straightforward: all the work is done
  ## by the python `watchdog` library. For simplicity watchdog watches the
  ## whole root directory and then filters out files we're not interested in by
  ## asking the index whether it knows about files that have changed.
  ## On any changes the files are dropped into the scheduler's queue which does
  ## the rest.

  def run_watchdog(self):
    import time
    import watchdog
    import watchdog.observers
    import watchdog.events
    context = self
    class EventHandler(watchdog.events.FileSystemEventHandler):
      def on_any_event(self, event):
        context.on_watchdog_event(event)
    observer = watchdog.observers.Observer()
    observer.schedule(EventHandler(), self.options.root, recursive=True)
    observer.start()
    while True:
      time.sleep(1)

  # Fired by watchdog whenever a file changes.
  def on_watchdog_event(self, event):
    path = event.src_path
    source = self.index.get_absolute_source(path)
    if not source is None:
      self.scheduler.schedule(source)

  ## ### Writing the output
  ##
  ## While the conversion itself it delegated to the converter, writing the
  ## output to a file is done here. The source object knows where it will be
  ## written too (it needs to know to know how to resolve cross references)
  ## so all we have to do here is ensure that the file can be written and then
  ## do it.

  # Converts a source file, writing the results into the output file.
  def convert_file(self, source):
    logging.info("Processing %s", source)
    self.current_source = source
    markdown = self.converter.convert_source(source)
    output_file = source.get_absolute_output_path(self.options.out)
    self.ensure_parent_folder(output_file)
    port = open(output_file, "wt")
    try:
      port.write(markdown)
    finally:
      self.current_source = None
      port.close()

  # Given a file, ensures that its parent folder has been created.
  def ensure_parent_folder(self, path):
    parent = os.path.dirname(path)
    if not os.path.exists(parent):
      os.makedirs(parent)

  # Writes the assets into the output directory.
  def copy_assets(self):
    asset_path = os.path.join(self.options.out, 'assets')
    if not os.path.exists(asset_path):
      os.makedirs(asset_path)
    markdown = pkg_resources.resource_string(__name__, "assets/markdown-default.css")
    self.copy_asset(markdown, os.path.join(asset_path, "markdown.css"))
    # Pygments comes with some style sheets so just copy one of those.
    formatter = pygments.formatters.HtmlFormatter(style=self.options.pygments_style)
    code = formatter.get_style_defs('.codehilite')
    self.copy_asset(code, os.path.join(asset_path, "code.css"))

  def copy_asset(self, source, target):
    logging.info("Creating asset %s", target)
    port = open(target, "wt")
    try:
      port.write(source)
    finally:
      port.close()

  # Returns any extra headers to insert into the HTML output.
  def get_headers(self):
    # Automatically enable refresh in watchdog mode.
    refresh = self.options.refresh
    if refresh is None and self.options.watchdog:
      refresh = "10"
    if refresh is None:
      return ""
    else:
      return "<meta http-equiv=\"refresh\" content=\"%s\"/>" % refresh


  # Returns the source file referred to by the given cross reference.
  def resolve_cross_reference(self, name):
    whos_asking = self.current_source
    return self.index.resolve_cross_reference(name, whos_asking)

  # Builds and returns a new option parser for the flags understood by this
  # script.
  def build_option_parser(self):
    parser = argparse.ArgumentParser()
    parser.add_argument('--out', default='doc',
      help="Root directory of the output. Default 'doc'.")
    parser.add_argument('--root', required=True,
      help="Root directory where the input lives.")
    parser.add_argument('--profile', default=False, action="store_true",
      help="Dump a profile on program exit.")
    parser.add_argument('--watchdog', default=False, action="store_true",
      help="Use watchdog to convert files as they change.")
    parser.add_argument('--refresh', default=None,
      help="If set, inserts a refresh meta-tag in the output html.")
    parser.add_argument('--pygments-style', default="default", dest="pygments_style",
      help="Which formatter style to use for the generated output")
    parser.add_argument('--log', default='INFO', help="Log level to use.")
    parser.add_argument('pattern', nargs='+',
      help="Globs (for instance '**/*.c') used to locate files under the root")
    return parser

  # Checks that the given options are valid.
  def validate_options(self):
    all_styles = list(pygments.styles.get_all_styles())
    if not self.options.pygments_style in all_styles:
      print "Unknown --pygments-style '%s'" % self.options.pygments_style
      print "The available styles are: %s" % ", ".join(all_styles)
      sys.exit(1)


def main():
  main = Hemingway(sys.argv[1:])
  try:
    main.run()
  except KeyboardInterrupt, ki:
    logging.info("Interrupted; exiting.")
    sys.exit(1)


## ## Unit tests.
##
## This function runs all the tests. Everything is wrapped in a function rather
## than at toplevel because the script shouldn't depend on unittest being
## available unless you're actually going to run the tests. This still adds some
## overhead to running script -- whether you run them or not they have to be
## parsed -- but it shouldn't be too bad.
##
## The {{#test_language_infos}} test is more for development than an actual 
## test. Also, it takes a long time to run.

def please_run_the_tests():
  import unittest

  # Unit tests for hemingway.
  class HemingwayTest(unittest.TestCase):

    # Generates all the lexers known by pygment.
    def list_pygment_lexers(self):
      for lexer_tuple in pygments.lexers.get_all_lexers():
        long_name = lexer_tuple[0]
        aliases = lexer_tuple[1]
        all_names = [long_name] + list(aliases)
        for name in all_names:
          try:
            lexer = pygments.lexers.get_lexer_by_name(name)
            yield lexer
            break
          except pygments.lexers.ClassNotFound:
            continue

    def test_initial_from_regexp(self):
      def run_test(expected, input):
        found = LanguageInfo.initial_from_regexp(input)
        self.assertEquals(expected, found)
      run_test("#", "#.*$")
      run_test("#", "#.*\\n")
      run_test("#", "#.*?$")
      run_test("#", "#.*?\\n")
      run_test("#", "^#.*$")
      run_test("#", "^\\s*#.*$")
      run_test("#", "\\s*#.*$")
      run_test("##", "##.*$")
      run_test("//", "//.*$")
      run_test("\"", "^\\s*\".*")
      run_test(";", ";.*$")
      run_test("@", "^@.*$")
      run_test("!", "^!.*$")
      run_test("%", "^%.*\\n")

    def test_double_initial(self):
      def run_test(expected, input):
        found = LanguageInfo.marker_from_initial(input)
        self.assertEquals(expected, found)
      run_test("##", "#")
      run_test("///", "//")
      run_test("#//", "#/")

    def test_strip_marker(self):
      def run_test(expected, marker, line):
        found = CommentBlock.strip_marker(marker, line)
        self.assertEquals(expected, found)
      run_test("Foo", "#", "#Foo")
      run_test("Foo", "#", "# Foo")
      run_test(" Foo", "#", "#  Foo")
      run_test("#Foo", None, "#Foo")
      run_test("## Foo", "#", "### Foo")
      run_test("# Foo", "##", "### Foo")
      run_test("Foo", "###", "### Foo")
      run_test("## Foo", "###", "## Foo")

    def test_header_to_anchor_name(self):
      def run_test(expected, input):
        found = AutoAnchorProcessor.header_to_anchor_name(input)
        self.assertEquals(expected, found)
      run_test("Foo", "  Foo  ")
      run_test("FooBarBaz", "  Foo bar baz ")
      run_test("FooBar12", "  Foo bar 12 ")
      run_test("FooBarBaz", "Foo.bar.baz!")

    def test_language_infos(self):
      return
      for lexer in self.list_pygment_lexers():        
        language = LanguageInfo.for_lexer(lexer)
        if language is None:
          continue
        if len(language.get_comment_regexes()) == 0:
          continue
        if language.initial is None:
          print lexer, language.get_comment_regexes()

  # For crying out loud, why must this be so complicated?!?
  loader = unittest.TestLoader()
  suite = loader.loadTestsFromTestCase(HemingwayTest)
  unittest.TextTestRunner().run(suite)


## ## Entry-point
##
## The main entry-point uses this cheesy trick to decide whether to run the
## tests. You might imagine a situation where you actually wanted to run the
## script with four files called `please`, `run`, `the`, and `tests` and then
## this wouldn't do what you want -- but even if the hack wasn't there you
## wouldn't get the effect you'd want because no `--root` has been specified so
## the program would fail.

if __name__ == '__main__':
  print my_data
  if sys.argv[1:] == ['please', 'run', 'the', 'tests']:
    please_run_the_tests()
  else:
    main()

