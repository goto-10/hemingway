#!/usr/bin/python
#- Copyright 2014 GOTO 10.
#- Licensed under the Apache License, Version 2.0 (see LICENSE).

## # Lines And Boxes (LnB)
##
## Lines And Boxes converts ascii diagrams such as
##
##                   /------\           
##       +-------+   |      |           
##       |       |   | +--+ |   +--+--+ 
##       +-------+   | |  | |   |  |  | 
##                   | +--+ |   +--+--+ 
##                   |      |   |  |  | 
##          +---+    \------/   |  |  | 
##          |   |               +--+--+ 
##          +---+                       
##
## to SVG diagrams, in this case for instance
##
#%                   /------\           
#%       +-------+   |      |           
#%       |       |   | +--+ |   +--+--+ 
#%       +-------+   | |  | |   |  |  | 
#%                   | +--+ |   +--+--+ 
#%                   |      |   |  |  | 
#%          +---+    \------/   |  |  | 
#%          |   |               +--+--+ 
#%          +---+                       
##
## # Implementation

import dom
import sys

## The diagram converts ascii diagrams to images. It uses a graph-based approach
## where each diagram character used in a piece of text, like `/`, `-`, or `|`,
## are converted to nodes in a directed graph that connect to some number of
## neighbouring nodes. Strongly connected components of the complete graph
## corresponds to diagram elements; weakly connected components correspond to
## lines. For instance, the character `/` corresponds to this graph node,
##
##            |  ^
##            v v
##        <-- * -->
##          ^ ^
##        v   |
##
## because it can be the top-left corner, bottom-right corner, or on a diagonal
## line.
##
## ### Half graph node
##
## A half graph node is a node with half-edges. The edges of the half nodes
## corresponding to each character is given in the map below where `*` indicates
## a bi-directional half edge and `<`, `>`, `^`, and `v` are one directional
## half edges in the obvious direction.

class HalfGraphNode(object):

  def __init__(self, shape):
    self.ins = set()
    self.outs = set()
    # This looks pretty horrilble but my assumption is that it's more important
    # to make the shapes as easy to read as possible than this chunk of code.
    for (char, x, y) in HalfGraphNode.get_chars_for_shape(shape):
      point = (x, y)
      if (char == ' '):
        continue
      elif (char == '*'):
        self.ins.add(point)
        self.outs.add(point)
      elif (x < 0 and char == '<') or (x > 0 and char == '>'):
        self.outs.add(point)
      elif (x < 0 and char == '>') or (x > 0 and char == '<'):
        self.ins.add(point)
      elif (y < 0 and char == '^') or (y > 0 and char == 'v'):
        self.outs.add(point)
      elif (y < 0 and char == 'v') or (y > 0 and char == '^'):
        self.ins.add(point)
      else:
        raise AssertionError("Unexpected character %s" % char)
    self.is_empty = (len(self.ins) == 0) and (len(self.outs) == 0)

  # Generates the characters in a given shape one at a time along with their
  # relative offsets from the center. Doesn't generate the center. It's easy to
  # get this wrong so always use this one.
  @staticmethod
  def get_chars_for_shape(shape):
    assert len(shape) == 9
    index = 0
    for y in [-1, 0, 1]:
      for x in [-1, 0, 1]:
        if (x != 0) or (y != 0):
          yield (shape[index], x, y)
        index += 1

  # Returns true iff this node has a half-edge in the specified direction going
  # between it and the node at the given relative position. For instance,
  # has_edge(True, -1, -1) returns true iff there is a half-edge going in to
  # this node from the node above it to the left.
  def has_edge(self, is_in, x, y):
    if is_in:
      return (x, y) in self.ins
    else:
      return (x, y) in self.outs


## ## Diagram character registry.
##
## This registry keeps track of how the various diagram characters like `-` and
## `/` connect to each other.


class DiagramCharacterRegistry(object):

  # Half graph node with no edges.
  EMPTY_HALF_NODE = HalfGraphNode(
    "   "
    "   "
    "   "
  )

  def __init__(self):
    self.chars = {}

  def add_char(self, char, connectivity):
    self.chars[char] = HalfGraphNode("".join(connectivity))
    return self

  def get_half_node(self, char):
    return self.chars.get(char, DiagramCharacterRegistry.EMPTY_HALF_NODE)

  @staticmethod
  def get_default():
    return _DEFAULT_DIAGRAM_CHARACTER_REGISTRY


# The default characters.
_DEFAULT_DIAGRAM_CHARACTER_REGISTRY = (
  DiagramCharacterRegistry()
    .add_char("-", [
      "   ",
      "***",
      "   ",
    ])
    .add_char("=", [
      "   ",
      "***",
      "   ",
    ])
    .add_char("|", [
      " * ",
      " * ",
      " * ",
    ])
    .add_char(":", [
      " * ",
      " * ",
      " * ",
    ])
    .add_char("+", [
      " * ",
      "***",
      " * ",
    ])
    .add_char("/", [
      " v*",
      "<*>",
      "*^ ",
    ])
    .add_char("\\", [
      "*^ ",
      ">*<",
      " v*",
    ])
    .add_char("v", [
      "vvv",
      "   ",
      "   ",
    ])
    .add_char("^", [
      "   ",
      "   ",
      "^^^",
    ])
    .add_char("<", [
      "  <",
      "  <",
      "  <",
    ])
    .add_char(">", [
      ">  ",
      ">  ",
      ">  ",
    ])
  )


## ### Full nodes
##
## The full nodes are what the diagram graph is made up of. It is
## {{#get_full_nodes}}(generated) from the half nodes as described above: if two
## neighbouring half nodes agree that there's an edge between them then the full
## node will have that edge.

class Node(object):

  def __init__(self, x, y, ins, outs, component=None):
    self.x = x
    self.y = y
    self.ins = sorted(ins)
    self.outs = sorted(outs)
    self.index = None
    self.lowlink = None
    self.component = component
    self.is_monochrome = None

  def get_component(self):
    assert not self.component is None
    return self.component

  def has_edge(self, edge):
    return (edge in self.ins) or (edge in self.outs)

  def __str__(self):
    return str((self.x, self.y))


## ## Diagram processing
##
## 

class DiagramProcessor(object):

  def __init__(self, lines):
    self.lines = lines
    self.half_nodes = None
    self.full_nodes = None
    self.space_colors = None
    self.shapes = None
    self.diagram = None
    self.shape_registry = dom.ShapeRegistry().get_default()
    self.chars = DiagramCharacterRegistry.get_default()

  # Processes the diagram lines.
  def process(self):
    self.half_nodes = self.get_half_nodes()
    self.full_nodes = self.get_full_nodes()
    self.identify_connected_components()
    self.space_colors = self.flood_fill_spaces()
    self.regions = self.build_regions()
    self.mark_monochrome_nodes()
    self.diagram = dom.Diagram(self)
    self.shapes = self.extract_shapes()

  # Returns the analyzed diagram object.
  def get_diagram(self):
    if self.diagram is None:
      self.process()
    return self.diagram

  # Given a list of string lines that make up an ascii diagram returns an array
  # of arrays with the corresponding half nodes for each character.
  def get_half_nodes(self):
    def to_half_node(char):
      return self.chars.get_half_node(char)
    def line_to_half_nodes(line):
      return [to_half_node(char) for char in line]
    return map(line_to_half_nodes, self.lines)

  ## ### Building the set of full nodes.
  ##
  ## The full node set is built based on the half nodes. In the complete graph
  ## there will only be an edge between two nodes if their corresponding
  ## half-nodes both have that half edge. If only one or the other has it it'll
  ## be ignored. Intuitively you can think of it like this: a single diagram
  ## character on its own is ignored but when it's adjacent to another character
  ## that can connect with it (that is, that has the corresponding half edge)
  ## then it will be included in the diagram analysis. So a
  ##
  ##     -->
  ##
  ## will result in a sequence of three connected nodes because `-`s connect
  ## left and right and a `>` connects left, whereas
  ##
  ##     --<
  ##
  ## will only give two connected nodes, the `-`s, because the `<` doesn't
  ## connect with them.

  # Build the set of full nodes.
  def get_full_nodes(self):
    result = {}
    for y in range(0, len(self.half_nodes)):
      half_row = self.half_nodes[y]
      for x in range(0, len(half_row)):
        half_node = half_row[x]
        if half_node.is_empty:
          ## Fast case; we know most nodes are likely to be empty.
          continue
        ins = set()
        outs = set()
        for dx in [-1, 0, 1]:
          for dy in [-1, 0, 1]:
            if (dx == 0) and (dy == 0):
              # Nodes don't have edges to themselves.
              continue
            tx = x + dx
            ty = y + dy
            target = self.get_half_node(tx, ty)
            if half_node.has_edge(False, dx, dy):
              # There's an outgoing half-edge. See if the target has an in-going
              if target.has_edge(True, -dx, -dy):
                outs.add((tx, ty))
            if half_node.has_edge(True, dx, dy):
              # There's an ingoing half-edge. See if the source has an out-going.
              if target.has_edge(False, -dx, -dy):
                ins.add((tx, ty))
        if (len(ins) > 0) or (len(outs) > 0):
          result[(x, y)] = Node(x, y, ins, outs)
    return result

  ## ### Identifying components
  ##
  ## The next step is to identify strongly connected components using
  ## [Tarjan's algorithm](http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm).
  ## The purpose of this step is to distinguish between "proper" fully enclosed
  ## diagram elements such as these,
  ##
  ##     /--\   +--+--+
  ##     |  |   |  |  |
  ##     \--/   +--+--+
  ##
  ## From elements that may have nodes in the same places but aren't properly
  ## connected, such as these
  ##
  ##     ---\   /--/   ----->
  ##     ^  |   |  |    |  |
  ##     \--/   /--/   <-----
  ##
  ## These are all meaningful diagram elements but they're lines, they're not
  ## fully enclosed boxes.
  ##
  ## The key to how this works is that the half-edges and edges are directed,
  ## which is a property we haven't used until now. You can think of there
  ## being a flow around the diagram elements in the direction of the edges. In
  ## most cases the flow goes both ways but for some elements: arrow heads,
  ## slanted corners, etc, the flow goes only one way. Slanted edges, for
  ## instance, allow clockwise flow from the bottom to the right and from the
  ## top to the left:
  ##
  ##
  ##         |  /
  ##         / / /-->
  ##     <--/ / / 
  ##         /  |
  ##
  ## This means that if you have box with correct slanted corners there will be
  ## a directed flow clockwise all the way around the box, since the corners
  ## allow clockwise flow and the edges allow flow in both directions. If,
  ## however, one of the corners is wrong, or the box is broken, the flow 
  ## doesn't go all the way around and the elements won't make up a strongly
  ## connected component. And that's fine, then it's just a line or a set of
  ## lines and they get styled nicely too, just not as a box.
  ##
  ## Highly connected elements such as tables or adjacent boxes will also result
  ## in strongly connected flows and be identified as one component. A later
  ## step takes care of subdividing those.

  def identify_connected_components(self):
    next_index = [0]
    next_component = [0]
    stack = []
    # Generates the next sequential index.
    def get_next_index():
      result = next_index[0]
      next_index[0] += 1
      return result
    # Strongly connect the given node.
    def strong_connect(node):
      index = get_next_index()
      node.index = index
      node.lowlink = index
      stack.append(node)
      for (tx, ty) in node.outs:
        target = self.get_full_node(tx, ty)
        if target.index is None:
          strong_connect(target)
          node.lowlink = min(node.lowlink, target.lowlink)
        elif target in stack:
          node.lowlink = min(node.lowlink, target.index)
      if node.index == node.lowlink:
        last_popped = None
        component = next_component[0]
        next_component[0] += 1
        while not last_popped is node:
          last_popped = stack.pop()
          last_popped.component = component
    # Scan through the nodes in order (to ensure that the algorithm is
    # deterministic) and strongly connect them.
    keys = sorted(self.full_nodes.keys())
    for (x, y) in keys:
      node = self.get_full_node(x, y)
      if node.index is None:
        strong_connect(node)

  ## ### Flood filling spaces
  ##
  ## The Tarjan step is a prerequisite for identifying enclosing elements (that
  ## is, elements that fully enclose some region) such as boxes, but it's not
  ## enough. That only sets up the boundaries between the spaces.
  ##
  ## This step identifies each contiguous space by iteratively flood filling the
  ## parts of the diagram that don't have edges. Whenever you have a fully
  ## enclosed region you also, necessarily, have a contiguous area of space
  ## inside which is separated from another area of space outside. This step
  ## would color them differently which the following steps use to do the actual
  ## identification.

  # Flood fills all spaces between nodes.
  def flood_fill_spaces(self):
    colors = {}
    spaces = self.get_padded_spaces()
    def flood_space(root, color):
      worklist = set([root])
      while len(worklist) > 0:
        node = worklist.pop()
        (x, y) = node
        assert not node in colors
        colors[node] = color
        # Run through all the nodes' neighbours.
        for dx in [-1, 0, 1]:
          for dy in [-1, 0, 1]:
            if (dx == 0) and (dy == 0):
              continue
            tx = x + dx
            ty = y + dy
            target = (tx, ty)
            if (not target in spaces) or (target in colors) or (target in worklist):
              continue
            if not self.has_boundary(x, y, dx, dy):
              worklist.add(target)
    next_color = 0
    for node in spaces:
      if not node in colors:
        flood_space(node, next_color)
        next_color += 1
    return colors

  # Returns true if there is a boundary when going between (x, y) and
  # (x+dx, y+dy) that can't be crossed directly.
  def has_boundary(self, x, y, dx, dy):
    if (dx == 0) or (dy == 0):
      return self.has_straight_wall(x, y, dx, dy)
    else:
      return True

  # Is there a straight (as opposed to diagonal) wall between (x, y), and
  # (x + dx, y + dy) where either dx or dy is 0.
  def has_straight_wall(self, x, y, dx, dy):
    # Maps {-1, 0} -> 0, {1} -> 1.
    floor_dx = (dx + 1) / 2
    floor_dy = (dy + 1) / 2
    # Maps {-1} -> 0, {0, 1} -> 1.
    ceil_dx = (dx + 2) / 2
    ceil_dy = (dy + 2) / 2
    # The two nodes that may block passage between (x, y) and (x+dx, y+dy). To
    # be completely honest I derived this way of calculating the boundary nodes
    # by just writing up the four possibilities:
    #
    #     ( 0, -1) -> (0, 0) and (1, 0)
    #     ( 0,  1) -> (0, 1) and (1, 1)
    #     (-1,  0) -> (0, 0) and (0, 1)
    #     ( 1,  0) -> (1, 0) and (1, 1)
    floor_bound_node = self.get_full_node(x + floor_dx, y + floor_dy)
    if floor_bound_node is None:
      # If there is no floor bound node then there can't be a boundary so we
      # return false.
      return False
    ceil_bound_coords = (x + ceil_dx, y + ceil_dy)
    ceil_bound_node = self.get_full_node(x + ceil_dx, y + ceil_dy)
    if ceil_bound_node is None:
      # Same as above.
      return False
    if (floor_bound_node.get_component() != ceil_bound_node.get_component()):
      # Edges between walls that belong to different components don't block the
      # filling, only coherent components do that.
      return False
    return floor_bound_node.has_edge(ceil_bound_coords)

  # Returns a sorted list of all the positions that don't have nodes associated
  # with them. These are the positions to flood fill.
  def get_spaces(self):
    result = []
    for y in range(0, len(self.lines)):
      row = self.lines[y]
      for x in range(0, len(row)):
        if not (x, y) in self.full_nodes:
          result.append((x, y))
    return sorted(result)

  # Returns a sorted list of all the spaces in the diagram, as well as one space
  # if padding all around the diagram. The padding is there because the space
  # coloring should behave as if the diagram were surrounded by an infinite area
  # of space rather than "boxed in" by the edges of the diagram.
  def get_padded_spaces(self):
    result = []
    height = len(self.lines)
    width = reduce(max, map(len, self.lines), 0)
    for y in range(-1, height + 1):
      for x in range(-1, width + 1):
        if not (x, y) in self.full_nodes:
          result.append((x, y))
    return sorted(result)

  ## ### Identifying monochrome nodes
  ##
  ## This step uses the flood fill to identify which nodes are surrounded by
  ## the same color and which are surrounded by more. Lines are made up of of
  ## monochrome nodes whereas enclosing elements are made up of non-monochrome
  ## nodes since enclosing elements are adjacent to at least two regions, the
  ## outside and the inside of the enclosure.

  # Marks all nodes with whether they're monochrome or not.
  def mark_monochrome_nodes(self):
    for node in self.full_nodes.values():
      colors_seen = set()
      for dx in [-1, 0, 1]:
        for dy in [-1, 0, 1]:
          if (dx == 0) and (dy == 0):
            continue
          color = self.get_space_color(node.x + dx, node.y + dy)
          if not color is None:
            colors_seen.add(color)
      node.is_monochrome = (len(colors_seen) == 1)

  ## ### Building regions
  ##
  ## Using the space colors, builds a map from colors to the set of points that
  ## were given that color. This will be useful later, for instance when
  ## identifying shapes like tables.
  def build_regions(self):
    regions = {}
    for (point, color) in self.space_colors.items():
      if not color in regions:
        regions[color] = []
      regions[color].append(point)
    return dict([(c, dom.Region(c, p)) for (c, p) in regions.items()])
    

  ## ### Extracting the diagram shapes
  ##
  ## Based on the now thoroughly annotated graph, extracts the graph's shapes by
  ## first picking out the strongly connected non-monochrome components (they
  ## correspond to enclosing elements like boxes) and then traversing the
  ## monochrome parts in order of degree, so starting from nodes that have only
  ## one edge and going from there.
  def extract_shapes(self):
    shapes = []
    enclosing = {}
    # Extract the enclosing shapes.
    for node in self.all_full_nodes():
      component = node.get_component()
      if node.is_monochrome or (component in enclosing):
        # If this is monochrome of part of a component we've already processed
        # just ignore it.
        continue
      nodes = []
      for candidate in self.all_full_nodes():
        if candidate.is_monochrome or candidate.get_component() != component:
          continue
        nodes.append(candidate)
      # Determine the location and extent of the shape in absolute coordinates.
      absolute_points = [(n.x, n.y) for n in nodes]
      absolute_bounds = dom.Rect.get_bounds_from_points(absolute_points)
      # Adjust to get the relative coordinates.
      (ax, ay) = absolute_bounds.get_top_left()
      relative_points = [(x - ax, y - ay) for (x, y) in absolute_points]
      element = dom.TextElement(self, absolute_bounds, relative_points)
      shape = self.shape_registry.resolve(element)
      enclosing[component] = shape
      shapes.append(shape)
    return shapes

  def all_full_nodes(self):
    for key in sorted(self.full_nodes.keys()):
      yield self.full_nodes[key]

  # Returns the color of a given position or None if it is a wall.
  def get_space_color(self, x, y):
    return self.space_colors.get((x, y), None)

  # Returns the half node at (x, y). This will always return a value, outside
  # the lines it will be the empty half node.
  def get_half_node(self, x, y):
    if 0 <= y and y < len(self.half_nodes):
      row = self.half_nodes[y]
      if 0 <= x and x < len(row):
        return row[x]
    return DiagramCharacterRegistry.EMPTY_HALF_NODE

  # Returns the full node at (x, y). If there is no full node None will be
  # returned.
  def get_full_node(self, x, y):
    return self.full_nodes.get((x, y), None)

  # Returns the shapes identified in this diagram.
  def get_shapes(self):
    return self.shapes

  # Returns the character at position (x, y) relative to the top left corner of
  # this diagram.
  def get_character(self, x, y):
    if 0 <= y and y < len(self.lines):
      row = self.lines[y]
      if 0 <= x and x < len(row):
        return row[x]
    return ' '


def get_unit_test_suite():
  import unittest

  # Unit tests for hemingway.
  class LinesAndBoxesTest(unittest.TestCase):

    def test_chars_for_shape(self):
      test_shape = ("abc"
                    "def"
                    "ghi")
      test_order = [
        ("a", -1, -1),
        ("b",  0, -1),
        ("c",  1, -1),
        ("d", -1,  0),
        ("f",  1,  0),
        ("g", -1,  1),
        ("h",  0,  1),
        ("i",  1,  1),
      ]
      self.assertEquals(test_order, list(HalfGraphNode.get_chars_for_shape(test_shape)))

    # Test that the little direction diagrams get interpreted in the right way.
    def test_half_nodes(self):
      def run_test(expected_in, expected_out, node):
        for (char, x, y) in HalfGraphNode.get_chars_for_shape(expected_in):
          has_edge_in = node.has_edge(True, x, y)
          expect_edge_in = (char != ' ')
          self.assertEquals(has_edge_in, expect_edge_in)
        for (char, x, y) in HalfGraphNode.get_chars_for_shape(expected_out):
          has_edge_out = node.has_edge(False, x, y)
          expect_edge_out = (char != ' ')
          self.assertEquals(has_edge_out, expect_edge_out)

      vertical_line = (
          " X "
          " X "
          " X ")
      run_test(vertical_line, vertical_line, HalfGraphNode(
          " * "
          " * "
          " * "))

      horizontal_line = (
          "   "
          "XXX"
          "   ")
      run_test(horizontal_line, horizontal_line, HalfGraphNode(
          "   "
          "***"
          "   "))

      top_only = (
          " X "
          "   "
          "   ")
      bottom_only = (
          "   "
          "   "
          " X ")
      run_test(top_only, bottom_only, HalfGraphNode(
          " v "
          " v "
          " v "))

      run_test(bottom_only, top_only, HalfGraphNode(
          " ^ "
          " ^ "
          " ^ "))

      right_only = (
          "   "
          "  X"
          "   ")
      left_only = (
          "   "
          "X  "
          "   ")
      run_test(left_only, right_only, HalfGraphNode(
          "   "
          ">>>"
          "   "))      

      run_test(right_only, left_only, HalfGraphNode(
          "   "
          "<<<"
          "   "))

      all = (
          "XXX"
          "X X"
          "XXX")
      none = (
          "   "
          "   "
          "   ")
      run_test(none, all, HalfGraphNode(
          "^^^"
          "< >"
          "vvv"))

      run_test(all, none, HalfGraphNode(
          ">v<"
          "> <"
          ">^<"))

    # Test which full nodes are built based on a diagram.
    def test_full_nodes(self):
      def run_test(expected, lines):
        processor = DiagramProcessor(lines)
        processor.process()
        found = []
        for y in range(0, len(expected)):
          expected_row = expected[y]
          found_row = []
          for x in range(0, len(expected_row)):
            if processor.get_full_node(x, y) is None:
              found_char = " "
            else:
              found_char = "X"
            found_row.append(found_char)
          found.append("".join(found_row))
        self.assertEquals(expected, found)

      # A single character is not enough to produce a node.
      empty3x3 = [
        "   ",
        "   ",
        "   "
      ]
      run_test(empty3x3, [
        "   ",
        " | ",
        "   "
      ])
      run_test(empty3x3, [
        "   ",
        " - ",
        "   "
      ])

      # Two characters _are_ enough.
      run_test([
        "    ",
        " XX ",
        "    "
      ], [
        "    ",
        " -- ",
        "    "
      ])

      run_test([
        "   ",
        " X ",
        " X ",
        "   "
      ], [
        "   ",
        " | ",
        " | ",
        "   "
      ])

      # Some more complex shapes.
      square = [
        " XXXXX ",
        " X   X ",
        " XXXXX "
      ]
      run_test(square, [
        " /---\\ ",
        " |   | ",
        " \\---/ "])
      run_test(square, [
        " +---+ ",
        " |   | ",
        " +---+ "])
      run_test(square, [
        " +++++ ",
        " +   + ",
        " +++++ "])

      # You mighe expect the corners with the wrong diagonals to not end up as
      # nodes but they do -- they prevent the shape from being a proper square
      # (the edges take care of that) but they're part of the line segments that
      # the square gets broken into.
      run_test(square, [
        " /---/ ",
        " |   | ",
        " /---/ "])

      long_vertical = [
        " X ",
        " X ",
        " X "
      ]
      run_test(long_vertical, [
        " | ",
        " | ",
        " v "
      ])
      run_test(long_vertical, [
        " ^ ",
        " | ",
        " | "
      ])


      long_horizon = [
        "   ",
        "XXX",
        "   "
      ]
      run_test(long_horizon, [
        "   ",
        "-->",
        "   "
      ])
      run_test(long_horizon, [
        "   ",
        "<--",
        "   "
      ])

      run_test([
        " X ",
        " X ",
        "   "
      ], [
        " | ",
        " | ",
        " ^ "
      ])

      run_test([
        " X ",
        " X ",
        "   "
      ], [
        " | ",
        " | ",
        " > "
      ])

      run_test([
        "  X",
        " X ",
        "X  "
      ], [
        "  /",
        " / ",
        "/  "
      ])

      run_test([
        "    XXX",
        "   X   ",
        "XXX    "
      ], [
        "    /--",
        "   /   ",
        "--/    "
      ])

    # Given a diagram and the expected output returns an array of strings where
    # each character in a string corresponds to some property of the corresponding
    # node in the diagram. The thunk argument is called to provide the character
    # given the node.
    @staticmethod
    def get_diagram_property_map(expected, diagram, thunk):
      result = []
      for y in range(0, len(expected)):
        result_row = []
        for x in range(0, len(expected[y])):
          result_char = thunk(diagram, x, y)
          result_row.append(result_char)
        result.append("".join(result_row))
      return result

    def test_components(self):
      def run_test(expected, lines):
        processor = DiagramProcessor(lines)
        processor.process()
        def get_component_char(diagram, x, y):
          node = diagram.get_full_node(x, y)
          if node is None:
            return ' '
          else:
            return chr(ord('a') + node.get_component())
        found = LinesAndBoxesTest.get_diagram_property_map(expected, processor,
         get_component_char)
        self.assertEquals(expected, found)
      run_test([
        " aaaaa ",
        " a   a ",
        " aaaaa "
      ], [
        " +---+ ",
        " |   | ",
        " +---+ "
      ])
      run_test([
        " aaa bbb ",
        " a a b b ",
        " aaa bbb "
      ], [
        " +-+ +-+ ",
        " | | | | ",
        " +-+ +-+ "
      ])
      run_test([
        " aaaaaaa ",
        " a a a a ",
        " aaaaaaa "
      ], [
        " +-+-+-+ ",
        " | | | | ",
        " +-+-+-+ "
      ])
      run_test([
        " aaaa ",
        " a  a ",
        " aaaa "
      ], [
        " /--\\ ",
        " |  | ",
        " \\--/ "
      ])
      run_test([
        " baaf ",
        " c  h ",
        " deeg "
      ], [
        " /--/ ",
        " |  | ",
        " \--/ "
      ])
      run_test([
        " dccb ",
        " e  a ",
        " e  a "
      ], [
        " /--\ ",
        " |  | ",
        " |  | "
      ])
      run_test([
        " a  d ",
        " b  d ",
        " b  c "
      ], [
        " ^  | ",
        " |  | ",
        " |  v "
      ])

    # Turns a drawn test graph into a DiagramProcessor with the appropriate
    # nodes and edges
    def make_test_graph(self, lines):
      result = DiagramProcessor([])
      result.process()
      for x in range(0, 4):
        for y in range(0, 4):
          cy = y * 2
          cx = x * 4
          node_char = lines[cy][cx]
          if node_char == '.':
            continue
          edges = set()
          for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
              if (dx == 0) and (dy == 0):
                continue
              tx = x + dx
              ty = y + dy
              if tx < 0 or tx > 3:
                continue
              if ty < 0 or ty > 3:
                continue
              edge_char = lines[cy + dy][cx + dx]
              if edge_char != ' ':
                edges.add((tx, ty))
          result.full_nodes[(x, y)] = Node(x, y, edges, edges, component=node_char)
      return result

    def test_make_test_graph(self):
      def serialize_node(node):
        return (node.x, node.y, node.component, sorted(node.outs))
      def run_test(expected, lines):
        graph = self.make_test_graph(lines)
        nodes = graph.full_nodes
        found = [serialize_node(nodes[key]) for key in sorted(graph.full_nodes.keys())]
        self.assertEquals(expected, found)
      run_test([
        (1, 1, 'o', [])
      ], [
        ".   .   .   .",
        "             ",
        ".   o   .   .",
        "             ",
        ".   .   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (2, 1, 'o', [])
      ], [
        ".   .   .   .",
        "             ",
        ".   .   o   .",
        "             ",
        ".   .   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (1, 2, 'o', [])
      ], [
        ".   .   .   .",
        "             ",
        ".   .   .   .",
        "             ",
        ".   o   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (0, 0, 'o', []), (0, 1, 'o', []), (0, 2, 'o', []), (0, 3, 'o', []),
        (1, 0, 'o', []), (1, 1, 'o', []), (1, 2, 'o', []), (1, 3, 'o', []),
        (2, 0, 'o', []), (2, 1, 'o', []), (2, 2, 'o', []), (2, 3, 'o', []),
        (3, 0, 'o', []), (3, 1, 'o', []), (3, 2, 'o', []), (3, 3, 'o', []),
      ], [
        "o   o   o   o",
        "             ",
        "o   o   o   o",
        "             ",
        "o   o   o   o",
        "             ",
        "o   o   o   o"
      ])

      run_test([
        (1, 1, 'o', [(2, 1)]),
        (2, 1, 'o', [(1, 1)])
      ], [
        ".   .   .   .",
        "             ",
        ".   o---o   .",
        "             ",
        ".   .   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (1, 1, 'o', [(1, 2), (2, 1)]),
        (1, 2, 'o', [(1, 1)]),
        (2, 1, 'o', [(1, 1)])
      ], [
        ".   .   .   .",
        "             ",
        ".   o---o   .",
        "    |        ",
        ".   o   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (1, 2, 'o', [(2, 1)]),
        (2, 1, 'o', [(1, 2)]),
      ], [
        ".   .   .   .",
        "             ",
        ".   .   o   .",
        "     / /     ",
        ".   o   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (1, 0, 'o', [(1, 1)]),
        (1, 1, 'o', [(1, 0), (1, 2)]),
        (1, 2, 'o', [(1, 1), (2, 2)]),
        (2, 2, 'o', [(1, 2), (3, 2)]),
        (3, 2, 'o', [(2, 2)]),
      ], [
        ".   o   .   .",
        "    |        ",
        ".   o   .   .",
        "    |        ",
        ".   o---o---o",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        (0, 0, 'a', [(0, 1), (1, 0)]),
        (0, 1, 'a', [(0, 0), (1, 1)]),
        (1, 0, 'a', [(0, 0), (1, 1)]),
        (1, 1, 'a', [(0, 1), (1, 0)]),
        (2, 2, 'b', [(2, 3), (3, 2)]),
        (2, 3, 'b', [(2, 2), (3, 3)]),
        (3, 2, 'b', [(2, 2), (3, 3)]),
        (3, 3, 'b', [(2, 3), (3, 2)])
      ], [
        "a---a   .   .",
        "|   |        ",
        "a---a   .   .",
        "             ",
        ".   .   b---b",
        "        |   |",
        ".   .   b---b"
      ])

    def test_has_boundary(self):
      def run_test(expected, lines):
        graph = self.make_test_graph(lines)
        shape = []
        for dy in [-1, 0, 1]:
          row = []
          for dx in [-1, 0, 1]:
            if (dx == 0) and (dy == 0):
              char = "X"
            elif graph.has_boundary(1, 1, dx, dy):
              char = " "
            else:
              char = "X"
            row.append(char)
          shape.append("".join(row))
        self.assertEquals(expected, shape)

      run_test([
        "   ",
        " X ",
        "   "
      ], [
        ".   .   .   .",
        "             ",
        ".   o---o   .",
        "    |   |    ",
        ".   o---o   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        " X ",
        " X ",
        "   "
      ], [
        ".   o   o   .",
        "    |   |    ",
        ".   o   o   .",
        "    |   |    ",
        ".   o---o   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        "   ",
        " X ",
        " X "
      ], [
        ".   .   .   .",
        "             ",
        ".   o---o   .",
        "    |   |    ",
        ".   o   o   .",
        "    |   |    ",
        ".   o   o   ."
      ])
      run_test([
        "   ",
        " XX",
        "   "
      ], [
        ".   .   .   .",
        "             ",
        ".   o---o---o",
        "    |        ",
        ".   o---o---o",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        "   ",
        "XX ",
        "   "
      ], [
        ".   .   .   .",
        "             ",
        "o---o---o   .",
        "        |    ",
        "o---o---o   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        " X ",
        " XX",
        "   "
      ], [
        ".   o   .   .",
        "    |        ",
        ".   o   .   .",
        "    |        ",
        ".   o---o---o",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        "   ",
        "XX ",
        " X "
      ], [
        ".   .   .   .",
        "             ",
        "o---o---o   .",
        "        |    ",
        ".   .   o   .",
        "        |    ",
        ".   .   o   ."
      ])
      run_test([
        " X ",
        "XXX",
        " X "
      ], [
        ".   .   .   .",
        "             ",
        ".   .   .   .",
        "             ",
        ".   .   .   .",
        "             ",
        ".   .   .   ."
      ])
      run_test([
        "   ",
        "XXX",
        " X "
      ], [
        ".   .   .   .",
        "             ",
        "a---a---a   .",
        "        |    ",
        ".   .   b   .",
        "        |    ",
        ".   .   b   ."
      ])

    def test_flood_fill_spaces(self):
      def run_test(expected, lines):
        diagram = DiagramProcessor(lines)
        diagram.process()
        def get_color_char(diagram, x, y):
          color = diagram.get_space_color(x, y)
          if color is None:
            return lines[y][x]
          else:
            return chr(ord('a') + color)
        found = LinesAndBoxesTest.get_diagram_property_map(expected, diagram,
          get_color_char)
        self.assertEquals(expected, found)
      run_test([
        "aaaaaaa",
        "a+---+a",
        "a|bbb|a",
        "a+---+a",
        "aaaaaaa"
      ], [
        "       ",
        " +---+ ",
        " |   | ",
        " +---+ ",
        "       "
      ])
      run_test([
        "aaaaaaaaaaa",
        "a+---+---+a",
        "a|bbb|ccc|a",
        "a+---+---+a",
        "aaaaaaaaaaa"
      ], [
        "           ",
        " +---+---+ ",
        " |   |   | ",
        " +---+---+ ",
        "           "
      ])
      run_test([
        "aaaaaaaaaaa",
        "a+---+---+a",
        "a|bbbbbbb|a",
        "a+---+bbb+a",
        "aaaaa|bbb|a",
        "aaaaa+---+a",
        "aaaaaaaaaaa"
      ], [
        "           ",
        " +---+---+ ",
        " |       | ",
        " +---+   + ",
        "     |   | ",
        "     +---+ ",
        "           "
      ])
      run_test([
        "aaaaaaaaaaa",
        "a/---+---\\a",
        "a|bbbbbbb|a",
        "a\\---\\bbb+a",
        "aaaaa|bbb|a",
        "aaaaa\\---/a",
        "aaaaaaaaaaa"
      ], [
        "           ",
        " /---+---\\ ",
        " |       | ",
        " \\---\\   + ",
        "     |   | ",
        "     \\---/ ",
        "           "
      ])
      run_test([
        "aaaaaaaaaaa",
        "a-------->a",
        "aaaaaaaaaaa"
      ], [
        "           ",
        " --------> ",
        "           "
      ])
      run_test([
        "aaaaaa",
        "------",
        "aaaaaa",
      ], [
        "      ",
        "------",
        "      ",
      ])

    def test_monochrome_nodes(self):
      def run_test(expected, lines):
        diagram = DiagramProcessor(lines)
        diagram.process()
        def get_monochrome_char(diagram, x, y):
          node = diagram.get_full_node(x, y)
          if node is None:
            return ' '
          elif node.is_monochrome:
            return 'X'
          else:
            return '.'
        found = LinesAndBoxesTest.get_diagram_property_map(expected, diagram,
          get_monochrome_char)
        self.assertEquals(expected, found)

      run_test([
        "      ",
        " .... ",
        " .  . ",
        " .... ",
        "      "
      ], [
        "      ",
        " +--+ ",
        " |  | ",
        " +--+ ",
        "      "
      ])
      run_test([
        "      ",
        " X  X ",
        " X  X ",
        " XXXX ",
        "      "
      ], [
        "      ",
        " +  + ",
        " |  | ",
        " +--+ ",
        "      "
      ])
      run_test([
        "         ",
        " ....XXX ",
        " .  .    ",
        " ....    ",
        "         "
      ], [
        "         ",
        " +--+--> ",
        " |  |    ",
        " +--+    ",
        "         "
      ])
      run_test([
        "             ",
        " ....   .... ",
        " .  .XXX.  . ",
        " ....   .... ",
        "             "
      ], [
        "             ",
        " +--+   +--+ ",
        " |  |-->|  | ",
        " +--+   +--+ ",
        "             "
      ])
      run_test([
        "         ",
        " ....... ",
        " .  .  . ",
        " ....... ",
        "         "
      ], [
        "         ",
        " +--+--+ ",
        " |  |  | ",
        " +--+--+ ",
        "         "
      ])

    def test_elements(self):
      processor = DiagramProcessor([
        "                   ",
        " +------------+    ",
        " |            |    ",
        " |  +------+  |    ",
        " |  |      |  |    ",
        " |  | +--+ +--+    ",
        " |  | |  |         ",
        " |  | |  +-------+ ",
        " |  | |          | ",
        " +--+ +----------+ ",
        "                   ",
      ])
      shapes = processor.get_diagram().get_shapes()
      self.assertEquals(2, len(shapes))
      self.assertEquals(str(shapes[0].get_element()), "\n".join([
        "+------------+",
        "|            |",
        "|  +------+  |",
        "|  |      |  |",
        "|  |      +--+",
        "|  |          ",
        "|  |          ",
        "|  |          ",
        "+--+          ",
      ]))
      self.assertEquals(str(shapes[1].get_element()), "\n".join([
        "+--+        ",
        "|  |        ",
        "|  +-------+",
        "|          |",
        "+----------+",
      ]))

    def test_lines(self):
      processor = DiagramProcessor([
        "                         ",
        "  +----------------+     ",
        "  |                |  ^  ",
        "  |  +----------+  |  |  ",
        "  |  |             |  |  ",
        "  |  |  <----------+  |  ",
        "  |  |                |  ",
        "  |  +---------->     |  ",
        "  |                   |  ",
        "  +-------------------+  ",
        "                         ",
      ])
      processor.get_diagram()

    def test_shapes(self):
      def box(x, y, w, h):
        return ("box", x, y, w, h)
      def table(x, y, w, h, cols, rows):
        result = ["table", x, y, w, h]
        if len(cols) > 0:
          result += [["cols"] + list(cols)]
        if len(rows) > 0:
          result += [["rows"] + list(rows)]
        return result
      def flatten_rect(rect):
        return (rect.top_left.x, rect.top_left.y, rect.get_width(), rect.get_height())
      def flatten_shape(shape):
        (x, y) = shape.get_position()
        (w, h) = shape.get_extent()
        if isinstance(shape, dom.BoxShape):
          return box(x, y, w, h)
        elif isinstance(shape, dom.TableShape):
          return table(x, y, w, h, shape.get_columns(), shape.get_rows())
        else:
          return None
      def run_test(expected, lines):
        diagram = DiagramProcessor(lines)
        diagram.process()
        shapes = diagram.get_shapes()
        self.assertEquals(expected, map(flatten_shape, shapes))

      run_test([
        box(1, 1, 4, 3)
      ], [
        "      ",
        " +--+ ",
        " |  | ",
        " +--+ ",
        "      "
      ])

      run_test([
        box(1, 1, 4, 3),
        box(6, 1, 4, 3)
      ], [
        "           ",
        " +--+ +--+ ",
        " |  | |  | ",
        " +--+ +--+ ",
        "           "
      ])

      run_test([
        box(1, 2, 9, 3),
        box(4, 6, 5, 3),
        box(13, 1, 7, 6)
      ], [
        "                       ",
        "             /-----+   ",
        " +-------+   |     |   ",
        " |       |   |     |   ",
        " +-------+   |     |   ",
        "             |     |   ",
        "    +---+    +-----/   ",
        "    |   |              ",
        "    +---+              ",
        "                       "
      ])

      run_test([
        table(1, 1, 7, 5,
          [],
          [2])
      ], [
        "         ",
        " +-----+ ",
        " |     | ",
        " +-----+ ",
        " |     | ",
        " +-----+ ",
        "         ",
      ])

      run_test([
        table(1, 1, 7, 5,
          [3],
          [2])
      ], [
        "         ",
        " +--+--+ ",
        " |  |  | ",
        " +--+--+ ",
        " |  |  | ",
        " +--+--+ ",
        "         ",
      ])
      run_test([
        table(0, 0, 7, 5,
          [3],
          [2])
      ], [
        "+--+--+",
        "|  |  |",
        "+--+--+",
        "|  |  |",
        "+--+--+",
      ])

      run_test([
        table(4, 2, 11, 5,
          [3, 6],
          [2])
      ], [
        "                ",
        "                ",
        "    +--+--+---+ ",
        "    |  |  |   | ",
        "    +--+--+---+ ",
        "    |  |  |   | ",
        "    +--+--+---+ ",
        "                ",
      ])

  return LinesAndBoxesTest
