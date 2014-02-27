#!/usr/bin/python
#- Copyright 2014 GOTO 10.
#- Licensed under the Apache License, Version 2.0 (see LICENSE).

from abc import ABCMeta, abstractmethod
import sys


# A region within (or around) some walls.
class Region(object):

  def __init__(self, color, points):
    self.color = color
    self.points = points
    self.bounds_cache = None

  # Returns the tightest rectangle enclosing all the points in this region.
  def get_bounds(self):
    if self.bounds_cache is None:
      self.bounds_cache = Rect.get_bounds_from_points(self.points)
    return self.bounds_cache

  # Returns true if this region is a square.
  def is_square(self):
    return self.get_bounds().is_square_within(self.points, False)

  def __str__(self):
    return "region(%s)" % self.get_bounds()


class Element(object):
  __metaclass__ = ABCMeta

  def __init__(self, text):
    self.text = text

  # Returns a string that identifies the type of this element.
  @abstractmethod
  def get_type(self):
    return None

  # Returns a point giving the width and height of this shape.
  def get_extent(self):
    return self.text.get_extent()

  # Returns the position (top left x and y) of this shape in the whole diagram.
  def get_position(self):
    return self.text.get_position()

  def get_absolute_bounds(self):
    return self.text.get_absolute_bounds()

  # Returns a text component that provides access to the underlying text of this
  # shape.
  def get_text_component(self):
    return self.text

  def __init__(self, text):
    self.text = text


class Path(Element):

  TYPE = 'path'

  def __init__(self, text, points):
    super(Path, self).__init__(text)
    self.points = points

  def get_type(self):
    return Path.TYPE

  def get_points(self):
    return self.points


# Abstract superclass of diagram shapes.
class Shape(Element):
  
  IRREGULAR = 'irregular'


# A shape that encloses a region, for instance a box.
class EnclosingShape(Shape):
  
  @staticmethod
  def create(origin, proto):
    as_box = BoxShape.try_create(origin, proto, False)
    if not as_box is None:
      return as_box
    as_table = TableShape.try_create(origin, proto)
    if not as_table is None:
      return as_table
    return IrregularShape(origin, proto.get_nodes())


# A simple square box.
class BoxShape(EnclosingShape):

  TYPE = 'box'

  def get_type(self):
    return BoxShape.TYPE

  @staticmethod
  def try_resolve(element):
    return BoxShape.try_create(element, False)

  # Attempts to create a box shape from the given nodes. If the nodes are not
  # a box None will be returned.
  @staticmethod
  def try_create(element, allow_internal):
    (width, height) = element.get_extent()
    # Scan through all the points within the bounds of the shape and check that
    # they form a box.
    right_boundary = width - 1
    bottom_boundary = height - 1
    for x in range(0, width):
      for y in range(0, height):
        expect_wall = (x == 0) or (x == right_boundary) or (y == 0) or (y == bottom_boundary)
        found_wall = element.has_wall(x, y)
        if expect_wall:
          if not found_wall:
            # If we expected a wall and there was none it's definitely not a
            # box.
            return None
        else:
          if (not allow_internal) and found_wall:
            # If we didn't expect a wall that's okay if allow_internal is True.
            # If it's not, however, we don't allow there to be walls were we
            # don't expect them.
            return None
    return BoxShape(element)


class TableShape(Shape):

  TYPE = 'table'

  def __init__(self, element, boundary, cols, rows):
    super(TableShape, self).__init__(element)
    self.boundary = boundary
    self.cols = cols
    self.rows = rows

  def get_type(self):
    return TableShape.TYPE

  def get_boundary(self):
    return self.boundary

  def get_columns(self):
    return self.cols

  def get_rows(self):
    return self.rows

  @staticmethod
  def try_resolve(element):
    # Is the shape completely enclosed within a box?
    boundary = BoxShape.try_create(element, True)
    if boundary is None:
      return None
    (width, height) = element.get_extent()
    rows = []
    # Scan through the inside of the element looking for horizontal dividers.
    for row in range(1, height - 1):
      has_divider = True
      for col in range(1, width - 1):
        if not element.has_wall(col, row):
          has_divider = False
          break
      if has_divider:
        rows.append(row)
    # Ditto vertical dividers.
    cols = []
    for col in range(1, width - 1):
      has_divider = True
      for row in range(1, height - 1):
        if not element.has_wall(col, row):
          has_divider = False
          break
      if has_divider:
        cols.append(col)
    if (len(cols) == 0) and (len(rows) == 0):
      # Found no dividers so this doesn't look like a table.
      return None
    else:
      return TableShape(element, boundary, cols, rows)


# A shape that's not recognized as any other "nice" form.
class IrregularShape(Shape):

  def get_type(self):
    return None


## ## Shape registry
##
## A shape registry contains a list of types of shapes that are used to resolve
## the diagram elements. When processing a diagram, for each shape recognized
## the registry will determine which type of shape it is by having the regitry
## try each of the shapes it knows. The first shape whose `.try_resolve`
## static method returns a non-None value is the one that wins.

class ShapeRegistry(object):

  def __init__(self):
    self.shapes = []

  # Returns the most specific shape from this registry that recognizes the given
  # diagram element.
  def resolve(self, element):
    for ShapeType in self.shapes:
      result = ShapeType.try_resolve(element)
      if not result is None:
        return result
    return IrregularShape(element)

  # Adds a shape class to the set recognized by this registry. The handler will
  # be matched only after any other handlers that have been added before have
  # tried and failed to recognize a given shape.
  def add_shape(self, handler):
    self.shapes.append(handler)
    return self

  # Returns the default registry that recognized the built-in shapes.
  @staticmethod
  def get_default():
    return _DEFAULT_SHAPE_REGISTRY


_DEFAULT_SHAPE_REGISTRY = (
  ShapeRegistry()
    .add_shape(BoxShape)
    .add_shape(TableShape))


## ## Diagram
##
## The diagram type encapsulates a processed diagram.

class Diagram(object):

  def __init__(self, origin):
    self.origin = origin
    self.regions = None

  def get_elements(self):
    assert not self.origin.elements is None
    return self.origin.elements

  # Returns the element that has a wall at (x, y). If there is no such element
  # None is returned.
  def get_element_under(self, x, y):
    for element in self.get_elements():
      text = element.get_text_component()
      (lx, ty) = text.get_position()
      if text.has_wall(x - lx, y - ty):
        return element
    return None

  def get_regions(self):
    if not self.regions is None:
      return self.regions
    assert not self.origin.regions is None
    self.regions = []
    for color in sorted(self.origin.regions.keys()):
      self.regions.append(self.origin.regions[color])
    return self.regions

## ## Geometry
##
## These are generic geometry types used to simplify the shape api.

class Rect(object):

  def __init__(self, top_left, bottom_right):
    self.top_left = top_left
    self.bottom_right = bottom_right

  def get_top_left(self):
    return self.top_left

  def get_bottom_right(self):
    return self.bottom_right

  def get_width(self):
    return self.bottom_right.get_x() - self.top_left.get_x()

  def get_height(self):
    return self.bottom_right.get_y() - self.top_left.get_y()

  def get_extent(self):
    return Point(self.get_width(), self.get_height())

  def __iter__(self):
    yield self.top_left
    yield self.bottom_right

  # Returns True iff this rectangle completely contains the given rect.
  def contains(self, that):
    return (self.top_left.x <= that.top_left.x
      and self.top_left.y <= that.top_left.y
      and that.bottom_right.x <= self.bottom_right.x
      and that.bottom_right.y <= self.bottom_right.y)

  # Returns true the given set of points contains a square at the position of
  # this rectangle.
  def is_square_within(self, points, allow_internal):
    width = self.get_width() - 1
    height = self.get_height() - 1
    top_left = self.get_top_left()
    # Scan through all the points within the bounds of the shape and check that
    # they form a box.
    for x in range(0, width + 1):
      for y in range(0, height + 1):
        expect_wall = (x == 0) or (x == width) or (y == 0) or (y == height)
        found_wall = (top_left.x + x, top_left.y + y) in points
        if expect_wall:
          if not found_wall:
            # If we expected a wall and there was none it's definitely not a
            # box.
            return False
        else:
          if (not allow_internal) and found_wall:
            # If we didn't expect a wall that's okay if allow_internal is True.
            # If it's not, however, we don't allow there to be walls were we
            # don't expect them.
            return False
    return True

  # Given a point (x, y) returns a new point that is relative to the top left
  # corner of this rect.
  def make_point_relative(self, (x, y)):
    return (x - self.top_left.x, y - self.top_left.y)

  @staticmethod
  def get_bounds_from_points(points):
    top_left_x = sys.maxint
    top_left_y = sys.maxint
    bottom_right_x = -sys.maxint
    bottom_right_y = -sys.maxint
    for (x, y) in points:
      top_left_x = min(top_left_x, x)
      top_left_y = min(top_left_y, y)
      # Nodes are considered to have width 1 so we add 1 to get their 
      # right/bottom bounds.
      bottom_right_x = max(bottom_right_x, x)
      bottom_right_y = max(bottom_right_y, y)
    return Rect(Point(top_left_x, top_left_y),
      Point(bottom_right_x + 1, bottom_right_y + 1))

  def __str__(self):
    return "r%s->%s" % (self.top_left, self.bottom_right)


class Point(object):

  def __init__(self, x, y):
    self.x = x
    self.y = y

  def get_x(self):
    return self.x

  def get_y(self):
    return self.y

  def __str__(self):
    return "(%s, %s)" % (self.x, self.y)

  def __iter__(self):
    yield self.x
    yield self.y


# An individual component within a text diagram. Provides access to information
# about the element completely separated from the rest of the diagram. Also,
# by default coordinates are relative to the element itself not the full
# diagram so the top left corner is at (0, 0) regardless of where in the diagram
# the element is located.
class TextComponent(object):

  def __init__(self, processor, absolute_bounds, relative_points):
    self.processor = processor
    self.absolute_bounds = absolute_bounds
    self.relative_points = relative_points
    self.relative_point_set = set(relative_points)

  # Returns the character at position (x, y) relative to the top left corner of
  # the element.
  def get_character(self, x, y):
    if not (x, y) in self.relative_point_set:
      return ' '
    else:
      (tx, ty) = self.absolute_bounds.get_top_left()
      return self.processor.get_character(tx + x, ty + y)

  # Does this element have a wall at relative position (x, y)?
  def has_wall(self, x, y):
    return (x, y) in self.relative_point_set

  # Returns the extent of this element, that is, a point giving the width and
  # height of the element.
  def get_extent(self):
    return self.absolute_bounds.get_extent()

  # Returns the (x, y) of the top left corner of this element within the full
  # diagram.
  def get_position(self):
    return self.absolute_bounds.get_top_left()

  def get_absolute_bounds(self):
    return self.absolute_bounds

  def __str__(self):
    (w, h) = self.absolute_bounds.get_extent()
    rows = []
    for y in range(0, h):
      col = []
      for x in range(0, w):
        col.append(self.get_character(x, y))
      rows.append("".join(col))
    return "\n".join(rows)
