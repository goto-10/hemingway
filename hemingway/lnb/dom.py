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


# Abstract superclass of diagram shapes.
class Shape(object):
  __metaclass__ = ABCMeta
  
  IRREGULAR = 'irregular'

  def __init__(self, origin, nodes):
    self.origin = origin
    self.nodes = nodes
    self.bounds_cache = None
    self.walls_cache = None

  # Returns a string that identifies the type of this shape.
  @abstractmethod
  def get_type(self):
    return None

  # Returns a bounding rectangle that fully contains this shape.
  def get_bounds(self):
    if self.bounds_cache is None:
      self.bounds_cache = Rect.get_bounds_from_points([(n.x, n.y) for n in self.nodes])
    return self.bounds_cache

  # Returns a WallMap that describes the walls of this shape.
  def get_walls(self):
    if self.walls_cache is None:
      self.walls_cache = WallMap(set([(n.x, n.y) for n in self.nodes]))
    return self.walls_cache

  # Returns an array of this shape's nodes.
  def get_nodes(self):
    return self.nodes


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

  def __init__(self, origin, nodes, bounds):
    super(BoxShape, self).__init__(origin, nodes)
    self.bounds = bounds

  def get_type(self):
    return BoxShape.TYPE

  # Returns a rect that describes the bounds of this box.
  def get_bounds(self):
    return self.bounds

  @staticmethod
  def try_resolve(origin, proto):
    return BoxShape.try_create(origin, proto, False)

  # Attempts to create a box shape from the given nodes. If the nodes are not
  # a box None will be returned.
  @staticmethod
  def try_create(origin, proto, allow_internal):
    bounds = proto.get_bounds()
    walls = proto.get_walls()
    width = bounds.get_width() - 1
    height = bounds.get_height() - 1
    top_left = bounds.get_top_left()
    # Scan through all the points within the bounds of the shape and check that
    # they form a box.
    for x in range(0, width + 1):
      for y in range(0, height + 1):
        expect_wall = (x == 0) or (x == width) or (y == 0) or (y == height)
        found_wall = walls.has_wall(top_left.x + x, top_left.y + y)
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
    return BoxShape(origin, proto.get_nodes(), bounds)


class TableShape(Shape):

  TYPE = 'table'

  def __init__(self, origin, nodes, boundary, cells):
    super(TableShape, self).__init__(origin, nodes)
    self.boundary = boundary
    self.cells = cells

  def get_type(self):
    return TableShape.TYPE

  def get_boundary(self):
    return self.boundary

  @staticmethod
  def try_resolve(origin, proto):
    # Is the shape completely enclosed within a box?
    boundary = BoxShape.try_create(origin, proto, True)
    if boundary is None:
      return None
    # Are all the regions within the table rectangular?
    regions = origin.get_diagram().get_regions()
    cells = []
    for region in regions:
      if not proto.get_bounds().contains(region.get_bounds()):
        continue
      if region.is_square():
        cells.append(region)
      else:
        return None
    return TableShape(origin, proto.get_nodes(), boundary, cells)

  # Returns the list of cells within this table.
  def get_cells(self):
    return self.cells


# A shape that's not recognized as any other "nice" form.
class IrregularShape(Shape):

  def get_type(self):
    return None


# This type is used temporarily while determining which kind of shape a set of
# nodes correspond to.
class UnknownShape(Shape):
  
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
  def resolve(self, origin, proto):
    for ShapeType in self.shapes:
      result = ShapeType.try_resolve(origin, proto)
      if not result is None:
        return result
    return IrregularShape(origin, proto)

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

  def get_shapes(self):
    assert not self.origin.shapes is None
    return self.origin.shapes

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


# A bitmap that knows where the walls are in a shape.
class WallMap(object):

  def __init__(self, walls):
    self.walls = walls

  def has_wall(self, x, y):
    return (x, y) in self.walls
