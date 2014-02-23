#- Copyright 2013 GOTO 10.
#- Licensed under the Apache License, Version 2.0 (see LICENSE).

import setuptools

setuptools.setup(
  name = "Hemingway",
  version = "0.0.1",
  description = "",
  author = "Christian Plesner Hansen",
  url = "http://c7n.p5r.org",
  packages = setuptools.find_packages(),
  entry_points = {
    'console_scripts': [
      'hemingway = hemingway.main:main',
    ]
  },
  install_requires = ['markdown', 'pygments'],
  extras_require = {'watchdog': 'watchdog'},
  include_package_data = True
)
