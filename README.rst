===========================================================
 stacktest.el - in buffer testing of projects in OpenStack
===========================================================

OpenStack projects have a relatively well defined testing interface
that uses ``tox`` and ``testrepository``.

This is a mode to make testing using those be easily available inside
of emacs. This reuses the compile infrastructure in emacs and is based
upon nosemacs that does this same functionality for nose.

Installation
============

Add the following to your .emacs configuration::

  (require 'stacktest)
  ;; optionally enable for all python files
  (add-hook 'python-mode-hook 'stacktest-mode)

Usage
=====

From within a python file you can run any of the following:

- M-x stacktest-one : runs the current test function your cursor is in
- M-x stacktest-debug-one : runs the current test function your cursor
  is in with OS_DEBUG=True (turns on debug logging)
- M-x stacktest-pdb-one : runs the current test function your cursor
  is in pdb
- M-x stacktest-module : runs all the tests in the current file
- M-x stacktest-all : runs a full ``tox`` run in an emacs buffer

You can bind these to more convenient keys. The various modes are
extremely useful when iterating on test development. The compilation
buffer is annotated so that failure points are clickable back into the
code in question.

Assumptions
===========

When running in -one or -module mode the testing skips tox / testr
entirely and just uses subunit.run (if subunit-trace is available) or
testtools.run from the .tox/py27 directory. This means we assume
you've run ``tox -e py27`` at least once before running this.

Futures
=======

Things that would be nice to have (PRs welcomed)

- figure out if there is any way to do this over tramp
- further enhance the compilation buffer regexp to make it easier to
  understand at a glance
