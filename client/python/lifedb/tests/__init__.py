# -*- coding: utf-8 -*-
#
# Copyright (C) 2007 Christopher Lenz
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

import unittest

from lifedb.tests import client

def suite():
    suite = unittest.TestSuite()
    suite.addTest(client.suite())
    return suite

if __name__ == '__main__':
    unittest.main(defaultTest='suite')
