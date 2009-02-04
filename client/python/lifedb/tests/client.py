# -*- coding: utf-8 -*-
#
# Copyright (C) 2007 Christopher Lenz
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

import doctest
import os
import unittest
import StringIO

from lifedb import client

class DatabaseTestCase(unittest.TestCase):

    def setUp(self):
        uri = os.environ.get('LIFEDB_URI', client.DEFAULT_BASE_URI)
        self.server = client.Server(uri)

    def tearDown(self):
        pass

    def test_login_success(self):
        data = self.server.login('foo', 'bar')
        self.assert_('session' in data)
        self.assert_(len(data['session']) == 36)
        
    def test_login_failure(self):
        self.assertRaises(client.ResourceForbidden, self.server.login, 'bar', 'foo')

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(DatabaseTestCase, 'test'))
    suite.addTest(doctest.DocTestSuite(client))
    return suite


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
