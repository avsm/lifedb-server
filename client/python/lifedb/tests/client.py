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

class LoginTestCase(unittest.TestCase):

    def setUp(self):
        uri = os.environ.get('LIFEDB_URI', client.DEFAULT_BASE_URI)
        self.server = client.Server(uri)

    def tearDown(self):
        pass

    def test_login_success(self):
        data = self.server.login('foo', 'bar')
        self.assert_('session' in data)  # session has been returned
        self.assert_(len(data['session']) == 36) # it is a UUID4
        self.assertEquals(data['session'], data['session'].upper()) # in uppercase
        return data
        
    def test_login_failure(self):
        self.assertRaises(client.ResourceForbidden, self.server.login, 'bar', 'foo')

def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(LoginTestCase, 'test'))
    #suite.addTest(doctest.DocTestSuite(client))
    return suite


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
