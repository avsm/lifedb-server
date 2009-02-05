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
        self.username = os.environ.get('LIFEDB_TEST_USERNAME', 'foo')
        self.password = os.environ.get('LIFEDB_TEST_PASSWORD', 'bar')
        self.server = client.Server(uri)
        self.session = None
        
    def tearDown(self):
        pass

    def doLogin(self):
        self.server.login(self.username, self.password)
        self.assert_(self.server.session())  # session has been returned
        self.assert_(len(self.server.session()) == 36) # it is a UUID4
        self.assertEquals(self.server.session(), self.server.session().upper()) # in uppercase
        
    def test_login_success(self):
        self.doLogin()
        
    def test_login_failure(self):
        self.assertRaises(client.ResourceForbidden, self.server.login, 'bar', 'foo')
        self.assertRaises(client.ResourceForbidden, self.server.login, '', '')

    def test_logged_in_ping(self):
        self.doLogin()
        data = self.server.ping()
        self.assertEquals(data, "pong")
        
    def test_not_logged_in_ping(self):
        self.assertRaises(client.ResourceForbidden, self.server.ping)
        
    def test_logout(self):
        self.doLogin()
        data = self.server.ping ()
        self.assertEquals(data, "pong")
        self.server.logout()
        self.assertRaises(client.ResourceForbidden, self.server.ping)
        
def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(LoginTestCase, 'test'))
    #suite.addTest(doctest.DocTestSuite(client))
    return suite


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
