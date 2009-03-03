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
import time

from lifedb import client

class BaseTestCase(unittest.TestCase):
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

class LoginTestCase(BaseTestCase):
        
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

class TasksTestCase(BaseTestCase):
    def test_task_create(self):
        self.doLogin()
        self.server.task_create("foo","single","echo hello")
        tasks = self.server.task_list()
        self.assert_('foo' in tasks)
        self.assertEquals(tasks['foo']['cmd'], 'echo hello')
        self.assertEquals(tasks['foo']['mode'], 'single')
        self.destroy_and_check("foo")
        self.server.logout()

    def destroy_and_check(self, name):
        self.server.task_destroy(name)
        tasks = self.server.task_list ()
        self.assert_('foo' not in tasks)
 
    def test_task_create_not_logged_in(self):
        self.assertRaises(client.ResourceForbidden, self.server.task_create, 
             "foo", "single", "echo hello")
    
    def long_test_task_periodic_create(self):
        self.doLogin()
        period=3
        self.server.task_create("bar","periodic","echo ppp",period=period)
        tasks = self.server.task_list()
        self.assert_('bar' in tasks)
        self.assertEquals(tasks['bar']['mode'], 'periodic')
        self.assertEquals(tasks['bar']['cmd'], 'echo ppp')
        self.assertEquals(tasks['bar']['period'], period)
        time.sleep(period*4+1)
        self.destroy_and_check("bar")
        self.server.logout()

    def test_task_constant_create(self):
        self.doLogin()
        self.server.task_create("foo","constant","echo daemon")
        tasks = self.server.task_list()
        self.assert_('foo' in tasks)
        self.assertEquals(tasks['foo']['mode'], 'constant')
        self.destroy_and_check('foo')
        self.server.logout()

    def test_task_get(self):
        self.doLogin()
        self.server.task_create("xxx","single", "sleep 10000")
        task = self.server.task_get("xxx")
        self.assertEquals(task['cmd'], 'sleep 10000')
        self.destroy_and_check('xxx')
        self.server.logout()

    def test_task_negative_get(self):
        self.doLogin()
        self.assertRaises(client.ResourceNotFound, self.server.task_get, "nonexistent")
        self.server.logout()

    def test_task_get_not_logged_in(self):
        self.assertRaises(client.ResourceForbidden, self.server.task_get, "nonexistent")

    def test_task_create_invalid(self):
        self.doLogin()
        self.assertRaises(client.ServerError, self.server.task_create, 
            'invalid', 'xxx', 'echo whatever')
        self.server.logout()
        
    def long_test_task_overload(self):
        max_tasks = 10
        self.doLogin()
        for t in range(max_tasks):
            self.server.task_create("foo%d" % t, "single", "sleep 10000%d" % t)
        for t in range(5):
            self.assertRaises(client.ServerError, self.server.task_create, 
                "bar", "single", "echo fail")
        for t in range(max_tasks):
            self.server.task_destroy("foo%d" % t)
        self.server.logout()

    def very_long_test_task_fd_leak(self):
        self.doLogin()
        for t in range(2000):
           self.server.task_create("foo", "single", "echo foo %s" % t)
           self.server.task_create("bar", "single", "false")
           self.server.task_destroy("foo")
           self.server.task_destroy("bar")
        self.server.logout()
   
def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(LoginTestCase, 'test'))
    suite.addTest(unittest.makeSuite(TasksTestCase, 'test'))
    suite.addTest(unittest.makeSuite(TasksTestCase, 'long_test'))
    #suite.addTest(unittest.makeSuite(TasksTestCase, 'very_long_test'))
    return suite


if __name__ == '__main__':
    unittest.main(defaultTest='suite')
