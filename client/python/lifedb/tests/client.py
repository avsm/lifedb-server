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
import tempfile

from lifedb import client
import httplib2
httplib2.debuglevel = 0

class BaseTestCase(unittest.TestCase):
    def setUp(self):
        self.uri = os.environ.get('LIFEDB_URI', client.DEFAULT_BASE_URI)
        self.username = os.environ.get('LIFEDB_TEST_USERNAME', 'foo')
        self.password = os.environ.get('LIFEDB_TEST_PASSWORD', 'bar')

    def tearDown(self):
        pass

class LoginOKTestCase(BaseTestCase):
    def setUp(self):
        BaseTestCase.setUp(self)
        self.server = client.Server(self.username, self.password, uri=self.uri)

class LoginBadTestCase(BaseTestCase):
    def setUp(self):
        BaseTestCase.setUp(self)
        self.server = client.Server("BADUSERNAME", "BADPASSWD", uri=self.uri)
        
class BasicPassTestCase(LoginOKTestCase):
    def test_logged_in_ping(self):
        data = self.server.ping()
        self.assertEquals(data, "pong")

class BasicFailTestCase(LoginBadTestCase):
    def test_not_logged_in_ping(self):
        self.assertRaises(client.ResourceForbidden, self.server.ping)
        
class TasksPassTestCase(LoginOKTestCase):
    def test_task_create(self):
        self.server.task_create("foo","Dummy","single","dummy", args={'WANTSLEEP':'100'})
        tasks = self.server.task_list()
        self.assert_('foo' in tasks)
        self.destroy_and_check("foo")

    def destroy_and_check(self, name):
        self.server.task_destroy(name)
        tasks = self.server.task_list ()
        self.assert_('foo' not in tasks)
 
    def long_test_task_periodic_create(self):
        period=3
        tmp = tempfile.NamedTemporaryFile()
        args = { 'TMPFILELOC' : tmp.name }
        cmd="echo foo >> %s" % tmp.name
        self.server.task_create("bar","Dummy","periodic","",period=period, args=args)
        tasks = self.server.task_list()
        self.assert_('bar' in tasks)
        time.sleep(period*4+1)
        self.destroy_and_check("bar")
        f = open(tmp.name, 'r')
        lines = map(str.strip, f.readlines())
        f.close()
        tmp.close()
        print lines
        self.assertEquals(lines,['foo','foo','foo','foo'])

    def test_task_constant_create(self):
        self.server.task_create("foo","Dummy","constant","",args={'WANTSLEEP':'100'})
        tasks = self.server.task_list()
        self.assert_('foo' in tasks)
        self.assertEquals(tasks['foo']['info']['mode'], 'constant')
        self.destroy_and_check('foo')

    def test_task_get(self):
        self.server.task_create("xxx", "Dummy", "single", "", args={'WANTSLEEP':'50'})
        task = self.server.task_get("xxx")
        self.assertEquals(task['info']['plugin'], 'Dummy')
        self.destroy_and_check('xxx')

    def test_task_negative_get(self):
        self.assertRaises(client.ResourceNotFound, self.server.task_get, "nonexistent")

    def test_task_create_invalid(self):
        self.assertRaises(client.ServerError, self.server.task_create, 
            'invalid', 'xxx', 'yyy', '')
        
    def long_test_task_overload(self):
        max_tasks = 10
        for t in range(max_tasks):
            self.server.task_create("foo%d" % t, "Dummy", "single","", args={'WANTSLEEP':'100000'}) 
        for t in range(5):
            self.assertRaises(client.ServerError, self.server.task_create, 
                "bar", "Dummy", "single", "")
        for t in range(max_tasks):
            self.server.task_destroy("foo%d" % t)

    def very_long_test_task_fd_leak(self):
        for t in range(2000):
           self.server.task_create("foo", "Dummy", "single", "", args={'WANTSLEEP':'100'})
           self.server.task_create("bar", "Dummy", "single", "", args={'WANTSLEEP':'100'})
           self.server.task_destroy("foo")
           self.server.task_destroy("bar")

class TasksFailTestCase(LoginBadTestCase):
    def test_task_create_not_logged_in(self):
        self.assertRaises(client.ResourceForbidden, self.server.task_create, 
             "foo", "Dummy", "single", "")
    
    def test_task_get_not_logged_in(self):
        self.assertRaises(client.ResourceForbidden, self.server.task_get, "nonexistent")

class PasswordPassTestCase(LoginOKTestCase):
    def test_passwd_create(self):
        username="notsecret"
        password="verysecret"
        service="arandomwebsite"
        self.server.password_create(service, username, password)
        rpass = self.server.password_get(service, username)
        self.assertEqual(rpass, password)
        self.server.password_delete(service, username)
        self.assertRaises(client.ResourceNotFound, self.server.password_get, username, password)

class PasswordFailTestCase(LoginBadTestCase):
    def test_passwd_create(self):
        self.assertRaises(client.ResourceForbidden, self.server.password_create, 'x','x','x')
        self.assertRaises(client.ResourceForbidden, self.server.password_get, 'x', 'x')
        self.assertRaises(client.ResourceForbidden, self.server.password_delete, 'x', 'x')
        
def suite():
    suite = unittest.TestSuite()
    suite.addTest(unittest.makeSuite(PasswordPassTestCase, 'test'))
    suite.addTest(unittest.makeSuite(PasswordFailTestCase, 'test'))
    suite.addTest(unittest.makeSuite(BasicPassTestCase, 'test'))
    suite.addTest(unittest.makeSuite(BasicFailTestCase, 'test'))
    suite.addTest(unittest.makeSuite(TasksPassTestCase, 'test'))
    suite.addTest(unittest.makeSuite(TasksFailTestCase, 'test'))
    suite.addTest(unittest.makeSuite(TasksPassTestCase, 'long_test'))
    #suite.addTest(unittest.makeSuite(TasksPassTestCase, 'very_long_test'))
    return suite

if __name__ == '__main__':
    unittest.main(defaultTest='suite')
