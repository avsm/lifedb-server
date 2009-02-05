# -*- coding: utf-8 -*-
#
# Copyright (C) 2009 Anil Madhavapeddy
# Copyright (C) 2007-2008 Christopher Lenz
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.
# 3. The name of the author may not be used to endorse or promote
#    products derived from this software without specific prior
#    written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
# GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
# IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
# IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""Python client API for LifeDB.

"""

import httplib2
from urllib import quote, urlencode
import re
import socket
try:
    import simplejson as json
except ImportError:
    import json # Python 2.6

__docformat__ = 'restructuredtext en'

DEFAULT_BASE_URI = 'http://localhost:5985/'


class ResourceNotFound(Exception):
    """Exception raised when a 404 HTTP error is received in response to a
    request.
    """

class ServerError(Exception):
    """Exception raised when an unexpected HTTP error is received in response
    to a request.
    """

class ResourceForbidden(Exception):
    """Exception raised when a login request fails or an invalid session is passed
    to a request.
    """

class Server(object):
    """Representation of a LifeDB server.
    """

    def __init__(self, uri=DEFAULT_BASE_URI, cache=None, timeout=None):
        """Initialize the server object.
        
        :param uri: the URI of the server (for example
                    ``http://localhost:5985/``)
        :param cache: either a cache directory path (as a string) or an object
                      compatible with the ``httplib2.FileCache`` interface. If
                      `None` (the default), no caching is performed.
        :param timeout: socket timeout in number of seconds, or `None` for no
                        timeout
        """
        http = httplib2.Http(cache=cache, timeout=timeout)
        http.force_exception_to_status_code = False
        self.resource = Resource(http, uri)

    def __nonzero__(self):
        """Return whether the server is available."""
        try:
            self.resource.head()
            return True
        except:
            return False

    def __repr__(self):
        return '<%s %r>' % (type(self).__name__, self.resource.uri)

    def _get_version(self):
        resp, data = self.resource.get()
        return data['version']
    version = property(_get_version, doc="""\
        The version number tuple for the LifeDB server.

        Note that this results in a request being made, and can also be used
        to check for the availability of the server.
        
        :type: `unicode`
        """)

    def login(self, username, password):
        resp, data = self.resource.post('/login', content={ 'username' : username, 'password' : password } )
        self.resource.session = data['session']
       
    def logout(self):
        if self.resource.session:
            resp, data = self.resource.post('/logout', content={})
            self.resource.session = None
 
    def ping(self):
        resp, data = self.resource.get(path='/ping')
        return data
        
    def session(self):
        return self.resource.session
        
# Internals


class Resource(object):

    def __init__(self, http, uri):
        if http is None:
            http = httplib2.Http()
            http.force_exception_to_status_code = False
        self.http = http
        self.uri = uri
        self.session = None

    def __call__(self, path):
        return type(self)(self.http, uri(self.uri, path))

    def delete(self, path=None, headers=None, **params):
        return self._request('DELETE', path, headers=headers, **params)

    def get(self, path=None, headers=None, **params):
        return self._request('GET', path, headers=headers, **params)

    def head(self, path=None, headers=None, **params):
        return self._request('HEAD', path, headers=headers, **params)

    def post(self, path=None, content=None, headers=None, **params):
        return self._request('POST', path, content=content, headers=headers,
                             **params)

    def put(self, path=None, content=None, headers=None, **params):
        return self._request('PUT', path, content=content, headers=headers,
                             **params)

    def _request(self, method, path=None, content=None, headers=None,
                 **params):
        from lifedb import __version__
        headers = headers or {}
        headers.setdefault('Accept', 'application/json')
        headers.setdefault('User-Agent', 'lifedb-python %s' % __version__)
        if self.session:
            headers.setdefault('Session', self.session)
        body = None
        if content is not None:
            if not isinstance(content, basestring):
                body = json.dumps(content, allow_nan=False,
                                  ensure_ascii=False).encode('utf-8')
                headers.setdefault('Content-Type', 'application/json')
            else:
                body = content
            headers.setdefault('Content-Length', str(len(body)))

        def _make_request(retry=1):
            try:
                return self.http.request(uri(self.uri, path, **params), method,
                                             body=body, headers=headers)
            except socket.error, e:
                if retry > 0 and e.args[0] == 54: # reset by peer
                    return _make_request(retry - 1)
                raise
        resp, data = _make_request()

        status_code = int(resp.status)
        if data and resp.get('content-type') == 'application/json':
            try:
                data = json.loads(data)
            except ValueError:
                pass

        if status_code >= 400:
            if type(data) is dict:
                error = (data.get('error'), data.get('reason'))
            else:
                error = data
            if status_code == 404:
                raise ResourceNotFound(error)
            elif status_code == 403:
                raise ResourceForbidden(error)
            else:
                raise ServerError((status_code, error))

        return resp, data


def uri(base, *path, **query):
    """Assemble a uri based on a base, any number of path segments, and query
    string parameters.

    >>> uri('http://example.org/', '/_all_dbs')
    'http://example.org/_all_dbs'
    """
    if base and base.endswith('/'):
        base = base[:-1]
    retval = [base]

    # build the path
    path = '/'.join([''] +
                    [unicode_quote(s.strip('/')) for s in path
                     if s is not None])
    if path:
        retval.append(path)

    # build the query string
    params = []
    for name, value in query.items():
        if type(value) in (list, tuple):
            params.extend([(name, i) for i in value if i is not None])
        elif value is not None:
            if value is True:
                value = 'true'
            elif value is False:
                value = 'false'
            params.append((name, value))
    if params:
        retval.extend(['?', unicode_urlencode(params)])

    return ''.join(retval)


def unicode_quote(string, safe=''):
    if isinstance(string, unicode):
        string = string.encode('utf-8')
    return quote(string, safe)


def unicode_urlencode(data):
    if isinstance(data, dict):
        data = data.items()
    params = []
    for name, value in data:
        if isinstance(value, unicode):
            value = value.encode('utf-8')
        params.append((name, value))
    return urlencode(params)

