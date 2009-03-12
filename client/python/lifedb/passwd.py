from lifedb import client
import getopt
import getpass
import sys

def usage(exit_code):
    print "Usage: %s -u <lifedb_user> [-s lifedb_uri] -n <username> -x <service> [-d]" % sys.argv[0]
    print "Default is to set the password or [-d] to delete it"
    exit(exit_code)

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "hu:n:x:ds:")
    except getopt.GetoptError, err:
        print str(err)
        usage(2)
    service=None
    username=None
    server_username = None
    uri=client.DEFAULT_BASE_URI
    mode="set"
    for o, a in opts:
        if o == "-n":
            username = a
        elif o == '-x':
            service = a
        elif o == '-u':
            server_username = a
        elif o == '-s':
            uri = a
        elif o == "-d":
            mode = "del"
        elif o == '-h':
            usage(0)
        else:
            assert False, "unhandled option"
    if service == None or username == None or server_username == None:
        usage(1)
    if mode == "set":
        passwd = getpass.getpass("LifeDB Server Password: ")
        s = client.Server(server_username, passwd, uri=uri)
        spasswd = getpass.getpass("Password for %s@%s: " % (username, service))
        try:
            s.password_create(service, username, spasswd)
        except client.ResourceForbidden,e :
            print "Incorrect LifeDB server password."
            exit(1)

if __name__ == "__main__":
    main()

