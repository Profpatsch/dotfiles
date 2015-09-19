#!/usr/bin/python3

import re
import sys
import subprocess

def get_password(server, username):
    import os
    import os.path
    import fcntl
    import hashlib

    home = os.path.expanduser("~")
    password_file = "{}/.config/mailpasswords/{}.asc".format(home, username)

    # create dir for cache
    cachedir = os.getenv("XDG_CACHE_HOME", os.path.join(home, ".cache/unlock-gpg-keys"))
    try:
        os.makedirs(cachedir)
    except:
        pass
    # create file lock for the password file
    cachefile = "{}/{}".format(cachedir, hashlib.sha256(password_file).hexdigest())
    with open(cachefile, 'w') as f:
        print("file {} opened".format(cachefile))
        fcntl.flock(f, fcntl.LOCK_EX)
        print("file {} locked".format(cachefile))
        pw = decrypt_file(password_file)
        print("password read from {}".format(password_file))
        return pw.strip()

def decrypt_file(password_file):
    import gpgme
    import io
    with open(password_file, 'rb') as f:
        c = gpgme.Context()
        plain = io.BytesIO()
        encrypted = io.BytesIO(f.read())
        c.decrypt(encrypted, plain)
        return plain.getvalue().decode()


def set_password(server, username, password):
    # keyring.set_password(server, username, password)
    raise NotImplemented()

# def oimaptransfolder_acc1(foldername):
#     if(foldername == "INBOX"):
#         retval = "acc1"
#     else:
#         retval = "acc1." + foldername
#     retval = re.sub("/", ".", retval)
#     return retval
    
# def localtransfolder_acc1(foldername):
#     if(foldername == "acc1"):
#         retval = "INBOX"
#     else:
#         # remove leading 'acc1.'
#         retval = re.sub("acc1\.", "", foldername)
#     retval = re.sub("\.", "/", retval)
#     return retval
