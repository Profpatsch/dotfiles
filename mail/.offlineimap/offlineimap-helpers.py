#!/usr/bin/python3

import re
import sys
import os
import keyring
import subprocess
# import getpass

# class Keyring(object):
#     def __init__(self, name, server, protocol):
#         self._name = name
#         self._server = server
#         self._protocol = protocol
#         self._keyring = 'imap'

#     # def has_credentials(self):
#     #     try:
#     #         attrs = {"server": self._server, "protocol": self._protocol}
#     #         items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
#     #         return len(items) > 0
#     #     except gkey.DeniedError:
#     #         return False

#     def get_password(self, user):
#         attrs = {"server": self._server, "protocol": self._protocol,
#                 "user": user}
#         items = gkey.find_items_sync(gkey.ITEM_NETWORK_PASSWORD, attrs)
#         return items[0].secret

#     def set_credentials(self, (user, pw)):
#         attrs = {
#                 "user": user,
#                 "server": self._server
#                 "protocol": self._protocol,
#             }
#         gkey.item_create_sync(self._keyring,
#                 gkey.ITEM_NETWORK_PASSWORD, self._name, attrs, pw, True)

def get_password(server, username):
    # print(server + " " + username)
    # pw = keyring.get_password(server, username)
    # return(pw)
    home = os.environ['HOME']
    pw = subprocess.check_output(["gpg2", "--no-tty", "-q", "-d", "{}/.config/mailpasswords/{}.asc".format(home, username)])
    return pw.strip()


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
