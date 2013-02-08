# -*- coding: utf-8 -*-

PLUGIN_NAME = u'MetaFlac ReplayGain'
PLUGIN_AUTHOR = u'Olivier Kaloudoff (based on Johannes Weißl)'
PLUGIN_DESCRIPTION = '''Analyse your albums using the MetaFlac ReplayGain scanner.'''
PLUGIN_VERSION = "0.2"
PLUGIN_API_VERSIONS = ["0.10", "0.15"] 

import os
import sys
import subprocess
from subprocess import Popen, PIPE

from PyQt4 import QtCore, QtGui

from picard.album import Album
from picard.track import Track
from picard.file import File
from picard.ui.options import register_options_page, OptionsPage
from picard.ui.itemviews import (BaseAction, register_file_action, register_album_action)
from picard.config import TextOption


def get_metaflac_path():
    # Linux paths are not Windows paths
    p = ''
    if os.name == 'posix':
        p = os.path.expanduser('/sw/bin/metaflac')
        if not os.path.exists(p):     p = ''
    return p


class Ui_MetaFlacReplayGainOptionsPage(object):
    def setupUi(self, MetaFlacReplayGainOptionsPage):
        MetaFlacReplayGainOptionsPage.setObjectName("MetaFlacReplayGainOptionsPage")
        MetaFlacReplayGainOptionsPage.resize(394, 300)
        self.verticalLayout = QtGui.QVBoxLayout(MetaFlacReplayGainOptionsPage)
        self.verticalLayout.setObjectName("verticalLayout")
        self.groupBox = QtGui.QGroupBox(MetaFlacReplayGainOptionsPage)
        self.groupBox.setObjectName("groupBox")
        self.vboxlayout = QtGui.QVBoxLayout(self.groupBox)
        self.vboxlayout.setObjectName("vboxlayout")
        self.label = QtGui.QLabel(self.groupBox)
        self.label.setObjectName("label")
        self.vboxlayout.addWidget(self.label)
        self.horizontalLayout = QtGui.QHBoxLayout()
        self.horizontalLayout.setObjectName("horizontalLayout")
        self.metaflac_path = QtGui.QLineEdit(self.groupBox)
        self.metaflac_path.setObjectName("metaflac_path")
        self.horizontalLayout.addWidget(self.metaflac_path)
        self.MetaFlac_browse = QtGui.QPushButton(self.groupBox)
        self.MetaFlac_browse.setObjectName("MetaFlac_browse")
        self.horizontalLayout.addWidget(self.MetaFlac_browse)
        self.vboxlayout.addLayout(self.horizontalLayout)
        self.verticalLayout.addWidget(self.groupBox)
        spacerItem = QtGui.QSpacerItem(368, 187, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.verticalLayout.addItem(spacerItem)

        self.retranslateUi(MetaFlacReplayGainOptionsPage)
        QtCore.QMetaObject.connectSlotsByName(MetaFlacReplayGainOptionsPage)

        self.MetaFlac_browse.clicked.connect(self.selectFile)

    def retranslateUi(self, MetaFlacReplayGainOptionsPage):
        self.groupBox.setTitle(QtGui.QApplication.translate("MetaFlacReplayGainOptionsPage", "MetaFlac ReplayGain Scanner", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("MetaFlacReplayGainOptionsPage", _("Path to metaflac executable"), None, QtGui.QApplication.UnicodeUTF8))
        self.MetaFlac_browse.setText(QtGui.QApplication.translate("MetaFlacReplayGainOptionsPage", _("Browse..."), None, QtGui.QApplication.UnicodeUTF8))

    def selectFile(self):
        dir = os.path.dirname(unicode(self.metaflac_path.text()))
        dir = dir if os.path.exists(dir) else ''
        path = QtGui.QFileDialog.getOpenFileName(directory=dir)
        if path:
            self.metaflac_path.setText(path)

io_enc = sys.getfilesystemencoding()
def encode_cmd(cmd):
    # passing unicode to Popen doesn't work in Windows
    if not os.path.supports_unicode_filenames or os.name == 'nt':
        cmd = [x.encode(io_enc) if isinstance(x, unicode) else x for x in cmd]
    return cmd

def run_MetaFlac(mode, files, tagger):
    metaflac_mode = {
        '--add-replay-gain':     'Scan per-file track gain',
        '--remove-replay-gain':  'Remove ReplayGain information from files',
    }
    metaflac_binary = tagger.config.setting['metaflac_rgscan_metaflac_path']
    MetaFlac = [metaflac_binary, mode]
    cmd = []
    if os.name == 'posix':
        cmd += MetaFlac + files
    
    Popen(encode_cmd(cmd))

def get_files(objs):
    files = []
    for obj in objs:
        if isinstance(obj, Album) or isinstance(obj, Track):
            for f in obj.iterfiles():
                files.append(f.filename)
        elif isinstance(obj, File):
            files.append(obj.filename)
    return files

class MetaFlacReplayGainScanAlbumByTags(BaseAction):
    NAME = _("MetaFlac: &Scan selection as albums (by tags)...")
    def callback(self, objs):
        run_MetaFlac('--add-replay-gain', get_files(objs), self.tagger)

class MetaFlacReplayGainScanTrack(BaseAction):
    NAME = _("MetaFlac:: &Scan per-file track gain...")
    def callback(self, objs):
        run_MetaFlac('--add-replay-gain', get_files(objs), self.tagger)

class MetaFlacReplayGainRemove(BaseAction):
    NAME = _("MetaFlac:: &Remove ReplayGain information from files...")
    def callback(self, objs):
        run_MetaFlac('--remove-replay-gain', get_files(objs), self.tagger)

class MetaFlacReplayGainOptionsPage(OptionsPage):
    NAME = "metaflac_rgscan"
    TITLE = "MetaFlac ReplayGain"
    PARENT = "plugins"

    options = [
        TextOption('setting', 'metaflac_rgscan_metaflac_path', get_metaflac_path()),
    ]

    def __init__(self, parent=None):
        super(MetaFlacReplayGainOptionsPage, self).__init__(parent)
        self.ui = Ui_MetaFlacReplayGainOptionsPage()
        self.ui.setupUi(self)

    def load(self):
        self.ui.metaflac_path.setText(self.config.setting['metaflac_rgscan_metaflac_path'])

    def save(self):
        self.config.setting['metaflac_rgscan_metaflac_path'] = unicode(self.ui.metaflac_path.text())

register_file_action(MetaFlacReplayGainScanTrack())
register_file_action(MetaFlacReplayGainRemove())
register_album_action(MetaFlacReplayGainScanAlbumByTags())
register_album_action(MetaFlacReplayGainRemove())
register_options_page(MetaFlacReplayGainOptionsPage)
