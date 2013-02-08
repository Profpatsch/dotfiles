PLUGIN_NAME = u"The Sorting Plugin"
PLUGIN_AUTHOR = u"Aaron Lambers"
PLUGIN_DESCRIPTION = ""
PLUGIN_VERSION = "0.2"
PLUGIN_API_VERSIONS = ["0.9.0", "0.10"]


from PyQt4 import QtCore
from picard.file import File
from picard.album import Album
from picard.cluster import Cluster
from picard.util import format_time
from picard.ui.itemviews import BaseAction, register_album_action, register_cluster_action



class SortAlbumsByArtist(BaseAction):
    NAME = "Sort Albums by Artist"

    def callback(self, objs):
        if len(objs) != 1 or not isinstance(objs[0], Album):
            return
        album = objs[0]

        # Temporary list for sorting.
        albums = []

        # Make a copy of all the data in the albums
        for album in self.tagger.albums:
            albums.append(album)

        # Delete all the albums from the AlbumTreeView.  We only have to
        # manipulate the display and not the actual album data.  Use the
        # temp list as the album reference to avoid iterator related problems.
        for album in albums:
            self.tagger.window.panel.views[1].remove_album(album)

        # Sort the list.
        albums.sort(key=lambda album: (album.metadata["artist"], album.metadata[u"album"]))

        # Copy all the data back to the AlbumTreeView
        for album in albums:
            self.tagger.window.panel.views[1].add_album(album)
            self.tagger.window.panel.views[1].update_album(album)

register_album_action(SortAlbumsByArtist())



class SortAlbumsByTitle(BaseAction):
    NAME = "Sort Albums by Title"

    def callback(self, objs):
        if len(objs) != 1 or not isinstance(objs[0], Album):
            return
        album = objs[0]

        # Temporary list for sorting.
        albums = []

        # Make a copy of all the data in the albums
        for album in self.tagger.albums:
            albums.append(album)

        # Delete all the albums from the AlbumTreeView.  We only have to
        # manipulate the display and not the actual album data.  Use the
        # temp list as the album reference to avoid iterator related problems.
        for album in albums:
            self.tagger.window.panel.views[1].remove_album(album)

        # Sort the list.
        albums.sort(key=lambda album: (album.metadata[u"album"], album.metadata["artist"]))

        # Copy all the data back to the AlbumTreeView
        for album in albums:
            self.tagger.window.panel.views[1].add_album(album)
            self.tagger.window.panel.views[1].update_album(album)

register_album_action(SortAlbumsByTitle())



class SortFilesByArtist(BaseAction):
    NAME = "Sort Files by Artist"

    def callback(self, objs):
        if len(objs) != 1 or not isinstance(objs[0], Cluster):
            return
        cluster = objs[0]

        # Temporary list for sorting.
        files = []

        # Create a placeholder file to keep the cluster from being deleted
        # when all the original files are removed.
        tmpFile = File("tmpFile")


        # Make a copy of all the file data in the cluster
        for file in cluster.files:
            files.append(file)

        # Add the paceholder.
        cluster.add_file(tmpFile)

        # Delete all the files from the cluster.  Use the temp list as the
        # file reference to avoid iterator related problems.
        for file in files:
            cluster.remove_file(file)

        # Sort the list.
        files.sort(key=lambda file: (file.metadata["artist"], file.metadata["title"]))

        # Copy all the data back to the cluster
        for file in files:
            cluster.add_file(file)

        # Remove the placeholder
        cluster.remove_file(tmpFile)

register_cluster_action(SortFilesByArtist())



class SortFilesByTitle(BaseAction):
    NAME = "Sort Files by Title"

    def callback(self, objs):
        if len(objs) != 1 or not isinstance(objs[0], Cluster):
            return
        cluster = objs[0]

        # Temporary list for sorting.  Working on the original cluster file
        # list doesn't seem to work.
        files = []

        # Create a placeholder file to keep the cluster from being deleted
        # when all the original files are removed.
        tmpFile = File("tmpFile")


        # Make a copy of all the file data in the cluster
        for file in cluster.files:
            files.append(file)

        # Add the paceholder.
        cluster.add_file(tmpFile)

        # Delete all the files from the cluster.  Use the temp list as the
        # file reference to avoid iterator related problems.
        for file in files:
            cluster.remove_file(file)

        # Sort the list.
        files.sort(key=lambda file: (file.metadata["title"], file.metadata["artist"]))

        # Copy all the data back to the cluster
        for file in files:
            cluster.add_file(file)

        # Remove the placeholder
        cluster.remove_file(tmpFile)

register_cluster_action(SortFilesByTitle())






class SortClustersByArtist(BaseAction):
    NAME = "Sort Clusters by Artist"

    def callback(self, objs):
        if len(objs) != 1 or not isinstance(objs[0], Cluster):
            return
        cluster = objs[0]

        # Temporary list for sorting.
        clusters = []

        # Make a copy of all the data in the clusters.
        for cluster in self.tagger.clusters:
            clusters.append(cluster)

        # Delete all the clusters from the cluster list.  Use the temp list
        # as the cluster reference to avoid iterator related problems.
        for cluster in clusters:
            self.tagger.window.panel.views[0].remove_cluster(cluster)

        # Sort the list.
        clusters.sort(key=lambda album: (album.metadata["artist"], album.metadata[u"album"]))

        # Copy all the data back to the cluster list.
        for cluster in clusters:
            self.tagger.window.panel.views[0].add_cluster(cluster)

register_cluster_action(SortClustersByArtist())




class SortClustersByTitle(BaseAction):
    NAME = "Sort Clusters by Title"

    def callback(self, objs):
        if len(objs) != 1 or not isinstance(objs[0], Cluster):
            return
        cluster = objs[0]

        # Temporary list for sorting.
        clusters = []

        # Make a copy of all the data in the clusters.
        for cluster in self.tagger.clusters:
            clusters.append(cluster)

        # Delete all the clusters from the cluster list.  Use the temp list
        # as the cluster reference to avoid iterator related problems.
        for cluster in clusters:
            self.tagger.window.panel.views[0].remove_cluster(cluster)

        # Sort the list.
        clusters.sort(key=lambda album: (album.metadata[u"album"], album.metadata["artist"]))

        # Copy all the data back to the AlbumTreeView
        for cluster in clusters:
            self.tagger.window.panel.views[0].add_cluster(cluster)

register_cluster_action(SortClustersByTitle())

