import os
import shutil
import snowsaw
import platform
from socket import gethostname
from distutils.dir_util import copy_tree


class CopyExit(Exception):
    pass


class Copy(snowsaw.Plugin):
    """
    Core plugin to copy dotfiles (for non version controlled files).
    """

    _directive = "copy"

    def can_handle(self, directive):
        return directive == self._directive

    def handle(self, directive, data):
        if directive != self._directive:
            raise ValueError(
                'Core plugin "Copy" cannot handle the directive "{}"'.format(directive)
            )
        return self._process_copies(data)

    def _process_copies(self, copies):
        """
        Processes specified copies.

        :param copies: The copies to process
        :return: True if the copies have been processed successfully, False otherwise
        """
        success = True
        defaults = self._context.defaults().get("copy", {})
        hostname = gethostname()
        platform_name = platform.system()
        for destination, source in copies.items():
            destination = os.path.expandvars(destination)
            relative = defaults.get("relative", False)
            force = defaults.get("force", False)
            recopy = defaults.get("recopy", False)
            create = defaults.get("create", False)
            hosts = defaults.get("hosts", {})
            platforms = defaults.get("platforms", {})
            if isinstance(source, dict):
                relative = source.get("relative", relative)
                force = source.get("force", force)
                recopy = source.get("recopy", recopy)
                create = source.get("create", create)
                path = self._default_source(destination, source.get("path"))
                hosts = source.get("hosts", hosts)
                platforms = source.get("platforms", platforms)
            else:
                path = self._default_source(destination, source)
            path = os.path.expandvars(os.path.expanduser(path))

            try:
                if hosts:
                    host_config_found = False
                    for host in hosts.items():
                        if not host[0] == hostname:
                            self._log.lowinfo(
                                "Skipped host specific copy {} -> {}".format(
                                    destination,
                                    os.path.join(
                                        self._context.snowblock_dir(), host[1]
                                    ),
                                )
                            )
                            continue
                        else:
                            path = os.path.expandvars(
                                os.path.expanduser(hosts.get(hostname))
                            )
                            host_config_found = True
                    if not host_config_found:
                        raise CopyExit()

                if platforms:
                    plat_config_found = False
                    for plat in platforms.items():
                        if not plat[0] == platform_name:
                            self._log.lowinfo(
                                "Skipped platform specific copy {} -> {}".format(
                                    destination,
                                    os.path.join(
                                        self._context.snowblock_dir(), plat[1]
                                    ),
                                )
                            )
                            continue
                        else:
                            plat_config_found = True
                    if not plat_config_found:
                        raise CopyExit()
            except CopyExit:
                continue

            if not self._exists(os.path.join(self._context.snowblock_dir(), path)):
                success = False
                self._log.warning(
                    "Nonexistent target {} -> {}".format(destination, path)
                )
                continue

            if create:
                success &= self._create(destination)
            if force or recopy:
                success &= self._delete(path, destination, relative, force)
            success &= self._copy(path, destination, relative)
        if success:
            self._log.info("=> All copies have been set up")
        else:
            self._log.error("Some copies were not successfully set up")
        return success

    def _default_source(self, destination, source):
        """
        Sets the default source if the value is empty, the configured source otherwise.

        If the source for a copy is null, it uses the basename of the destination.
        Leading dot characters in the basename will be stripped.

        Allows simplified configuration files by avoiding unnecessary duplicate values.

        :param destination: The copy destination
        :param source: The copy source
        :return: The source string
        """
        if source is None:
            basename = os.path.basename(destination)
            if basename.startswith("."):
                return basename[1:]
            else:
                return basename
        else:
            return source

    def _is_copy(self, path):
        """
        Checks if the specified path already exists.

        :param path: The path to check
        :return: True if the path is a copy
        """
        return os.path.isfile(os.path.expanduser(path))

    def _copy_destination(self, path):
        """
        Gets the destination of the specified copy.

        :param path: The copy to get the destination of
        :return: The copy destination
        """
        path = os.path.expanduser(path)
        return path

    def _exists(self, path):
        """
        Checks if the specified path exists.

        :param path: The path to check
        :return: True if the path exists, False for broken symbolic copies otherwise
        """
        path = os.path.expanduser(path)
        return os.path.exists(path)

    def _create(self, path):
        """
        Creates a copy to the specified path.

        :param path: The path to the copy to create
        :return: True if the copy has been created successfully, False otherwise
        """
        success = True
        parent = os.path.abspath(os.path.join(os.path.expanduser(path), os.pardir))
        if not self._exists(parent):
            try:
                os.makedirs(parent)
            except OSError:
                self._log.warning('Failed to create directory "{}"'.format(parent))
                success = False
            else:
                self._log.lowinfo('Creating directory "{}"'.format(parent))
        return success

    def _delete(self, source, path, relative, force):
        """
        Deletes the specified path.

        :param source: The copy source
        :param path: The path to delete
        :param relative: Flag to indicate if the specified parameters are relative
        :param force: Flag to indicate if the deletion should be forced
        :return: True if the path has been deleted successfully, False otherwise
        """
        success = True
        source = os.path.join(self._context.snowblock_dir(), source)
        fullpath = os.path.expanduser(path)
        if relative:
            source = self._relative_path(source, fullpath)
        if (self._is_copy(path) and self._copy_destination(path) != source) or (
            self._exists(path) and not self._is_copy(path)
        ):
            removed = False
            try:
                if os.path.iscopy(fullpath):
                    os.uncopy(fullpath)
                    removed = True
                elif force:
                    if os.path.isdir(fullpath):
                        shutil.rmtree(fullpath)
                        removed = True
                    else:
                        os.remove(fullpath)
                        removed = True
            except OSError:
                self._log.warning("Failed to remove {}".format(path))
                success = False
            else:
                if removed:
                    self._log.lowinfo("Removing {}".format(path))
        return success

    def _relative_path(self, source, destination):
        """
        Gets the relative path to get to the source path from the destination path.

        :param source: The source path
        :param destination: The destination path
        :return: The relative path
        """
        destination_dir = os.path.dirname(destination)
        return os.path.relpath(source, destination_dir)

    def _copy(self, source, copy_name, relative):
        """
        Copies the specified copy_name to the source.

        :param source: The source path to get copyed
        :param copy_name: The name of the copy to copy
        :return: True if the copy has been copyed successfully, False otherwise
        """
        success = False
        destination = os.path.expanduser(copy_name)
        absolute_source = os.path.join(self._context.snowblock_dir(), source)
        if relative:
            source = self._relative_path(absolute_source, destination)
        else:
            source = absolute_source
        if (
            not self._exists(copy_name)
            and self._is_copy(copy_name)
            and self._copy_destination(copy_name) != source
        ):
            self._log.warning(
                "Invalid copy {} -> {}".format(
                    copy_name, self._copy_destination(copy_name)
                )
            )
        elif not self._exists(copy_name) and self._exists(absolute_source):
            try:
                if os.path.isdir(source):
                    if source.endswith("/"):
                        source = source[:-1]
                    if destination.endswith("/"):
                        destination = destination[:-1]
                    copy_tree(source, destination)
                else:
                    shutil.copy(source, destination)
                self._log.lowinfo("Creating copy {} -> {}".format(copy_name, source))
                success = True
            except OSError as e:
                self._log.warning(
                    "Copy failed {} -> {}: {}".format(copy_name, source, e)
                )
        elif self._exists(copy_name) and not self._is_copy(copy_name):
            self._log.warning(
                "{} already exists but is a regular file or directory".format(copy_name)
            )
        elif self._is_copy(copy_name) and self._copy_destination(copy_name) != source:
            self._log.warning(
                "File already exists for copy {} -> {}".format(
                    copy_name, self._copy_destination(copy_name)
                )
            )
        elif not self._exists(absolute_source):
            if self._is_copy(copy_name):
                self._log.warning(
                    "Nonexistent target {} -> {}".format(copy_name, source)
                )
            else:
                self._log.warning(
                    "Nonexistent target for {} : {}".format(copy_name, source)
                )
        else:
            self._log.lowinfo("Link already exists {} -> {}".format(copy_name, source))
            success = True
        return success
