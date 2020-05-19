The purpose of this directory is to build a single executable compatible with
as many Linux distributions as possible. If you just want to install caide on
your system, refer to INSTALL.txt.

To build this kind of "universal" executable, we need to:

* Link all libraries except for glibc statically.
* Build on a system with an old-ish glibc (CentOS 7).

The scripts here do this by installing CentOS in a chroot on a Debian-based
host (see host.sh); they also set up the environment for both 32-bit and
64-bit compilation. But Docker container or a VM would work in a similar way.

