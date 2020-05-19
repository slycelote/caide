#!/bin/env bash
set -e

chroot_dir=$1
mkdir "$chroot_dir"
chroot_dir=$( cd "$chroot_dir" ; pwd )
user=$(whoami)

this_dir=$( cd $(dirname "${BASH_SOURCE[0]}") ; pwd )
[ -d "$this_dir" ] || exit 255

# Download and verify dependencies.
sudo apt install -y curl

cd /tmp
curl -o ghcup https://downloads.haskell.org/~ghcup/0.1.5/x86_64-linux-ghcup-0.1.5 -L
curl -o ghc-i386.tar.lz https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-i386-deb9-linux.tar.lz -L
curl -o gcc.tar.xz http://ftpmirror.gnu.org/gnu/gcc/gcc-10.1.0/gcc-10.1.0.tar.xz -L
curl -o phantomjs.tar.bz2 https://bitbucket.org/ariya/phantomjs/downloads/phantomjs-2.1.1-linux-x86_64.tar.bz2 -L
curl -o centos-release.rpm http://mirror.centos.org/centos/7/os/x86_64/Packages/centos-release-7-8.2003.0.el7.centos.x86_64.rpm -L
md5sum -c "$this_dir"/MD5SUMS
chmod +x ghcup

# Bootstrap CentOS in chroot. See
# https://zaufi.github.io/administration/2014/06/10/howto-make-a-centos-chroot

sudo apt install -y schroot rpm yum
sudo mkdir /etc/schroot/centos7
sudo cp "$this_dir"/fstab /etc/schroot/centos7
cat "$this_dir"/schroot.conf   | sudo tee -a /etc/schroot/schroot.conf
printf "directory=$chroot_dir\n" | sudo tee -a /etc/schroot/schroot.conf
printf "users=$user\n"           | sudo tee -a /etc/schroot/schroot.conf
sudo rpm --root="$chroot_dir" --rebuilddb
sudo rpm --root="$chroot_dir" -i centos-release.rpm

sudo mkdir -p /etc/pki
sudo ln -s "$chroot_dir"/etc/pki/rpm-gpg /etc/pki/rpm-gpg

sudo yum --installroot="$chroot_dir" update
sudo yum --installroot="$chroot_dir" install centos-release
sudo yum --installroot="$chroot_dir" install yum sudo

sudo rm /etc/pki/rpm-gpg
sudo apt purge rpm yum
sudo apt autoremove --purge

sudo mv centos-release.rpm "$chroot_dir"
printf "$user\tALL=(ALL)\tALL\n" | sudo tee "$chroot_dir"/etc/sudoers.d/99_host-user

schroot -c centos7 -u "$user" --directory / -- bash -c \
    "sudo rm -rf /var/lib/rpm && sudo rpm --rebuilddb && sudo rpm -i /centos-release.rpm && sudo yum install yum sudo"

sudo rm "$chroot_dir"/centos-release.rpm

sudo mkdir -p "$chroot_dir"/home/$user
sudo chown $user:$user -R "$chroot_dir"/home/$user

# Copy binaries, build scripts and configuration into the chroot.

mkdir -p "$chroot_dir"/home/$user/{bin,bin32,conf,.ghcup/bin}
cp -R "$this_dir"/{bin,bin32,conf,guest.sh} "$chroot_dir"/home/$user/
tar -xf phantomjs.tar.bz2
mv phantomjs-2.1.1-linux-x86_64/bin/phantomjs "$chroot_dir"/home/$user/bin
rm -rf phantomjs*
mv ghcup "$chroot_dir"/home/$user/.ghcup/bin
tar -C "$chroot_dir"/home/$user/ -xf gcc.tar.xz
rm gcc.tar.xz
sudo apt install lzip
tar -C "$chroot_dir"/home/$user/ -xf ghc-i386.tar.lz
rm ghc-i386.tar.lz

# Install dependencies inside the chroot.
schroot -c centos7 -u "$user" --directory /home/$user -- ./guest.sh

