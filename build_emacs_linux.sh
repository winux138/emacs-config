#!/usr/bin/env bash

# installed dependencies:
#   aspell
#   aspell-de
#   aspell-en
#   aspell-ro
#   autoconf
#   cmake
#   hunspell
#   hunspell-de-ch
#   hunspell-de-de
#   hunspell-en-us
#   hunspell-ro
#   libenchant-2-dev
#   libgccjit-14-dev
#   libgccjit0
#   libgnutls28-dev
#   libotf-dev
#   libsqlite3-dev
#   libxml2-dev

set -e

# build
if [[ $1 ]];
then
    build_dir=$(readlink -f "$1")
else
    build_dir=$(dirname $(readlink -f "$0"))/build
fi

mkdir -p $build_dir
pushd sources

./autogen.sh git && ./autogen.sh autoconf

#
# For LTO:
# CFLAGS+=-flto -fuse-linker-plugin
#  --enable-link-time-optimization
./configure CFLAGS='-flto -fuse-linker-plugin  -Wall -Wextra -pedantic -O3 -pipe -fipa-pta -fallow-store-data-races -fno-semantic-interposition -fuse-ld=gold' LD="/usr/bin/ld.gold" \
 --disable-gc-mark-trace \
 --enable-link-time-optimization \
 --prefix=$build_dir \
 --sysconfdir=/etc \
 --libexecdir=$build_dir/usr/lib \
 --localstatedir=$build_dir/var \
 --with-modules \
 --without-gconf \
 --without-gsettings \
 --with-x-toolkit=no \
 --without-xaw3d \
 --without-m17n-flt \
 --without-cairo \
 --without-compress-install \
 --with-native-compilation=aot \
 --with-mailutils \
 --with-xft \
 --with-rsvg \
 --with-pgtk \
 --with-gnutls \
 --with-tree-sitter \
 --with-xml2 \
 --with-libotf

 # --with-mps - is related to IGC garbage collector
 # --with-xwidgets \ - it causes crashes with latest GTK and the support has been disabled, compilation fails
 # --with-sound=alsa \

native_comp_options="-O3 -pipe -fipa-pta -fallow-store-data-races -fno-semantic-interposition"
BYTE_COMPILE_EXTRA_FLAGS='--eval "(setq native-comp-speed 3 native-comp-compiler-options $native_comp_options native-comp-async-jobs-number 8)"' make -j$(nproc)

# You may need to run this if 'loaddefs.el' files become corrupt.
pushd "lisp"
make autoloads
popd

make -j$(nproc) install
popd

$build_dir/bin/emacs --eval "(native-compile-async \"~/.emacs.d/elpa/\" t)"

