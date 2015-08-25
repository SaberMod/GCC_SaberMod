#!/usr/bin/env python
#
# Copyright (C) 2015 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
from __future__ import print_function

import argparse
import multiprocessing
import os
import subprocess
import sys


def android_path(path=''):
    top = os.getenv('ANDROID_BUILD_TOP', '')
    if not top:
        sys.exit('ANDROID_BUILD_TOP not set. Cannot continue.\n'
                 'Please set ANDROID_BUILD_TOP to point to the root of an '
                 'Android tree.')
    return os.path.realpath(os.path.join(top, path))


def get_default_package_dir():
    return android_path('out/ndk')


def get_default_host():
    if sys.platform in ('linux', 'linux2'):
        return 'linux'
    elif sys.platform == 'darwin':
        return 'darwin'
    else:
        raise RuntimeError('Unsupported host: {}'.format(sys.platform))


ALL_TOOLCHAINS = (
    'arm-linux-androideabi', 'aarch64-linux-android',
    'mipsel-linux-android', 'mips64el-linux-android',
    'x86', 'x86_64',
)


class ArgParser(argparse.ArgumentParser):
    def __init__(self):
        super(ArgParser, self).__init__(
            description='Builds GCC for Android.')

        self.add_argument(
            '--host', choices=('darwin', 'linux', 'windows', 'windows64'),
            help='Build binaries for given OS (e.g. linux).')
        self.add_argument(
            '--package-dir', help='Directory to place the packaged GCC.',
            default=get_default_package_dir())
        self.add_argument(
            '--toolchain', choices=ALL_TOOLCHAINS,
            help='Toolchain to build. Builds all if not present.')


def main():
    args = ArgParser().parse_args()

    os.chdir(android_path('toolchain/gcc'))

    toolchain_path = android_path('toolchain')
    ndk_path = android_path('prebuilts/ndk/current')

    GCC_VERSION = '4.9'
    jobs_arg = '-j{}'.format(multiprocessing.cpu_count() * 2)

    package_dir_arg = '--package-dir={}'.format(args.package_dir)

    toolchains = ALL_TOOLCHAINS
    if args.toolchain is not None:
        toolchains = [args.toolchain]

    host = args.host
    if host is None:
        host = get_default_host()

    mingw_arg = ''
    if host in ('windows', 'windows64'):
        mingw_arg = '--mingw'

    try_64_arg = '--try-64'
    if host == 'windows':
        try_64_arg = '--try-64'

    ndk_build_tools_path = android_path('prebuilts/ndk/current/build/tools')
    build_env = dict(os.environ)
    build_env['NDK_BUILDTOOLS_PATH'] = ndk_build_tools_path
    build_env['ANDROID_NDK_ROOT'] = ndk_path

    print('Building {} toolchains: {}'.format(host, ' '.join(toolchains)))
    for toolchain in toolchains:
        toolchain_name = '-'.join([toolchain, GCC_VERSION])
        build_cmd = [
            'bash', 'build-gcc.sh', toolchain_path, ndk_path, toolchain_name,
            package_dir_arg, '--verbose', try_64_arg, mingw_arg, jobs_arg,
        ]

        subprocess.check_call(build_cmd, env=build_env)

if __name__ == '__main__':
    main()
