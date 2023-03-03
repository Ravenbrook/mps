#!/usr/bin/env python3
#
#                MAKE THE MEMORY MANAGEMENT REFERENCE
#             Gareth Rees, Ravenbrook Limited, 2014-05-23
# 
# $Id$
#
#
# 1. INTRODUCTION
#
# This script builds the Memory Management Reference website from the
# Memory Pool System manual.
#
# The whole build procedure is as follows:
#
# 1. Sync //info.ravenbrook.com/project/mps/master/manual/...
# 2. make html MMREF=1
# 3. Run this script
#
#
# 2. DESIGN
#
# We build the Memory Management Reference out of the Memory Pool
# System manual because:
#
# 1. having a single set of sources makes it easier to work on;
# 2. the glossary is a vital tool in organizing the MPS manual;
# 3. cross-references from the MMRef to the MPS are an opportunity
#    for advertising the latter to readers of the former.
#
#
# 3. DEPENDENCIES
#
# html5lib <https://pypi.python.org/pypi/html5lib>
# six <https://pypi.python.org/pypi/six>

import os
import re
from shutil import copyfile
import sys
from urllib.parse import urljoin
from xml.etree.cElementTree import Element

import html5lib


# 4. CONFIGURATION

# Subdirectories of the MPS manual that belong in the MMRef.
MMREF_DIRS = 'glossary mmref _images _static'.split()

# Top-level files that belong in the MMRef.
MMREF_FILES = dict(index='mmref-index', bib='bib', copyright='mmref-copyright')

# Regular expression matching files to be included in the MMRef.
URL_FILTER_RE = re.compile(r'/html/(?:(?:{})\.html)?(?:#.*)?$|/(?:{})/'.format(
    '|'.join(MMREF_FILES), '|'.join(MMREF_DIRS)))

# Root URL for the MPS manual.
REWRITE_URL = 'https://www.ravenbrook.com/project/mps/master/manual/html/'


def rewrite_links(src, src_base, url_filter, rewrite_base,
                  url_attributes=(('a', 'href'),)):
    """Rewrite URLs and anchors in src and return the result.

    First, src is parsed as HTML. Second, all URLs found in the
    document are resolved relative to src_base and the result passed
    to the functions url_filter. If this returns False, the URL is
    resolved again, this time relative to rewrite_base, and the result
    stored back to the document. Third, all DT elements with an id
    starting "term-X" are rewritten to contain another anchor with id
    "X" (for backwards compatiblity with links to the old manual).
    Finally, the updated document is serialized as HTML and returned.

    The keyword argument url_attributes is a sequence of (tag,
    attribute) pairs that contain URLs to be rewritten.

    """
    tree = html5lib.parse(src, treebuilder='etree')

    for tag, attr in url_attributes:
        for e in tree.iter('{http://www.w3.org/1999/xhtml}' + tag):
            u = e.get(attr)
            if u and not url_filter(urljoin(src_base, u)):
                rewritten = urljoin(rewrite_base, u)
                if u != rewritten:
                    e.set(attr, rewritten)

    id_re = re.compile(r'-([a-z])')
    for dt in tree.iter('{http://www.w3.org/1999/xhtml}dt'):
        id = dt.get('id')
        if id and id.startswith('term-'):
            span = Element('span', id=id_re.sub(r'.\1', id[5:]))
            span.text = dt.text
            dt.text = ''
            for child in list(dt):
                dt.remove(child)
                span.append(child)
            dt.insert(0, span)
            
    tree_walker = html5lib.getTreeWalker('etree')
    html_serializer = html5lib.serializer.HTMLSerializer()
    return u''.join(html_serializer.serialize(tree_walker(tree)))


def newer(src, target):
    """Return True if src is newer (that is, modified more recently) than
    target, False otherwise.

    """
    return (not os.path.isfile(target)
            or os.path.getmtime(target) < os.path.getmtime(src)
            or os.path.getmtime(target) < os.path.getmtime(__file__))


def rewrite_file(src_dir, rel_dir, src_filename, target_path, rewrite_url):
    src_path = os.path.join(src_dir, src_filename)
    if not newer(src_path, target_path):
        return
    print("Rewriting links in {} -> {}".format(src_path, target_path))
    with open(src_path) as file:
        src = file.read()
    src_base = '/{}/'.format(src_dir)
    url_filter = URL_FILTER_RE.search
    rewrite_base = urljoin(rewrite_url, rel_dir + '/')
    result = rewrite_links(src, src_base, url_filter, rewrite_base)
    with open(target_path, 'w') as file:
        file.write(result)


def main(src_root='html', target_root='mmref'):
    for directory in MMREF_DIRS:
        src_dir = os.path.join(src_root, directory)
        target_dir = os.path.join(target_root, directory)
        os.makedirs(target_dir, exist_ok=True)
        for filename in os.listdir(src_dir):
            src_path = os.path.join(src_dir, filename)
            target_path = os.path.join(target_dir, filename)
            if filename.endswith('.html'):
                rewrite_file(src_dir, directory, filename, target_path,
                             REWRITE_URL)
            elif os.path.isfile(src_path):
                copyfile(src_path, target_path)
    for target, src in MMREF_FILES.items():
        rewrite_file(src_root, '', '{}.html'.format(src),
                     os.path.join(target_root, '{}.html'.format(target)),
                     REWRITE_URL)


if __name__ == '__main__':
    main(*sys.argv[1:])


# B. DOCUMENT HISTORY
#
# 2014-05-23 GDR Created.
# 2018-10-29 GDR Port to Python 3 and html5lib 0.9999999.
#
#
# C. COPYRIGHT AND LICENCE
#
# Copyright (c) 2014-2018 Ravenbrook Ltd.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the
#    distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#
# $Id$
