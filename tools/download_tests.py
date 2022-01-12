import hashlib
import io
import os
import sys
import urllib
import zipfile

OUTDIR = sys.argv[1]

def file_already_downloaded(filename, expected_hash):
    try:
        with open(os.path.join(OUTDIR, filename), 'rb') as file:
            return hashlib.sha256(file.read()).hexdigest() == expected_hash
    except FileNotFoundError:
        return False

if not (file_already_downloaded(
    'zexdoc.com',
    'ae5caf315612ec8814272583f5ca3fa64ddbb10340c0c53899ac334bcb330e10',
) and file_already_downloaded(
    'zexall.com',
    'd2242b91ee8845102cab989163cafd24baa64290b3d16485a90caab435763a67',
)):
    with urllib.request.urlopen('https://mdfs.net/Software/Z80/Exerciser/CPM.zip') as download:
        archive = zipfile.ZipFile(io.BytesIO(download.read()))
        archive.extract('zexdoc.com', OUTDIR)
        archive.extract('zexall.com', OUTDIR)
