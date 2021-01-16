import re
import sys


s3_uri = '{{ cookiecutter.s3_uri }}'
if not s3_uri.startswith('[OPTIONAL]'):
    if (not s3_uri.startswith('s3://')
        or not s3_uri.endswith('/')):
        print('s3_uri must start with "s3://" and end with "/"')
        print('examples:')
        print('s3://my-bucket-eu-central-1/')
        print('s3://my-bucket-eu-central-1/prefix1/prefix2/')
        sys.exit(1)
