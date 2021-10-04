import unittest

from {{ cookiecutter.package_name }}.run import main

class TestMain(unittest.TestCase):

    def test_run_main(self):
        main()
