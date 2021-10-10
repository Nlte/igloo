import unittest

from {{ cookiecutter.package_name }}.main import main

class TestMain(unittest.TestCase):

    def test_main(self):
        main()
