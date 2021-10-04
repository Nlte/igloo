import unittest

from {{ cookiecutter.package_name }}.hello import say_hello


class TestHello(unittest.TestCase):

    def test_say_hello_returns_string(self):
        s = say_hello()
        self.assertTrue(isinstance(s, str))
