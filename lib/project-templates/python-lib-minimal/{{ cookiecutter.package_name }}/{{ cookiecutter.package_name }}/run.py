from {{ cookiecutter.package_name }}.hello import say_hello


def main():
    print(say_hello())
