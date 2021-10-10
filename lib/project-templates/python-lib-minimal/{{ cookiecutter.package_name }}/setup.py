from setuptools import setup


def readme():
    with open('README.md') as f:
        return f.read()

def reqs():
    with open('requirements.txt') as f:
        return f.read().splitlines()


setup(
    name='{{ cookiecutter.package_name }}',
    version='0.1',
    description='',
    long_description=readme(),
    keywords='',
    url='',
    author='',
    author_email='',
    license='',
    packages=['{{ cookiecutter.package_name }}'],
    install_requires=reqs(),
    entry_points={
        'console_scripts': ['{{ cookiecutter.package_name }}-main={{ cookiecutter.package_name }}.main:main'],
    },
    include_package_data=True,
    zip_safe=False
)
