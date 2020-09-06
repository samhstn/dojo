import pytest

def hello(name):
    return 'hello %s!' % name

def test_hello():
    assert hello('world') == 'hello world!'
