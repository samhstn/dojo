import unittest

def hello(name):
    return 'hello %s!' % name

class TestHello(unittest.TestCase):
    def test_basic(self):
        self.assertEquals(hello('world'), 'hello world!')

if __name__ == '__main__':
    unittest.main()
