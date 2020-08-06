const test = require('tape');

function hello(name) {
  return `hello ${name}!`;
}

test('hello', (t) => {
  t.equal(hello('world'), 'hello world!');
  t.end();
});
