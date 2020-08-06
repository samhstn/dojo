### Python

### Requirements

[Node](https://formulae.brew.sh/formula/python@3.8).

Check we have it installed with:

```bash
$ python3 --version
Python 3.8.5
```

### Quick Start

```bash
# jump in to this directory
cd python

# activate the virtual environment
source venv/bin/activate

# install the dependencies
pip install -r requirements.txt

# run the tests
python index.py

# watch the tests:
watchmedo shell-command --patterns='*.py' --command='python index.py' .

# when finished, we deactivate our virtual environment
deactivate
```

[back](..)
