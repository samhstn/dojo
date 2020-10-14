### Python

### Requirements

[Python 3](https://formulae.brew.sh/formula/python@3.8).

Check we have it installed with:

```bash
$ python3 --version
Python 3.8.5
```

### Quick Start

```bash
# jump in to this directory
cd python

# create a venv directory if it doesn't exist
python3 -m venv venv

# activate the virtual environment
source venv/bin/activate

# install the dependencies
pip install -r requirements.txt

# run the tests
pytest index.py

# watch the tests:
watchmedo shell-command --patterns='*.py' --command='pytest index.py' .

# when finished, we deactivate our virtual environment
deactivate
```

[back](..)
