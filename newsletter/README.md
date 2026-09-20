# Tests

From the `newsletter` directory, run:

```sh
uv run python -m unittest discover -s subscribe -p 'test_*.py' -v
uv run python -m unittest discover -s unsubscribe -p 'test_*.py' -v
```

The tests mock AWS clients before importing each Lambda, so they do not need
AWS credentials or access AWS services. Unsubscribe tests generate disposable
signing keys in memory and do not read the newsletter's private key.
