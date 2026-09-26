# Tests

From the `newsletter` directory, run:

```sh
uv run python -m unittest discover -s subscribe -p 'test_*.py' -v
uv run python -m unittest discover -s unsubscribe -p 'test_*.py' -v
uv run python -m unittest discover -s sender -p 'test_*.py' -v
```

The tests mock AWS clients before importing each Lambda, so they do not need
AWS credentials or access AWS services. Unsubscribe tests generate disposable
signing keys in memory and do not read the newsletter's private key.

# Send a test newsletter

In GitHub Actions, select **Send test newsletter**, choose **Run workflow**,
and enter your email address in `recipient`. The workflow uses the existing
AWS role and `UNSUBSCRIBE_PRIVATE_KEY` secret.

Each run sends the newest entry from the live blog feed to that one address,
even if it was sent previously. It does not read or update the subscriber
database. The normal scheduled newsletter is unchanged.

To run locally with AWS credentials and the unsubscribe private key available:

```sh
uv run python sender/send.py --test-recipient you@example.com
```

Test emails use the normal email content, including a working unsubscribe link
for the test recipient.
