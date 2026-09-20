import hashlib
import importlib.util
import json
import os
from pathlib import Path
import unittest
from unittest.mock import Mock, patch
from urllib.parse import parse_qs, urlparse


class ConditionalCheckFailedException(Exception):
    pass


class SubscribeTest(unittest.TestCase):
    def setUp(self):
        self.ses = Mock()
        self.dynamodb = Mock()
        self.table = self.dynamodb.Table.return_value
        self.dynamodb.meta.client.exceptions.ConditionalCheckFailedException = (
            ConditionalCheckFailedException
        )

        # Mock factories before import: the Lambda creates AWS clients globally.
        spec = importlib.util.spec_from_file_location(
            "subscribe_lambda", Path(__file__).with_name("lambda_function.py")
        )
        self.handler = importlib.util.module_from_spec(spec)
        with (
            patch.dict(os.environ, {"TABLE_NAME": "test-subscribers"}),
            patch("boto3.client", return_value=self.ses),
            patch("boto3.resource", return_value=self.dynamodb),
        ):
            spec.loader.exec_module(self.handler)

    def subscribe(self, email):
        return self.handler.lambda_handler(
            {"body": json.dumps({"email": email})}, None
        )

    def assert_success(self, response):
        self.assertEqual(response["statusCode"], 200)
        self.assertEqual(json.loads(response["body"]), {"success": True})

    def test_subscription_stores_hash_and_emails_confirmation_link(self):
        token = "test-confirmation-token"
        now = 1_800_000_000
        with (
            patch.object(self.handler.secrets, "token_urlsafe", return_value=token),
            patch.object(self.handler.time, "time", return_value=now),
        ):
            response = self.subscribe(" Reader+Blog@Example.COM ")

        self.assert_success(response)
        self.dynamodb.Table.assert_called_once_with("test-subscribers")
        self.table.put_item.assert_called_once_with(
            Item={
                "email": "reader+blog@example.com",
                "status": "pending",
                "token_hash": hashlib.sha256(token.encode()).hexdigest(),
                "created_at": now,
                "token_expires_at": now + 24 * 60 * 60,
            },
            ConditionExpression="attribute_not_exists(email) OR #status <> :subscribed",
            ExpressionAttributeNames={"#status": "status"},
            ExpressionAttributeValues={":subscribed": "subscribed"},
        )
        self.ses.send_email.assert_called_once()
        message = self.ses.send_email.call_args.kwargs
        self.assertEqual(message["FromEmailAddress"], "newsletter@kuniga.me")
        self.assertEqual(
            message["Destination"], {"ToAddresses": ["reader+blog@example.com"]}
        )
        body = message["Content"]["Simple"]["Body"]["Text"]["Data"]
        link = next(line for line in body.splitlines() if line.startswith("https://"))
        parsed = urlparse(link)
        self.assertEqual(parsed.netloc, "newsletter.kuniga.me")
        self.assertEqual(parsed.path, "/confirm")
        self.assertEqual(
            parse_qs(parsed.query),
            {"email": ["reader+blog@example.com"], "token": [token]},
        )

    def test_missing_or_invalid_email_has_no_aws_side_effects(self):
        events = [{}, {"body": "{}"}]
        events.extend(
            {"body": json.dumps({"email": email})}
            for email in ["", "   ", "not-an-email"]
        )
        for event in events:
            with self.subTest(event=event):
                response = self.handler.lambda_handler(event, None)
                self.assertEqual(response["statusCode"], 400)
                self.assertEqual(json.loads(response["body"]), {"error": "Invalid email"})
                self.table.put_item.assert_not_called()
                self.ses.send_email.assert_not_called()

    def test_already_subscribed_returns_success_without_sending_email(self):
        self.table.put_item.side_effect = ConditionalCheckFailedException()

        self.assert_success(self.subscribe("reader@example.com"))

        self.table.put_item.assert_called_once()
        self.ses.send_email.assert_not_called()

    def test_database_failure_propagates_without_sending_email(self):
        self.table.put_item.side_effect = RuntimeError("Database unavailable")

        with self.assertRaisesRegex(RuntimeError, "Database unavailable"):
            self.subscribe("reader@example.com")

        self.ses.send_email.assert_not_called()

    def test_email_failure_propagates_instead_of_returning_success(self):
        self.ses.send_email.side_effect = RuntimeError("Email service unavailable")

        with self.assertRaisesRegex(RuntimeError, "Email service unavailable"):
            self.subscribe("reader@example.com")

        self.table.put_item.assert_called_once()
        self.ses.send_email.assert_called_once()


if __name__ == "__main__":
    unittest.main()
