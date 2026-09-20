import base64
import importlib.util
import os
from html.parser import HTMLParser
from pathlib import Path
import unittest
from unittest.mock import Mock, patch
from urllib.parse import urlencode

from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey


class ConditionalCheckFailedException(Exception):
    pass


class FormParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.forms = []
        self.fields = {}

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "form":
            self.forms.append(attrs)
        elif tag == "input":
            self.fields[attrs["name"]] = attrs["value"]


class UnsubscribeTest(unittest.TestCase):
    def setUp(self):
        self.dynamodb = Mock()
        self.table = self.dynamodb.Table.return_value
        self.dynamodb.meta.client.exceptions.ConditionalCheckFailedException = (
            ConditionalCheckFailedException
        )
        # Generate a disposable key; never read the newsletter's private key.
        self.private_key = Ed25519PrivateKey.generate()
        spec = importlib.util.spec_from_file_location(
            "unsubscribe_lambda", Path(__file__).with_name("lambda_function.py")
        )
        self.handler = importlib.util.module_from_spec(spec)
        with (
            patch.dict(os.environ, {"TABLE_NAME": "test-subscribers"}),
            patch("boto3.resource", return_value=self.dynamodb),
        ):
            spec.loader.exec_module(self.handler)
        self.handler.public_key = self.private_key.public_key()

    def signature(self, email):
        signature = self.private_key.sign(email.strip().lower().encode("utf-8"))
        return base64.urlsafe_b64encode(signature).decode("ascii").rstrip("=")

    def request(self, method, fields=None, encoded=False):
        event = {"requestContext": {"http": {"method": method}}}
        if method == "GET":
            event["queryStringParameters"] = fields
        elif method == "POST":
            body = urlencode(fields or {})
            event["body"] = (
                base64.b64encode(body.encode()).decode() if encoded else body
            )
            event["isBase64Encoded"] = encoded
        # Keep the Lambda's request logging out of test output.
        with patch("builtins.print"):
            return self.handler.lambda_handler(event, None)

    def test_signature_verifies_normalized_email_without_base64_padding(self):
        email = " Reader+Blog@Example.COM "
        sig = self.signature(email)
        self.assertNotIn("=", sig)
        self.assertTrue(self.handler.verify_signature(email, sig))

    def test_signature_rejects_tampering_and_malformed_values(self):
        sig = self.signature("reader@example.com")
        other_key = Ed25519PrivateKey.generate()
        wrong_key_sig = base64.urlsafe_b64encode(
            other_key.sign(b"reader@example.com")
        ).decode().rstrip("=")
        for email, value in [
            ("someone-else@example.com", sig),
            ("reader@example.com", wrong_key_sig),
            ("reader@example.com", "a"),
            ("reader@example.com", "not-a-signature"),
            ("reader@example.com", ""),
        ]:
            with self.subTest(email=email, signature=value):
                self.assertFalse(self.handler.verify_signature(email, value))

    def test_get_renders_confirmation_form_without_changing_subscription(self):
        email = 'reader+<tag>&"@example.com'
        sig = self.signature(email)
        response = self.request("GET", {"email": email, "sig": sig})

        self.assertEqual(response["statusCode"], 200)
        self.assertEqual(response["headers"]["Content-Type"], "text/html; charset=utf-8")
        self.assertNotIn(email, response["body"])
        self.assertIn("&lt;tag&gt;&amp;&quot;", response["body"])
        parser = FormParser()
        parser.feed(response["body"])
        self.assertEqual(parser.forms, [{"method": "POST"}])
        self.assertEqual(parser.fields, {"email": email, "sig": sig})
        self.table.update_item.assert_not_called()

    def test_missing_fields_return_400_without_database_writes(self):
        for method in ["GET", "POST"]:
            for fields in [None, {}, {"email": "reader@example.com"}, {"sig": "sig"}]:
                with self.subTest(method=method, fields=fields):
                    self.assertEqual(self.request(method, fields)["statusCode"], 400)
                    self.table.update_item.assert_not_called()

    def test_invalid_signature_returns_403_without_database_writes(self):
        for method in ["GET", "POST"]:
            with self.subTest(method=method):
                response = self.request(
                    method, {"email": "reader@example.com", "sig": "invalid"}
                )
                self.assertEqual(response["statusCode"], 403)
                self.table.update_item.assert_not_called()

    def test_post_unsubscribes_with_plain_or_base64_encoded_body(self):
        email = " Reader+Blog@Example.COM "
        fields = {"email": email, "sig": self.signature(email)}
        for encoded in [False, True]:
            with self.subTest(encoded=encoded):
                self.table.reset_mock()
                response = self.request("POST", fields, encoded=encoded)
                self.assertEqual(response["statusCode"], 200)
                self.assertIn("You have been unsubscribed", response["body"])
                self.table.update_item.assert_called_once_with(
                    Key={"email": "reader+blog@example.com"},
                    UpdateExpression="SET #status = :unsubscribed",
                    ConditionExpression="attribute_exists(email)",
                    ExpressionAttributeNames={"#status": "status"},
                    ExpressionAttributeValues={":unsubscribed": "unsubscribed"},
                )

    def test_missing_subscriber_returns_same_success_response(self):
        fields = {"email": "reader@example.com", "sig": self.signature("reader@example.com")}
        expected = self.request("POST", fields)
        self.table.update_item.side_effect = ConditionalCheckFailedException()
        self.assertEqual(self.request("POST", fields), expected)

    def test_database_failure_propagates_instead_of_returning_success(self):
        self.table.update_item.side_effect = RuntimeError("Database unavailable")
        fields = {"email": "reader@example.com", "sig": self.signature("reader@example.com")}
        with self.assertRaisesRegex(RuntimeError, "Database unavailable"):
            self.request("POST", fields)

    def test_unsupported_or_missing_method_returns_405(self):
        for method in ["PUT", "DELETE", None]:
            with self.subTest(method=method):
                self.assertEqual(self.request(method)["statusCode"], 405)
                self.table.update_item.assert_not_called()


if __name__ == "__main__":
    unittest.main()
