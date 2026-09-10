import html
import json
import os

import boto3

import base64
from pathlib import Path

from cryptography.exceptions import InvalidSignature
from cryptography.hazmat.primitives import serialization

dynamodb = boto3.resource("dynamodb")
table = dynamodb.Table(os.environ["TABLE_NAME"])

PUBLIC_KEY_PATH = Path(__file__).with_name("unsubscribe-public.pem")

public_key = serialization.load_pem_public_key(
    PUBLIC_KEY_PATH.read_bytes()
)

def verify_signature(email, encoded_signature):
    try:
        # We removed "=" padding when creating the URL, so restore it.
        encoded_signature += "=" * (-len(encoded_signature) % 4)
        signature = base64.urlsafe_b64decode(encoded_signature)

        payload = email.strip().lower().encode("utf-8")

        public_key.verify(signature, payload)
        return True

    except (InvalidSignature, ValueError):
        return False

def response(status_code, body, content_type="text/html; charset=utf-8"):
    return {
        "statusCode": status_code,
        "headers": {
            "Content-Type": content_type,
        },
        "body": body,
    }


def lambda_handler(event, context):
    print(json.dumps(event))
    method = event.get("requestContext", {}).get("http", {}).get("method")

    if method == "GET":
        params = event.get("queryStringParameters") or {}
        email = params.get("email", "").strip().lower()
        sig = params.get("sig", "")

        if not email or not sig:
            return response(400, "<p>Invalid unsubscribe link.</p>")

        if not verify_signature(email, sig):
            return response(403, "<p>Invalid unsubscribe link.</p>")

        safe_email = html.escape(email)
        safe_sig = html.escape(sig)

        return response(
            200,
            f"""
<!doctype html>
<html>
  <body>
    <p>Unsubscribe {safe_email} from kuniga.me?</p>

    <form method="POST">
    <form method="POST">
        <input type="hidden" name="email" value="{safe_email}">
        <input type="hidden" name="sig" value="{safe_sig}">
        <button type="submit">Unsubscribe</button>
    </form>
  </body>
</html>
""",
        )

    if method == "POST":
        body = event.get("body", "")

        if event.get("isBase64Encoded"):
            body = base64.b64decode(body).decode("utf-8")

        from urllib.parse import parse_qs

        fields = parse_qs(body)
        email = fields.get("email", [""])[0].strip().lower()
        sig = fields.get("sig", [""])[0]

        if not email or not sig:
            return response(400, "<p>Invalid unsubscribe request.</p>")

        if not verify_signature(email, sig):
            return response(403, "<p>Invalid unsubscribe request.</p>")

        try:
            table.update_item(
                Key={"email": email},
                UpdateExpression="SET #status = :unsubscribed",
                ConditionExpression="attribute_exists(email)",
                ExpressionAttributeNames={
                    "#status": "status",
                },
                ExpressionAttributeValues={
                    ":unsubscribed": "unsubscribed",
                },
            )
        except dynamodb.meta.client.exceptions.ConditionalCheckFailedException:
            # Don't reveal whether an address is subscribed.
            pass

        return response(
            200,
            "<p>You have been unsubscribed from kuniga.me.</p>",
        )

    return response(405, "Method not allowed.")
