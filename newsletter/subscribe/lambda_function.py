import hashlib
import json
import os
import secrets
import time
import boto3

from urllib.parse import urlencode

ses = boto3.client("sesv2")

dynamodb = boto3.resource("dynamodb")
table = dynamodb.Table(os.environ["TABLE_NAME"])


def lambda_handler(event, context):
    body = json.loads(event.get("body", "{}"))
    email = body.get("email", "").strip().lower()

    if not email or "@" not in email:
        return {
            "statusCode": 400,
            "body": json.dumps({"error": "Invalid email"}),
        }

    confirmation_token = secrets.token_urlsafe(32)

    token_hash = hashlib.sha256(
        confirmation_token.encode("utf-8")
    ).hexdigest()

    now = int(time.time())
    expires_at = now + 24 * 60 * 60  # 24 hours

    table.put_item(
        Item={
            "email": email,
            "status": "pending",
            "token_hash": token_hash,
            "created_at": now,
            "token_expires_at": expires_at,
        }
    )

    params = urlencode({
        "email": email,
        "token": confirmation_token,
    })

    # confirmation_url = f"https://kuniga.me/newsletter/confirm?{params}"
    confirmation_url = (
        "https://6bw9ncflac.execute-api.us-west-2.amazonaws.com/confirm"
        f"?{params}"
    )

    ses.send_email(
        FromEmailAddress="newsletter@kuniga.me",
        Destination={
            "ToAddresses": [email],
        },
        Content={
            "Simple": {
                "Subject": {
                    "Data": "Confirm your subscription to kuniga.me",
                },
                "Body": {
                    "Text": {
                        "Data": (
                            "Thanks for subscribing!\n\n"
                            "Confirm your subscription by visiting:\n\n"
                            f"{confirmation_url}\n\n"
                            "This link expires in 24 hours."
                        ),
                    },
                },
            }
        },
    )

    return {
        "statusCode": 200,
        "body": json.dumps({"success": True}),
    }
