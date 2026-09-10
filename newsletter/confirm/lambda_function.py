import hashlib
import json
import os
import time

import boto3

dynamodb = boto3.resource("dynamodb")
table = dynamodb.Table(os.environ["TABLE_NAME"])


def response(status_code, message):
    return {
        "statusCode": status_code,
        "headers": {
            "Content-Type": "text/plain; charset=utf-8",
        },
        "body": message,
    }


def lambda_handler(event, context):
    params = event.get("queryStringParameters") or {}

    email = params.get("email", "").strip().lower()
    token = params.get("token", "")

    if not email or not token:
        return response(400, "Invalid confirmation link.")

    result = table.get_item(
        Key={"email": email}
    )

    subscriber = result.get("Item")

    if not subscriber:
        return response(400, "Invalid confirmation link.")

    if subscriber.get("status") != "pending":
        return response(400, "This subscription is no longer pending.")

    token_hash = hashlib.sha256(
        token.encode("utf-8")
    ).hexdigest()

    if token_hash != subscriber.get("token_hash"):
        return response(400, "Invalid confirmation link.")

    if int(time.time()) > int(subscriber["token_expires_at"]):
        return response(400, "This confirmation link has expired.")

    table.update_item(
        Key={"email": email},
        UpdateExpression=(
            "SET #status = :subscribed, confirmed_at = :now "
            "REMOVE token_hash, token_expires_at"
        ),
        ExpressionAttributeNames={
            "#status": "status",
        },
        ExpressionAttributeValues={
            ":subscribed": "subscribed",
            ":now": int(time.time()),
        },
    )

    return response(
        200,
        "Subscription confirmed! You'll receive new posts from kuniga.me.",
    )
