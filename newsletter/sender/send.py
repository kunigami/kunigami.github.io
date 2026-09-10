import base64
import feedparser
from pathlib import Path
from urllib.parse import urlencode
from bs4 import BeautifulSoup
from html import escape


from cryptography.hazmat.primitives import serialization

import boto3

REGION = "us-west-2"
TABLE_NAME = "newsletter-subscribers"
MAX_SUBSCRIBERS = 500

dynamodb = boto3.resource("dynamodb", region_name=REGION)

subscribers_table = dynamodb.Table(TABLE_NAME)
ses = boto3.client("sesv2", region_name=REGION)


UNSUBSCRIBE_URL = (
    "https://6bw9ncflac.execute-api.us-west-2.amazonaws.com/unsubscribe"
)

PRIVATE_KEY_PATH = Path(__file__).parent.parent / "unsubscribe-private.pem"

FEED_URL = "https://www.kuniga.me/feed.xml"


def load_unsubscribe_private_key():
    return serialization.load_pem_private_key(
        PRIVATE_KEY_PATH.read_bytes(),
        password=None,
    )


def mark_sent(email, post_id):
    subscribers_table.update_item(
        Key={"email": email},
        UpdateExpression="SET last_sent_post = :post_id",
        ExpressionAttributeValues={
            ":post_id": post_id,
        },
    )

def get_latest_post():
    feed = feedparser.parse(FEED_URL)

    if not feed.entries:
        raise RuntimeError("RSS feed has no entries")

    entry = feed.entries[0]

    return {
        "title": entry.title,
        "url": entry.link,
        "id": entry.get("id", entry.link),
        "content_html": entry["content"][0]["value"],
    }

def make_unsubscribe_url(email: str) -> str:
    private_key = load_unsubscribe_private_key()
    normalized_email = email.strip().lower()

    # Canonical payload. The verifier must construct this identically.
    payload = normalized_email.encode("utf-8")

    signature = private_key.sign(payload)

    # URL-safe base64, without unnecessary "=" padding.
    encoded_signature = (
        base64.urlsafe_b64encode(signature)
        .decode("ascii")
        .rstrip("=")
    )

    query = urlencode(
        {
            "email": normalized_email,
            "sig": encoded_signature,
        }
    )

    return f"{UNSUBSCRIBE_URL}?{query}"

def get_subscribers():
    items = []

    response = subscribers_table.scan(
        FilterExpression="#status = :subscribed",
        ExpressionAttributeNames={
            "#status": "status",
        },
        ExpressionAttributeValues={
            ":subscribed": "subscribed",
        },
    )

    items.extend(response.get("Items", []))

    while "LastEvaluatedKey" in response:
        response = subscribers_table.scan(
            FilterExpression="#status = :subscribed",
            ExpressionAttributeNames={
                "#status": "status",
            },
            ExpressionAttributeValues={
                ":subscribed": "subscribed",
            },
            ExclusiveStartKey=response["LastEvaluatedKey"],
        )

        items.extend(response.get("Items", []))

    return items

def send_email(email, post):
    unsubscribe_url = make_unsubscribe_url(email)

    content_html = post["content_html"]

    content_text = BeautifulSoup(
        content_html,
        "html.parser",
    ).get_text(
        "\n",
        strip=True,
    )

    text_body = f"""
New post on kuniga.me:

{post["title"]}

{content_text}

Read online:
{post["url"]}

Unsubscribe:
{unsubscribe_url}
""".strip()

    html_body = f"""
<!doctype html>
<html>
  <body>
    <h1>{escape(post["title"])}</h1>

    {content_html}

    <p>
      <a href="{escape(post["url"])}">Read online</a>
    </p>

    <hr>

    <p>
      <a href="{escape(unsubscribe_url)}">Unsubscribe</a>
    </p>
  </body>
</html>
""".strip()

    ses.send_email(
        FromEmailAddress="newsletter@kuniga.me",
        Destination={
            "ToAddresses": [email],
        },
        Content={
            "Simple": {
                "Subject": {
                    "Data": post["title"],
                },
                "Body": {
                    "Text": {
                        "Data": text_body,
                    },
                    "Html": {
                        "Data": html_body,
                    },
                },
            },
        },
    )

def main():
    post = get_latest_post()

    subscribers = get_subscribers()

    print(f"Found {len(subscribers)} confirmed subscribers.")

    if len(subscribers) > MAX_SUBSCRIBERS:
        raise RuntimeError(
            f"Refusing to send to {len(subscribers)} subscribers; "
            f"limit is {MAX_SUBSCRIBERS}"
        )


    for subscriber in subscribers:
        email = subscriber["email"]

        if subscriber.get("last_sent_post") == post["id"]:
            print(f"Skipping {email}: already sent.")
            continue

        print(f"Sending to {email}...")
        send_email(email, post)
        mark_sent(email, post["id"])


if __name__ == "__main__":
    main()
