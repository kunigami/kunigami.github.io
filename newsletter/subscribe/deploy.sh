#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

FUNCTION_NAME="newsletter-subscribe"
REGION="us-west-2"
PROFILE="personal"

echo "Packaging $FUNCTION_NAME..."

rm -f function.zip
zip -q function.zip lambda_function.py

echo "Deploying $FUNCTION_NAME..."

aws lambda update-function-code \
  --function-name "$FUNCTION_NAME" \
  --zip-file fileb://function.zip \
  --region "$REGION" \
  --profile "$PROFILE"

echo "Done."