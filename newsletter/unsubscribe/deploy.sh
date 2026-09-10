#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

FUNCTION_NAME="newsletter-unsubscribe"
REGION="us-west-2"
PROFILE="personal"

echo "Building Lambda package..."

rm -rf package function.zip
mkdir package

uv pip install \
  --target package \
  --python 3.14 \
  --python-platform x86_64-manylinux2014 \
  cryptography

cp lambda_function.py package/
cp unsubscribe-public.pem package/

echo "Creating ZIP..."

(
  cd package
  zip -qr ../function.zip .
)

echo "Deploying $FUNCTION_NAME..."

aws lambda update-function-code \
  --function-name "$FUNCTION_NAME" \
  --zip-file fileb://function.zip \
  --region "$REGION" \
  --profile "$PROFILE"

echo "Done."
