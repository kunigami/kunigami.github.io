#!/bin/bash
SQL='WITH dices AS (
  SELECT d1.id, d2.id, d1.value as v1, d2.value as v2
  FROM dice d1, dice d2
)
SELECT
  1.0*COUNT(*)/(SELECT COUNT(*) FROM dices)
FROM dices
WHERE v1=4 and v2=2'
sqlite3 :memory: -cmd '.mode csv' -cmd '.import dice.csv dice' "${SQL}"
