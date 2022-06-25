#!/bin/bash
SQL='WITH human AS (
  SELECT h.id, h.value as hv, w.value as wv
  FROM height h JOIN weight w ON h.id = w.id
  WHERE h.value=180
)
SELECT
  1.0*COUNT(*)/(SELECT COUNT(*) FROM human)
FROM human
WHERE wv=80'
sqlite3 :memory: -cmd '.mode csv' -cmd '.import height.csv height' -cmd '.import weight.csv weight' "${SQL}"
