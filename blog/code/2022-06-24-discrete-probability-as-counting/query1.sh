#!/bin/bash
SQL='SELECT 1.0*COUNT(*)/(SELECT COUNT(*) FROM dice) FROM dice WHERE value = 4'
sqlite3 :memory: -cmd '.mode csv' -cmd '.import dice.csv dice' "${SQL}"
