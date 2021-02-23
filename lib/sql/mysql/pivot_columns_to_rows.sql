/* 

Input

id | a  | b  | c 
-: | :- | :- | :-
 1 | a1 | b1 | c1
 2 | a2 | b2 | c2

Output
id | col1 | col2
-: | :--- | :---
 1 | a1   | a   
 1 | b1   | b   
 1 | c1   | c   
 2 | a2   | a   
 2 | b2   | b   
 2 | c2   | c

*/

-- MySQL >= 8.0.19
SELECT t.id, x.*
FROM mytable t
CROSS JOIN lateral (VALUES 
    row(a, 'a'), 
    row(b, 'b'),
    row(c, 'c')
) AS x(col1, col2);

-- MySQL < 8.0.19

SELECT t.id,
  c.col,
  CASE c.col
    WHEN 'a' THEN a
    WHEN 'b' THEN b
    WHEN 'c' THEN c
  END AS data
FROM yourtable t -- can be a subquery
CROSS JOIN
(
  SELECT 'a' AS col
  UNION ALL SELECT 'b'
  UNION ALL SELECT 'c'
) c


SELECT
  c.col,
  CASE c.col
  	WHEN 'url' THEN CONCAT('url = ', url)
  	WHEN 'consumer_token' THEN CONCAT('consumer_token = ', consumer_token)
  	WHEN 'consumer_secret' THEN CONCAT('consumer_secret = ', consumer_secret)
    WHEN 'access_token' THEN CONCAT('access_token = ', access_token)
    WHEN 'access_secret' THEN CONCAT('access_secret = ', access_secret)
  END AS data
FROM (
    SELECT
    CONCAT('https://', a.domain, '/') AS "url",
    oc.consumer_key as "consumer_token",
    oc.consumer_secret as "consumer_secret",
    ot.token as "access_token",
    ot.token_secret as "access_secret"
    FROM oauth_tokens ot
    INNER JOIN oauth_consumers oc on oc.id = ot.consumerId
    INNER JOIN accounts a ON a.id = ot.accountId
    WHERE ot.token = '{token}'
) t
CROSS JOIN
(
  SELECT 'url' AS col
  UNION ALL SELECT 'consumer_token'
  UNION ALL SELECT 'consumer_secret'
  UNION ALL SELECT 'access_token'
  UNION ALL SELECT 'access_secret'
) c;
