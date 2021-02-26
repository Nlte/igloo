SELECT reltuples AS "row_count"
FROM pg_class
WHERE relname = 'table_name';
