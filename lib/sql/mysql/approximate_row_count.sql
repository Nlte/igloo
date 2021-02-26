SELECT table_rows "row_count"
FROM information_schema.tables
WHERE table_name="{table_name}" AND table_schema="{database_name}";
