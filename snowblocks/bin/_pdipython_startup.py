import pandas as pd
import sys

if len(sys.argv[1]) < 2:
        print('usage: pdipython <file>')
        sys.exit(1)

f = sys.argv[1]
if f.endswith(('.xlsx', '.xls')):
        df = pd.read_excel(f)
else:
        df = pd.read_csv(f, sep=';')
print(df.head())
