#!/usr/bin/env python3

"""
df[cols].apply(lambda x: ','.join(x.dropna().values))
"""

import pandas as pd
import numpy as np

df = pd.DataFrame({
    'col1' : ["a","b",3],
    'col2'  : ["ab",np.nan, 4],
    'col3' : ["w","e", 6]
})

print(df.head())

df['new_col'] = df.apply(lambda x: ','.join(x.dropna().astype(str).values), axis=1)

print(df.head())
